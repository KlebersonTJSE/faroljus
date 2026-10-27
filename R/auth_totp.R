# =====================================================
# R/auth_totp.R
# Autenticação via TOTP (Time-based One-Time Password)
# Compatível com Microsoft Authenticator, Google Authenticator, etc.
# Implementa RFC 4226 (HOTP) e RFC 6238 (TOTP) usando apenas
# 'digest' (HMAC-SHA1) e 'jsonlite' (base64), já usados no app.
# =====================================================

library(digest)

# -----------------------------------------------------
# BASE32 (RFC 4648) - decodificação
# -----------------------------------------------------

.base32_alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567", "")[[1]]

base32_decode <- function(secret) {
    
    secret <- toupper(gsub("[^A-Za-z2-7]", "", secret))
    
    if (nchar(secret) == 0) {
        stop("Chave secreta TOTP vazia ou inválida")
    }
    
    chars <- strsplit(secret, "")[[1]]
    
    bitstring <- paste(
        vapply(chars, function(ch) {
            idx <- match(ch, .base32_alphabet) - 1
            if (is.na(idx)) stop("Caractere inválido na chave secreta TOTP")
            paste(rev(as.integer(intToBits(idx))[1:5]), collapse = "")
        }, character(1)),
        collapse = ""
    )
    
    n_bytes <- floor(nchar(bitstring) / 8)
    
    if (n_bytes == 0) {
        stop("Chave secreta TOTP inválida (muito curta)")
    }
    
    bytes <- vapply(seq_len(n_bytes), function(i) {
        byte_bits <- substr(bitstring, (i - 1) * 8 + 1, i * 8)
        strtoi(byte_bits, base = 2)
    }, integer(1))
    
    as.raw(bytes)
    
}

# -----------------------------------------------------
# BASE32 (RFC 4648) - codificação (sem padding "=", como os
# Authenticators esperam)
# -----------------------------------------------------

base32_encode <- function(bytes) {

    bits <- paste(
        vapply(as.integer(bytes), function(b) {
            paste(rev(as.integer(intToBits(b))[1:8]), collapse = "")
        }, character(1)),
        collapse = ""
    )

    sobra <- (5 - nchar(bits) %% 5) %% 5
    bits  <- paste0(bits, strrep("0", sobra))

    inicios <- seq(1, nchar(bits), by = 5)
    idx <- strtoi(substring(bits, inicios, inicios + 4), base = 2)

    paste(.base32_alphabet[idx + 1], collapse = "")

}

# -----------------------------------------------------
# GERAÇÃO DE CHAVE SECRETA (base32, 160 bits)
# -----------------------------------------------------
# Usa o gerador criptográfico do sistema (openssl::rand_bytes), como no
# FarolJus Lite. A versão anterior usava sample(), cujo gerador
# (Mersenne-Twister) é previsível e afetado por qualquer set.seed() feito
# no processo — não serve para gerar segredo de autenticação.
# -----------------------------------------------------

gerar_totp_secret <- function(n_bytes = 20) {

    base32_encode(openssl::rand_bytes(n_bytes))

}

# -----------------------------------------------------
# CONTADOR -> 8 BYTES BIG-ENDIAN (aritmética em double,
# evita overflow de inteiro de 32 bits do R)
# -----------------------------------------------------

.contador_para_bytes <- function(contador) {
    
    bytes <- integer(8)
    
    for (i in 8:1) {
        bytes[i] <- contador %% 256
        contador <- contador %/% 256
    }
    
    as.raw(bytes)
    
}

# -----------------------------------------------------
# HOTP (RFC 4226) - dynamic truncation
# -----------------------------------------------------

hotp_gerar <- function(secret_base32, contador, digitos = 6) {
    
    chave <- base32_decode(secret_base32)
    msg   <- .contador_para_bytes(contador)
    
    hash <- digest::hmac(
        key       = chave,
        object    = msg,
        algo      = "sha1",
        serialize = FALSE,
        raw       = TRUE
    )
    
    offset <- (as.integer(hash[length(hash)])) %% 16
    
    b <- as.integer(hash[(offset + 1):(offset + 4)])
    
    valor <- bitwAnd(b[1], 0x7f) * 2^24 +
        b[2] * 2^16 +
        b[3] * 2^8 +
        b[4]
    
    codigo <- valor %% (10^digitos)
    
    formatC(codigo, width = digitos, format = "d", flag = "0")
    
}

# -----------------------------------------------------
# TOTP (RFC 6238)
# -----------------------------------------------------

totp_gerar <- function(secret_base32, tempo = Sys.time(), passo = 30, digitos = 6) {
    
    contador <- floor(as.numeric(tempo) / passo)
    hotp_gerar(secret_base32, contador, digitos)
    
}

# Verifica com tolerância de +/- 1 passo (30s) para lidar com
# pequenas diferenças de relógio entre servidor e celular.
#
# Devolve o CONTADOR (passo de 30s) que bateu com o código, ou NA se
# nenhum bateu. Quem chama usa esse contador para impedir que o mesmo
# código seja reaproveitado (ver autenticar_totp(), coluna
# usuarios_totp.ultimo_contador).
totp_contador_valido <- function(secret_base32, codigo, tempo = Sys.time(),
                                 passo = 30, digitos = 6, janela = 1) {

    codigo <- trimws(as.character(codigo))

    if (length(codigo) != 1 || !grepl(paste0("^[0-9]{", digitos, "}$"), codigo)) {
        return(NA_real_)
    }

    contador_atual <- floor(as.numeric(tempo) / passo)

    for (desvio in -janela:janela) {

        esperado <- tryCatch(
            hotp_gerar(secret_base32, contador_atual + desvio, digitos),
            error = function(e) NA_character_
        )

        if (!is.na(esperado) && identical(esperado, codigo)) {
            return(contador_atual + desvio)
        }

    }

    NA_real_

}

totp_verificar <- function(secret_base32, codigo, tempo = Sys.time(),
                           passo = 30, digitos = 6, janela = 1) {

    !is.na(totp_contador_valido(secret_base32, codigo, tempo, passo, digitos, janela))

}

# -----------------------------------------------------
# URI DE PROVISIONAMENTO (para QR code / cadastro manual)
# Padrão aceito por Microsoft Authenticator, Google Authenticator etc.
# -----------------------------------------------------

# Emissor: nome exibido no app Authenticator ao lado do login.
# Configurável via TOTP_EMISSOR no .Renviron (padrão "Farol Jus"). Trocar
# o emissor NÃO invalida cadastros já feitos (o código depende só da
# chave secreta) — só muda o rótulo dos cadastros novos ou refeitos.
TOTP_EMISSOR_PADRAO <- "Farol Jus"

totp_provisioning_uri <- function(login, secret_base32,
                                  emissor = Sys.getenv("TOTP_EMISSOR", unset = TOTP_EMISSOR_PADRAO)) {
    
    if (!nzchar(trimws(emissor))) {
        emissor <- TOTP_EMISSOR_PADRAO
    }
    
    
    paste0(
        "otpauth://totp/",
        utils::URLencode(paste0(emissor, ":", login), reserved = TRUE),
        "?secret=", secret_base32,
        "&issuer=", utils::URLencode(emissor, reserved = TRUE),
        "&algorithm=SHA1&digits=6&period=30"
    )
    
}

# -----------------------------------------------------
# QR CODE (opcional) - usa o pacote 'qrcode' se disponível.
# Se não estiver instalado, retorna NULL e a UI mostra a
# chave secreta para entrada manual (todo authenticator suporta).
# -----------------------------------------------------

gerar_qrcode_base64 <- function(texto) {
    
    if (!requireNamespace("qrcode", quietly = TRUE)) {
        return(NULL)
    }
    
    qr <- qrcode::qr_code(texto)
    
    arquivo_tmp <- tempfile(fileext = ".png")
    
    grDevices::png(arquivo_tmp, width = 260, height = 260, bg = "white")
    graphics::par(mar = c(0, 0, 0, 0))
    plot(qr)
    grDevices::dev.off()
    
    bytes <- readBin(arquivo_tmp, "raw", file.info(arquivo_tmp)$size)
    unlink(arquivo_tmp)
    
    paste0("data:image/png;base64,", jsonlite::base64_enc(bytes))
    
}

# =====================================================
# FUNÇÕES DE BANCO (usam a conexão SQLite já aberta em app.R)
# =====================================================

# -----------------------------------------------------
# AUDITORIA DE LOGIN (tabela login_auditoria)
# -----------------------------------------------------

# A data/hora é gravada SEMPRE em UTC, no formato ISO 8601 com "Z"
# (ex.: 2026-10-15T13:00:00Z) — o SQLite entende esse formato em
# date()/julianday(). Antes era gravada na hora local do servidor sem
# fuso (mesma correção do FarolJus Lite). O login é limitado a 100
# caracteres: o campo vem do navegador e não deve inflar o banco.
registrar_auditoria <- function(con, login, metodo, sucesso) {

    tryCatch({

        DBI::dbExecute(
            con,
            "INSERT INTO login_auditoria (login, metodo, sucesso, datahora) VALUES (?, ?, ?, ?)",
            params = list(
                substr(trimws(as.character(login %||% "")), 1, 100),
                metodo,
                as.integer(isTRUE(sucesso)),
                format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
            )
        )

    }, error = function(e) {
        warning(paste("Falha ao gravar auditoria de login:", e$message))
    })

}

# Fuso usado para EXIBIR a auditoria (e classificar Manhã/Tarde/Noite).
FUSO_AUDITORIA <- Sys.getenv("FUSO_AUDITORIA", unset = "America/Maceio")

# Converte a coluna datahora da auditoria para POSIXct no fuso local.
# Aceita os dois formatos que podem existir no banco:
#   - novo:   "2026-10-15T13:00:00Z"  (UTC)
#   - antigo: "2026-10-15 10:00:00"   (hora local de quem gravou — é
#             interpretado em FUSO_AUDITORIA, como o código antigo fazia)
converter_datahora_auditoria <- function(x) {

    x <- as.character(x)
    eh_utc <- !is.na(x) & grepl("Z$", x)

    segundos <- rep(NA_real_, length(x))

    segundos[eh_utc] <- as.numeric(as.POSIXct(
        x[eh_utc], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
    ))

    segundos[!eh_utc] <- as.numeric(as.POSIXct(
        x[!eh_utc], format = "%Y-%m-%d %H:%M:%S", tz = FUSO_AUDITORIA
    ))

    as.POSIXct(segundos, origin = "1970-01-01", tz = FUSO_AUDITORIA)

}

# -----------------------------------------------------
# BLOQUEIO POR TENTATIVAS (força bruta)
# -----------------------------------------------------
# Um código TOTP tem só 6 dígitos; sem limite, um script consegue testar
# códigos sem parar até acertar — e, no login AD, testar senhas de um
# servidor pelo app. Depois de LOGIN_MAX_FALHAS falhas para o MESMO login
# (qualquer método) dentro de LOGIN_JANELA_MIN minutos, contadas a partir
# do último acesso bem-sucedido, o login fica bloqueado até a janela
# passar. As tentativas feitas durante o bloqueio são auditadas com
# método "BLOQUEADO" e não prolongam o bloqueio. (Mesma regra do FarolJus
# Lite.)
# -----------------------------------------------------

LOGIN_MAX_FALHAS <- suppressWarnings(as.integer(Sys.getenv("LOGIN_MAX_FALHAS", unset = "5")))
if (is.na(LOGIN_MAX_FALHAS) || LOGIN_MAX_FALHAS < 1) LOGIN_MAX_FALHAS <- 5L

LOGIN_JANELA_MIN <- suppressWarnings(as.integer(Sys.getenv("LOGIN_JANELA_MIN", unset = "15")))
if (is.na(LOGIN_JANELA_MIN) || LOGIN_JANELA_MIN < 1) LOGIN_JANELA_MIN <- 15L

login_bloqueado <- function(con, login) {

    login <- substr(trimws(as.character(login %||% "")), 1, 100)

    falhas <- tryCatch(
        DBI::dbGetQuery(
            con,
            sprintf("
            SELECT COUNT(*) AS n
            FROM login_auditoria
            WHERE login = ?
              AND sucesso = 0
              AND metodo <> 'BLOQUEADO'
              AND julianday(datahora) >= julianday('now', '-%d minutes')
              AND julianday(datahora) > COALESCE((
                    SELECT MAX(julianday(datahora))
                    FROM login_auditoria
                    WHERE login = ? AND sucesso = 1
                  ), 0)
            ", LOGIN_JANELA_MIN),
            params = list(login, login)
        )$n[1],
        error = function(e) 0
    )

    isTRUE(falhas >= LOGIN_MAX_FALHAS)

}

# -----------------------------------------------------
# INTERVALO DE DATAS DA AUDITORIA
# Menor e maior data já registradas em login_auditoria — usado para
# dimensionar o slider de período na aba "Auditoria - Gráfico" (ver
# modules/mod_totp_admin.R).
# -----------------------------------------------------

obter_intervalo_auditoria <- function(con) {
    
    DBI::dbGetQuery(
        con,
        "SELECT MIN(date(datahora)) AS minimo, MAX(date(datahora)) AS maximo FROM login_auditoria"
    )
    
}

# -----------------------------------------------------
# LISTAGEM DE AUDITORIA (com empresa derivada)
# -----------------------------------------------------
# A tabela login_auditoria não tem uma coluna "empresa" própria — o
# login AD grava o método como "AD:<empresa>" (ver observeEvent(input$entrar)
# em app.R), então a empresa é extraída dali. Para login TOTP (método
# apenas "TOTP", sem empresa embutida), a empresa é obtida via LEFT JOIN
# com usuarios_totp pela coluna "distro" do cadastro atual daquele login.
# -----------------------------------------------------

listar_auditoria_login <- function(con, data_inicio, data_fim) {
    
    DBI::dbGetQuery(
        con,
        "
    SELECT
      a.id        AS id,
      a.login     AS login,
      CASE
        WHEN a.metodo LIKE 'AD:%' THEN substr(a.metodo, 4)
        ELSE COALESCE(u.distro, '')
      END         AS empresa,
      a.metodo    AS metodo,
      a.sucesso   AS sucesso,
      a.datahora  AS datahora
    FROM login_auditoria a
    LEFT JOIN usuarios_totp u ON u.login = a.login
    WHERE date(a.datahora) BETWEEN ? AND ?
    ORDER BY a.datahora DESC
    ",
        params = list(
            as.character(data_inicio),
            as.character(data_fim)
        )
    )
    
}

# -----------------------------------------------------
# ESQUEMA DO BANCO (usuarios_totp / login_auditoria)
# -----------------------------------------------------
# Chame esta função uma vez, logo após abrir a conexão SQLite (ver
# app.R). É idempotente — pode rodar a cada início da aplicação:
#
#   1) Cria as tabelas usuarios_totp e login_auditoria se ainda não
#      existirem. Antes, a função assumia que usuarios_totp já existia
#      (dbListFields direto), então um banco novo/vazio derrubava a
#      aplicação na inicialização.
#   2) Migração multi-empresa: instalações de uma versão anterior deste
#      app têm usuarios_totp sem a coluna "distro" — CREATE TABLE IF NOT
#      EXISTS não adiciona colunas a uma tabela já existente, então isso
#      é feito à parte.
#
# Tabelas e dados existentes nunca são recriados nem sobrescritos.
garantir_schema_totp <- function(con) {
    
    DBI::dbExecute(
        con,
        "
    CREATE TABLE IF NOT EXISTS usuarios_totp (
      login      TEXT PRIMARY KEY,
      nome       TEXT NOT NULL,
      secret_key TEXT NOT NULL,
      distro     TEXT,
      ativo      INTEGER NOT NULL DEFAULT 1
    )
    "
    )
    
    DBI::dbExecute(
        con,
        "
    CREATE TABLE IF NOT EXISTS login_auditoria (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      login    TEXT,
      metodo   TEXT,
      sucesso  INTEGER,
      datahora TEXT
    )
    "
    )
    
    colunas_totp <- DBI::dbListFields(con, "usuarios_totp")
    
    if (!("distro" %in% colunas_totp)) {
        DBI::dbExecute(con, "ALTER TABLE usuarios_totp ADD COLUMN distro TEXT")
    }

    # Migração: último contador TOTP aceito para cada login — impede que
    # um código já usado (ex.: visto por cima do ombro) seja reaproveitado
    # dentro da janela de 90s em que ainda seria válido.
    if (!("ultimo_contador" %in% colunas_totp)) {
        DBI::dbExecute(con, "ALTER TABLE usuarios_totp ADD COLUMN ultimo_contador INTEGER")
    }

    # Índices da auditoria: login_bloqueado() roda a cada tentativa de
    # login e filtra por login; a tela de Administração filtra por data.
    # Sem índice, as duas consultas varrem a tabela inteira, que só cresce.
    DBI::dbExecute(
        con,
        "CREATE INDEX IF NOT EXISTS idx_login_auditoria_login ON login_auditoria (login, sucesso)"
    )
    DBI::dbExecute(
        con,
        "CREATE INDEX IF NOT EXISTS idx_login_auditoria_datahora ON login_auditoria (datahora)"
    )

    invisible(TRUE)

}

# -----------------------------------------------------
# AUTENTICAÇÃO TOTP (login + código de 6 dígitos)
# Retorna lista com dados do usuário (incluindo a empresa/"distro"
# associada ao cadastro) em caso de sucesso, ou NULL em caso de falha.
# Sempre grava auditoria.
# -----------------------------------------------------

autenticar_totp <- function(con, login, codigo) {
    
    login <- trimws(login)
    
    registro <- DBI::dbGetQuery(
        con,
        "SELECT login, nome, secret_key, distro, ativo, ultimo_contador FROM usuarios_totp WHERE login = ? AND ativo = 1",
        params = list(login)
    )

    sucesso <- FALSE
    dados   <- NULL

    if (nrow(registro) == 1) {

        contador <- tryCatch(
            totp_contador_valido(registro$secret_key[1], codigo),
            error = function(e) NA_real_
        )

        ultimo <- suppressWarnings(as.numeric(registro$ultimo_contador[1]))

        # Código certo, mas de um passo igual ou anterior ao último já
        # aceito: é reaproveitamento — tratado como falha.
        valido <- !is.na(contador) && (is.na(ultimo) || contador > ultimo)

        if (valido) {

            DBI::dbExecute(
                con,
                "UPDATE usuarios_totp SET ultimo_contador = ? WHERE login = ?",
                params = list(contador, registro$login[1])
            )

            sucesso <- TRUE
            
            dados <- list(
                login       = registro$login[1],
                displayName = registro$nome[1],
                distro      = registro$distro[1]
            )
            
        }
        
    }
    
    registrar_auditoria(con, login, "TOTP", sucesso)
    
    dados
    
}

# -----------------------------------------------------
# CADASTRO / RECADASTRO DE USUÁRIO TOTP
# Gera uma nova chave secreta e grava/atualiza no banco, associando o
# usuário a uma empresa (distro). A empresa é sempre gravada em
# MAIÚSCULAS — ver nota de normalização em distro_env() (R/utils.R) e
# authenticate_ad() (R/auth.R): login AD e login TOTP da mesma empresa
# precisam resolver para o mesmo conjunto de variáveis/subpastas.
# -----------------------------------------------------

cadastrar_usuario_totp <- function(con, login, nome, distro) {
    
    login  <- trimws(login)
    nome   <- trimws(nome)
    distro <- toupper(trimws(distro))
    
    if (login == "" || nome == "") {
        stop("Login e nome são obrigatórios")
    }
    
    if (distro == "") {
        stop("Empresa (distro) é obrigatória para o cadastro TOTP")
    }
    
    secret <- gerar_totp_secret()
    
    existe <- DBI::dbGetQuery(
        con,
        "SELECT login FROM usuarios_totp WHERE login = ?",
        params = list(login)
    )
    
    if (nrow(existe) > 0) {
        
        DBI::dbExecute(
            con,
            # Chave nova => zera o anti-replay (os contadores da chave
            # antiga não têm relação com os da nova).
            "UPDATE usuarios_totp SET nome = ?, secret_key = ?, distro = ?, ativo = 1, ultimo_contador = NULL WHERE login = ?",
            params = list(nome, secret, distro, login)
        )
        
    } else {
        
        DBI::dbExecute(
            con,
            "INSERT INTO usuarios_totp (login, nome, secret_key, distro, ativo) VALUES (?, ?, ?, ?, 1)",
            params = list(login, nome, secret, distro)
        )
        
    }
    
    list(
        login  = login,
        distro = distro,
        secret = secret,
        uri    = totp_provisioning_uri(login, secret)
    )
    
}

desativar_usuario_totp <- function(con, login) {
    
    DBI::dbExecute(
        con,
        "UPDATE usuarios_totp SET ativo = 0 WHERE login = ?",
        params = list(login)
    )
    
}

listar_usuarios_totp <- function(con) {
    
    DBI::dbGetQuery(
        con,
        "SELECT login, nome, distro, ativo FROM usuarios_totp ORDER BY login"
    )
    
}