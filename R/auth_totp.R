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
# GERAÇÃO DE CHAVE SECRETA (base32, 160 bits)
# -----------------------------------------------------

gerar_totp_secret <- function(tamanho = 32) {
    
    paste(
        sample(.base32_alphabet, tamanho, replace = TRUE),
        collapse = ""
    )
    
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
totp_verificar <- function(secret_base32, codigo, tempo = Sys.time(),
                           passo = 30, digitos = 6, janela = 1) {
    
    codigo <- trimws(as.character(codigo))
    
    if (!grepl(paste0("^[0-9]{", digitos, "}$"), codigo)) {
        return(FALSE)
    }
    
    contador_atual <- floor(as.numeric(tempo) / passo)
    
    for (desvio in -janela:janela) {
        
        esperado <- tryCatch(
            hotp_gerar(secret_base32, contador_atual + desvio, digitos),
            error = function(e) NA_character_
        )
        
        if (!is.na(esperado) && identical(esperado, codigo)) {
            return(TRUE)
        }
        
    }
    
    FALSE
    
}

# -----------------------------------------------------
# URI DE PROVISIONAMENTO (para QR code / cadastro manual)
# Padrão aceito por Microsoft Authenticator, Google Authenticator etc.
# -----------------------------------------------------

totp_provisioning_uri <- function(login, secret_base32, emissor = "RadarSocial") {
    
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

registrar_auditoria <- function(con, login, metodo, sucesso) {
    
    tryCatch({
        
        DBI::dbExecute(
            con,
            "INSERT INTO login_auditoria (login, metodo, sucesso, datahora) VALUES (?, ?, ?, ?)",
            params = list(
                login,
                metodo,
                as.integer(sucesso),
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            )
        )
        
    }, error = function(e) {
        warning(paste("Falha ao gravar auditoria de login:", e$message))
    })
    
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
# ESQUEMA MULTI-EMPRESA (coluna "distro")
# -----------------------------------------------------
# Instalações que já rodaram uma versão anterior deste app (antes do
# suporte multi-empresa) têm a tabela usuarios_totp sem a coluna
# "distro" — CREATE TABLE IF NOT EXISTS não adiciona colunas a uma
# tabela já existente, então isso é feito à parte. Chame esta função
# uma vez, logo após abrir a conexão SQLite (ver app.R).
garantir_schema_totp <- function(con) {
    
    colunas_totp <- DBI::dbListFields(con, "usuarios_totp")
    
    if (!("distro" %in% colunas_totp)) {
        DBI::dbExecute(con, "ALTER TABLE usuarios_totp ADD COLUMN distro TEXT")
    }
    
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
        "SELECT login, nome, secret_key, distro, ativo FROM usuarios_totp WHERE login = ? AND ativo = 1",
        params = list(login)
    )
    
    sucesso <- FALSE
    dados   <- NULL
    
    if (nrow(registro) == 1) {
        
        valido <- tryCatch(
            totp_verificar(registro$secret_key[1], codigo),
            error = function(e) FALSE
        )
        
        if (valido) {
            
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
            "UPDATE usuarios_totp SET nome = ?, secret_key = ?, distro = ?, ativo = 1 WHERE login = ?",
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