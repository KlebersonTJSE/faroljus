# =====================================================
# R/tabelas_referencia.R
# -----------------------------------------------------
# Tabelas auxiliares do MPM/CNJ guardadas no SQLite da aplicação e
# usadas para mostrar DESCRIÇÕES no lugar de CÓDIGOS nas telas.
#
# FONTES (arquivos):
#   - tabelas_auxiliares_mpm.xlsx  planilha oficial do MPM, uma aba por
#                                  tabela (cargos, situações, sexo, raça,
#                                  deficiência, cotas, competências...);
#   - municipios.csv               UF;seq_municipio;nome;cod_ibge
#   - competencias.csv             id;descricao
#   - tipos_classificacao_unidade.csv  tipo_unidade;id;descricao
#
#   Cada arquivo pode estar em dois lugares; o primeiro tem prioridade:
#     1) <PASTA_ALERTAS>/referencias/  — enviado pelo "Gerenciar
#        Arquivos" do módulo Tabelas Auxiliares (não versionado);
#     2) referencias/                  — padrão do sistema (versionado;
#        são tabelas públicas, sem dados pessoais).
#   Uma tabela que existe em mais de um arquivo (ex.: Competências, na
#   planilha e no CSV) vem do CSV específico.
#
# BANCO (faroljus.db): uma tabela tab_* por tabela auxiliar, sempre com
# (codigo TEXT PRIMARY KEY, descricao TEXT) e, em algumas, colunas extras
# (sigla, tipo_unidade, uf, cod_ibge). tab_carga_referencia registra de
# qual arquivo cada tabela veio e a assinatura (MD5) do arquivo.
#
# CARGA (carregar_tabelas_referencia(), chamada em app.R e depois de um
# envio pelo Gerenciar Arquivos):
#   - a tabela é (re)carregada do arquivo quando o arquivo de origem MUDA
#     (outra assinatura ou outro arquivo) — o conteúdo é substituído numa
#     transação; enquanto o arquivo não muda, ajustes feitos direto no
#     banco são preservados;
#   - em seguida, o conteúdo do banco vira os vetores usados pelos
#     módulos (CARGO_MAGISTRADO, SITUACAO_PROFISSIONAL_SERVIDOR, REF_*...).
#     Tabela vazia ou ausente: fica o padrão definido no código.
# Qualquer erro só gera um aviso: os códigos continuam aparecendo como
# vieram nos arquivos.
# =====================================================

PASTA_REFERENCIAS <- here::here("referencias")
PASTA_REFERENCIAS_ENVIADAS <- file.path(PASTA_ALERTAS_BASE, "referencias")

# Arquivos de referência aceitos (nome fixo de gravação).
ARQUIVOS_REFERENCIA <- list(
    mpm = list(
        arquivo = "tabelas_auxiliares_mpm.xlsx",
        rotulo = "Tabelas auxiliares do MPM (planilha com uma aba por tabela)"
    ),
    municipios = list(
        arquivo = "municipios.csv",
        rotulo = "Municípios (UF;seq_municipio;nome;cod_ibge)"
    ),
    competencias = list(
        arquivo = "competencias.csv",
        rotulo = "Competências (id;descricao)"
    ),
    classificacao = list(
        arquivo = "tipos_classificacao_unidade.csv",
        rotulo = "Tipo e classificação de unidade judiciária (tipo_unidade;id;descricao)"
    )
)

# Vetores nomeados (nome = código, valor = descrição) das tabelas novas.
# Os de cargo/situação (CARGO_SERVIDOR, SITUACAO_PROFISSIONAL_SERVIDOR,
# CARGO_MAGISTRADO, SITUACAO_PROFISSIONAL_MAGISTRADO) são definidos nos
# módulos e substituídos aqui pelo conteúdo do banco.
REF_AREAS_ATUACAO <- character(0)
REF_ENTRANCIAS <- character(0)
REF_REGIMES_COTAS <- character(0)
REF_DEFICIENCIAS <- character(0)
REF_TIPOS_UNIDADE <- character(0)
REF_SEXOS <- character(0)
REF_IDENTIDADES_GENERO <- character(0)
REF_RACAS_COR <- character(0)
REF_FORMAS_PROMOCAO <- character(0)
REF_COMPETENCIAS <- character(0)
REF_CLASSIFICACOES_UNIDADE <- character(0)
REF_MUNICIPIOS <- character(0)

# =====================================================
# REGISTRO DAS TABELAS
# -----------------------------------------------------
#   tabela     nome no SQLite
#   titulo     nome exibido na tela Tabelas Auxiliares
#   uso        onde a tabela é usada (texto da tela)
#   constante  vetor da aplicação preenchido com o conteúdo
#   aba        aba da planilha do MPM (comparada sem acento/pontuação)
#   csv        arquivo CSV específico (chave de ARQUIVOS_REFERENCIA)
#   extras     colunas extras: nome no banco = coluna de origem
# =====================================================

TABELAS_REFERENCIA <- list(
    list(tabela = "tab_cargo_magistrado", titulo = "Cargos de magistrados",
         uso = "Magistrados (Cargo), Painel e verificações de inconsistência",
         constante = "CARGO_MAGISTRADO", aba = "Cargos magistrados"),
    list(tabela = "tab_situacao_profissional_magistrado", titulo = "Situação profissional atual - magistrados",
         uso = "Magistrados (Situação profissional atual)",
         constante = "SITUACAO_PROFISSIONAL_MAGISTRADO", aba = "Sit. prof. atual magistrado"),
    list(tabela = "tab_forma_promocao_magistrado", titulo = "Forma de promoção de magistrados",
         uso = "Magistrados (Forma de promoção)",
         constante = "REF_FORMAS_PROMOCAO", aba = "Forma de promoção magistrado"),
    list(tabela = "tab_cargo_servidor", titulo = "Cargos de servidores",
         uso = "Pessoal e Auxiliar (Cargo), Painel e verificações de inconsistência",
         constante = "CARGO_SERVIDOR", aba = "Cargos servidores"),
    list(tabela = "tab_situacao_profissional_servidor", titulo = "Situação profissional atual - servidores",
         uso = "Pessoal e Auxiliar (Situação profissional atual)",
         constante = "SITUACAO_PROFISSIONAL_SERVIDOR", aba = "Sit. prof. atual servidor"),
    list(tabela = "tab_area_atuacao", titulo = "Área de atuação (Quadro de Pessoal)",
         uso = "Pessoal e Auxiliar (Área de atuação)",
         constante = "REF_AREAS_ATUACAO", aba = "Área atuação quadro"),
    list(tabela = "tab_sexo", titulo = "Sexo",
         uso = "Magistrados e Pessoal e Auxiliar (Sexo)",
         constante = "REF_SEXOS", aba = "Sexo"),
    list(tabela = "tab_identidade_genero", titulo = "Identidade de gênero",
         uso = "Magistrados e Pessoal e Auxiliar (Identidade de gênero)",
         constante = "REF_IDENTIDADES_GENERO", aba = "Identidade gênero"),
    list(tabela = "tab_raca_cor", titulo = "Raça / Cor",
         uso = "Magistrados e Pessoal e Auxiliar (Raça / Cor)",
         constante = "REF_RACAS_COR", aba = "Raça_cor"),
    list(tabela = "tab_deficiencia", titulo = "Deficiência",
         uso = "Magistrados e Pessoal e Auxiliar (Deficiência)",
         constante = "REF_DEFICIENCIAS", aba = "Deficiência"),
    list(tabela = "tab_regime_cotas", titulo = "Regime de cotas",
         uso = "Magistrados e Pessoal e Auxiliar (Foi aprovado(a) em Regime de Cotas)",
         constante = "REF_REGIMES_COTAS", aba = "Regime de cotas"),
    list(tabela = "tab_competencia", titulo = "Competências do juízo",
         uso = "Serventias (Competências)",
         constante = "REF_COMPETENCIAS", aba = "Competencias juízo", csv = "competencias"),
    list(tabela = "tab_tipo_unidade", titulo = "Tipo de unidade judiciária",
         uso = "Serventias (Tipo de Unidade Judiciária — pela sigla)",
         constante = "REF_TIPOS_UNIDADE", aba = "Tipo unidade judiciária",
         extras = c(sigla = "sigla")),
    list(tabela = "tab_classificacao_unidade", titulo = "Classificação da unidade judiciária",
         uso = "Serventias (Classificação da Unidade Judiciária)",
         constante = "REF_CLASSIFICACOES_UNIDADE", aba = "Classificação unid. judiciária",
         csv = "classificacao", extras = c(tipo_unidade = "tipo")),
    list(tabela = "tab_entrancia", titulo = "Entrância (estrutura do tribunal)",
         uso = paste0("Só consulta: descreve quantas entrâncias o TRIBUNAL tem; a coluna Entrância ",
                      "das serventias (1ª, 2ª...) é outra informação e continua como no arquivo"),
         constante = "REF_ENTRANCIAS", aba = "Entrância"),
    list(tabela = "tab_municipio", titulo = "Municípios",
         uso = "Serventias (Municípios Abrangidos)",
         constante = "REF_MUNICIPIOS", csv = "municipios", extras = c(uf = "uf", cod_ibge = "cod_ibge"))
)

names(TABELAS_REFERENCIA) <- vapply(TABELAS_REFERENCIA, `[[`, character(1), "tabela")

# Texto para comparação de nomes de aba/cabeçalho: minúsculo, sem
# acento, pontuação vira espaço.
normalizar_rotulo_ref <- function(x) {
    x <- tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII"))
    x <- gsub("[^a-z0-9]+", " ", x)
    trimws(x)
}

limpar_descricao_ref <- function(x) {
    x <- stringr::str_squish(as.character(x))
    sub("\\s*;+$", "", x)
}

# Código como texto: sem espaços, sem ".0" e sem zero à esquerda
# ("06" -> "6"); siglas (UJ1, NDR) ficam como estão.
codigo_ref <- function(x) {
    x <- trimws(as.character(x))
    x <- sub("^(\\d+)[.,]0+$", "\\1", x)
    ifelse(!is.na(x) & grepl("^\\d+$", x), sub("^0+(?=\\d)", "", x, perl = TRUE), x)
}

# Monta o data.frame padrão (codigo, descricao, extras...) a partir das
# colunas de origem; descarta linhas sem código.
montar_tabela_ref <- function(codigo, descricao, extras = list()) {
    df <- data.frame(codigo = codigo_ref(codigo), descricao = limpar_descricao_ref(descricao),
                     stringsAsFactors = FALSE)
    for (n in names(extras)) df[[n]] <- trimws(as.character(extras[[n]]))
    df <- df[!is.na(df$codigo) & df$codigo != "" & !is.na(df$descricao) & df$descricao != "", , drop = FALSE]
    df[!duplicated(df$codigo), , drop = FALSE]
}

# ---- Leitura da planilha do MPM -------------------------------------

# Devolve lista nomeada (tabela -> data.frame padrão) com as abas
# reconhecidas da planilha. Uma aba é reconhecida pelo nome (sem
# acento/pontuação); a coluna de código é a primeira "seq_..." e a de
# descrição, a "dsc_...".
ler_planilha_mpm <- function(arquivo) {

    abas <- readxl::excel_sheets(arquivo)
    abas_norm <- normalizar_rotulo_ref(abas)

    out <- list()

    for (t in TABELAS_REFERENCIA) {

        if (is.null(t$aba)) next

        i <- match(normalizar_rotulo_ref(t$aba), abas_norm)
        if (is.na(i)) next

        d <- as.data.frame(
            suppressMessages(readxl::read_excel(arquivo, sheet = abas[i], col_types = "text", .name_repair = "minimal")),
            stringsAsFactors = FALSE
        )
        cab <- tolower(trimws(names(d)))

        col_cod <- which(startsWith(cab, "seq_"))[1]
        col_dsc <- which(startsWith(cab, "dsc_"))[1]
        if (is.na(col_cod) || is.na(col_dsc)) next

        extras <- list()
        if (identical(t$tabela, "tab_tipo_unidade") && "sigla" %in% cab) {
            extras$sigla <- d[[which(cab == "sigla")[1]]]
        }
        if (identical(t$tabela, "tab_classificacao_unidade") && any(startsWith(cab, "tipo_unid"))) {
            extras$tipo_unidade <- d[[which(startsWith(cab, "tipo_unid"))[1]]]
        }

        out[[t$tabela]] <- montar_tabela_ref(d[[col_cod]], d[[col_dsc]], extras)
    }

    out

}

# ---- Leitura dos CSVs ------------------------------------------------

ler_csv_ref_bruto <- function(arquivo) {

    linhas <- readLines(arquivo, encoding = "UTF-8", warn = FALSE)
    if (!all(validUTF8(linhas))) {
        linhas <- iconv(readLines(arquivo, warn = FALSE), "latin1", "UTF-8")
    }
    linhas <- sub("^﻿", "", enc2utf8(linhas))
    linhas <- linhas[nzchar(trimws(linhas))]

    if (length(linhas) < 2) {
        return(NULL)
    }

    partes <- strsplit(linhas, ";", fixed = TRUE)
    cab <- tolower(trimws(partes[[1]]))
    cab <- cab[cab != ""]

    df <- as.data.frame(
        lapply(seq_along(cab), function(i) vapply(partes[-1], function(p) trimws(p[i] %||% NA_character_), character(1))),
        stringsAsFactors = FALSE
    )
    names(df) <- cab
    df

}

# Tipo de um CSV de referência pelo cabeçalho (ver ARQUIVOS_REFERENCIA).
tipo_csv_referencia <- function(cab) {
    cab <- tolower(trimws(cab))
    if (all(c("seq_municipio", "nome") %in% cab)) return("municipios")
    if (all(c("tipo_unidade", "id", "descricao") %in% cab)) return("classificacao")
    if (all(c("id", "descricao") %in% cab)) return("competencias")
    NA_character_
}

ler_csv_referencia <- function(arquivo) {

    d <- ler_csv_ref_bruto(arquivo)
    if (is.null(d)) return(list())

    switch(
        tipo_csv_referencia(names(d)) %||% "",
        municipios = list(tab_municipio = montar_tabela_ref(
            d$seq_municipio, d$nome, list(uf = d$uf, cod_ibge = d$cod_ibge)
        )),
        competencias = list(tab_competencia = montar_tabela_ref(d$id, d$descricao)),
        classificacao = list(tab_classificacao_unidade = montar_tabela_ref(
            d$id, d$descricao, list(tipo_unidade = d$tipo_unidade)
        )),
        list()
    )

}

# Conteúdo (lista tabela -> data.frame) de um arquivo de referência.
ler_arquivo_referencia <- function(arquivo) {
    tryCatch(
        if (identical(tolower(tools::file_ext(arquivo)), "xlsx")) ler_planilha_mpm(arquivo) else ler_csv_referencia(arquivo),
        error = function(e) {
            warning("Não foi possível ler ", basename(arquivo), ": ", conditionMessage(e))
            list()
        }
    )
}

# Identifica um arquivo enviado: "mpm" (planilha com pelo menos 3 abas
# reconhecidas), "municipios", "competencias", "classificacao" ou NA.
identificar_arquivo_referencia <- function(caminho, extensao = tolower(tools::file_ext(caminho))) {

    tryCatch({
        if (identical(extensao, "xlsx")) {
            abas <- normalizar_rotulo_ref(readxl::excel_sheets(caminho))
            conhecidas <- normalizar_rotulo_ref(unlist(lapply(TABELAS_REFERENCIA, `[[`, "aba")))
            if (sum(abas %in% conhecidas) >= 3) "mpm" else NA_character_
        } else if (identical(extensao, "csv")) {
            d <- ler_csv_ref_bruto(caminho)
            if (is.null(d)) NA_character_ else tipo_csv_referencia(names(d))
        } else {
            NA_character_
        }
    }, error = function(e) NA_character_)

}

# ---- Arquivos disponíveis --------------------------------------------

# Caminho do arquivo `chave` (ver ARQUIVOS_REFERENCIA) que vale agora:
# o enviado, se existir; senão o padrão; senão NULL. `local` diz qual.
arquivo_referencia_vigente <- function(chave) {
    nome <- ARQUIVOS_REFERENCIA[[chave]]$arquivo
    enviado <- file.path(PASTA_REFERENCIAS_ENVIADAS, nome)
    padrao <- file.path(PASTA_REFERENCIAS, nome)
    if (file.exists(enviado)) return(list(caminho = enviado, local = "enviado"))
    if (file.exists(padrao)) return(list(caminho = padrao, local = "padrao"))
    NULL
}

# ---- Banco -----------------------------------------------------------

colunas_esperadas_ref <- function(t) c("codigo", "descricao", names(t$extras))

garantir_esquema_referencia <- function(con) {

    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS tab_carga_referencia (
            tabela       TEXT PRIMARY KEY,
            arquivo      TEXT,
            local        TEXT,
            assinatura   TEXT,
            registros    INTEGER,
            carregado_em TEXT
        )")

    for (t in TABELAS_REFERENCIA) {

        esperadas <- colunas_esperadas_ref(t)

        # Tabela de uma versão anterior com outro formato (ex.:
        # tab_cargo_servidor com "nomenclatura", tab_municipio com
        # "seq_municipio"): é recriada no formato novo e recarregada do
        # arquivo de origem.
        if (DBI::dbExistsTable(con, t$tabela) &&
            !identical(DBI::dbListFields(con, t$tabela), esperadas)) {
            DBI::dbExecute(con, sprintf("DROP TABLE %s", t$tabela))
            DBI::dbExecute(con, "DELETE FROM tab_carga_referencia WHERE tabela = ?", params = list(t$tabela))
        }

        extras_sql <- if (length(t$extras) > 0) paste0(", ", paste(names(t$extras), "TEXT", collapse = ", ")) else ""

        DBI::dbExecute(con, sprintf(
            "CREATE TABLE IF NOT EXISTS %s (codigo TEXT PRIMARY KEY, descricao TEXT NOT NULL%s)",
            t$tabela, extras_sql
        ))
    }

}

gravar_tabela_referencia <- function(con, t, df, arquivo, local, assinatura) {

    cols <- colunas_esperadas_ref(t)
    for (c in cols) if (!(c %in% names(df))) df[[c]] <- NA_character_
    df <- df[cols]

    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, sprintf("DELETE FROM %s", t$tabela))
        DBI::dbExecute(
            con,
            sprintf("INSERT INTO %s (%s) VALUES (%s)", t$tabela, paste(cols, collapse = ", "),
                    paste(rep("?", length(cols)), collapse = ", ")),
            params = unname(as.list(df))
        )
        DBI::dbExecute(
            con,
            "INSERT OR REPLACE INTO tab_carga_referencia (tabela, arquivo, local, assinatura, registros, carregado_em)
             VALUES (?, ?, ?, ?, ?, ?)",
            params = list(t$tabela, basename(arquivo), local, assinatura, nrow(df),
                          format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
        )
    })

}

# Vetor (código -> descrição) de uma tabela do banco. Tipo de unidade
# também responde pela sigla (UJ1...), que é o que vem nos arquivos.
vetor_referencia <- function(con, t) {
    d <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", t$tabela))
    if (nrow(d) == 0) return(NULL)
    v <- setNames(as.character(d$descricao), as.character(d$codigo))
    if ("sigla" %in% names(d)) {
        s <- !is.na(d$sigla) & d$sigla != ""
        v <- c(setNames(as.character(d$descricao[s]), toupper(d$sigla[s])), v)
    }
    v
}

carregar_tabelas_referencia <- function(con, forcar = FALSE, ambiente = globalenv()) {

    tryCatch({

        garantir_esquema_referencia(con)

        cargas <- DBI::dbGetQuery(con, "SELECT tabela, arquivo, assinatura FROM tab_carga_referencia")

        # Conteúdo de cada arquivo vigente, lido uma vez.
        vigentes <- Filter(Negate(is.null), lapply(names(ARQUIVOS_REFERENCIA), arquivo_referencia_vigente))
        names(vigentes) <- vapply(vigentes, function(v) basename(v$caminho), character(1))
        conteudo <- lapply(vigentes, function(v) ler_arquivo_referencia(v$caminho))
        assinaturas <- vapply(vigentes, function(v) unname(tools::md5sum(v$caminho)), character(1))

        atualizadas <- character(0)

        for (t in TABELAS_REFERENCIA) {

            # Fonte: o CSV específico (se houver e tiver a tabela); senão
            # a planilha do MPM.
            fontes <- c(
                if (!is.null(t$csv)) ARQUIVOS_REFERENCIA[[t$csv]]$arquivo,
                ARQUIVOS_REFERENCIA$mpm$arquivo
            )
            fonte <- Find(function(f) !is.null(conteudo[[f]][[t$tabela]]), fontes)

            if (is.null(fonte)) next

            atual <- cargas[cargas$tabela == t$tabela, , drop = FALSE]
            vazia <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", t$tabela))$n == 0
            mudou <- nrow(atual) == 0 || !identical(atual$arquivo[1], fonte) ||
                !identical(atual$assinatura[1], assinaturas[[fonte]])

            if (forcar || mudou || vazia) {
                gravar_tabela_referencia(con, t, conteudo[[fonte]][[t$tabela]], fonte,
                                         vigentes[[fonte]]$local, assinaturas[[fonte]])
                atualizadas <- c(atualizadas, t$tabela)
            }
        }

        for (t in TABELAS_REFERENCIA) {
            v <- vetor_referencia(con, t)
            if (!is.null(v)) assign(t$constante, v, envir = ambiente)
        }

        invisible(atualizadas)

    }, error = function(e) {

        warning(
            "Não foi possível preparar as tabelas auxiliares do MPM: ", conditionMessage(e),
            ". Os códigos serão exibidos como vieram (ou com as descrições padrão do sistema)."
        )

        invisible(character(0))

    })

}

# Resumo para a tela Tabelas Auxiliares: uma linha por tabela.
listar_tabelas_referencia <- function(con) {

    cargas <- tryCatch(DBI::dbGetQuery(con, "SELECT * FROM tab_carga_referencia"), error = function(e) NULL)

    do.call(rbind, lapply(TABELAS_REFERENCIA, function(t) {
        n <- tryCatch(DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", t$tabela))$n, error = function(e) 0L)
        c_ <- if (!is.null(cargas)) cargas[cargas$tabela == t$tabela, , drop = FALSE] else NULL
        data.frame(
            tabela = t$tabela,
            titulo = t$titulo,
            uso = t$uso,
            registros = as.integer(n),
            arquivo = if (!is.null(c_) && nrow(c_) > 0) c_$arquivo[1] else NA_character_,
            local = if (!is.null(c_) && nrow(c_) > 0) c_$local[1] else NA_character_,
            carregado_em = if (!is.null(c_) && nrow(c_) > 0) c_$carregado_em[1] else NA_character_,
            stringsAsFactors = FALSE
        )
    }))

}

ler_tabela_referencia <- function(con, tabela) {
    if (!(tabela %in% names(TABELAS_REFERENCIA))) {
        return(data.frame())
    }
    d <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", tabela))
    num <- suppressWarnings(as.numeric(d$codigo))
    d[order(is.na(num), num, d$codigo), , drop = FALSE]
}

# =====================================================
# CÓDIGOS -> DESCRIÇÕES NAS TELAS
# =====================================================

# Troca código(s) pela(s) descrição(ões) da tabela. Aceita vários
# códigos na mesma célula ("366, 1546" ou "366;1546" ou "1 | 2"), com ou
# sem zero à esquerda; o resultado junta as descrições com "; ". Um
# código que não está na tabela é mantido como veio (não esconde dado
# inesperado), e uma célula vazia continua vazia.
codigos_para_nomes <- function(valores, tabela, maiusculas = FALSE) {

    valores <- as.character(valores)

    if (length(tabela) == 0 || length(valores) == 0) {
        return(valores)
    }

    chave <- if (maiusculas) toupper else identity
    names(tabela) <- chave(names(tabela))

    distintos <- unique(valores[!is.na(valores) & trimws(valores) != ""])

    traduzidos <- vapply(distintos, function(v) {
        partes <- trimws(strsplit(v, "[,;|]")[[1]])
        partes <- partes[partes != ""]
        nomes <- unname(tabela[chave(codigo_ref(partes))])
        paste(ifelse(is.na(nomes), partes, nomes), collapse = "; ")
    }, character(1))

    out <- valores
    achou <- !is.na(valores) & valores %in% distintos
    out[achou] <- traduzidos[match(valores[achou], distintos)]
    out

}

# Colunas de cadastro de PESSOAS (magistrados e servidores) traduzidas
# para exibição, além de Cargo e Situação (que cada módulo já traduz).
# As regex são ancoradas no início do nome normalizado, então colunas
# "Alerta: Sexo" (mensagens do MPM) não são tocadas.
REGRAS_DESCRICAO_PESSOAS <- list(
    list(regex = "^area de atuacao", constante = "REF_AREAS_ATUACAO"),
    list(regex = "^sexo$", constante = "REF_SEXOS", maiusculas = TRUE),
    list(regex = "^identidade de genero", constante = "REF_IDENTIDADES_GENERO"),
    list(regex = "^raca", constante = "REF_RACAS_COR"),
    list(regex = "^(tipo de )?deficiencia", constante = "REF_DEFICIENCIAS"),
    list(regex = "^(foi aprovad.*)?regime de cotas", constante = "REF_REGIMES_COTAS"),
    list(regex = "^forma de promocao", constante = "REF_FORMAS_PROMOCAO")
)

traduzir_descricoes_mpm <- function(df, regras = REGRAS_DESCRICAO_PESSOAS) {

    if (is.null(df) || nrow(df) == 0) {
        return(df)
    }

    nomes <- normalizar_nome_coluna(names(df))

    for (r in regras) {
        tabela <- get0(r$constante, envir = globalenv(), ifnotfound = character(0))
        for (i in which(stringr::str_detect(nomes, r$regex))) {
            df[[i]] <- codigos_para_nomes(df[[i]], tabela, maiusculas = isTRUE(r$maiusculas))
        }
    }

    df

}
