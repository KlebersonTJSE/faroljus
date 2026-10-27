# =====================================================
# R/tabelas_referencia.R
# -----------------------------------------------------
# Tabelas de referência do MPM/CNJ guardadas no SQLite da aplicação,
# usadas para mostrar NOMES no lugar de CÓDIGOS (principalmente no
# módulo Serventias):
#
#   tab_municipio               seq_municipio -> nome (UF, código IBGE)
#                               "Municípios Abrangidos" (ex.: 366 -> AREIA BRANCA)
#   tab_competencia             codigo -> descricao
#                               "Competências" (ex.: 1 -> Juízo Único)
#   tab_classificacao_unidade   codigo -> descricao (+ tipo de unidade)
#                               "Classificação da Unidade Judiciária"
#                               (ex.: 109 -> PA - Postos Avançados)
#   tab_tipo_unidade            sigla -> descricao
#                               "Tipo de Unidade Judiciária" (ex.: UJ1)
#
# Origem: os arquivos da pasta referencias/ (versionados — são tabelas
# públicas do MPM, sem dados pessoais):
#   referencias/municipios.csv                 (UF;seq_municipio;nome;cod_ibge)
#   referencias/competencias.csv               (id;descricao)
#   referencias/tipos_classificacao_unidade.csv (tipo_unidade;id;descricao)
#
# Na inicialização (carregar_tabelas_referencia(), chamada em app.R):
#   1) cria as tabelas se não existirem;
#   2) insere os registros dos CSVs que ainda não estão no banco
#      (INSERT OR IGNORE, numa transação) — nunca sobrescreve um ajuste
#      feito direto no banco; para atualizar uma descrição, edite o banco
#      (ou apague a linha e reinicie o app, que ela volta do CSV);
#   3) lê as tabelas para os vetores REF_* abaixo, usados pelos módulos.
# Qualquer erro só gera um aviso: os códigos continuam aparecendo como
# vieram nos arquivos.
# =====================================================

PASTA_REFERENCIAS <- here::here("referencias")

# Vetores nomeados (nome = código, valor = texto exibido). Vazios até
# carregar_tabelas_referencia() rodar: sem eles, nada é traduzido.
REF_MUNICIPIOS <- character(0)
REF_COMPETENCIAS <- character(0)
REF_CLASSIFICACOES_UNIDADE <- character(0)
REF_TIPOS_UNIDADE <- character(0)

# Tipo de Unidade Judiciária: as siglas usadas na tabela de
# classificações. Não vêm em arquivo próprio do MPM; as descrições
# seguem a própria tabela de classificações (unidades de 1º grau, de 2º
# grau/tribunais superiores e áreas de apoio direto à atividade
# judicante). Editáveis no banco, como as demais.
SEMENTE_TIPOS_UNIDADE <- c(
    "UJ1"  = "Unidade Judiciária de 1º grau",
    "UJ2"  = "Unidade Judiciária de 2º grau ou de tribunal superior",
    "AADJ" = "Área de apoio direto à atividade judicante",
    "ND"   = "Não determinado"
)

# Lê um CSV de referência (";", UTF-8 com ou sem BOM). Ignora as colunas
# vazias do fim da linha (alguns arquivos terminam com ";;") e linhas em
# branco. Devolve data.frame de texto, com os nomes do cabeçalho.
ler_csv_referencia <- function(arquivo, colunas) {

    if (!file.exists(arquivo)) {
        warning("Arquivo de referência não encontrado: ", arquivo)
        return(NULL)
    }

    linhas <- readLines(arquivo, encoding = "UTF-8", warn = FALSE)
    linhas <- sub("^﻿", "", enc2utf8(linhas))
    linhas <- linhas[nzchar(trimws(linhas))]

    if (length(linhas) < 2) {
        return(NULL)
    }

    partes <- strsplit(linhas, ";", fixed = TRUE)
    cabecalho <- trimws(partes[[1]])
    idx <- match(colunas, cabecalho)

    if (anyNA(idx)) {
        warning("Colunas ausentes em ", basename(arquivo), ": ", paste(colunas[is.na(idx)], collapse = ", "))
        return(NULL)
    }

    df <- as.data.frame(
        lapply(idx, function(i) vapply(partes[-1], function(p) trimws(p[i] %||% NA_character_), character(1))),
        stringsAsFactors = FALSE
    )
    names(df) <- colunas

    df[!is.na(df[[colunas[1]]]) & df[[colunas[1]]] != "", , drop = FALSE]

}

carregar_tabelas_referencia <- function(con, pasta = PASTA_REFERENCIAS, ambiente = globalenv()) {

    tryCatch({

        mun <- ler_csv_referencia(file.path(pasta, "municipios.csv"), c("UF", "seq_municipio", "nome", "cod_ibge"))
        comp <- ler_csv_referencia(file.path(pasta, "competencias.csv"), c("id", "descricao"))
        clas <- ler_csv_referencia(file.path(pasta, "tipos_classificacao_unidade.csv"), c("tipo_unidade", "id", "descricao"))

        DBI::dbWithTransaction(con, {

            DBI::dbExecute(con, "
                CREATE TABLE IF NOT EXISTS tab_municipio (
                    seq_municipio INTEGER PRIMARY KEY,
                    uf            TEXT,
                    nome          TEXT NOT NULL,
                    cod_ibge      TEXT
                )")

            DBI::dbExecute(con, "
                CREATE TABLE IF NOT EXISTS tab_competencia (
                    codigo    INTEGER PRIMARY KEY,
                    descricao TEXT NOT NULL
                )")

            DBI::dbExecute(con, "
                CREATE TABLE IF NOT EXISTS tab_tipo_unidade (
                    sigla     TEXT PRIMARY KEY,
                    descricao TEXT NOT NULL
                )")

            DBI::dbExecute(con, "
                CREATE TABLE IF NOT EXISTS tab_classificacao_unidade (
                    codigo       INTEGER PRIMARY KEY,
                    tipo_unidade TEXT,
                    descricao    TEXT NOT NULL
                )")

            if (!is.null(mun) && nrow(mun) > 0) {
                DBI::dbExecute(
                    con,
                    "INSERT OR IGNORE INTO tab_municipio (seq_municipio, uf, nome, cod_ibge) VALUES (?, ?, ?, ?)",
                    params = list(as.integer(mun$seq_municipio), mun$UF, mun$nome, mun$cod_ibge)
                )
            }

            if (!is.null(comp) && nrow(comp) > 0) {
                DBI::dbExecute(
                    con,
                    "INSERT OR IGNORE INTO tab_competencia (codigo, descricao) VALUES (?, ?)",
                    params = list(as.integer(comp$id), comp$descricao)
                )
            }

            DBI::dbExecute(
                con,
                "INSERT OR IGNORE INTO tab_tipo_unidade (sigla, descricao) VALUES (?, ?)",
                params = list(names(SEMENTE_TIPOS_UNIDADE), unname(SEMENTE_TIPOS_UNIDADE))
            )

            if (!is.null(clas) && nrow(clas) > 0) {
                DBI::dbExecute(
                    con,
                    "INSERT OR IGNORE INTO tab_classificacao_unidade (codigo, tipo_unidade, descricao) VALUES (?, ?, ?)",
                    params = list(as.integer(clas$id), clas$tipo_unidade, clas$descricao)
                )
            }

        })

        ler <- function(sql) DBI::dbGetQuery(con, sql)
        vetor <- function(df) setNames(as.character(df[[2]]), as.character(df[[1]]))

        assign("REF_MUNICIPIOS", vetor(ler("SELECT seq_municipio, nome FROM tab_municipio")), envir = ambiente)
        assign("REF_COMPETENCIAS", vetor(ler("SELECT codigo, descricao FROM tab_competencia")), envir = ambiente)
        assign("REF_TIPOS_UNIDADE", vetor(ler("SELECT sigla, descricao FROM tab_tipo_unidade")), envir = ambiente)
        assign("REF_CLASSIFICACOES_UNIDADE",
               vetor(ler("SELECT codigo, descricao FROM tab_classificacao_unidade")), envir = ambiente)

        invisible(TRUE)

    }, error = function(e) {

        warning(
            "Não foi possível preparar as tabelas de referência (municípios, competências, ",
            "classificação de unidades): ", conditionMessage(e), ". Os códigos serão exibidos como vieram."
        )

        invisible(FALSE)

    })

}

# Troca código(s) pelo(s) nome(s) da tabela de referência. Aceita vários
# códigos na mesma célula ("366, 1546" ou "366;1546" ou "366 | 1546"),
# com ou sem zero à esquerda; o resultado junta os nomes com "; ". Um
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
        cod <- chave(sub("^0+(?=\\d)", "", sub("[.,]0+$", "", partes), perl = TRUE))
        nomes <- unname(tabela[cod])
        paste(ifelse(is.na(nomes), partes, nomes), collapse = "; ")
    }, character(1))

    out <- valores
    achou <- !is.na(valores) & valores %in% distintos
    out[achou] <- traduzidos[match(valores[achou], distintos)]
    out

}
