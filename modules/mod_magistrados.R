# =====================================================
# modules/mod_magistrados.R
# -----------------------------------------------------
# Módulo "Magistrados" (aba Magistrados do menu principal), com três
# abas — mesma estrutura de mod_serventias.R:
#
#   1) "Base de Dados - Magistrados" — cadastro de magistrados do
#      MPM/CNJ, lido DIRETAMENTE de magistrados.xlsx (ou .csv). A tabela
#      mostra as colunas do arquivo, na mesma ordem (Situação
#      profissional atual e Cargo por extenso, quando houver
#      nomenclatura cadastrada). Filtros: Status (padrão "Ativo", se a
#      coluna existir), CPF, Nome, Cargo, Situação profissional atual,
#      Órgão de lotação e Detalhe (busca o texto em todas as colunas).
#      Gráfico com agrupamento selecionável.
#
#   2) "Alertas - Magistrados" — arquivo de alertas de Magistrados do
#      MPM, alertas_magistrados.csv (ou .xlsx). Filtros Alerta, Conflito,
#      Cargo, CPF e Nome; alertas consolidados na tabela; gráficos por
#      tipo de alerta e por Cargo; CSV.
#
#   3) "Inconsistências - Magistrados" — verificações automáticas sobre a
#      base (ver verificar_inconsistencias_magistrados()): cargo 4 com
#      perfil de Desembargador(a) e Aposentadoria com perfil de
#      afastamento. Tabela com o motivo de cada apontamento e CSV.
#
# As abas 1 e 2 têm Tabela, Gráfico e Gerar arquivo CSV.
#
# Os dois arquivos ficam na subpasta de magistrados da empresa:
#
#   <PASTA_ALERTAS>/<EMPRESA>/magistrados/magistrados.xlsx          (ou .csv)
#   <PASTA_ALERTAS>/<EMPRESA>/magistrados/alertas_magistrados.csv   (ou .xlsx)
#
# Se existirem .xlsx e .csv do mesmo tipo, o .xlsx tem prioridade.
#
# "Gerenciar Arquivos" (comum às duas abas) apaga cada arquivo
# separadamente e envia um ou dois arquivos de uma vez. O TIPO de cada
# arquivo enviado é identificado pelas colunas (ver
# identificar_tipo_arquivo_magistrados()), e ele é gravado com o nome
# fixo correspondente + a extensão original.
#
# CÓDIGOS -> NOMENCLATURA: "Situação profissional atual" e "Cargo" vêm
# como código no arquivo do MPM. A correspondência fica em dois vetores
# R deste arquivo (SITUACAO_PROFISSIONAL_MAGISTRADO e CARGO_MAGISTRADO),
# PRÓPRIOS dos magistrados — a codificação é diferente da do Quadro de
# Pessoal. Sem banco de dados: tudo roda só em R, sobre os arquivos
# enviados. Um código sem nomenclatura cadastrada é exibido como veio.
#
# DEPENDÊNCIA: reaproveita funções/constantes definidas em mod_pessoal_auxiliar.R
# e mod_inconsistencias.R — precisa ser carregado DEPOIS deles no app.R.
# Todos os nomes globais deste arquivo são exclusivos (sufixo/prefixo
# "magistrado"/"mag_") para não colidir com os de
# mod_serventias.R.
# =====================================================

library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(DT)
library(echarts4r)
library(ggiraph)
library(readxl)

local({
    
    dependencias <- c(
        "MAX_UPLOAD_MB",
        "SUBPASTA_MAGISTRADOS",
        "caminho_alertas",
        "tempo_decorrido",
        "colunas_por_prefixo",
        "colunas_com_dados",
        "consolidar_colunas_por_linha",
        "normalizar_nome_coluna",
        "codigo_para_nomenclatura",
        "codigo_limpo",
        "converter_data",
        "formatar_data",
        "lotacoes_ativas_pessoas",
        "remover_registros_excluidos",
        "novo_envio_vazio",
        "ui_novo_envio",
        "chave_cpf",
        "data_formato_envio",
        "rotular_orgaos",
        "gravar_arquivo_substituindo",
        "mod_inconsistencias_ui",
        "mod_inconsistencias_server",
        "EXTENSOES_PLANILHA",
        "ROTULO_VALOR_VAZIO",
        "extensao_arquivo",
        "ler_arquivo_dados",
        "limpar_dados_lidos",
        "limpar_cabecalho",
        "coluna_por_regex",
        "valores_coluna_regex",
        "opcoes_combo",
        "selecao_valida",
        "normalizar_texto_busca",
        "texto_busca_linhas",
        "ui_csv_dados",
        "grafico_barras_girafe",
        "normalizar_nome_arquivo",
        "REGEX_COL_CPF",
        "REGEX_COL_NOME",
        "REGEX_COL_SITUACAO",
        "REGEX_COL_CARGO",
        "REGEX_COL_SEXO",
        "REGEX_COL_NATURALIDADE"
    )
    
    faltando <- dependencias[!vapply(dependencias, exists, logical(1))]
    
    if (length(faltando) > 0) {
        stop(
            "mod_magistrados.R precisa ser carregado depois de ",
            "mod_pessoal_auxiliar.R e mod_inconsistencias.R. Não encontrado: ",
            paste(faltando, collapse = ", ")
        )
    }
    
})

# =====================================================
# ARQUIVOS DO MÓDULO
# -----------------------------------------------------
# Cada tipo tem um nome fixo (radical) e pode estar em .xlsx ou .csv.
# A ordem de EXTENSOES_PLANILHA (mod_pessoal_auxiliar.R) define a prioridade.
# =====================================================

TIPOS_ARQUIVO_MAGISTRADOS <- list(
    base = list(
        radical = "magistrados",
        rotulo = "Base de Dados - Magistrados"
    ),
    # Arquivo que o órgão envia ao MPM (mesmo layout da base). Reconhecido
    # no upload por ter "envio" no nome do arquivo (como em Pessoal e
    # Auxiliar).
    envio = list(
        radical = "magistrados_envio",
        rotulo = "Envio - Magistrados"
    ),
    alertas = list(
        radical = "alertas_magistrados",
        rotulo = "Alertas - Magistrados"
    )
)

ARQUIVO_BASE_MAGISTRADOS <- "magistrados.xlsx"
ARQUIVO_ALERTAS_MAGISTRADOS <- "alertas_magistrados.csv"
ARQUIVO_ENVIO_MAGISTRADOS <- "magistrados_envio.xlsx"

# Subpasta de magistrados da empresa (<PASTA_ALERTAS>/<EMPRESA>/magistrados).
pasta_magistrados <- function(empresa) {
    caminho_alertas(empresa, SUBPASTA_MAGISTRADOS)
}

# Arquivos do tipo existentes na pasta, na ordem de prioridade (.xlsx
# antes de .csv). Nome comparado de forma tolerante
# (normalizar_nome_arquivo(), de mod_pessoal_auxiliar.R): "Magistrados.xlsx" ou
# "Alertas Magistrados.csv" também são reconhecidos.
mag_arquivos_do_tipo <- function(pasta, tipo) {
    
    if (is.null(pasta) || !dir.exists(pasta)) {
        return(character(0))
    }
    
    existentes <- list.files(pasta, full.names = TRUE)
    
    ok <- extensao_arquivo(existentes) %in% EXTENSOES_PLANILHA &
        normalizar_nome_arquivo(existentes) == TIPOS_ARQUIVO_MAGISTRADOS[[tipo]]$radical
    
    achados <- existentes[ok]
    achados[order(match(extensao_arquivo(achados), EXTENSOES_PLANILHA))]
    
}

mag_arquivo_do_tipo <- function(pasta, tipo) {
    achados <- mag_arquivos_do_tipo(pasta, tipo)
    if (length(achados) > 0) achados[1] else NULL
}

# =====================================================
# LEITURA E CARREGAMENTO (.xlsx ou .csv)
# =====================================================

ler_arquivo_magistrados <- function(caminho, extensao = extensao_arquivo(caminho),
                                    nome_exibicao = basename(caminho), n_max = Inf) {
    ler_arquivo_dados(caminho, extensao, nome_exibicao, aba_preferida = "magistrados", n_max = n_max)
}

carregar_arquivo_magistrados <- function(arquivo) {
    
    if (is.null(arquivo) || !file.exists(arquivo)) {
        return(data.frame())
    }
    
    # Lido e limpo uma vez por processo e reaproveitado por todas as sessões (e pelos outros módulos e pelo Painel) enquanto o arquivo não mudar — ver com_cache_arquivo() em R/utils.R.
    com_cache_arquivo(arquivo, "magistrados", function() {
        limpar_dados_lidos(ler_arquivo_magistrados(arquivo))
    })
    
}

# =====================================================
# COLUNAS (nome normalizado: sem acento, minúsculo)
# -----------------------------------------------------
# CPF, Nome, Situação profissional, Cargo, Sexo e Naturalidade usam as
# regex já definidas em mod_pessoal_auxiliar.R. As regex são ancoradas no início
# do nome, então "Alerta: Cargo" nunca é confundida com "Cargo".
# =====================================================

REGEX_COL_ORGAO_MAGISTRADO <- "^orgao de lotacao"
REGEX_COL_STATUS_MAGISTRADO <- "^status$"
REGEX_COL_RACA_MAGISTRADO <- "^raca"
REGEX_COL_PROMOCAO_MAGISTRADO <- "^forma de promocao"

# Assinatura do arquivo de magistrados do MPM: a coluna "Órgão de
# lotação do magistrado(a)". Distingue dos arquivos do Quadro de Pessoal
# ("Órgão de lotação do(a) Servidor(a) ou Auxiliar"), que também têm CPF.
REGEX_ASSINATURA_MAGISTRADO <- "^orgao de lotacao do\\(?a?\\)? ?magistrad"

# Opções de agrupamento do gráfico da base (rótulo -> regex da coluna).
AGRUPAMENTOS_BASE_MAGISTRADOS <- c(
    "Cargo"                        = REGEX_COL_CARGO,
    "Situação profissional atual"  = REGEX_COL_SITUACAO,
    "Órgão de lotação"             = REGEX_COL_ORGAO_MAGISTRADO,
    "Forma de promoção"            = REGEX_COL_PROMOCAO_MAGISTRADO,
    "Sexo"                         = REGEX_COL_SEXO,
    "Raça / Cor"                   = REGEX_COL_RACA_MAGISTRADO,
    "Naturalidade"                 = REGEX_COL_NATURALIDADE,
    "Status"                       = REGEX_COL_STATUS_MAGISTRADO
)

# =====================================================
# DETALHE DE UMA BARRA DO GRÁFICO (Base de Dados > Gráfico > Cargo)
# -----------------------------------------------------
# Ao clicar numa barra com "Agrupar por" = Cargo, abre uma janela com os
# registros daquele cargo (já com os filtros da aba aplicados) nas
# colunas abaixo — na ordem em que aparecem aqui. Coluna que não existir
# no arquivo fica de fora.
# =====================================================

COLUNAS_DETALHE_CARGO_MAGISTRADOS <- c(
    "CPF"                           = REGEX_COL_CPF,
    "Nome"                          = REGEX_COL_NOME,
    "Cargo"                         = REGEX_COL_CARGO,
    "Situação Profissional"         = REGEX_COL_SITUACAO,
    "Data de Início da Situação"    = REGEX_COL_INICIO_SITUACAO,
    "Data de Saída da Situação"     = REGEX_COL_SAIDA_SITUACAO
)

# `df`: dados de exibição (cargo por extenso) já filtrados; `cargo`: o
# valor da barra clicada. Datas em dd/mm/aaaa; ordem por nome.
detalhe_cargo_magistrados <- function(df, cargo) {

    # Mesmo valor que o gráfico usa para agrupar (inclusive o rótulo de
    # cargo vazio).
    grupos <- valores_coluna_regex(df, REGEX_COL_CARGO)
    if (is.null(grupos) || nrow(df) == 0) {
        return(data.frame())
    }

    sel <- df[grupos == cargo, , drop = FALSE]

    colunas <- list()
    for (rotulo in names(COLUNAS_DETALHE_CARGO_MAGISTRADOS)) {
        col <- coluna_por_regex(sel, COLUNAS_DETALHE_CARGO_MAGISTRADOS[[rotulo]])
        if (is.null(col)) next
        v <- sel[[col]]
        if (startsWith(rotulo, "Data")) {
            datas <- converter_data(v)
            v <- ifelse(is.na(datas), ifelse(is.na(v), "", as.character(v)), formatar_data(datas))
        }
        colunas[[rotulo]] <- v
    }

    out <- as.data.frame(colunas, check.names = FALSE, stringsAsFactors = FALSE)

    if ("Nome" %in% names(out)) {
        out <- out[order(out$Nome), , drop = FALSE]
    }

    rownames(out) <- NULL
    out

}

# =====================================================
# IDENTIFICAÇÃO DO TIPO DE ARQUIVO (upload)
# -----------------------------------------------------
#   - sem CPF ou sem "Órgão de lotação do magistrado(a)"       -> NA
#   - com colunas "Alerta..."/"Conflito..."                     -> alertas
#   - sem colunas de alerta                                     -> base
# =====================================================

identificar_tipo_arquivo_magistrados <- function(df, nome_arquivo = NULL) {
    
    if (is.null(df) || ncol(df) == 0) {
        return(NA_character_)
    }
    
    names(df) <- limpar_cabecalho(names(df))
    
    if (is.null(coluna_por_regex(df, REGEX_COL_CPF)) ||
        is.null(coluna_por_regex(df, REGEX_ASSINATURA_MAGISTRADO))) {
        return(NA_character_)
    }
    
    tem_alertas <- any(str_starts(names(df), "Alerta") | str_starts(names(df), "Conflito"))
    
    if (tem_alertas) {
        return("alertas")
    }
    
    # Sem colunas de alerta: base ou envio. É envio quando o nome do
    # arquivo tem "envio" (mesma regra de Pessoal e Auxiliar) OU quando
    # não tem a coluna Status — o MPM acrescenta Status na base que ele
    # devolve; o arquivo gerado pelo órgão para envio não tem (ex.:
    # "Magistrados_i.xlsx", "serventias_i.xlsx").
    eh_envio <- (!is.null(nome_arquivo) && length(nome_arquivo) == 1 && !is.na(nome_arquivo) &&
        str_detect(normalizar_nome_arquivo(nome_arquivo), REGEX_NOME_ARQUIVO_ENVIO)) ||
        is.null(coluna_por_regex(df, REGEX_COL_STATUS_MAGISTRADO))
    
    if (eh_envio) "envio" else "base"
    
}

# =====================================================
# CÓDIGO -> NOMENCLATURA (Situação profissional atual e Cargo)
# -----------------------------------------------------
# Vetores próprios dos magistrados (a codificação é diferente da do
# Quadro de Pessoal), iguais às abas "Sit. prof. atual magistrado" e
# "Cargos magistrados" da planilha Tabelas Auxiliares do MPM. São o
# PADRÃO: na inicialização, o conteúdo das tabelas do SQLite os substitui
# (ver R/tabelas_referencia.R e Configurações > Tabelas Auxiliares).
#
# Atenção: as versões anteriores deste arquivo tinham outra numeração
# (ex.: 10 = Aposentadoria, cargo 3 = Desembargador(a)), que NÃO é a do
# MPM — no MPM, 10 = Licenças ou Concessões, 12 = Aposentado(a), cargo 3
# = Juiz(a) Substituto(a) de 2º grau e cargo 4 = Desembargador(a).
# =====================================================

SITUACAO_PROFISSIONAL_MAGISTRADO <- c(
    "1"  = "Presidente do Tribunal",
    "2"  = "Vice Presidente do Tribunal",
    "3"  = "Diretor(a) de Escola de Magistratura",
    "4"  = "Ouvidor(a)",
    "5"  = "Corregedor(a)",
    "6"  = "Juiz(a) convocado(a) para substituição de Desembargador(a) ou Ministro(a)",
    "7"  = "Juiz(a) Auxiliar ou Juiz(a) Instrutor(a) ou Desembargador(a) que atua na alta administração de Tribunal/Conselho",
    "8"  = "Ocupante de cargo próprio na Jurisdição",
    "9"  = "Afastado(a) por decisão administrativa",
    "10" = "Licenças ou Concessões previstas em lei",
    "11" = "Ingresso por Remoção externa",
    "12" = "Aposentado(a)",
    "13" = "Falecido(a)",
    "14" = "Exonerado(a)",
    "15" = "Demitido(a)",
    "16" = "Diretor(a) de foro",
    "17" = "Saída por Remoção Externa",
    "18" = "Ocupante de cargo em acumulação na Jurisdição",
    "19" = "Magistrado(a) convocado para atuar em outro Tribunal ou Conselho"
)

CARGO_MAGISTRADO <- c(
    "1"  = "Juiz(a) Titular",
    "2"  = "Juiz(a) Substituto(a)",
    "3"  = "Juiz(a) Substituto(a) de 2º grau",
    "4"  = "Desembargador(a)",
    "5"  = "Ministro(a) ou Conselheiro(a)",
    "6"  = "Magistrado(a) que atua no 1º grau eleitoral",
    "7"  = "Magistrado(a) que atua como titular no 2º grau eleitoral ou no TSE",
    "8"  = "Magistrado(a) que atua como substituto no 2º grau eleitoral ou no TSE",
    "9"  = "Juiz(a) da classe advogado que atua como titular no TRE ou TSE",
    "10" = "Juiz(a) da classe advogado que atua como substituto no TRE ou TSE",
    "11" = "Juiz(a) Auxiliar que atua no 1º grau"
)

# Troca código por nomenclatura nas colunas Situação profissional atual
# e Cargo (se existirem). Só para exibição.
traduzir_codigos_magistrados <- function(df) {
    
    if (is.null(df) || nrow(df) == 0) {
        return(df)
    }
    
    col_situacao <- coluna_por_regex(df, REGEX_COL_SITUACAO)
    col_cargo <- coluna_por_regex(df, REGEX_COL_CARGO)
    
    if (!is.null(col_situacao)) {
        df[[col_situacao]] <- codigo_para_nomenclatura(df[[col_situacao]], SITUACAO_PROFISSIONAL_MAGISTRADO)
    }
    
    if (!is.null(col_cargo)) {
        df[[col_cargo]] <- codigo_para_nomenclatura(df[[col_cargo]], CARGO_MAGISTRADO)
    }
    
    # Demais códigos de cadastro (sexo, raça/cor, deficiência, cotas,
    # forma de promoção...) pelas tabelas auxiliares do MPM no SQLite —
    # ver traduzir_descricoes_mpm(), em R/tabelas_referencia.R.
    if (exists("traduzir_descricoes_mpm", mode = "function")) {
        df <- traduzir_descricoes_mpm(df)
    }
    
    df
    
}

# =====================================================
# ALERTAS — CONSOLIDA ALERTA(S)/CONFLITO(S) DETECTADO(S) (só tabela)
# -----------------------------------------------------
# Mantém as colunas fixas (tudo que não começa com "Alerta"/"Conflito")
# e troca as colunas "Alerta: <tipo>" por "Alerta(s) Detectado".
# "Conflito(s) Detectado" só aparece se o arquivo tiver conflitos.
# =====================================================

consolidar_alertas_magistrados_tabela <- function(df) {
    
    if (is.null(df) || nrow(df) == 0) {
        return(df)
    }
    
    colunas_alerta <- colunas_por_prefixo(df, "Alerta")
    colunas_conflito <- colunas_por_prefixo(df, "Conflito")
    
    colunas_fixas <- setdiff(names(df), c(colunas_alerta, colunas_conflito))
    
    df_exibicao <- df[colunas_fixas]
    df_exibicao[["Alerta(s) Detectado"]] <- consolidar_colunas_por_linha(df, colunas_alerta)
    
    if (length(colunas_conflito) > 0) {
        df_exibicao[["Conflito(s) Detectado"]] <- consolidar_colunas_por_linha(df, colunas_conflito)
    }
    
    df_exibicao
    
}

# =====================================================
# INCONSISTÊNCIAS — VERIFICAÇÕES AUTOMÁTICAS NA BASE
# -----------------------------------------------------
# Rodam sobre a base de magistrados com os CÓDIGOS originais (antes da
# tradução) e apontam padrões que contrariam as Tabelas Auxiliares do
# MPM. São indícios para conferência, não correções automáticas.
#
#   1) CARGO 3 COM PERFIL DE DESEMBARGADOR
#      Cargo 3 = "Juiz(a) Substituto(a) de 2º grau". Um CPF com cargo 3
#      é apontado quando, em algum registro com cargo 3:
#        - exerce situação privativa de desembargador (1 Presidente do
#          Tribunal, 2 Vice Presidente, 3 Diretor(a) de Escola de
#          Magistratura, 4 Ouvidor(a), 5 Corregedor(a)); e/ou
#        - está lotado em gabinete de desembargador (nome da serventia
#          "G-<nº> DES./DESA. ...", lido da base de Serventias da
#          empresa, se ela tiver sido enviada).
#      Gravidade Alta com as duas evidências; Média com uma.
#
#   2) APOSENTADORIA (SITUAÇÃO 12) COM PERFIL DE AFASTAMENTO
#      Aposentadoria é situação definitiva: não deveria ter data de
#      saída nem ser seguida de nova situação do mesmo magistrado. Um
#      registro com situação 12 é apontado quando:
#        - tem data de saída; e/ou
#        - o mesmo CPF tem registro posterior em outra situação (retorno).
#      Gravidade Alta se houve retorno ou se durou até
#      LIMITE_DIAS_APOSENTADORIA_CURTA dias (perfil de férias/licença,
#      que deveria ir como 10 - Licenças ou Concessões previstas em lei,
#      ou 9 - Afastado(a) por decisão administrativa); Média nos demais
#      casos.
# =====================================================

CARGO_MAG_DESEMBARGADOR <- "4"
CARGO_MAG_SUBSTITUTO_2GRAU <- "3"
SITUACOES_MAG_DIRECAO <- c("1", "2", "3", "4", "5")
SITUACAO_MAG_APOSENTADORIA <- "12"
LIMITE_DIAS_APOSENTADORIA_CURTA <- 60

VERIFICACAO_MAG_CARGO <- "Cargo 3 com perfil de Desembargador(a)"
VERIFICACAO_MAG_APOSENTADORIA <- "Aposentadoria com perfil de afastamento"

REGEX_COL_INICIO_SITUACAO_MAG <- "^data de inicio da situacao"
REGEX_COL_SAIDA_SITUACAO_MAG <- "^data de saida da situacao"

# Nome de gabinete de desembargador na base de Serventias do TJSE:
# "G-11 DES. ROBERTO ...", "G-13 DESA. ANA ...".
REGEX_GABINETE_DESEMBARGADOR <- "^G-?\\s*\\d+\\s+DESA?\\b"

# Funções genéricas de mod_pessoal_auxiliar.R, com os nomes usados neste módulo:
# códigos "4.0" -> "4"; datas dd/mm/aaaa, aaaa-mm-dd ou serial do Excel.
mag_codigo <- codigo_limpo
mag_data <- converter_data
mag_fmt_data <- formatar_data

# Mapa código -> nome da serventia a partir da base de Serventias da
# empresa (módulo mod_serventias.R). Vetor nomeado vazio se o
# módulo não estiver carregado ou a base não tiver sido enviada.
mag_nomes_orgaos <- function(empresa) {
    
    necessarias <- c(
        "pasta_serventias", "arquivo_do_tipo", "carregar_arquivo_serventias",
        "REGEX_COL_CODIGO_SERVENTIA", "REGEX_COL_NOME_SERVENTIA"
    )
    
    if (!all(vapply(necessarias, exists, logical(1)))) {
        return(character(0))
    }
    
    tryCatch({
        s <- carregar_arquivo_serventias(arquivo_do_tipo(pasta_serventias(empresa), "base"))
        col_cod <- coluna_por_regex(s, REGEX_COL_CODIGO_SERVENTIA)
        col_nome <- coluna_por_regex(s, REGEX_COL_NOME_SERVENTIA)
        
        if (nrow(s) == 0 || is.null(col_cod) || is.null(col_nome)) {
            return(character(0))
        }
        
        mapa <- s[[col_nome]]
        names(mapa) <- mag_codigo(s[[col_cod]])
        mapa[!duplicated(names(mapa))]
    }, error = function(e) character(0))
    
}

# Estrutura (vazia) do resultado das verificações.
inconsistencias_magistrados_vazio <- function() {
    tibble(
        `Verificação` = character(0),
        Gravidade = character(0),
        CPF = character(0),
        Nome = character(0),
        `Órgão de lotação` = character(0),
        Cargo = character(0),
        `Situação profissional atual` = character(0),
        `Data de início da situação` = character(0),
        `Data de saída da situação` = character(0),
        `Duração (dias)` = integer(0),
        Status = character(0),
        Motivo = character(0)
    )
}

# Executa as duas verificações. `base` = dados_base() (códigos crus);
# `nomes_orgaos` = mag_nomes_orgaos(). Devolve uma linha por registro
# da base apontado, com o motivo por extenso.
verificar_inconsistencias_magistrados <- function(base, nomes_orgaos = character(0)) {
    
    vazio <- inconsistencias_magistrados_vazio()

    # Registros marcados como "Exclusão de registro por erro" = S não
    # entram nas verificações (remover_registros_excluidos(), mod_pessoal_auxiliar.R).
    base <- remover_registros_excluidos(base)

    if (is.null(base) || nrow(base) == 0) {
        return(vazio)
    }

    col <- function(regex) coluna_por_regex(base, regex)
    valor <- function(regex) {
        c <- col(regex)
        if (is.null(c)) rep(NA_character_, nrow(base)) else as.character(base[[c]])
    }
    
    if (is.null(col(REGEX_COL_CPF)) || is.null(col(REGEX_COL_CARGO)) || is.null(col(REGEX_COL_SITUACAO))) {
        return(vazio)
    }
    
    d <- tibble(
        CPF = valor(REGEX_COL_CPF),
        Nome = valor(REGEX_COL_NOME),
        orgao = mag_codigo(valor(REGEX_COL_ORGAO_MAGISTRADO)),
        cargo = mag_codigo(valor(REGEX_COL_CARGO)),
        situacao = mag_codigo(valor(REGEX_COL_SITUACAO)),
        inicio = mag_data(valor(REGEX_COL_INICIO_SITUACAO_MAG)),
        saida = mag_data(valor(REGEX_COL_SAIDA_SITUACAO_MAG)),
        Status = valor(REGEX_COL_STATUS_MAGISTRADO)
    ) %>%
        mutate(
            nome_orgao = unname(nomes_orgaos[orgao]),
            orgao_exibicao = ifelse(is.na(nome_orgao), orgao, paste0(orgao, " - ", nome_orgao)),
            gabinete_des = !is.na(nome_orgao) &
                str_detect(
                    str_to_upper(stringi::stri_trans_general(nome_orgao, "Latin-ASCII")),
                    REGEX_GABINETE_DESEMBARGADOR
                )
        )
    
    montar <- function(x, verificacao) {
        tibble(
            `Verificação` = verificacao,
            Gravidade = x$Gravidade,
            CPF = x$CPF,
            Nome = x$Nome,
            `Órgão de lotação` = x$orgao_exibicao,
            Cargo = x$cargo,
            `Situação profissional atual` = x$situacao,
            `Data de início da situação` = mag_fmt_data(x$inicio),
            `Data de saída da situação` = mag_fmt_data(x$saida),
            `Duração (dias)` = as.integer(x$saida - x$inicio),
            Status = x$Status,
            Motivo = x$Motivo
        )
    }
    
    # ---- 1) Cargo 3 com perfil de Desembargador ----
    
    rotulo_sit <- SITUACAO_PROFISSIONAL_MAGISTRADO
    
    c4 <- d %>% filter(cargo == CARGO_MAG_SUBSTITUTO_2GRAU)
    
    evid <- c4 %>%
        group_by(CPF) %>%
        summarise(
            direcao = paste(
                unique(na.omit(ifelse(situacao %in% SITUACOES_MAG_DIRECAO, rotulo_sit[situacao], NA))),
                collapse = ", "
            ),
            gabinetes = paste(unique(nome_orgao[gabinete_des]), collapse = "; "),
            .groups = "drop"
        ) %>%
        filter(direcao != "" | gabinetes != "") %>%
        mutate(
            Gravidade = ifelse(direcao != "" & gabinetes != "", "Alta", "Média"),
            Motivo = paste0(
                "Cadastrado(a) como Juiz(a) Substituto(a) de 2º grau (cargo 3), mas ",
                ifelse(direcao != "", paste0("exerce ", direcao, " (situação privativa de desembargador)"), ""),
                ifelse(direcao != "" & gabinetes != "", " e ", ""),
                ifelse(gabinetes != "", paste0("está lotado(a) em gabinete de desembargador (", gabinetes, ")"), ""),
                ". Provável Desembargador(a) - cargo 4."
            )
        )
    
    r1 <- c4 %>%
        inner_join(evid %>% select(CPF, Gravidade, Motivo), by = "CPF") %>%
        arrange(Nome, inicio)
    
    # ---- 2) Aposentadoria com perfil de afastamento ----
    
    ap <- d %>%
        mutate(.linha = row_number()) %>%
        filter(situacao == SITUACAO_MAG_APOSENTADORIA)
    
    # Retorno = registro posterior em situação ATIVA. Outra situação
    # encerrada depois da aposentadoria (ex.: 13 Falecimento) não é
    # retorno — antes, todo aposentado que depois faleceu saía como
    # "volta como Falecimento", gravidade Alta.
    outras <- d %>% filter(!(situacao %in% SITUACOES_MAG_ENCERRADAS), !is.na(inicio))
    
    retorno <- if (nrow(ap) > 0 && nrow(outras) > 0) {
        ap %>%
            select(.linha, CPF, ref = inicio, saida) %>%
            mutate(ref = coalesce(saida, ref)) %>%
            inner_join(outras %>% select(CPF, ret_inicio = inicio, ret_sit = situacao),
                       by = "CPF", relationship = "many-to-many") %>%
            filter(ret_inicio > ref) %>%
            group_by(.linha) %>%
            slice_min(ret_inicio, n = 1, with_ties = FALSE) %>%
            ungroup() %>%
            select(.linha, ret_inicio, ret_sit)
    } else {
        tibble(.linha = integer(0), ret_inicio = as.Date(character(0)), ret_sit = character(0))
    }
    
    r2 <- ap %>%
        left_join(retorno, by = ".linha") %>%
        mutate(
            dias = as.integer(saida - inicio),
            tem_saida = !is.na(saida),
            voltou = !is.na(ret_inicio),
            curta = tem_saida & !is.na(dias) & dias <= LIMITE_DIAS_APOSENTADORIA_CURTA
        ) %>%
        filter(tem_saida | voltou) %>%
        mutate(
            Gravidade = ifelse(voltou | curta, "Alta", "Média"),
            Motivo = paste0(
                "Aposentadoria (situação 12) é definitiva, mas ",
                ifelse(
                    tem_saida,
                    paste0(
                        "tem data de saída",
                        ifelse(is.na(dias), "", paste0(" (duração de ", dias, " dia(s))"))
                    ),
                    ""
                ),
                ifelse(tem_saida & voltou, " e ", ""),
                ifelse(
                    voltou,
                    paste0(
                        "o magistrado volta em ", mag_fmt_data(ret_inicio), " como ",
                        coalesce(unname(rotulo_sit[ret_sit]), paste("situação", ret_sit))
                    ),
                    ""
                ),
                ifelse(voltou | curta, ". Perfil de afastamento temporário - provável situação 10 (Licenças ou Concessões previstas em lei) ou 9 (Afastado(a) por decisão administrativa).", ".")
            )
        ) %>%
        arrange(Nome, inicio)
    
    bind_rows(
        if (nrow(r1) > 0) montar(r1, VERIFICACAO_MAG_CARGO) else vazio,
        if (nrow(r2) > 0) montar(r2, VERIFICACAO_MAG_APOSENTADORIA) else vazio
    )
    
}

# =====================================================
# LOTAÇÕES ATUAIS (usadas pelo módulo Serventias)
# -----------------------------------------------------
# Último registro de cada CPF, em situação não encerrada (12
# Aposentado(a), 13 Falecido(a), 14 Exonerado(a), 15 Demitido(a), 17
# Saída por Remoção Externa — numeração do MPM). Alimenta a
# verificação "Serventia inativa com lotação ativa" de
# mod_serventias.R.
# =====================================================

SITUACOES_MAG_ENCERRADAS <- c("12", "13", "14", "15", "17")

carregar_lotacoes_magistrados_empresa <- function(empresa) {
    tryCatch(
        lotacoes_ativas_pessoas(
            carregar_arquivo_magistrados(mag_arquivo_do_tipo(pasta_magistrados(empresa), "base")),
            REGEX_COL_ORGAO_MAGISTRADO,
            SITUACOES_MAG_ENCERRADAS
        ),
        error = function(e) lotacoes_ativas_pessoas(NULL, REGEX_COL_ORGAO_MAGISTRADO, character(0))
    )
}

# =====================================================
# NOVO ENVIO — CARGOS DISTINTOS ATIVOS / DUPLICIDADE (Magistrados)
# -----------------------------------------------------
# Adaptação da regra de Pessoal e Auxiliar (gerar_novo_envio_duplicidade(),
# em mod_pessoal_auxiliar.R). No arquivo de alertas de Magistrados do MPM a
# duplicidade aparece como "Alerta: Inconsistência" = "Há cargos
# distintos ativos para o mesmo CPF informado" (ex.: promoção de
# Juiz(a) Substituto(a) para Titular sem encerrar o registro anterior);
# "Conflito: Duplicidade no próprio Tribunal", se vier, também conta.
#
# Para cada CPF com esse alerta, gera DUAS linhas, nesta ordem:
#
#   1) o registro da BASE DE DADOS que continua em aberto — sem Data de
#      saída, com situação não encerrada (fora de 10, 12, 13, 14 e 20 —
#      SITUACOES_MAG_ENCERRADAS), cargo DIFERENTE do cargo do envio e
#      Data de início anterior à do envio (o mais recente, se houver
#      mais de um) —, igual ao da base, só com a inclusão da "Data de
#      saída da situação" = "Data de início da situação" do ENVIO;
#   2) o registro do ENVIO, exatamente como está no arquivo de envio
#      (o mais recente do CPF, se houver mais de um).
#
# As colunas são as do arquivo de envio, na mesma ordem; colunas do
# envio que a base não tem ficam vazias (ex.: a base do MPM tem
# "Status", o envio não). Valores como nos arquivos (códigos, não os
# textos da tela); datas seriais do Excel saem em dd/mm/aaaa.
#
# Devolve list(dados, cpfs_conflito, sem_envio, sem_base, fora_situacao)
# — fora_situacao fica sempre vazio aqui (a situação já é critério de
# escolha do registro da base); sem_base lista os CPFs sem registro em
# aberto, de outro cargo, anterior ao envio.
# =====================================================

# Texto do alerta (normalizado: minúsculo, sem acento) que indica a
# duplicidade de magistrados.
REGEX_TEXTO_DUPLICIDADE_MAG <- "cargos? distintos? ativos?|duplicidade"

cpfs_com_duplicidade_magistrados <- function(alertas) {
    
    if (is.null(alertas) || nrow(alertas) == 0) {
        return(character(0))
    }
    
    col_cpf <- coluna_por_regex(alertas, REGEX_COL_CPF)
    
    if (is.null(col_cpf)) {
        return(character(0))
    }
    
    nomes <- normalizar_nome_coluna(names(alertas))
    
    # 1) Coluna de conflito de duplicidade (qualquer conteúdo).
    cols_conflito <- names(alertas)[str_detect(nomes, REGEX_CONFLITO_DUPLICIDADE_TRIBUNAL)]
    
    # 2) Qualquer coluna Alerta/Conflito cujo TEXTO fale em cargos
    #    distintos ativos ou duplicidade.
    cols_texto <- names(alertas)[str_detect(nomes, "^(alerta|conflito)")]
    
    marcado <- rep(FALSE, nrow(alertas))
    
    for (c in cols_conflito) {
        v <- alertas[[c]]
        marcado <- marcado | (!is.na(v) & str_trim(v) != "")
    }
    
    for (c in cols_texto) {
        v <- normalizar_texto_busca(coalesce(as.character(alertas[[c]]), ""))
        marcado <- marcado | str_detect(v, REGEX_TEXTO_DUPLICIDADE_MAG)
    }
    
    cpfs <- unique(chave_cpf(alertas[[col_cpf]][marcado]))
    cpfs[cpfs != ""]
    
}

gerar_novo_envio_magistrados <- function(envio, base, alertas) {
    
    if (is.null(envio) || ncol(envio) == 0) {
        return(novo_envio_vazio())
    }
    
    colunas <- names(envio)
    vazio <- novo_envio_vazio(colunas)
    
    if (nrow(envio) == 0) {
        return(vazio)
    }
    
    cpfs <- cpfs_com_duplicidade_magistrados(alertas)
    
    if (length(cpfs) == 0) {
        return(vazio)
    }
    
    vazio$cpfs_conflito <- cpfs
    
    col_cpf_envio <- coluna_por_regex(envio, REGEX_COL_CPF)
    col_inicio_envio <- coluna_por_regex(envio, REGEX_COL_INICIO_SITUACAO)
    col_saida_envio <- coluna_por_regex(envio, REGEX_COL_SAIDA_SITUACAO)
    col_cargo_envio <- coluna_por_regex(envio, REGEX_COL_CARGO)
    
    if (is.null(col_cpf_envio) || is.null(col_inicio_envio)) {
        vazio$sem_envio <- cpfs
        return(vazio)
    }
    
    base <- remover_registros_excluidos(base)
    tem_base <- !is.null(base) && nrow(base) > 0
    col_cpf_base <- if (tem_base) coluna_por_regex(base, REGEX_COL_CPF)
    tem_base <- tem_base && !is.null(col_cpf_base)
    col_inicio_base <- if (tem_base) coluna_por_regex(base, REGEX_COL_INICIO_SITUACAO)
    col_saida_base <- if (tem_base) coluna_por_regex(base, REGEX_COL_SAIDA_SITUACAO)
    col_situacao_base <- if (tem_base) coluna_por_regex(base, REGEX_COL_SITUACAO)
    col_cargo_base <- if (tem_base) coluna_por_regex(base, REGEX_COL_CARGO)
    
    correspondencia <- if (tem_base) {
        names(base)[match(normalizar_nome_coluna(colunas), normalizar_nome_coluna(names(base)))]
    } else {
        rep(NA_character_, length(colunas))
    }
    
    n_base <- if (tem_base) nrow(base) else 0
    valor_base <- function(col) if (is.null(col)) rep(NA_character_, n_base) else as.character(base[[col]])
    
    chave_envio <- chave_cpf(envio[[col_cpf_envio]])
    inicio_envio <- converter_data(envio[[col_inicio_envio]])
    cargo_envio <- if (is.null(col_cargo_envio)) rep(NA_character_, nrow(envio)) else codigo_limpo(envio[[col_cargo_envio]])
    
    chave_base <- if (tem_base) chave_cpf(base[[col_cpf_base]]) else character(0)
    inicio_base <- converter_data(valor_base(col_inicio_base))
    saida_base <- str_trim(coalesce(valor_base(col_saida_base), ""))
    situacao_base <- codigo_limpo(valor_base(col_situacao_base))
    cargo_base <- codigo_limpo(valor_base(col_cargo_base))
    
    como_texto <- function(v) {
        v <- as.character(v)
        v[is.na(v)] <- ""
        v
    }
    
    linhas <- list()
    sem_envio <- character(0)
    sem_base <- character(0)
    
    for (cpf in cpfs) {
        
        i_env <- which(chave_envio == cpf)
        
        if (length(i_env) == 0) {
            sem_envio <- c(sem_envio, cpf)
            next
        }
        
        # Envio mais recente do CPF (empate: o último no arquivo).
        ordem <- order(!is.na(inicio_envio[i_env]), inicio_envio[i_env], i_env)
        i_env <- i_env[ordem][length(ordem)]
        data_ini_env <- inicio_envio[i_env]
        
        # Registro da base ainda em aberto, de outro cargo, anterior ao
        # envio.
        i_base <- which(chave_base == cpf)
        
        if (length(i_base) > 0 && !is.na(data_ini_env)) {
            i_base <- i_base[
                !is.na(inicio_base[i_base]) & inicio_base[i_base] < data_ini_env &
                    saida_base[i_base] == "" &
                    !(situacao_base[i_base] %in% SITUACOES_MAG_ENCERRADAS) &
                    (is.na(cargo_envio[i_env]) | is.na(cargo_base[i_base]) | cargo_base[i_base] != cargo_envio[i_env])
            ]
        } else {
            i_base <- integer(0)
        }
        
        if (length(i_base) == 0) {
            sem_base <- c(sem_base, cpf)
            next
        }
        
        i_base <- i_base[order(inicio_base[i_base], i_base)][length(i_base)]
        
        reg_env <- vapply(colunas, function(c) como_texto(envio[[c]][i_env]), character(1))
        
        reg_base <- vapply(seq_along(colunas), function(k) {
            c <- correspondencia[k]
            if (is.na(c)) "" else como_texto(base[[c]][i_base])
        }, character(1))
        names(reg_base) <- colunas
        
        if (!is.null(col_saida_envio)) {
            reg_base[[col_saida_envio]] <- reg_env[[col_inicio_envio]]
        }
        
        linhas[[length(linhas) + 1]] <- reg_base
        linhas[[length(linhas) + 1]] <- reg_env
    }
    
    dados <- if (length(linhas) > 0) {
        as.data.frame(do.call(rbind, linhas), check.names = FALSE, stringsAsFactors = FALSE)
    } else {
        vazio$dados
    }
    names(dados) <- colunas
    rownames(dados) <- NULL
    
    for (c in colunas[str_detect(normalizar_nome_coluna(colunas), REGEX_COLS_DATA_ENVIO)]) {
        dados[[c]] <- data_formato_envio(dados[[c]])
    }
    
    list(dados = dados, cpfs_conflito = cpfs, sem_envio = sem_envio, sem_base = sem_base,
         fora_situacao = character(0))
    
}

# =====================================================
# ABA DE CADASTRO (Base de Dados / Envio - Magistrados)
# -----------------------------------------------------
# Mesma estrutura de Pessoal e Auxiliar (ui_aba_cadastro_pessoal(), em
# mod_pessoal_auxiliar.R): as duas abas têm os mesmos filtros (Status, CPF, Nome,
# Cargo, Situação, Órgão de lotação e Detalhe), gráfico e CSV. `p` é o
# prefixo dos ids ("base" ou "envio": base_status, envio_status...). O
# lado do servidor é servidor_aba_cadastro_magistrados().
# =====================================================

ui_aba_cadastro_magistrados <- function(ns, p, titulo, icone) {
    
    id <- function(x) ns(paste0(p, "_", x))
    
    tabPanel(
        title = tagList(icon(icone, class = "me-1"), titulo),
        value = p,
        
        div(
            class = "al-card",
            # Bloqueado (handler JS "bloquear-filtros", em app.R) enquanto
            # não houver arquivo.
            id = id("filtros"),
            
            div(class = "al-card-titulo", "Filtros"),
            
            div(
                class = "al-filtros-aviso",
                icon("lock", class = "me-2"),
                "Filtros indisponíveis: não há arquivo para exibir. Envie-o pelo botão \"Gerenciar Arquivos\"."
            ),
            
            fluidRow(
                column(
                    2,
                    # Começa em "Ativo"; se o arquivo não tiver coluna
                    # Status, volta para "Todos".
                    selectInput(
                        id("status"), "Status",
                        choices = c("Todos", "Ativo"),
                        selected = "Ativo",
                        width = "100%"
                    )
                ),
                column(3, textInput(id("cpf"), "CPF", width = "100%")),
                column(7, textInput(id("nome"), "Nome", width = "100%"))
            ),
            
            fluidRow(
                column(4, selectInput(id("cargo"), "Cargo", choices = c("Todos"), width = "100%")),
                column(
                    4,
                    selectInput(id("situacao"), "Situação profissional atual", choices = c("Todos"), width = "100%")
                ),
                column(4, selectInput(id("orgao"), "Órgão de lotação", choices = c("Todos"), width = "100%"))
            ),
            
            fluidRow(
                column(
                    12,
                    textInput(
                        id("detalhe"), "Detalhe",
                        placeholder = "Procura o texto digitado em todas as colunas da tabela",
                        width = "100%"
                    )
                )
            ),
            
            fluidRow(
                column(
                    12,
                    actionButton(
                        id("limpar_filtros"),
                        tagList(icon("filter-circle-xmark", class = "me-2"), "Limpar Filtros"),
                        class = "btn btn-outline-secondary btn-sm"
                    )
                )
            )
        ),
        
        div(
            class = "al-conteudo",
            
            tabsetPanel(
                tabPanel(
                    tagList(icon("table", class = "me-1"), "Tabela"),
                    DTOutput(id("tabela"))
                ),
                tabPanel(
                    tagList(icon("chart-column", class = "me-1"), "Gráfico"),
                    
                    fluidRow(
                        class = "mt-3",
                        id = id("grafico_controles"),
                        column(
                            4,
                            selectInput(
                                id("grafico_agrupar"), "Agrupar por",
                                choices = names(AGRUPAMENTOS_BASE_MAGISTRADOS),
                                selected = "Cargo",
                                width = "100%"
                            )
                        ),
                        column(
                            4,
                            selectInput(
                                id("grafico_qtd"), "Exibir",
                                choices = c(
                                    "Os 20 maiores grupos" = "20",
                                    "Os 50 maiores grupos" = "50",
                                    "Todos os grupos" = "0"
                                ),
                                selected = "20",
                                width = "100%"
                            )
                        )
                    ),
                    
                    girafeOutput(id("grafico"))
                ),
                tabPanel(
                    tagList(icon("file-csv", class = "me-1"), "Gerar arquivo CSV"),
                    uiOutput(id("csv_ui"))
                )
            )
        )
    )
    
}

# Chamado dentro de mod_magistrados_server(). `dados` é o
# reactiveVal com o arquivo lido; `p`, o prefixo dos ids da UI. Devolve a
# versão de exibição, a filtrada, a função que atualiza os combos e a
# mensagem de "sem arquivo" (usada na janela de aviso).
servidor_aba_cadastro_magistrados <- function(input, output, session, p, dados,
                                              msg_sem_dados, nome_csv,
                                              mapa_orgaos = reactive(character(0)),
                                              extra_csv = NULL,
                                              detalhe_cargo = FALSE) {
    
    ns <- session$ns
    id <- function(x) paste0(p, "_", x)
    valor <- function(x) input[[id(x)]]
    
    MSG_SEM_RESULTADO <- "Nenhum magistrado encontrado com os filtros aplicados."
    
    # Mesmas colunas e ordem do arquivo; Cargo e Situação por extenso e
    # o Órgão como "código - nome da serventia" (se a base de Serventias
    # tiver sido enviada), como em Pessoal e Auxiliar.
    exibicao <- reactive({
        df <- traduzir_codigos_magistrados(dados())
        col_orgao <- coluna_por_regex(df, REGEX_COL_ORGAO_MAGISTRADO)
        if (nrow(df) > 0 && !is.null(col_orgao)) {
            df[[col_orgao]] <- rotular_orgaos(df[[col_orgao]], mapa_orgaos())
        }
        df
    })
    
    # Quando o arquivo passa de "sem dados" para "com dados" (primeiro
    # envio, troca de empresa), o Status volta ao padrão "Ativo".
    status_anterior <- character(0)
    
    atualizar_combos <- function() {
        
        df <- exibicao()
        
        status <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_STATUS_MAGISTRADO))
        cargos <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_CARGO))
        situacoes <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_SITUACAO))
        orgaos <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_ORGAO_MAGISTRADO))
        
        status_atual <- if (length(status_anterior) == 0) "Ativo" else valor("status")
        status_anterior <<- status
        
        updateSelectInput(session, id("status"), choices = c("Todos", status),
                          selected = selecao_valida(status_atual, status, padrao = "Ativo"))
        updateSelectInput(session, id("cargo"), choices = c("Todos", cargos),
                          selected = selecao_valida(valor("cargo"), cargos))
        updateSelectInput(session, id("situacao"), choices = c("Todos", situacoes),
                          selected = selecao_valida(valor("situacao"), situacoes))
        updateSelectInput(session, id("orgao"), choices = c("Todos", orgaos),
                          selected = selecao_valida(valor("orgao"), orgaos))
        
    }
    
    # Status volta ao padrão "Ativo" (não para "Todos").
    observeEvent(valor("limpar_filtros"), {
        status <- opcoes_combo(valores_coluna_regex(exibicao(), REGEX_COL_STATUS_MAGISTRADO))
        updateSelectInput(session, id("status"), selected = selecao_valida("Ativo", status))
        updateTextInput(session, id("cpf"), value = "")
        updateTextInput(session, id("nome"), value = "")
        updateSelectInput(session, id("cargo"), selected = "Todos")
        updateSelectInput(session, id("situacao"), selected = "Todos")
        updateSelectInput(session, id("orgao"), selected = "Todos")
        updateTextInput(session, id("detalhe"), value = "")
    }, ignoreInit = TRUE)
    
    busca <- reactive({
        texto_busca_linhas(exibicao())
    })
    
    # Espera o usuário parar de digitar (400 ms) antes de filtrar.
    cpf_d <- debounce(reactive(valor("cpf")), 400)
    nome_d <- debounce(reactive(valor("nome")), 400)
    detalhe_d <- debounce(reactive(valor("detalhe")), 400)
    
    filtrados <- reactive({
        df <- exibicao()
        
        if (nrow(df) == 0) {
            return(df)
        }
        
        manter <- rep(TRUE, nrow(df))
        
        filtrar_combo <- function(sel, regex) {
            if (!is.null(sel) && sel != "Todos") {
                v <- valores_coluna_regex(df, regex)
                if (!is.null(v)) manter <<- manter & v == sel
            }
        }
        
        filtrar_combo(valor("status"), REGEX_COL_STATUS_MAGISTRADO)
        filtrar_combo(valor("cargo"), REGEX_COL_CARGO)
        filtrar_combo(valor("situacao"), REGEX_COL_SITUACAO)
        filtrar_combo(valor("orgao"), REGEX_COL_ORGAO_MAGISTRADO)
        
        cpf <- str_trim(cpf_d() %||% "")
        col_cpf <- coluna_por_regex(df, REGEX_COL_CPF)
        if (cpf != "" && !is.null(col_cpf)) {
            manter <- manter & contem_cpf(df[[col_cpf]], cpf)
        }
        
        nome <- str_trim(nome_d() %||% "")
        col_nome <- coluna_por_regex(df, REGEX_COL_NOME)
        if (nome != "" && !is.null(col_nome)) {
            manter <- manter & contem_texto(df[[col_nome]], nome)
        }
        
        detalhe <- str_trim(detalhe_d() %||% "")
        if (detalhe != "") {
            manter <- manter & str_detect(busca(), fixed(normalizar_texto_busca(detalhe)))
        }
        
        df[manter, , drop = FALSE]
    })
    
    output[[id("tabela")]] <- renderDT({
        # Sem arquivo: a área fica vazia — o aviso aparece numa janela ao
        # selecionar a aba (ver mostrar_alerta_sem_arquivo()).
        shiny::validate(need(nrow(dados()) > 0, FALSE))
        
        df <- filtrados()
        
        shiny::validate(need(nrow(df) > 0, MSG_SEM_RESULTADO))
        
        tabela_dt(df)
    })
    
    # ---- Gráfico (agrupamento selecionável) ----
    
    resumo <- reactive({
        if (nrow(dados()) == 0) {
            return(list(resumo = NULL, mensagem = FALSE))
        }
        
        df <- filtrados()
        
        if (nrow(df) == 0) {
            return(list(resumo = NULL, mensagem = MSG_SEM_RESULTADO))
        }
        
        agrupar <- valor("grafico_agrupar") %||% "Cargo"
        regex <- AGRUPAMENTOS_BASE_MAGISTRADOS[[agrupar]]
        grupos <- valores_coluna_regex(df, regex)
        
        if (is.null(grupos)) {
            return(list(
                resumo = NULL,
                mensagem = sprintf("Coluna \"%s\" não encontrada no arquivo carregado.", agrupar)
            ))
        }
        
        col_cpf <- coluna_por_regex(df, REGEX_COL_CPF)
        cpfs <- if (is.null(col_cpf)) rep(NA_character_, nrow(df)) else df[[col_cpf]]
        
        r <- tibble(Grupo = grupos, CPF = cpfs) %>%
            group_by(Grupo) %>%
            summarise(
                Quantidade = n(),
                Pessoas = n_distinct(CPF[!is.na(CPF) & CPF != ""]),
                .groups = "drop"
            ) %>%
            arrange(desc(Quantidade), Grupo)
        
        list(resumo = r, mensagem = NULL, agrupar = agrupar)
    })
    
    output[[id("grafico")]] <- renderGirafe({
        r <- resumo()
        
        shiny::validate(need(is.null(r$mensagem), r$mensagem))
        
        total <- nrow(r$resumo)
        limite <- suppressWarnings(as.integer(valor("grafico_qtd")))
        
        exibido <- if (!is.na(limite) && limite > 0) head(r$resumo, limite) else r$resumo
        
        exibido <- exibido %>%
            mutate(
                dica = sprintf(
                    "<b>%s</b><br/>%s registro(s)<br/>%s magistrado(s) (CPF distintos)",
                    htmltools::htmlEscape(Grupo),
                    format(Quantidade, big.mark = ".", decimal.mark = ","),
                    format(Pessoas, big.mark = ".", decimal.mark = ",")
                )
            )
        
        clicavel <- clique_cargo_ativo()
        
        if (clicavel) {
            exibido$dica <- paste0(exibido$dica, "<br/><i>Clique para ver os magistrados</i>")
        }
        
        grafico_barras_girafe(
            exibido,
            titulo = sprintf("Quantidade de Registros por %s", r$agrupar),
            subtitulo = sprintf(
                "Exibindo %d de %d grupo(s)%s", nrow(exibido), total,
                if (clicavel) " · clique numa barra para ver os registros" else ""
            ),
            selecionavel = clicavel
        )
    })
    
    # ---- Clique numa barra (Agrupar por = Cargo): janela com os registros ----
    
    clique_cargo_ativo <- reactive({
        isTRUE(detalhe_cargo) && identical(valor("grafico_agrupar") %||% "Cargo", "Cargo")
    })
    
    detalhe_atual <- reactiveVal(NULL)
    
    observeEvent(input[[paste0(id("grafico"), "_selected")]], {
        
        cargo <- input[[paste0(id("grafico"), "_selected")]]
        
        if (length(cargo) == 0 || !clique_cargo_ativo()) {
            return()
        }
        
        cargo <- cargo[[1]]
        
        # Desmarca a barra, para que um novo clique na mesma barra volte a
        # abrir a janela.
        session$sendCustomMessage(paste0(ns(id("grafico")), "_set"), character(0))
        
        tab <- detalhe_cargo_magistrados(filtrados(), cargo)
        detalhe_atual(list(cargo = cargo, tabela = tab))
        
        n_pessoas <- if ("CPF" %in% names(tab)) {
            length(unique(tab$CPF[!is.na(tab$CPF) & tab$CPF != ""]))
        } else {
            NA_integer_
        }
        
        fmt <- function(x) format(x, big.mark = ".", decimal.mark = ",")
        
        showModal(modalDialog(
            title = tagList(icon("gavel", class = "me-2"), cargo),
            size = "xl",
            easyClose = TRUE,
            footer = modalButton("Fechar"),
            
            div(
                class = "d-flex flex-wrap gap-4 mb-3",
                div(tags$small(class = "text-muted d-block", "Registros"), tags$strong(fmt(nrow(tab)))),
                if (!is.na(n_pessoas)) {
                    div(tags$small(class = "text-muted d-block", "Magistrados (CPF distintos)"), tags$strong(fmt(n_pessoas)))
                },
                div(
                    tags$small(class = "text-muted d-block", "Filtros"),
                    tags$span("os mesmos aplicados na aba")
                )
            ),
            
            DTOutput(ns(id("detalhe_cargo_tabela")))
        ))
        
    }, ignoreInit = TRUE)
    
    output[[id("detalhe_cargo_tabela")]] <- renderDT({
        d <- detalhe_atual()
        shiny::validate(need(!is.null(d) && nrow(d$tabela) > 0, "Nenhum registro para este cargo."))
        datatable(
            d$tabela,
            rownames = FALSE,
            filter = "top",
            width = "100%",
            options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
        )
    })
    
    # ---- Gerar arquivo CSV ----
    
    # `extra_csv`: UI adicional da aba "Gerar arquivo CSV" (no Envio, o
    # botão "Gerar Novo Envio").
    output[[id("csv_ui")]] <- renderUI({
        tagList(
            ui_csv_dados(ns, id("download_csv"), nrow(filtrados()) > 0),
            if (is.function(extra_csv)) extra_csv()
        )
    })
    
    output[[id("download_csv")]] <- downloadHandler(
        
        filename = function() {
            paste0(nome_csv, "-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
        },
        
        content = function(file) {
            inicio <- Sys.time()
            
            gravar_csv_tabela(filtrados(), file, valor("tabela_rows_all"))
            
            showNotification(
                sprintf("Arquivo CSV gerado em %.2fs.", tempo_decorrido(inicio)),
                type = "message"
            )
        }
    )
    
    list(
        exibicao = exibicao,
        filtrados = filtrados,
        atualizar_combos = atualizar_combos,
        msg_sem_dados = msg_sem_dados,
        detalhe_cargo = detalhe_atual
    )
    
}

# =====================================================
# UI
# =====================================================

mod_magistrados_ui <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        
        # ===================================================
        # ESTILO (escopo deste módulo) — mesmo visual dos demais
        # ===================================================
        
        tags$head(
            tags$style(HTML(sprintf("

        #%1$s .al-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 1rem;
          margin-bottom: 1.25rem;
        }

        #%1$s .al-titulo {
          display: flex;
          align-items: center;
          gap: .65rem;
        }

        #%1$s .al-titulo-icone {
          width: 44px;
          height: 44px;
          border-radius: 50%%;
          display: flex;
          align-items: center;
          justify-content: center;
          background: linear-gradient(135deg, #003366, #0d6efd);
          color: #fff;
          font-size: 1.1rem;
          flex-shrink: 0;
          box-shadow: 0 .3rem .8rem rgba(13,110,253,.2);
        }

        #%1$s .al-titulo h4 {
          margin: 0;
          font-weight: 700;
          letter-spacing: -.01em;
        }

        #%1$s .al-acoes {
          display: flex;
          gap: .6rem;
          flex-wrap: wrap;
        }

        #%1$s .btn-acao {
          font-weight: 600;
          border-radius: .6rem;
          padding: .55rem 1.1rem;
        }

        #%1$s .srv-secoes > .nav-tabs {
          margin-bottom: 1.25rem;
        }

        #%1$s .srv-secoes > .nav-tabs .nav-link {
          font-size: 1rem;
          padding: .6rem 1.1rem;
        }

        #%1$s .al-card {
          background: #fff;
          border: 1px solid rgba(0,0,0,.06);
          border-radius: .9rem;
          box-shadow: 0 .2rem .6rem rgba(15,23,42,.05);
          padding: 1.25rem 1.25rem .5rem 1.25rem;
          margin-bottom: 1.5rem;
        }

        #%1$s .al-card-titulo {
          font-weight: 600;
          font-size: .8rem;
          text-transform: uppercase;
          letter-spacing: .04em;
          color: #6c757d;
          margin-bottom: .9rem;
        }

        #%1$s .al-conteudo {
          background: #fff;
          border: 1px solid rgba(0,0,0,.06);
          border-radius: .9rem;
          box-shadow: 0 .2rem .6rem rgba(15,23,42,.05);
          padding: 1.25rem;
        }

        #%1$s .nav-tabs .nav-link.active {
          font-weight: 600;
          color: #0d6efd;
        }

        .al-modal-secao-titulo {
          font-weight: 600;
          font-size: 1rem;
          display: flex;
          align-items: center;
          gap: .5rem;
          margin-bottom: .35rem;
        }

        .al-modal-secao-desc {
          font-size: .82rem;
          color: #6c757d;
          margin-bottom: .85rem;
        }

        .srv-arquivo-linha {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: .75rem;
          padding: .6rem .75rem;
          border: 1px solid #e9ecef;
          border-radius: .6rem;
          margin-bottom: .5rem;
        }

        .srv-arquivo-nome {
          font-weight: 600;
          font-size: .9rem;
        }

        .srv-arquivo-info {
          font-size: .78rem;
          color: #6c757d;
        }

        #%1$s .dataTables_wrapper {
          width: 100%% !important;
        }

        /* Tabelas que cabem na tela ocupam a largura toda. Sem
           !important: com rolagem horizontal, o DataTables precisa dar
           a mesma largura (maior que a tela) ao cabeçalho e aos dados —
           forçar 100%% na tabela de dados desalinhava os títulos das
           colunas ao redimensionar a janela. */
        #%1$s table.dataTable {
          min-width: 100%%;
        }

      ", id)))
        ),
        
        div(
            
            id = id,
            
            # =================================================
            # CABEÇALHO - TÍTULO + AÇÕES (valem para as duas abas)
            # =================================================
            
            div(
                class = "al-header",
                
                div(
                    class = "al-titulo",
                    div(class = "al-titulo-icone", icon("gavel")),
                    tags$h4("Magistrados")
                ),
                
                div(
                    class = "al-acoes",
                    
                    actionButton(
                        ns("atualizar"),
                        tagList(icon("rotate", class = "me-2"), "Atualizar Dados"),
                        class = "btn btn-outline-primary btn-acao"
                    ),
                    
                    actionButton(
                        ns("gerenciar_arquivos"),
                        tagList(icon("folder-open", class = "me-2"), "Gerenciar Arquivos"),
                        class = "btn btn-primary btn-acao"
                    )
                )
                
            ),
            
            div(
                class = "srv-secoes",
                
                tabsetPanel(
                    id = ns("secao"),
                    
                    # =============================================
                    # ABA 1 — BASE DE DADOS e ABA 2 — ENVIO
                    # -------------------------------------------------
                    # Mesma estrutura de Pessoal e Auxiliar: as duas abas
                    # têm os mesmos filtros, gráfico e CSV (ver
                    # ui_aba_cadastro_magistrados()).
                    # =============================================
                    
                    ui_aba_cadastro_magistrados(ns, "base", titulo = "Base de Dados", icone = "database"),
                    
                    ui_aba_cadastro_magistrados(ns, "envio", titulo = "Envio", icone = "paper-plane"),
                    
                    # =============================================
                    # ABA 3 — ALERTAS - MAGISTRADOS
                    # -------------------------------------------------
                    # "Alerta" e "Conflito" se excluem mutuamente.
                    # =============================================
                    
                    tabPanel(
                        title = tagList(icon("triangle-exclamation", class = "me-1"), "Alertas"),
                        value = "alertas",
                        
                        div(
                            class = "al-card",
                            # Bloqueado (handler JS "bloquear-filtros") sem arquivo.
                            id = ns("alertas_filtros"),
                            
                            div(class = "al-card-titulo", "Filtros"),
                            
                            div(
                                class = "al-filtros-aviso",
                                icon("lock", class = "me-2"),
                                "Filtros indisponíveis: não há arquivo de alertas para exibir. Envie-o pelo botão \"Gerenciar Arquivos\"."
                            ),
                            
                            fluidRow(
                                column(4, selectInput(ns("alerta"), "Alerta", choices = c("Todos"), width = "100%")),
                                column(4, selectInput(ns("conflito"), "Conflito", choices = c("Todos"), width = "100%")),
                                column(4, selectInput(ns("cargo"), "Cargo", choices = c("Todos"), width = "100%"))
                            ),
                            
                            fluidRow(
                                column(4, textInput(ns("cpf"), "CPF", width = "100%")),
                                column(8, textInput(ns("nome"), "Nome", width = "100%"))
                            ),
                            
                            fluidRow(
                                column(
                                    12,
                                    actionButton(
                                        ns("limpar_filtros"),
                                        tagList(icon("filter-circle-xmark", class = "me-2"), "Limpar Filtros"),
                                        class = "btn btn-outline-secondary btn-sm"
                                    )
                                )
                            )
                        ),
                        
                        div(
                            class = "al-conteudo",
                            
                            tabsetPanel(
                                tabPanel(
                                    tagList(icon("table", class = "me-1"), "Tabela"),
                                    DTOutput(ns("tabela"))
                                ),
                                tabPanel(
                                    tagList(icon("chart-column", class = "me-1"), "Gráfico"),
                                    uiOutput(ns("grafico_ui")),
                                    tags$hr(class = "my-4"),
                                    girafeOutput(ns("grafico_cargo"))
                                ),
                                tabPanel(
                                    tagList(icon("file-csv", class = "me-1"), "Gerar arquivo CSV"),
                                    uiOutput(ns("csv_ui"))
                                )
                            )
                        )
                    ),
                    
                    # =============================================
                    # ABA 4 — INCONSISTÊNCIAS - MAGISTRADOS
                    # -------------------------------------------------
                    # Verificações automáticas sobre a base (ver
                    # verificar_inconsistencias_magistrados()).
                    # =============================================
                    
                    tabPanel(
                        title = tagList(
                            icon("clipboard-check", class = "me-1"),
                            "Inconsistências",
                            mod_inconsistencias_badge_ui(ns("inc"))
                        ),
                        value = "inconsistencias",
                        
                        mod_inconsistencias_ui(
                            ns("inc"),
                            verificacoes = c(VERIFICACAO_MAG_CARGO, VERIFICACAO_MAG_APOSENTADORIA),
                            filtros_texto = c("CPF" = "CPF", "Nome" = "Nome")
                        )
                    )
                )
            )
        )
    )
    
}

# =====================================================
# SERVER
# =====================================================

mod_magistrados_server <- function(id, ativo = reactive(TRUE), empresa = reactive(NULL)) {
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        # Começam vazios: a empresa só é conhecida depois do login.
        dados_base <- reactiveVal(data.frame())   # magistrados.xlsx/.csv
        dados_envio <- reactiveVal(data.frame())  # magistrados_envio.xlsx/.csv
        nomes_orgaos <- reactiveVal(character(0)) # código -> nome (base de Serventias)
        dados <- reactiveVal(data.frame())        # alertas_magistrados.csv/.xlsx
        
        empresa_definida <- function() {
            e <- empresa()
            !is.null(e) && !is.na(e) && trimws(e) != ""
        }
        
        recarregar <- function() {
            pasta <- pasta_magistrados(empresa())
            dados_base(carregar_arquivo_magistrados(mag_arquivo_do_tipo(pasta, "base")))
            dados_envio(carregar_arquivo_magistrados(mag_arquivo_do_tipo(pasta, "envio")))
            dados(carregar_arquivo_magistrados(mag_arquivo_do_tipo(pasta, "alertas")))
            # Nomes dos órgãos (para achar gabinetes de desembargador).
            # A base de Serventias é de outro módulo: mudanças nela entram
            # aqui no "Atualizar Dados" ou na troca de empresa.
            nomes_orgaos(mag_nomes_orgaos(empresa()))
        }
        
        # ID novo do fileInput a cada abertura do modal (ver mod_pessoal_auxiliar.R).
        contador_upload <- reactiveVal(0)
        
        id_upload_atual <- function() {
            paste0("upload_magistrados_", contador_upload())
        }
        
        entrada_upload_atual <- function() {
            input[[id_upload_atual()]]
        }
        
        # Carga SOB DEMANDA (ver mod_pessoal_auxiliar.R): só quando o módulo é
        # aberto pela primeira vez para a empresa da sessão, em vez de logo
        # no login. Com a empresa vazia (logout), os dados são descartados.
        empresa_carregada <- reactiveVal(NULL)
        
        observeEvent(list(empresa(), ativo()), {
            
            if (!empresa_definida()) {
                if (!is.null(empresa_carregada())) {
                    dados_base(data.frame())
                    dados_envio(data.frame())
                    dados(data.frame())
                    nomes_orgaos(character(0))
                    empresa_carregada(NULL)
                }
                return(invisible(NULL))
            }
            
            req(ativo())
            
            if (!identical(empresa_carregada(), empresa())) {
                recarregar()
                empresa_carregada(empresa())
            }
            
        }, ignoreInit = FALSE)
        
        # =================================================
        # EXIBIÇÃO (códigos -> nomenclatura)
        # -------------------------------------------------
        # Base: mesmas colunas e ordem do arquivo; filtros, Detalhe,
        # gráfico e CSV usam esta versão.
        # Alertas: a tradução é aplicada ANTES dos filtros, então o combo
        # Cargo, o gráfico por Cargo e a tabela mostram a mesma coisa.
        # =================================================
        
        # Abas Base de Dados e Envio (mesmos filtros, gráfico e CSV) — ver
        # servidor_aba_cadastro_magistrados().
        aba_base <- servidor_aba_cadastro_magistrados(
            input, output, session, "base",
            dados = dados_base,
            msg_sem_dados = paste0(
                "Não existe arquivo de Magistrados (magistrados.xlsx ou magistrados.csv)",
                " para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviá-lo."
            ),
            nome_csv = "base-magistrados",
            mapa_orgaos = nomes_orgaos,
            detalhe_cargo = TRUE
        )
        
        aba_envio <- servidor_aba_cadastro_magistrados(
            input, output, session, "envio",
            dados = dados_envio,
            msg_sem_dados = paste0(
                "Não existe arquivo de envio de Magistrados (magistrados_envio.xlsx ou .csv)",
                " para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviá-lo",
                " (o nome do arquivo precisa conter \"envio\")."
            ),
            nome_csv = "envio-magistrados",
            mapa_orgaos = nomes_orgaos,
            extra_csv = function() uiOutput(ns("novo_envio_ui"))
        )
        
        # ---- Envio > Gerar arquivo CSV > "Gerar Novo Envio" ----
        # Ver gerar_novo_envio_magistrados(). Usa os arquivos como foram
        # lidos (códigos, sem a tradução da tela) e todos os registros (os
        # filtros da aba não se aplicam).
        novo_envio <- reactive({
            gerar_novo_envio_magistrados(dados_envio(), dados_base(), dados())
        })
        
        output$novo_envio_ui <- renderUI({
            
            r <- novo_envio()
            pode_gerar <- nrow(r$dados) > 0
            
            ui_novo_envio(
                ns, pode_gerar,
                descricao = tagList(
                    "Para cada CPF com o alerta ", tags$b("Há cargos distintos ativos para o mesmo CPF"),
                    " (ou o conflito ", tags$b("Duplicidade no próprio Tribunal"), ") nos Alertas, gera duas linhas, ",
                    "com as mesmas colunas do arquivo de envio: o registro da Base de Dados que continua em aberto ",
                    "(sem Data de saída, situação não encerrada e cargo diferente do envio, com início anterior ao do envio) ",
                    "apenas com a inclusão da ", tags$b("Data de saída da situação"),
                    " igual à Data de início da situação do envio, e logo abaixo o registro do envio, sem alteração. ",
                    "Os valores saem como nos arquivos (códigos, não os textos da tela). Os filtros desta aba não se aplicam."
                ),
                resumo = if (length(r$cpfs_conflito) == 0) {
                    "Nenhum CPF com esse alerta no arquivo de alertas."
                } else {
                    tagList(
                        sprintf("%d CPF(s) com o alerta; %d no novo envio (%d linhas).",
                                length(r$cpfs_conflito), nrow(r$dados) / 2, nrow(r$dados)),
                        if (length(r$sem_envio) > 0) {
                            div(class = "text-warning", sprintf("%d CPF(s) não estão no arquivo de envio.", length(r$sem_envio)))
                        },
                        if (length(r$sem_base) > 0) {
                            div(class = "text-warning", sprintf(
                                "%d CPF(s) sem registro em aberto, de outro cargo, na Base de Dados com início anterior ao do envio.",
                                length(r$sem_base)
                            ))
                        }
                    )
                }
            )
            
        })
        
        output$novo_envio_download <- downloadHandler(
            
            filename = function() {
                paste0("novo-envio-magistrados-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
            },
            
            content = function(file) {
                r <- novo_envio()
                readr::write_excel_csv2(r$dados, file, na = "")
                
                showNotification(
                    sprintf("Novo envio gerado: %d linha(s) de %d CPF(s).", nrow(r$dados), nrow(r$dados) / 2),
                    type = "message"
                )
            }
        )
        
        base_exibicao <- aba_base$exibicao
        dados_base_filtrados <- aba_base$filtrados
        envio_exibicao <- aba_envio$exibicao
        dados_envio_filtrados <- aba_envio$filtrados
        
        alertas_exibicao <- reactive({
            traduzir_codigos_magistrados(dados())
        })
        
        # =================================================
        # COMBOS
        # =================================================
        
        atualizar_combos_base <- function() {
            aba_base$atualizar_combos()
            aba_envio$atualizar_combos()
        }
        
        atualizar_combos <- function() {
            
            df <- alertas_exibicao()
            
            colunas_alerta <- colunas_com_dados(df, colunas_por_prefixo(df, "Alerta"))
            colunas_conflito <- colunas_com_dados(df, colunas_por_prefixo(df, "Conflito"))
            cargos <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_CARGO))
            
            updateSelectInput(
                session, "alerta",
                choices = c("Todos", colunas_alerta),
                selected = selecao_valida(input$alerta, colunas_alerta)
            )
            
            updateSelectInput(
                session, "conflito",
                choices = c("Todos", colunas_conflito),
                selected = selecao_valida(input$conflito, colunas_conflito)
            )
            
            updateSelectInput(
                session, "cargo",
                choices = c("Todos", cargos),
                selected = selecao_valida(input$cargo, cargos)
            )
            
        }
        
        atualizar_todos_combos <- function() {
            atualizar_combos_base()
            atualizar_combos()
        }
        
        observeEvent(list(dados_base(), dados_envio(), dados(), ativo()), {
            req(ativo())
            atualizar_todos_combos()
        }, ignoreInit = FALSE)
        
        observeEvent(input$atualizar, {
            req(empresa())
            inicio <- Sys.time()
            
            # "Atualizar Dados" sempre relê do disco.
            limpar_cache_arquivos(file.path(PASTA_ALERTAS_BASE, empresa()))
            recarregar()
            empresa_carregada(empresa())
            atualizar_todos_combos()
            
            showNotification(
                sprintf("Dados atualizados em %.2fs.", tempo_decorrido(inicio)),
                type = "message"
            )
        })
        
        # ----------------------------------------
        # LIMPAR FILTROS
        # ----------------------------------------
        
        observeEvent(input$limpar_filtros, {
            updateSelectInput(session, "alerta", selected = "Todos")
            updateSelectInput(session, "conflito", selected = "Todos")
            updateSelectInput(session, "cargo", selected = "Todos")
            updateTextInput(session, "cpf", value = "")
            updateTextInput(session, "nome", value = "")
        })
        
        # ----------------------------------------
        # EXCLUSÃO MÚTUA (Alerta x Conflito)
        # ----------------------------------------
        
        observeEvent(input$alerta, {
            req(input$alerta)
            if (input$alerta != "Todos" && !is.null(input$conflito) && input$conflito != "Todos") {
                updateSelectInput(session, "conflito", selected = "Todos")
            }
        }, ignoreInit = TRUE)
        
        observeEvent(input$conflito, {
            req(input$conflito)
            if (input$conflito != "Todos" && !is.null(input$alerta) && input$alerta != "Todos") {
                updateSelectInput(session, "alerta", selected = "Todos")
            }
        }, ignoreInit = TRUE)
        
        # =================================================
        # GERENCIAR ARQUIVOS (comum às duas abas)
        # =================================================
        
        linha_arquivo_modal <- function(pasta, tipo, id_apagar) {
            
            info <- TIPOS_ARQUIVO_MAGISTRADOS[[tipo]]
            existentes <- mag_arquivos_do_tipo(pasta, tipo)
            
            nome_exibido <- if (length(existentes) > 0) {
                basename(existentes[1])
            } else {
                paste0(info$radical, ".xlsx / .csv")
            }
            
            icone <- if (length(existentes) > 0 && extensao_arquivo(existentes[1]) == "xlsx") {
                "file-excel"
            } else {
                "file-csv"
            }
            
            div(
                class = "srv-arquivo-linha",
                div(
                    div(class = "srv-arquivo-nome", icon(icone, class = "me-1"), nome_exibido),
                    div(
                        class = "srv-arquivo-info",
                        info$rotulo, " — ",
                        if (length(existentes) > 0) {
                            sprintf(
                                "atualizado em %s",
                                format(file.info(existentes[1])$mtime, "%d/%m/%Y %H:%M")
                            )
                        } else {
                            "ainda não enviado"
                        },
                        if (length(existentes) > 1) {
                            tags$div(
                                class = "text-warning",
                                sprintf("também na pasta (ignorado): %s", paste(basename(existentes[-1]), collapse = ", "))
                            )
                        }
                    )
                ),
                if (length(existentes) > 0) {
                    actionButton(
                        ns(id_apagar),
                        tagList(icon("trash", class = "me-1"), "Apagar"),
                        class = "btn btn-outline-danger btn-sm"
                    )
                }
            )
            
        }
        
        abrir_gerenciar_arquivos <- function() {
            
            if (!empresa_definida()) {
                showNotification(
                    "Nenhuma empresa definida para esta sessão. Faça login novamente (ou use \"Trocar empresa\").",
                    type = "error",
                    duration = 8
                )
                return(invisible(NULL))
            }
            
            pasta <- pasta_magistrados(empresa())
            
            contador_upload(contador_upload() + 1)
            
            showModal(modalDialog(
                title = div(
                    style = "position:relative; padding-right:28px;",
                    icon("folder-open", class = "me-2"),
                    sprintf("Gerenciar Arquivos de Magistrados — %s", empresa()),
                    tags$button(
                        type = "button",
                        class = "btn-close",
                        style = "position:absolute; top:2px; right:0;",
                        `aria-label` = "Fechar",
                        onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
                            ns("fechar_modal_magistrados")
                        )
                    )
                ),
                size = "m",
                easyClose = TRUE,
                
                div(
                    class = "mb-4",
                    
                    div(
                        class = "al-modal-secao-titulo",
                        icon("folder", class = "text-secondary"),
                        "Arquivos na pasta"
                    ),
                    
                    linha_arquivo_modal(pasta, "base", "apagar_base"),
                    linha_arquivo_modal(pasta, "envio", "apagar_envio"),
                    linha_arquivo_modal(pasta, "alertas", "apagar_alertas")
                ),
                
                tags$hr(),
                
                div(
                    
                    div(
                        class = "al-modal-secao-titulo",
                        icon("upload", class = "text-primary"),
                        "Enviar arquivos para a pasta"
                    ),
                    
                    div(
                        class = "al-modal-secao-desc",
                        "Selecione o arquivo de Magistrados, o de envio e/ou o de Alertas de ",
                        "Magistrados gerados pelo MPM, em ", tags$b(".xlsx"), " ou ", tags$b(".csv"),
                        " (um de cada tipo, limite de ", MAX_UPLOAD_MB,
                        " MB no total). O tipo é identificado pelas colunas e pelo nome: com colunas ",
                        "\"Alerta: ...\" o arquivo é gravado como ", tags$code("alertas_magistrados"),
                        "; sem alertas e com ", tags$b("envio"), " no nome, como ",
                        tags$code("magistrados_envio"), "; sem alertas, como ", tags$code("magistrados"),
                        " — sempre com a extensão original. Um arquivo já existente do ",
                        "mesmo tipo é substituído."
                    ),
                    
                    div(
                        class = "text-muted mb-2",
                        style = "font-size: .78rem;",
                        icon("triangle-exclamation", class = "me-1"),
                        "Esses arquivos ficam no disco local da aplicação e podem ",
                        "ser perdidos em um reinício no shinyapps.io."
                    ),
                    
                    fileInput(
                        ns(id_upload_atual()),
                        NULL,
                        multiple = TRUE,
                        accept = c(
                            ".csv", ".xlsx", "text/csv",
                            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                        ),
                        width = "100%",
                        buttonLabel = "Procurar...",
                        placeholder = "Nenhum arquivo selecionado"
                    ),
                    
                    actionButton(
                        ns("enviar_arquivos"),
                        tagList(icon("upload", class = "me-2"), "Enviar para Pasta"),
                        class = "btn btn-primary w-100"
                    )
                ),
                
                footer = actionButton(ns("fechar_modal_magistrados"), "Fechar")
            ))
            
        }
        
        observeEvent(input$gerenciar_arquivos, abrir_gerenciar_arquivos())
        
        observeEvent(input$fechar_modal_magistrados, {
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
        })
        
        # =================================================
        # SEM ARQUIVO: JANELA DE AVISO E FILTROS BLOQUEADOS
        # -------------------------------------------------
        # Igual a Pessoal e Auxiliar. Nas abas Base de Dados, Envio e
        # Alertas, quando não há arquivo:
        #   - ao selecionar a aba (ou abrir o módulo nela), abre uma janela
        #     com o aviso e o botão "Gerenciar Arquivos";
        #   - os filtros (e os controles do gráfico) ficam bloqueados até
        #     o arquivo ser enviado — handler JS "bloquear-filtros", em
        #     app.R;
        #   - a área da tabela e do gráfico fica vazia.
        # =================================================
        
        ABAS_COM_ARQUIVO <- list(
            base = list(
                rotulo = "Base de Dados",
                mensagem = function() aba_base$msg_sem_dados,
                sem_arquivo = function() nrow(dados_base()) == 0,
                ids = function() c(ns("base_filtros"), ns("base_grafico_controles"))
            ),
            envio = list(
                rotulo = "Envio",
                mensagem = function() aba_envio$msg_sem_dados,
                sem_arquivo = function() nrow(dados_envio()) == 0,
                ids = function() c(ns("envio_filtros"), ns("envio_grafico_controles"))
            ),
            alertas = list(
                rotulo = "Alertas",
                mensagem = function() MSG_SEM_ARQUIVOS,
                sem_arquivo = function() nrow(dados()) == 0,
                ids = function() ns("alertas_filtros")
            )
        )
        
        # Última janela de aviso aberta (aba) — usado nos testes.
        alerta_sem_arquivo_aberto <- reactiveVal(NULL)
        
        mostrar_alerta_sem_arquivo <- function(aba) {
            
            cfg <- ABAS_COM_ARQUIVO[[aba]]
            alerta_sem_arquivo_aberto(aba)
            
            showModal(modalDialog(
                div(
                    class = "al-sem-arquivo",
                    role = "alert",
                    div(class = "al-sem-arquivo-icone", icon("folder-open")),
                    div(class = "al-sem-arquivo-titulo", sprintf("%s: nenhum arquivo para exibir", cfg$rotulo)),
                    div(class = "al-sem-arquivo-texto", cfg$mensagem()),
                    div(
                        class = "al-sem-arquivo-nota",
                        "Os filtros desta aba ficam indisponíveis até o arquivo ser enviado."
                    )
                ),
                title = NULL,
                size = "m",
                easyClose = TRUE,
                footer = tagList(
                    modalButton("Fechar"),
                    actionButton(
                        ns("sem_arquivo_gerenciar"),
                        tagList(icon("folder-open", class = "me-2"), "Gerenciar Arquivos"),
                        class = "btn btn-primary"
                    )
                )
            ))
            
        }
        
        # Ao selecionar Base de Dados, Envio ou Alertas — ou ao abrir o
        # módulo numa delas —, sem arquivo: abre a janela de aviso.
        observeEvent(list(input$secao, ativo()), {
            req(isTRUE(ativo()), empresa_definida())
            aba <- input$secao %||% "base"
            cfg <- ABAS_COM_ARQUIVO[[aba]]
            if (!is.null(cfg) && cfg$sem_arquivo()) {
                mostrar_alerta_sem_arquivo(aba)
            }
        }, ignoreInit = FALSE)
        
        observeEvent(input$sem_arquivo_gerenciar, abrir_gerenciar_arquivos(), ignoreInit = TRUE)
        
        # Filtros bloqueados enquanto não houver arquivo (reaplicado quando
        # os dados mudam e quando o módulo é aberto).
        filtros_bloqueados <- reactive({
            vapply(ABAS_COM_ARQUIVO, function(cfg) cfg$sem_arquivo(), logical(1))
        })
        
        observe({
            bloq <- filtros_bloqueados()
            ativo()
            for (aba in names(ABAS_COM_ARQUIVO)) {
                session$sendCustomMessage(
                    "bloquear-filtros",
                    list(ids = as.list(ABAS_COM_ARQUIVO[[aba]]$ids()), bloqueado = isTRUE(bloq[[aba]]))
                )
            }
        })
        
        # ----------------------------------------
        # APAGAR (um arquivo por vez)
        # ----------------------------------------
        
        tipo_para_apagar <- reactiveVal(NULL)
        
        pedir_confirmacao_apagar <- function(tipo) {
            req(empresa())
            
            info <- TIPOS_ARQUIVO_MAGISTRADOS[[tipo]]
            existentes <- mag_arquivos_do_tipo(pasta_magistrados(empresa()), tipo)
            
            if (length(existentes) == 0) {
                showNotification(sprintf("Não há arquivo de %s na pasta.", info$rotulo), type = "warning")
                return(invisible(NULL))
            }
            
            tipo_para_apagar(tipo)
            
            removeModal()
            
            showModal(modalDialog(
                title = "Confirmar exclusão",
                sprintf(
                    "Tem certeza que deseja apagar o arquivo %s (%s) da empresa %s? Esta ação não pode ser desfeita.",
                    paste(basename(existentes), collapse = " e "), info$rotulo, empresa()
                ),
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(ns("confirmar_apagar"), "Apagar", class = "btn-danger")
                )
            ))
        }
        
        observeEvent(input$apagar_base, pedir_confirmacao_apagar("base"))
        observeEvent(input$apagar_envio, pedir_confirmacao_apagar("envio"))
        observeEvent(input$apagar_alertas, pedir_confirmacao_apagar("alertas"))
        
        observeEvent(input$confirmar_apagar, {
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
            
            req(empresa(), tipo_para_apagar())
            
            tipo <- tipo_para_apagar()
            info <- TIPOS_ARQUIVO_MAGISTRADOS[[tipo]]
            tipo_para_apagar(NULL)
            
            inicio <- Sys.time()
            existentes <- mag_arquivos_do_tipo(pasta_magistrados(empresa()), tipo)
            nomes_existentes <- paste(basename(existentes), collapse = " e ")
            
            if (length(existentes) == 0) {
                showNotification(sprintf("Não há arquivo de %s na pasta.", info$rotulo), type = "warning")
                return()
            }
            
            removidos <- tryCatch(
                file.remove(existentes),
                error = function(e) {
                    showNotification(sprintf("Erro ao apagar o arquivo: %s", conditionMessage(e)), type = "error")
                    NULL
                }
            )
            
            if (is.null(removidos)) {
                return()
            }
            
            if (all(removidos)) {
                showNotification(
                    sprintf("Arquivo %s apagado em %.2fs.", nomes_existentes, tempo_decorrido(inicio)),
                    type = "message"
                )
            } else {
                showNotification(
                    sprintf("O arquivo %s não pôde ser apagado (verifique se está aberto em outro programa).", nomes_existentes),
                    type = "error"
                )
            }
            
            recarregar()
            atualizar_todos_combos()
        })
        
        # ----------------------------------------
        # UPLOAD (um ou dois arquivos; tipo identificado pelas colunas)
        # ----------------------------------------
        
        upload_pendente <- reactiveVal(NULL)
        
        processar_upload <- function(pendente) {
            req(pendente, empresa())
            
            pasta <- pasta_magistrados(empresa())
            req(pasta)
            
            inicio <- Sys.time()
            
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
            
            gravados <- character(0)
            
            for (i in seq_len(nrow(pendente))) {
                
                info <- TIPOS_ARQUIVO_MAGISTRADOS[[pendente$tipo[i]]]
                
                # O anterior do mesmo tipo, em QUALQUER formato, é
                # substituído (senão um .xlsx antigo continuaria com
                # prioridade sobre um .csv novo) — mas só DEPOIS de o novo
                # estar gravado (gravar_arquivo_substituindo(), em
                # R/utils.R). Antes, o anterior era apagado primeiro e uma
                # falha na cópia deixava a empresa sem o arquivo.
                anteriores <- mag_arquivos_do_tipo(pasta, pendente$tipo[i])

                destino <- paste0(info$radical, ".", pendente$extensao[i])

                if (gravar_arquivo_substituindo(pendente$datapath[i], file.path(pasta, destino), anteriores)) {
                    gravados <- c(gravados, sprintf("%s → %s", pendente$name[i], destino))
                } else {
                    showNotification(
                        sprintf("O arquivo %s não pôde ser copiado para a pasta.", pendente$name[i]),
                        type = "error"
                    )
                }
            }
            
            if (length(gravados) > 0) {
                showNotification(
                    sprintf(
                        "Enviado(s) em %.2fs: %s. Atualizando as tabelas...",
                        tempo_decorrido(inicio),
                        paste(gravados, collapse = "; ")
                    ),
                    type = "message",
                    duration = 8
                )
            }
            
            upload_pendente(NULL)
            
            tryCatch({
                recarregar()
                atualizar_todos_combos()
            }, error = function(e) {
                showNotification(
                    sprintf("Arquivo(s) enviado(s), mas houve erro ao recarregar as tabelas: %s", conditionMessage(e)),
                    type = "error",
                    duration = 15
                )
            })
        }
        
        observeEvent(input$enviar_arquivos, {
            up <- entrada_upload_atual()
            req(up, empresa())
            
            extensoes <- extensao_arquivo(up$name)
            
            tipos <- vapply(
                seq_len(nrow(up)),
                function(i) {
                    if (!(extensoes[i] %in% EXTENSOES_PLANILHA)) {
                        return(NA_character_)
                    }
                    identificar_tipo_arquivo_magistrados(
                        ler_arquivo_magistrados(up$datapath[i], extensoes[i], up$name[i], n_max = 5),
                        nome_arquivo = up$name[i]
                    )
                },
                character(1)
            )
            
            invalidos <- up$name[is.na(tipos)]
            
            if (length(invalidos) > 0) {
                showNotification(
                    sprintf(
                        "Não reconhecido(s) como arquivo de Magistrados ou de Alertas de Magistrados em .xlsx ou .csv (ignorado[s]): %s. O arquivo precisa ter as colunas CPF e \"Órgão de lotação do magistrado(a)\".",
                        paste(invalidos, collapse = ", ")
                    ),
                    type = "error",
                    duration = 12
                )
            }
            
            validos <- !is.na(tipos)
            
            if (!any(validos)) {
                return(invisible(NULL))
            }
            
            if (any(duplicated(tipos[validos]))) {
                showNotification(
                    "Foram selecionados dois arquivos do mesmo tipo. Envie no máximo um arquivo de cada tipo (base, envio e alertas de Magistrados).",
                    type = "error",
                    duration = 12
                )
                return(invisible(NULL))
            }
            
            pendente <- data.frame(
                name = up$name[validos],
                datapath = up$datapath[validos],
                tipo = tipos[validos],
                extensao = extensoes[validos],
                stringsAsFactors = FALSE
            )
            
            upload_pendente(pendente)
            
            pasta <- pasta_magistrados(empresa())
            
            a_substituir <- vapply(
                pendente$tipo,
                function(t) {
                    existentes <- mag_arquivos_do_tipo(pasta, t)
                    if (length(existentes) > 0) paste(basename(existentes), collapse = ", ") else NA_character_
                },
                character(1)
            )
            a_substituir <- a_substituir[!is.na(a_substituir)]
            
            if (length(a_substituir) > 0) {
                
                removeModal()
                
                showModal(modalDialog(
                    title = "Arquivo(s) existente(s) na pasta",
                    sprintf(
                        "A pasta da empresa %s já contém: %s. Deseja substituir pelo(s) arquivo(s) selecionado(s)?",
                        empresa(),
                        paste(a_substituir, collapse = ", ")
                    ),
                    footer = tagList(
                        modalButton("Cancelar"),
                        actionButton(ns("confirmar_substituir"), "Substituir", class = "btn-danger")
                    )
                ))
                
            } else {
                processar_upload(pendente)
            }
        })
        
        observeEvent(input$confirmar_substituir, {
            processar_upload(upload_pendente())
        })
        
        # =================================================
        # ABA 3 — ALERTAS - MAGISTRADOS
        # =================================================
        
        MSG_SEM_ARQUIVOS <- paste0(
            "Não existe arquivo de alertas de Magistrados (alertas_magistrados.csv ou alertas_magistrados.xlsx)",
            " para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviá-lo."
        )
        
        MSG_ALERTAS_SEM_RESULTADO <- "Nenhum registro encontrado com os filtros aplicados."
        
        MSG_SEM_BASE <- aba_base$msg_sem_dados
        
        # Espera o usuário parar de digitar (400 ms) antes de filtrar.
        cpf_d <- debounce(reactive(input$cpf), 400)
        nome_d <- debounce(reactive(input$nome), 400)
        
        dados_filtrados <- reactive({
            df <- alertas_exibicao()
            
            if (nrow(df) == 0) {
                return(df)
            }
            
            manter <- rep(TRUE, nrow(df))
            
            filtrar_texto <- function(valor, regex) {
                valor <- str_trim(valor %||% "")
                col <- coluna_por_regex(df, regex)
                if (valor != "" && !is.null(col)) {
                    manter <<- manter & str_detect(
                        normalizar_texto_busca(coalesce(df[[col]], "")),
                        fixed(normalizar_texto_busca(valor))
                    )
                }
            }
            
            filtrar_preenchida <- function(coluna) {
                if (!is.null(coluna) && coluna != "Todos" && coluna %in% names(df)) {
                    v <- df[[coluna]]
                    manter <<- manter & !is.na(v) & str_trim(v) != ""
                }
            }
            
            filtrar_texto(cpf_d(), REGEX_COL_CPF)
            filtrar_texto(nome_d(), REGEX_COL_NOME)
            filtrar_preenchida(input$alerta)
            filtrar_preenchida(input$conflito)
            
            if (!is.null(input$cargo) && input$cargo != "Todos") {
                v <- valores_coluna_regex(df, REGEX_COL_CARGO)
                if (!is.null(v)) manter <- manter & v == input$cargo
            }
            
            df[manter, , drop = FALSE]
        })
        
        tabela_exibicao <- reactive({
            consolidar_alertas_magistrados_tabela(dados_filtrados())
        })
        
        output$tabela <- renderDT({
            # Sem arquivo: a área fica vazia — o aviso aparece numa janela
            # ao selecionar a aba (ver mostrar_alerta_sem_arquivo()).
            shiny::validate(need(nrow(dados()) > 0, FALSE))
            
            df <- tabela_exibicao()
            
            shiny::validate(need(nrow(df) > 0, MSG_ALERTAS_SEM_RESULTADO))
            
            datatable(
                df,
                filter = "top",
                rownames = FALSE,
                width = "100%",
                options = list(
                    pageLength = 20,
                    scrollX = TRUE,
                    autoWidth = TRUE,
                    width = "100%"
                )
            )
        })
        
        # ---- Gráficos: tipo de Alerta/Conflito (echarts4r) e Cargo (ggiraph) ----
        
        resumo_tipos <- reactive({
            if (nrow(dados()) == 0) {
                return(list(resumo = NULL, mensagem = FALSE))
            }
            
            df <- dados_filtrados()
            
            if (nrow(df) == 0) {
                return(list(resumo = NULL, mensagem = MSG_ALERTAS_SEM_RESULTADO))
            }
            
            colunas_tipo <- c(
                colunas_por_prefixo(df, "Alerta"),
                colunas_por_prefixo(df, "Conflito")
            )
            
            if (length(colunas_tipo) == 0) {
                return(list(
                    resumo = NULL,
                    mensagem = "Nenhuma coluna de Alerta/Conflito encontrada no arquivo carregado."
                ))
            }
            
            resumo <- map_dfr(colunas_tipo, function(col) {
                valores <- df[[col]]
                tibble(Tipo = col, Quantidade = sum(!is.na(valores) & str_trim(valores) != ""))
            }) %>%
                filter(Quantidade > 0) %>%
                arrange(Quantidade) %>%
                mutate(Tipo = str_wrap(Tipo, 50))
            
            if (nrow(resumo) == 0) {
                return(list(resumo = NULL, mensagem = "Nenhum alerta/conflito encontrado nos dados atuais."))
            }
            
            list(resumo = resumo, mensagem = NULL)
        })
        
        resumo_cargos <- reactive({
            if (nrow(dados()) == 0) {
                return(list(resumo = NULL, mensagem = FALSE))
            }
            
            df <- dados_filtrados()
            
            if (nrow(df) == 0) {
                return(list(resumo = NULL, mensagem = MSG_ALERTAS_SEM_RESULTADO))
            }
            
            valores <- valores_coluna_regex(df, REGEX_COL_CARGO)
            
            if (is.null(valores)) {
                return(list(resumo = NULL, mensagem = "Coluna \"Cargo\" não encontrada no arquivo carregado."))
            }
            
            resumo <- tibble(Cargo = valores) %>%
                count(Cargo, name = "Quantidade") %>%
                arrange(Quantidade)
            
            list(resumo = resumo, mensagem = NULL)
        })
        
        output$grafico_ui <- renderUI({
            r <- resumo_tipos()
            n <- if (is.null(r$resumo)) 0 else nrow(r$resumo)
            altura <- if (n == 0) 120 else max(320, 40 * n + 110)
            
            echarts4rOutput(ns("grafico"), height = paste0(altura, "px"))
        })
        
        output$grafico <- renderEcharts4r({
            r <- resumo_tipos()
            
            shiny::validate(need(is.null(r$mensagem), r$mensagem))
            
            r$resumo %>%
                e_charts(Tipo) %>%
                e_bar(Quantidade, name = "Registros") %>%
                e_flip_coords() %>%
                e_labels(position = "right") %>%
                e_color(COR_GRAFICO) %>%
                e_title("Quantidade de Registros por Tipo de Alerta/Conflito") %>%
                e_tooltip(trigger = "item") %>%
                e_legend(show = FALSE) %>%
                e_grid(containLabel = TRUE, left = "2%", right = "8%") %>%
                e_toolbox_feature(feature = "saveAsImage")
        })
        
        output$grafico_cargo <- renderGirafe({
            r <- resumo_cargos()
            
            shiny::validate(need(is.null(r$mensagem), r$mensagem))
            
            resumo <- r$resumo %>%
                mutate(
                    rotulo = str_wrap(Cargo, 45),
                    rotulo = factor(rotulo, levels = unique(rotulo)),
                    dica = sprintf(
                        "<b>%s</b><br/>%s registro(s)",
                        htmltools::htmlEscape(Cargo),
                        format(Quantidade, big.mark = ".", decimal.mark = ",")
                    )
                )
            
            p <- ggplot(resumo, aes(x = rotulo, y = Quantidade)) +
                geom_col_interactive(aes(tooltip = dica, data_id = Cargo), fill = COR_GRAFICO) +
                geom_text(aes(label = Quantidade), hjust = -0.2) +
                coord_flip() +
                scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
                labs(title = "Quantidade de Registros por Cargo", x = "", y = "Quantidade") +
                theme_minimal()
            
            girafe(
                ggobj = p,
                width_svg = 9,
                height_svg = max(3, 0.45 * nrow(resumo) + 1.5),
                options = list(
                    opts_hover(css = CSS_HOVER_GRAFICO),
                    opts_sizing(rescale = TRUE, width = 1)
                )
            )
        })
        
        output$csv_ui <- renderUI({
            df <- tabela_exibicao()
            ui_csv_dados(ns, "download_csv", !is.null(df) && nrow(df) > 0)
        })
        
        output$download_csv <- downloadHandler(
            
            filename = function() {
                paste0("alertas-magistrados-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
            },
            
            content = function(file) {
                inicio <- Sys.time()
                
                df <- tabela_exibicao()
                
                linhas_visiveis <- input$tabela_rows_all
                if (!is.null(linhas_visiveis)) {
                    df <- df[linhas_visiveis, , drop = FALSE]
                }
                
                readr::write_excel_csv2(df, file, na = "")
                
                showNotification(
                    sprintf("Arquivo CSV gerado em %.2fs.", tempo_decorrido(inicio)),
                    type = "message"
                )
            }
        )
        
        # =================================================
        # ABA 4 — INCONSISTÊNCIAS - MAGISTRADOS
        # -------------------------------------------------
        # Calculada sobre dados_base() (códigos originais). A tabela
        # mostra Cargo e Situação por extenso (mesma tradução das outras
        # abas) e o Órgão como "código - nome da serventia".
        # =================================================
        
        inconsistencias <- reactive({
            verificar_inconsistencias_magistrados(dados_base(), nomes_orgaos())
        })
        
        mod_inconsistencias_server(
            "inc",
            dados = reactive(traduzir_codigos_magistrados(inconsistencias())),
            verificacoes = reactive({
                sem_desembargador <- !any(
                    mag_codigo(valores_coluna_regex(dados_base(), REGEX_COL_CARGO)) == CARGO_MAG_DESEMBARGADOR,
                    na.rm = TRUE
                )
                lista <- list(
                    tagList(
                        "Cargo 3 (Juiz(a) Substituto(a) de 2º grau) exercendo Presidência, Vice, ",
                        "Corregedoria, Ouvidoria ou Direção da Escola, ou lotado em gabinete de desembargador.",
                        if (sem_desembargador) tags$b(" Nenhum registro da base usa o cargo 4 (Desembargador(a)).")
                    ),
                    sprintf(
                        "Aposentadoria (situação 12) com data de saída ou seguida de nova situação. Alta: houve retorno ou durou até %d dias.",
                        LIMITE_DIAS_APOSENTADORIA_CURTA
                    )
                )
                setNames(lista, c(VERIFICACAO_MAG_CARGO, VERIFICACAO_MAG_APOSENTADORIA))
            }),
            mensagem_sem_base = reactive({
                if (nrow(dados_base()) == 0) MSG_SEM_BASE
            }),
            avisos = reactive({
                if (length(nomes_orgaos()) == 0) {
                    div(
                        class = "alert alert-warning py-2",
                        style = "font-size: .85rem;",
                        icon("circle-info", class = "me-1"),
                        "A base de Serventias não foi encontrada: a lotação em gabinete de desembargador ",
                        "não pôde ser verificada (só as situações de direção foram consideradas). Envie-a ",
                        "no módulo Serventias e clique em \"Atualizar Dados\"."
                    )
                }
            }),
            col_entidade = "CPF",
            rotulo_entidade = "magistrado(s)",
            filtros_texto = c("CPF" = "CPF", "Nome" = "Nome"),
            prefixo_csv = "inconsistencias-magistrados"
        )
        
    })
}