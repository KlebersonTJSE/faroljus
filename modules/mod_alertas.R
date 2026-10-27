# =====================================================
# modules/mod_alertas.R
# -----------------------------------------------------
# Módulo "Pessoal e Auxiliar" (Quadro de Pessoal e Auxiliar do MPM/CNJ),
# com quatro abas — mesma estrutura do módulo de Magistrados, mais a aba
# de Envio:
#
#   1) "Base de Dados - Pessoal e Auxiliar" — cadastro (histórico de
#      situações de cada CPF), lido de
#      <PASTA_ALERTAS>/<EMPRESA>/pessoal_auxiliar/pessoal_auxiliar.xlsx
#      (ou .csv). Filtros Status (padrão "Ativo"), CPF, Nome, Cargo,
#      Situação, Órgão de lotação e Detalhe; gráfico com agrupamento
#      selecionável; CSV.
#
#   2) "Envio - Pessoal e Auxiliar" — arquivo que o órgão envia ao MPM,
#      lido de <PASTA_ALERTAS>/<EMPRESA>/pessoal_auxiliar/
#      pessoal_auxiliar_envio.xlsx (ou .csv), com os mesmos filtros,
#      gráfico e CSV da base (ui_aba_cadastro_pessoal() /
#      servidor_aba_cadastro_pessoal()).
#
#   3) "Alertas - Pessoal e Auxiliar" — arquivos de alertas do MPM/CNJ
#      (CSV ou XLSX) da pasta <PASTA_ALERTAS>/<EMPRESA>/alertas_csv.
#      Filtros, alertas consolidados na tabela, gráficos e CSV.
#
#   4) "Inconsistências - Pessoal e Auxiliar" — verificações automáticas
#      (ver verificar_inconsistencias_servidores()) sobre a base — ou,
#      sem ela, sobre os arquivos de alertas —, no mesmo formato da aba
#      de inconsistências dos Magistrados.
#
# "Gerenciar Arquivos" identifica se cada arquivo enviado é a base, o
# envio (pelo nome, que contém "envio") ou um arquivo de alertas (pelas
# colunas) — ver identificar_tipo_arquivo_pessoal() — e mostra a data de
# atualização de cada um.
#
# Também reúne as FUNÇÕES COMPARTILHADAS usadas pelos módulos
# mod_alertas_serventias.R e mod_alertas_magistrados.R (leitura de
# .xlsx/.csv, localização de colunas, combos, busca, gráfico, CSV) —
# por isso precisa ser carregado ANTES deles no app.R.
#
# Tudo roda só em R, sobre os arquivos enviados: nenhum banco de dados é
# usado por este módulo. As tabelas de código -> nomenclatura (Situação
# profissional atual e Cargo) ficam em vetores R neste próprio arquivo.
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
library(readxl)   # leitura direta de .xlsx

# =====================================================
# LIMITE DE TAMANHO DE UPLOAD
# -----------------------------------------------------
# Por padrão, o Shiny limita o tamanho TOTAL de um upload (soma de
# todos os arquivos selecionados de uma vez) a 5 MB. Aumentamos aqui
# para caber confortavelmente vários arquivos de uma vez.
#
# Essa opção é GLOBAL do processo R (não é possível limitar por
# fileInput ou por módulo). Como o FarolJus Lite só tem este módulo de
# dados, ela fica definida aqui.
#
# Configurável via MAX_UPLOAD_MB no .Renviron (ou nas Vars do
# shinyapps.io); sem essa variável, usa 50 MB como padrão. Isso não
# contorna um eventual limite de tamanho de requisição imposto pelo
# próprio shinyapps.io por cima do app — se o plano/instância tiver um
# teto menor que isso, ele ainda vale e não há como alterá-lo por
# código; nesse caso, ajuste MAX_UPLOAD_MB para um valor compatível.
# =====================================================

MAX_UPLOAD_MB <- suppressWarnings(
  as.numeric(Sys.getenv("MAX_UPLOAD_MB", unset = "50"))
)

if (is.na(MAX_UPLOAD_MB) || MAX_UPLOAD_MB <= 0) {
  MAX_UPLOAD_MB <- 50
}

options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)

# =====================================================
# CONFIGURAÇÃO (multi-empresa)
# -----------------------------------------------------
# Cada empresa (distro) tem suas próprias subpastas de arquivos:
#   <PASTA_ALERTAS>/<EMPRESA>/alertas_csv   (Quadro de Pessoal — este módulo)
#   <PASTA_ALERTAS>/<EMPRESA>/serventias    (mod_alertas_serventias.R)
#   <PASTA_ALERTAS>/<EMPRESA>/magistrados   (mod_alertas_magistrados.R)
# PASTA_ALERTAS (env var, default "data") define só o diretório raiz;
# a empresa é resolvida em tempo de execução a partir do usuário
# autenticado (ver parâmetro `empresa` de mod_alertas_server(),
# abaixo) — por isso o caminho NÃO é uma constante fixa calculada uma
# única vez ao carregar o módulo (isso seria compartilhado entre TODAS
# as sessões do app, já que rodam no mesmo processo R).
# =====================================================

PASTA_ALERTAS_BASE <- Sys.getenv("PASTA_ALERTAS", unset = "data")

SUBPASTA_ALERTAS <- "alertas_csv"
SUBPASTA_SERVENTIAS <- "serventias"
SUBPASTA_MAGISTRADOS <- "magistrados"

# Monta (e garante que existe) a subpasta da empresa informada.
# Retorna NULL se `empresa` não estiver definida (ex.: sessão ainda não
# autenticada).
caminho_alertas <- function(empresa, subpasta = SUBPASTA_ALERTAS) {

  if (is.null(empresa) || is.na(empresa) || trimws(empresa) == "") {
    return(NULL)
  }

  caminho <- file.path(PASTA_ALERTAS_BASE, trimws(empresa), subpasta)

  if (!dir.exists(caminho)) {
    dir.create(caminho, recursive = TRUE, showWarnings = FALSE)
  }

  caminho

}

# =====================================================
# TEMPO DE PROCESSAMENTO
# =====================================================

tempo_decorrido <- function(inicio) {
  round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 2)
}

# =====================================================
# NOTIFICAÇÃO SEGURA
# -----------------------------------------------------
# showNotification() só funciona dentro de uma sessão Shiny; fora dela
# (ex.: testes, carga no console) a mensagem vira só um warning.
# =====================================================

notificar <- function(msg, type = "warning", duration = 15) {

  if (!is.null(shiny::getDefaultReactiveDomain())) {
    showNotification(msg, type = type, duration = duration)
  }

  invisible(NULL)

}

# =====================================================
# ARQUIVOS DE PLANILHA (.xlsx / .csv)
# -----------------------------------------------------
# EXTENSOES_PLANILHA também define a PRIORIDADE de leitura nos módulos
# de Serventias e Magistrados: se existirem "x.xlsx" e "x.csv" do mesmo
# tipo, vale o .xlsx.
# =====================================================

EXTENSOES_PLANILHA <- c("xlsx", "csv")

# Rótulo usado no lugar de um valor vazio em combos e gráficos.
ROTULO_VALOR_VAZIO <- "(Não informado)"

extensao_arquivo <- function(caminho) {
  str_to_lower(tools::file_ext(as.character(caminho)))
}

# Nome do arquivo sem extensão, normalizado para comparação tolerante:
# "Alertas Serventias.CSV" / "alertas-serventias.csv" -> "alertas_serventias".
normalizar_nome_arquivo <- function(caminho) {

  nome <- tools::file_path_sans_ext(basename(as.character(caminho)))
  nome <- str_to_lower(stringi::stri_trans_general(nome, "Latin-ASCII"))
  nome <- str_replace_all(nome, "[^a-z0-9]+", "_")

  str_replace_all(nome, "^_+|_+$", "")

}

# =====================================================
# LEITURA DE UM ARQUIVO (.xlsx ou .csv)
# -----------------------------------------------------
# Todas as colunas são lidas como TEXTO (códigos, CPFs e CEPs mantêm os
# zeros à esquerda).
#
#   - .xlsx: lê a aba `aba_preferida` (comparação sem acento/maiúsculas),
#     se existir; senão, a primeira aba.
#   - .csv: tenta primeiro separado por ";" (padrão de exportação
#     brasileiro/Excel); se resultar em uma única coluna (delimitador
#     errado), tenta de novo com detecção automática. Se o texto não for
#     UTF-8 válido (CSV salvo pelo Excel em Windows-1252), relê como
#     Latin-1.
#
# Fica dentro de um tryCatch: em vez de travar o carregamento inteiro
# por causa de um arquivo ilegível (corrompido, formato inesperado),
# avisa qual arquivo deu problema e devolve NULL — os demais arquivos
# continuam sendo processados normalmente.
# =====================================================

ler_csv_texto <- function(caminho, encoding = "UTF-8", n_max = Inf) {

  df <- read_csv2(
    caminho,
    show_col_types = FALSE,
    locale = locale(encoding = encoding, decimal_mark = ",", grouping_mark = "."),
    col_types = cols(.default = col_character()),
    n_max = n_max,
    name_repair = "minimal",
    progress = FALSE
  )

  if (ncol(df) <= 1) {
    df <- read_delim(
      caminho,
      delim = NULL,
      show_col_types = FALSE,
      locale = locale(encoding = encoding),
      col_types = cols(.default = col_character()),
      n_max = n_max,
      name_repair = "minimal",
      progress = FALSE
    )
  }

  df

}

ler_arquivo_dados <- function(caminho, extensao = extensao_arquivo(caminho),
                              nome_exibicao = basename(caminho),
                              aba_preferida = NULL, n_max = Inf) {

  tryCatch({

    if (identical(extensao, "xlsx")) {

      abas <- readxl::excel_sheets(caminho)
      aba <- abas[1]

      if (!is.null(aba_preferida)) {
        achada <- abas[normalizar_nome_coluna(abas) == normalizar_nome_coluna(aba_preferida)]
        if (length(achada) > 0) aba <- achada[1]
      }

      df <- as.data.frame(
        readxl::read_excel(
          caminho,
          sheet = aba,
          col_types = "text",
          n_max = n_max,
          .name_repair = "minimal",
          progress = FALSE
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # Lidas como texto, as datas do Excel chegam como número serial
      # ("45658"): nas colunas "Data ...", vira dd/mm/aaaa.
      for (i in which(str_detect(normalizar_nome_coluna(names(df)), "^data"))) {
        v <- str_trim(df[[i]])
        serial <- !is.na(v) & str_detect(v, "^\\d{5}(\\.\\d+)?$")
        if (any(serial)) {
          v[serial] <- format(as.Date(floor(as.numeric(v[serial])), origin = "1899-12-30"), "%d/%m/%Y")
          df[[i]] <- v
        }
      }

      df

    } else {

      df <- ler_csv_texto(caminho, "UTF-8", n_max)

      texto <- c(names(df), unlist(lapply(df, function(x) x[!is.na(x)]), use.names = FALSE))

      if (!all(validUTF8(texto))) {
        df <- ler_csv_texto(caminho, "latin1", n_max)
      }

      as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

    }

  }, error = function(e) {

    msg <- paste0(
      "Não foi possível ler o arquivo ", nome_exibicao, ": ",
      conditionMessage(e), ". Esse arquivo foi ignorado."
    )

    warning(msg)
    notificar(msg)

    NULL

  })

}

# Mantida com o nome antigo (arquivos de alertas do Quadro de Pessoal).
ler_arquivo_alertas <- function(arquivo) {
  ler_arquivo_dados(arquivo)
}

# =====================================================
# LIMPEZA DOS DADOS LIDOS
# -----------------------------------------------------
#   - cabeçalho: remove BOM, espaços sobrando e os nomes automáticos
#     ("...24") que o readr cria para colunas sem nome (ex.: o ";" no
#     fim de cada linha do CSV do MPM);
#   - colunas sem nome: descartadas se vazias, ou nomeadas "Coluna N";
#   - nomes duplicados viram "Nome", "Nome 2"...;
#   - valores: sem espaços nas pontas; texto vazio vira NA;
#   - linhas totalmente vazias são descartadas.
# =====================================================

limpar_cabecalho <- function(nomes) {

  nomes <- as.character(nomes)
  nomes[is.na(nomes)] <- ""
  nomes <- str_remove_all(nomes, "﻿")
  nomes <- str_squish(nomes)

  # Nomes automáticos do readr/readxl para colunas sem cabeçalho.
  nomes[str_detect(nomes, "^\\.\\.\\.\\d+$")] <- ""

  nomes

}

limpar_dados_lidos <- function(df) {

  if (is.null(df) || ncol(df) == 0) {
    return(data.frame())
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  for (i in seq_along(df)) {
    v <- str_trim(as.character(df[[i]]))
    v[!is.na(v) & v == ""] <- NA_character_
    df[[i]] <- v
  }

  nomes <- limpar_cabecalho(names(df))

  sem_nome <- nomes == ""
  vazias <- vapply(df, function(v) all(is.na(v)), logical(1))

  manter <- !(sem_nome & vazias)
  df <- df[manter]
  nomes <- nomes[manter]

  nomes[nomes == ""] <- paste("Coluna", which(nomes == ""))
  names(df) <- make.unique(nomes, sep = " ")

  if (ncol(df) == 0) {
    return(data.frame())
  }

  linhas_vazias <- rowSums(!is.na(df)) == 0
  df <- df[!linhas_vazias, , drop = FALSE]
  rownames(df) <- NULL

  df

}

# =====================================================
# CARREGAMENTO
# -----------------------------------------------------
# Consolida todos os arquivos (.csv e .xlsx) da pasta de alertas da
# empresa. Arquivos com colunas diferentes entre si (ex.: um trouxe uma
# coluna "Alerta X" que o outro não tem) são combinados sem problema —
# bind_rows() preenche com NA o que faltar em cada um.
# =====================================================

carregar_alertas <- function(caminho) {

  if (is.null(caminho) || !dir.exists(caminho)) {
    return(data.frame())
  }

  arquivos <- list.files(caminho, full.names = TRUE)
  arquivos <- arquivos[extensao_arquivo(arquivos) %in% EXTENSOES_PLANILHA]

  # Arquivos temporários de trava do Excel/LibreOffice ("~$x.xlsx",
  # ".~lock.x.csv#").
  arquivos <- arquivos[!str_detect(basename(arquivos), "^(~\\$|\\.~lock)")]

  if (length(arquivos) == 0) {
    return(data.frame())
  }

  # Cada arquivo é lido e limpo uma vez por processo e reaproveitado
  # enquanto não mudar (com_cache_arquivo(), em R/utils.R).
  resultados <- map(arquivos, function(a) {
    com_cache_arquivo(a, "alertas_pessoal", function() limpar_dados_lidos(ler_arquivo_dados(a)))
  })

  # Remove arquivos ignorados (erro de leitura ou sem dados).
  resultados <- Filter(function(df) nrow(df) > 0, resultados)

  if (length(resultados) == 0) {
    return(data.frame())
  }

  bind_rows(resultados) %>%
    distinct() %>%
    as.data.frame(stringsAsFactors = FALSE, check.names = FALSE)

}

# =====================================================
# COLUNAS POR PREFIXO
# =====================================================

colunas_por_prefixo <- function(df, prefixo) {
  
  if (is.null(df) || nrow(df) == 0) {
    return(character(0))
  }
  
  names(df)[str_starts(names(df), prefixo)]
  
}

# =====================================================
# COLUNAS COM DADOS
# =====================================================

colunas_com_dados <- function(df, colunas) {
  
  if (length(colunas) == 0) {
    return(character(0))
  }
  
  colunas[
    vapply(
      colunas,
      function(col) {
        valores <- df[[col]]
        any(!is.na(valores) & str_trim(valores) != "")
      },
      logical(1)
    )
  ]
  
}

# =====================================================
# COLUNAS (localização tolerante a variações de cabeçalho)
# -----------------------------------------------------
# Compara pelo nome normalizado (sem acento, minúsculo, espaços
# simples) contra uma expressão regular. As regex são ancoradas no
# início do nome, então "Alerta: Cargo" nunca é confundida com "Cargo".
# =====================================================

# Normaliza o nome de uma coluna para comparação (sem acento, minúsculo,
# espaços simples, sem espaços nas pontas).
normalizar_nome_coluna <- function(x) {
  str_to_lower(str_squish(stringi::stri_trans_general(as.character(x), "Latin-ASCII")))
}

REGEX_COL_CPF <- "^cpf\\b"
REGEX_COL_NOME <- "^nome$"
REGEX_COL_SITUACAO <- "^situacao profissional"
REGEX_COL_CARGO <- "^cargo$"
REGEX_COL_SEXO <- "^sexo"
REGEX_COL_NATURALIDADE <- "^naturalidade"
REGEX_COL_INICIO_SITUACAO <- "^data de inicio da situacao"
REGEX_COL_SAIDA_SITUACAO <- "^data de saida da situacao"
REGEX_COL_ORGAO_SERVIDOR <- "^orgao de lotacao"

# Nome da primeira coluna cujo nome normalizado casa com `regex`, ou NULL.
coluna_por_regex <- function(df, regex) {

  if (is.null(df) || ncol(df) == 0) {
    return(NULL)
  }

  achadas <- names(df)[str_detect(normalizar_nome_coluna(names(df)), regex)]

  if (length(achadas) == 0) NULL else achadas[1]

}

# Valores da coluna para exibição (combos, filtros, gráficos): sem
# espaços nas pontas e com vazio trocado por ROTULO_VALOR_VAZIO. NULL se
# a coluna não existir.
valores_coluna_regex <- function(df, regex) {

  col <- coluna_por_regex(df, regex)

  if (is.null(col)) {
    return(NULL)
  }

  v <- str_trim(as.character(df[[col]]))
  ifelse(is.na(v) | v == "", ROTULO_VALOR_VAZIO, v)

}

# =====================================================
# COMBOS
# =====================================================

# Opções de um selectInput a partir dos valores de uma coluna: valores
# distintos em ordem (numérica, se todos forem números), com
# ROTULO_VALOR_VAZIO por último.
opcoes_combo <- function(valores) {

  if (is.null(valores) || length(valores) == 0) {
    return(character(0))
  }

  valores <- unique(as.character(valores[!is.na(valores)]))
  vazio <- valores[valores == ROTULO_VALOR_VAZIO]
  valores <- setdiff(valores, ROTULO_VALOR_VAZIO)

  numeros <- suppressWarnings(as.numeric(valores))

  valores <- if (length(valores) > 0 && !anyNA(numeros)) {
    valores[order(numeros)]
  } else {
    valores[order(normalizar_texto_busca(valores))]
  }

  c(valores, vazio)

}

# Seleção a manter num combo depois de as opções mudarem: a atual, se
# ainda existir (ou se for "Todos"); senão `padrao`, se existir; senão
# "Todos".
selecao_valida <- function(atual, opcoes, padrao = "Todos") {

  if (!is.null(atual) && length(atual) == 1 && (atual == "Todos" || atual %in% opcoes)) {
    return(atual)
  }

  if (padrao %in% opcoes) padrao else "Todos"

}

# =====================================================
# BUSCA DE TEXTO (filtros CPF, Nome, Detalhe...)
# -----------------------------------------------------
# Sem diferenciar maiúsculas/minúsculas nem acentos.
# =====================================================

normalizar_texto_busca <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  str_to_lower(stringi::stri_trans_general(x, "Latin-ASCII"))
}

# Texto de cada linha do data.frame (todas as colunas juntas), já
# normalizado — usado pelo filtro "Detalhe".
texto_busca_linhas <- function(df) {

  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(character(0))
  }

  colunas <- lapply(df, function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    v
  })

  normalizar_texto_busca(do.call(paste, c(unname(colunas), sep = " | ")))

}

# Filtro de texto "contém" (sem acento/maiúsculas) sobre uma coluna.
contem_texto <- function(valores, texto) {
  str_detect(normalizar_texto_busca(valores), fixed(normalizar_texto_busca(str_trim(texto))))
}

# Filtro de CPF: ignora pontos, traços, barras e espaços dos dois lados,
# então "003.894.515-09" e "00389451509" encontram o mesmo registro.
contem_cpf <- function(valores, texto) {
  sem_pontuacao <- function(x) gsub("[-./ ]", "", x)
  contem_texto(sem_pontuacao(valores), sem_pontuacao(texto))
}

# =====================================================
# UI COMPARTILHADA — "Gerar arquivo CSV"
# -----------------------------------------------------
# O botão só fica disponível quando a tabela correspondente tem dados.
# =====================================================

ui_csv_dados <- function(ns, id_download, tem_dados) {

  div(
    class = "mt-4",
    style = "max-width: 420px;",

    p(
      class = "text-muted",
      "Gera um arquivo .csv com os dados exibidos na aba \"Tabela\" (respeitando os filtros aplicados e a busca da tabela)."
    ),

    if (isTRUE(tem_dados)) {

      downloadButton(
        ns(id_download),
        "Gerar arquivo CSV",
        icon = icon("download"),
        class = "btn btn-primary btn-acao"
      )

    } else {

      tagList(
        tags$button(
          type = "button",
          class = "btn btn-primary btn-acao",
          disabled = "disabled",
          icon("download", class = "me-2"),
          "Gerar arquivo CSV"
        ),
        div(
          class = "text-muted mt-2",
          style = "font-size: .82rem;",
          "Não há dados na tabela para exportar."
        )
      )

    }
  )

}

# Grava o CSV (padrão Excel BR: ";" e UTF-8 com BOM) respeitando a busca
# global e os filtros de coluna do DT (input$<tabela>_rows_all).
gravar_csv_tabela <- function(df, file, linhas_visiveis = NULL) {

  if (!is.null(linhas_visiveis)) {
    df <- df[linhas_visiveis, , drop = FALSE]
  }

  readr::write_excel_csv2(df, file, na = "")

}

# Tabela padrão (DT) dos módulos.
tabela_dt <- function(df, ...) {

  datatable(
    df,
    filter = "top",
    rownames = FALSE,
    width = "100%",
    options = c(
      list(
        pageLength = 20,
        scrollX = TRUE,
        autoWidth = TRUE,
        width = "100%"
      ),
      list(...)
    )
  )

}

# =====================================================
# GRÁFICO DE BARRAS HORIZONTAIS (ggiraph)
# -----------------------------------------------------
# `resumo` precisa das colunas Grupo, Quantidade e dica (tooltip HTML),
# já na ordem desejada (maior primeiro) — a maior barra fica no topo.
# =====================================================

grafico_barras_girafe <- function(resumo, titulo, subtitulo = NULL) {

  resumo <- resumo %>%
    mutate(
      Grupo = as.character(Grupo),
      rotulo = str_wrap(Grupo, 45)
    )

  # Com coord_flip(), o primeiro nível fica embaixo: inverte a ordem.
  resumo$rotulo <- factor(resumo$rotulo, levels = rev(unique(resumo$rotulo)))

  p <- ggplot(resumo, aes(x = rotulo, y = Quantidade)) +
    geom_col_interactive(
      aes(tooltip = dica, data_id = Grupo),
      fill = "#2C7FB8"
    ) +
    geom_text(aes(label = Quantidade), hjust = -0.2, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = titulo, subtitle = subtitulo, x = "", y = "Quantidade") +
    theme_minimal()

  girafe(
    ggobj = p,
    width_svg = 9,
    height_svg = max(3, 0.32 * nrow(resumo) + 1.6),
    options = list(
      opts_hover(css = "fill:#0d6efd;cursor:pointer;"),
      opts_tooltip(css = "background:#fff;color:#212529;padding:8px 10px;border:1px solid #dee2e6;border-radius:6px;font-size:12px;max-width:420px;"),
      opts_sizing(rescale = TRUE, width = 1)
    )
  )

}

# =====================================================
# CÓDIGO -> NOMENCLATURA (Situação Profissional Atual e Cargo)
# -----------------------------------------------------
# No arquivo do Quadro de Pessoal e Auxiliar (servidores) do CNJ/MPM,
# as colunas "Situação Profissional Atual" e "Cargo" vêm como código
# numérico. A correspondência código -> nomenclatura fica nos vetores
# abaixo (Manual de Preenchimento do MPM — Quadro de Pessoal e
# Auxiliar), sem banco de dados: para ajustar uma nomenclatura, edite o
# vetor aqui mesmo.
#
# É só para exibição: a tabela e o CSV gerado a partir dela mostram a
# nomenclatura, enquanto dados(), dados_filtrados() e as verificações
# de inconsistência continuam com os códigos originais.
#
# Obs.: na Situação Profissional Atual não existem os códigos 4 e 5
# (a numeração pula de 3 para 6) — é assim no manual.
# =====================================================

SITUACAO_PROFISSIONAL_SERVIDOR <- c(
  "1"  = "Cargo de chefia",
  "2"  = "Outros cargos em comissão ou funções comissionadas",
  "3"  = "Não exerce cargo em comissão ou função comissionada",
  "6"  = "Afastado(a)",
  "7"  = "Aposentado(a)",
  "8"  = "Falecido(a)",
  "9"  = "Exoneração/Vacância",
  "10" = "Demitido(a)",
  "11" = "Saída por Remoção",
  "12" = "Saída por cessão/requisição",
  "13" = "Vigência de Contrato/Vínculo"
)

CARGO_SERVIDOR <- c(
  "1"  = "Servidor(a) efetivo(a) ou removido(a) para o Tribunal",
  "2"  = "Servidor(a) cedido(a) ou requisitado(a) de outro tribunal",
  "3"  = "Servidor(a) cedido(a) ou requisitado(a) de órgãos de fora do judiciário",
  "4"  = "Servidor(a) Comissionado(a) Sem vínculo",
  "5"  = "Estagiário(a)",
  "6"  = "Terceirizado(a)",
  "7"  = "Servidor(a) de serventia privatizada",
  "8"  = "Juiz(a) leigo(a)",
  "9"  = "Conciliador(a)",
  "10" = "Aprendiz",
  "11" = "Voluntário(a)",
  "12" = "Residência Jurídica",
  "13" = "Outros"
)

# Troca, em um vetor de valores, o código pela nomenclatura da tabela
# informada (vetor nomeado: nome = código, valor = nomenclatura). Aceita
# o código "puro" com ou sem zero à esquerda e com espaços ("6", "06",
# " 6 ", "6.0"). O que não for um código conhecido (valor vazio, já por
# extenso ou código fora da tabela) é mantido como veio, para não
# esconder dado inesperado.
codigo_para_nomenclatura <- function(valores, tabela) {

  valores_chr <- as.character(valores)

  codigos <- str_match(
    valores_chr,
    "^\\s*0*(\\d+)(?:[.,]0+)?\\s*$"
  )[, 2]

  nomes <- unname(tabela[codigos])

  ifelse(is.na(nomes), valores_chr, nomes)

}

# Aplica a conversão código -> nomenclatura nas colunas "Situação
# Profissional Atual" e "Cargo" do data.frame (se existirem). Só para
# exibição na tabela/CSV/gráfico.
traduzir_codigos_tabela <- function(df) {

  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  nomes_norm <- normalizar_nome_coluna(names(df))

  for (i in which(str_detect(nomes_norm, REGEX_COL_SITUACAO))) {
    df[[i]] <- codigo_para_nomenclatura(df[[i]], SITUACAO_PROFISSIONAL_SERVIDOR)
  }

  for (i in which(str_detect(nomes_norm, REGEX_COL_CARGO))) {
    df[[i]] <- codigo_para_nomenclatura(df[[i]], CARGO_SERVIDOR)
  }

  df

}

# =====================================================
# APOIO ÀS VERIFICAÇÕES DE INCONSISTÊNCIA (comum aos três módulos)
# =====================================================

# Códigos vindos do Excel podem chegar como "4.0" ou "04".
codigo_limpo <- function(x) {
  x <- str_trim(as.character(x))
  x <- sub("[.,]0+$", "", x)
  ifelse(!is.na(x) & str_detect(x, "^\\d+$"), sub("^0+(?=\\d)", "", x, perl = TRUE), x)
}

# Datas em dd/mm/aaaa, aaaa-mm-dd ou número serial do Excel.
converter_data <- function(x) {

  x <- str_trim(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))

  br <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}")
  out[br] <- as.Date(substr(x[br], 1, 10), format = "%d/%m/%Y")

  iso <- !is.na(x) & str_detect(x, "^\\d{4}-\\d{2}-\\d{2}")
  out[iso] <- as.Date(substr(x[iso], 1, 10), optional = TRUE)

  serial <- !is.na(x) & str_detect(x, "^\\d{5}(\\.\\d+)?$")
  out[serial] <- as.Date(floor(as.numeric(x[serial])), origin = "1899-12-30")

  out

}

formatar_data <- function(d) {
  ifelse(is.na(d), "", format(d, "%d/%m/%Y"))
}

# TRUE para "Ativo"/"Ativa" (sem acento/maiúsculas).
status_ativo <- function(x) {
  normalizar_nome_coluna(x) %in% c("ativo", "ativa")
}

# Chave para casar a lotação de uma pessoa com a base de Serventias:
# o código (se o valor for numérico) ou o nome normalizado (sem acento,
# minúsculo, só letras e números).
chave_nome_orgao <- function(x) {
  x <- normalizar_texto_busca(x)
  str_squish(str_replace_all(x, "[^a-z0-9]+", " "))
}

chave_lotacao <- function(x) {
  cod <- codigo_limpo(x)
  ifelse(
    !is.na(cod) & str_detect(cod, "^\\d+$"),
    paste0("cod:", cod),
    paste0("nome:", chave_nome_orgao(x))
  )
}

# Último registro (maior data de início) de cada CPF — a lotação e a
# situação "atuais" de cada pessoa. Empate de data: o último no arquivo.
#
# Registros SEM data de início só são usados quando o CPF não tem nenhum
# registro datado (aí vale o último no arquivo). Antes, o arrange()
# colocava os sem data no fim da ordenação, e um registro sem data
# "vencia" o mais recente datado — a lotação atual saía errada.
ultimo_registro_por_cpf <- function(d) {

  if (nrow(d) == 0) {
    return(d)
  }

  d %>%
    mutate(.ordem = row_number(), .tem_data = !is.na(inicio)) %>%
    filter(!is.na(CPF) & CPF != "") %>%
    group_by(CPF) %>%
    arrange(.tem_data, inicio, .ordem, .by_group = TRUE) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(-.ordem, -.tem_data)

}

# Índice da base de Serventias para consultar lotações: uma linha por
# chave (código e nome), dizendo se existe alguma serventia ATIVA com
# aquela chave e quais são os nomes/status encontrados.
indice_serventias <- function(serventias) {

  vazio <- tibble(
    chave = character(0), codigo = character(0), nome = character(0),
    status = character(0), alguma_ativa = logical(0)
  )

  if (is.null(serventias) || nrow(serventias) == 0 ||
      !exists("REGEX_COL_CODIGO_SERVENTIA") || !exists("REGEX_COL_NOME_SERVENTIA")) {
    return(vazio)
  }

  col_cod <- coluna_por_regex(serventias, REGEX_COL_CODIGO_SERVENTIA)
  col_nome <- coluna_por_regex(serventias, REGEX_COL_NOME_SERVENTIA)
  col_status <- coluna_por_regex(serventias, "^status$")

  if (is.null(col_cod) && is.null(col_nome)) {
    return(vazio)
  }

  s <- tibble(
    codigo = if (is.null(col_cod)) NA_character_ else codigo_limpo(serventias[[col_cod]]),
    nome = if (is.null(col_nome)) NA_character_ else serventias[[col_nome]],
    status = if (is.null(col_status)) NA_character_ else serventias[[col_status]]
  ) %>%
    # Status vazio/ausente conta como ativa (só um status explícito
    # diferente de "Ativo" torna a serventia inativa).
    mutate(ativa = is.na(status) | str_trim(status) == "" | status_ativo(status))

  bind_rows(
    s %>% filter(!is.na(codigo) & codigo != "") %>% mutate(chave = paste0("cod:", codigo)),
    s %>% filter(!is.na(nome) & nome != "") %>% mutate(chave = paste0("nome:", chave_nome_orgao(nome)))
  ) %>%
    group_by(chave) %>%
    summarise(
      codigo = paste(unique(na.omit(codigo)), collapse = ", "),
      nome = paste(unique(na.omit(nome)), collapse = "; "),
      status = paste(unique(na.omit(status)), collapse = ", "),
      alguma_ativa = any(ativa),
      .groups = "drop"
    )

}

# Base de Serventias da empresa (módulo mod_alertas_serventias.R), ou
# data.frame vazio se o módulo não estiver carregado ou a base não tiver
# sido enviada.
carregar_base_serventias_empresa <- function(empresa) {

  necessarias <- c("pasta_serventias", "arquivo_do_tipo", "carregar_arquivo_serventias")

  if (!all(vapply(necessarias, exists, logical(1)))) {
    return(data.frame())
  }

  tryCatch(
    carregar_arquivo_serventias(arquivo_do_tipo(pasta_serventias(empresa), "base")),
    error = function(e) data.frame()
  )

}

# Estrutura (vazia) do resultado das verificações por pessoa (Quadro de
# Pessoal e Magistrados).
inconsistencias_pessoas_vazio <- function() {
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
    Motivo = character(0)
  )
}

# =====================================================
# INCONSISTÊNCIAS — QUADRO DE PESSOAL E AUXILIAR
# -----------------------------------------------------
# Mesma "inteligência" da aba de inconsistências dos Magistrados,
# adaptada à codificação do Quadro de Pessoal. Rodam sobre os registros
# carregados com os CÓDIGOS originais (antes da tradução) e apontam
# padrões que contrariam o Manual do MPM. São indícios para conferência,
# não correções automáticas.
#
#   1) SITUAÇÃO DEFINITIVA SEGUIDA DE RETORNO
#      7 Aposentado(a), 8 Falecido(a), 9 Exoneração/Vacância e
#      10 Demitido(a) encerram o vínculo: não deveriam ter data de saída
#      nem ser seguidas de nova situação do mesmo CPF. Gravidade:
#        - Alta: falecimento seguido de qualquer registro; aposentadoria
#          seguida de retorno ou que durou até LIMITE_DIAS_SITUACAO_CURTA
#          dias (perfil de afastamento — provável 6 Afastado(a));
#        - Média: demais casos (exoneração/demissão seguidas de novo
#          registro podem ser reingresso legítimo — conferir).
#
#   2) CARGO INCOMPATÍVEL COM A SITUAÇÃO
#        - Alta: estagiário(a), terceirizado(a), juiz(a) leigo(a),
#          conciliador(a), aprendiz, voluntário(a) ou residente (cargos 5,
#          6, 8-12) em cargo de chefia/comissão (situação 1 ou 2) ou
#          aposentado(a) (7); comissionado(a) sem vínculo (cargo 4) que
#          "não exerce cargo em comissão" (situação 3);
#        - Média: cedido(a)/requisitado(a) (2, 3) ou comissionado(a) sem
#          vínculo (4) como aposentado(a) (7) — a aposentadoria é no órgão
#          de origem / no RGPS, o esperado seria saída (9, 12).
#
#   3) LOTAÇÃO EM SERVENTIA INATIVA
#      Último registro do CPF em situação não encerrada, lotado em
#      serventia que só aparece como NÃO ativa na base de Serventias da
#      empresa (casada pelo código ou pelo nome). Gravidade Alta. Só roda
#      se a base de Serventias tiver sido enviada.
#
#   4) DATAS DA SITUAÇÃO INCONSISTENTES
#        - Alta: data de início no futuro ou data de saída anterior à de
#          início;
#        - Média: data preenchida em formato não reconhecido.
# =====================================================

SITUACOES_SERV_DEFINITIVAS <- c("7", "8", "9", "10")
SITUACOES_SERV_ENCERRADAS <- c("7", "8", "9", "10", "11", "12")
SITUACOES_SERV_COMISSAO <- c("1", "2")
SITUACAO_SERV_SEM_COMISSAO <- "3"
SITUACAO_SERV_APOSENTADO <- "7"
SITUACAO_SERV_FALECIDO <- "8"
CARGOS_SERV_SEM_VINCULO_EFETIVO <- c("5", "6", "8", "9", "10", "11", "12")
CARGOS_SERV_CEDIDOS <- c("2", "3")
CARGO_SERV_COMISSIONADO <- "4"
LIMITE_DIAS_SITUACAO_CURTA <- 60

VERIFICACAO_SERV_DEFINITIVA <- "Situação definitiva seguida de retorno"
VERIFICACAO_SERV_CARGO <- "Cargo incompatível com a situação"
VERIFICACAO_SERV_LOTACAO <- "Lotação em serventia inativa"
VERIFICACAO_SERV_DATAS <- "Datas da situação inconsistentes"

VERIFICACOES_SERVIDORES <- c(
  VERIFICACAO_SERV_DEFINITIVA,
  VERIFICACAO_SERV_CARGO,
  VERIFICACAO_SERV_LOTACAO,
  VERIFICACAO_SERV_DATAS
)

# Monta, a partir de um data.frame de pessoas (servidores ou
# magistrados), a tabela de trabalho das verificações. NULL se faltar
# CPF, Cargo ou Situação.
preparar_registros_pessoas <- function(df, regex_orgao) {

  # Registros marcados como excluídos por erro não entram nas verificações.
  df <- remover_registros_excluidos(df)

  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  valor <- function(regex) {
    c <- coluna_por_regex(df, regex)
    if (is.null(c)) rep(NA_character_, nrow(df)) else as.character(df[[c]])
  }

  if (is.null(coluna_por_regex(df, REGEX_COL_CPF)) ||
      is.null(coluna_por_regex(df, REGEX_COL_CARGO)) ||
      is.null(coluna_por_regex(df, REGEX_COL_SITUACAO))) {
    return(NULL)
  }

  tibble(
    CPF = str_trim(valor(REGEX_COL_CPF)),
    Nome = valor(REGEX_COL_NOME),
    orgao = valor(regex_orgao),
    cargo = codigo_limpo(valor(REGEX_COL_CARGO)),
    situacao = codigo_limpo(valor(REGEX_COL_SITUACAO)),
    inicio_txt = valor(REGEX_COL_INICIO_SITUACAO),
    saida_txt = valor(REGEX_COL_SAIDA_SITUACAO),
    tem_coluna_saida = !is.null(coluna_por_regex(df, REGEX_COL_SAIDA_SITUACAO))
  ) %>%
    mutate(
      inicio = converter_data(inicio_txt),
      saida = converter_data(saida_txt),
      orgao_exibicao = orgao
    )

}

# Linhas no formato de inconsistencias_pessoas_vazio().
montar_inconsistencias_pessoas <- function(x, verificacao) {

  if (is.null(x) || nrow(x) == 0) {
    return(inconsistencias_pessoas_vazio())
  }

  tibble(
    `Verificação` = verificacao,
    Gravidade = x$Gravidade,
    CPF = x$CPF,
    Nome = x$Nome,
    `Órgão de lotação` = x$orgao_exibicao,
    Cargo = x$cargo,
    `Situação profissional atual` = x$situacao,
    `Data de início da situação` = formatar_data(x$inicio),
    `Data de saída da situação` = formatar_data(x$saida),
    `Duração (dias)` = as.integer(x$saida - x$inicio),
    Motivo = x$Motivo
  )

}

# Situação "definitiva" seguida de nova situação do mesmo CPF e/ou com
# data de saída. Usada pelos servidores e pelos magistrados (cada um com
# seus códigos e regras de gravidade, via `classificar`).
#   classificar(x) recebe as linhas apontadas (colunas situacao, dias,
#   tem_saida, voltou, ret_sit, curta) e devolve a Gravidade.
#
# RETORNO = registro posterior do mesmo CPF em situação ATIVA. Registros
# posteriores em outra situação encerrada (`encerradas`) não contam: um
# aposentado que depois falece não "voltou". A exceção são as situações
# em `qualquer_registro_posterior` (ex.: falecimento), para as quais
# qualquer registro depois delas é apontado.
situacoes_definitivas_com_retorno <- function(d, definitivas, rotulos, classificar,
                                              sufixo_motivo, limite_dias = LIMITE_DIAS_SITUACAO_CURTA,
                                              encerradas = definitivas,
                                              qualquer_registro_posterior = character(0)) {

  def <- d %>%
    mutate(.linha = row_number()) %>%
    filter(situacao %in% definitivas)

  if (nrow(def) == 0) {
    return(def[0, ])
  }

  outras <- d %>% filter(!is.na(inicio))

  retorno <- def %>%
    select(.linha, CPF, sit_def = situacao, ref = inicio, saida) %>%
    mutate(ref = coalesce(saida, ref)) %>%
    filter(!is.na(ref)) %>%
    inner_join(
      outras %>% select(CPF, ret_inicio = inicio, ret_sit = situacao),
      by = "CPF",
      relationship = "many-to-many"
    ) %>%
    filter(
      ret_inicio > ref,
      ret_sit != sit_def,
      !(ret_sit %in% encerradas) | sit_def %in% qualquer_registro_posterior
    ) %>%
    group_by(.linha) %>%
    slice_min(ret_inicio, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(.linha, ret_inicio, ret_sit)

  x <- def %>%
    left_join(retorno, by = ".linha") %>%
    mutate(
      dias = as.integer(saida - inicio),
      tem_saida = !is.na(saida),
      voltou = !is.na(ret_inicio),
      curta = tem_saida & !is.na(dias) & dias <= limite_dias
    ) %>%
    filter(tem_saida | voltou)

  if (nrow(x) == 0) {
    return(x)
  }

  x$Gravidade <- classificar(x)

  x %>%
    mutate(
      rotulo = coalesce(unname(rotulos[situacao]), paste("situação", situacao)),
      Motivo = paste0(
        rotulo, " (situação ", situacao, ") é definitiva, mas ",
        ifelse(
          tem_saida,
          paste0("tem data de saída", ifelse(is.na(dias), "", paste0(" (duração de ", dias, " dia(s))"))),
          ""
        ),
        ifelse(tem_saida & voltou, " e ", ""),
        ifelse(
          voltou,
          paste0(
            "volta em ", formatar_data(ret_inicio), " como ",
            coalesce(unname(rotulos[ret_sit]), paste("situação", ret_sit))
          ),
          ""
        ),
        sufixo_motivo(x),
        "."
      )
    ) %>%
    arrange(Nome, inicio)

}

# Datas da situação: início no futuro, saída antes do início ou data em
# formato não reconhecido.
datas_situacao_inconsistentes <- function(d, hoje = Sys.Date()) {

  x <- d %>%
    mutate(
      inicio_invalido = !is.na(inicio_txt) & inicio_txt != "" & is.na(inicio),
      saida_invalida = !is.na(saida_txt) & saida_txt != "" & is.na(saida),
      inicio_futuro = !is.na(inicio) & inicio > hoje,
      saida_antes = !is.na(inicio) & !is.na(saida) & saida < inicio
    ) %>%
    filter(inicio_invalido | saida_invalida | inicio_futuro | saida_antes)

  if (nrow(x) == 0) {
    return(x)
  }

  x %>%
    mutate(
      Gravidade = ifelse(inicio_futuro | saida_antes, "Alta", "Média"),
      Motivo = paste0(
        str_remove(
          paste0(
            ifelse(inicio_futuro, paste0("Data de início da situação no futuro (", formatar_data(inicio), "); "), ""),
            ifelse(saida_antes, "Data de saída anterior à data de início; ", ""),
            ifelse(inicio_invalido, paste0("Data de início em formato não reconhecido (\"", inicio_txt, "\"); "), ""),
            ifelse(saida_invalida, paste0("Data de saída em formato não reconhecido (\"", saida_txt, "\"); "), "")
          ),
          "; $"
        ),
        "."
      )
    ) %>%
    arrange(Nome, inicio)

}

# Pessoas cuja lotação ATUAL (último registro do CPF, em situação não
# encerrada) é uma serventia que só aparece como não ativa na base de
# Serventias. `indice` = indice_serventias().
lotacoes_em_serventias_inativas <- function(d, indice, encerradas) {

  if (nrow(d) == 0 || nrow(indice) == 0) {
    return(d[0, ])
  }

  d %>%
    ultimo_registro_por_cpf() %>%
    filter(!is.na(situacao) & situacao != "" & !(situacao %in% encerradas)) %>%
    filter(!is.na(orgao) & orgao != "") %>%
    mutate(chave = chave_lotacao(orgao)) %>%
    inner_join(indice %>% rename(serv_codigo = codigo, serv_nome = nome, serv_status = status), by = "chave") %>%
    filter(!alguma_ativa) %>%
    mutate(
      orgao_exibicao = ifelse(
        serv_nome != "" & chave_nome_orgao(serv_nome) != chave_nome_orgao(orgao),
        paste0(orgao, " - ", serv_nome),
        orgao
      ),
      Gravidade = "Alta",
      Motivo = paste0(
        "Último registro do CPF (situação ", situacao, ") tem lotação em ",
        ifelse(serv_nome != "", serv_nome, orgao),
        ifelse(serv_codigo != "", paste0(" (código ", serv_codigo, ")"), ""),
        ", que está com status \"", ifelse(serv_status != "", serv_status, "não informado"),
        "\" na base de Serventias. Conferir a lotação ou o status da serventia."
      )
    ) %>%
    arrange(Nome)

}

verificar_inconsistencias_servidores <- function(df, serventias = NULL, hoje = Sys.Date()) {

  vazio <- inconsistencias_pessoas_vazio()

  d <- preparar_registros_pessoas(df, REGEX_COL_ORGAO_SERVIDOR)

  if (is.null(d)) {
    return(vazio)
  }

  rot_sit <- SITUACAO_PROFISSIONAL_SERVIDOR

  # ---- 1) Situação definitiva seguida de retorno ----

  r1 <- situacoes_definitivas_com_retorno(
    d,
    definitivas = SITUACOES_SERV_DEFINITIVAS,
    encerradas = SITUACOES_SERV_ENCERRADAS,
    qualquer_registro_posterior = SITUACAO_SERV_FALECIDO,
    rotulos = rot_sit,
    classificar = function(x) {
      ifelse(
        x$situacao == SITUACAO_SERV_FALECIDO |
          (x$situacao == SITUACAO_SERV_APOSENTADO & (x$voltou | x$curta)),
        "Alta",
        "Média"
      )
    },
    sufixo_motivo = function(x) {
      ifelse(
        x$situacao == SITUACAO_SERV_APOSENTADO & (x$voltou | x$curta),
        ". Perfil de afastamento temporário - provável situação 6 (Afastado(a))",
        ifelse(
          x$situacao == SITUACAO_SERV_FALECIDO,
          ". Registro posterior ao falecimento",
          ifelse(x$voltou, ". Conferir se é reingresso (novo vínculo) ou erro de situação", "")
        )
      )
    }
  )

  # ---- 2) Cargo incompatível com a situação ----

  r2 <- d %>%
    mutate(
      sem_vinculo_comissao = cargo %in% CARGOS_SERV_SEM_VINCULO_EFETIVO & situacao %in% SITUACOES_SERV_COMISSAO,
      sem_vinculo_aposentado = cargo %in% CARGOS_SERV_SEM_VINCULO_EFETIVO & situacao == SITUACAO_SERV_APOSENTADO,
      comissionado_sem_comissao = cargo == CARGO_SERV_COMISSIONADO & situacao == SITUACAO_SERV_SEM_COMISSAO,
      cedido_aposentado = cargo %in% c(CARGOS_SERV_CEDIDOS, CARGO_SERV_COMISSIONADO) & situacao == SITUACAO_SERV_APOSENTADO
    ) %>%
    filter(sem_vinculo_comissao | sem_vinculo_aposentado | comissionado_sem_comissao | cedido_aposentado) %>%
    mutate(
      Gravidade = ifelse(cedido_aposentado & !sem_vinculo_aposentado, "Média", "Alta"),
      rotulo_cargo = coalesce(unname(CARGO_SERVIDOR[cargo]), paste("cargo", cargo)),
      rotulo_sit = coalesce(unname(rot_sit[situacao]), paste("situação", situacao)),
      Motivo = paste0(
        "Cadastrado(a) como ", rotulo_cargo, " (cargo ", cargo, ") com situação ",
        rotulo_sit, " (", situacao, "). ",
        case_when(
          sem_vinculo_comissao ~ "Esse tipo de vínculo não ocupa cargo de chefia, cargo em comissão nem função comissionada.",
          sem_vinculo_aposentado ~ "Esse tipo de vínculo não se aposenta pelo Tribunal - o esperado seria 9 (Exoneração/Vacância) ou 13 (Vigência de Contrato/Vínculo).",
          comissionado_sem_comissao ~ "Comissionado(a) sem vínculo ocupa, por definição, cargo em comissão - provável situação 1 ou 2.",
          TRUE ~ "A aposentadoria ocorre no órgão de origem/RGPS - o esperado seria 9 (Exoneração/Vacância) ou 12 (Saída por cessão/requisição)."
        )
      )
    ) %>%
    arrange(Nome, inicio)

  # ---- 3) Lotação em serventia inativa ----

  r3 <- lotacoes_em_serventias_inativas(d, indice_serventias(serventias), SITUACOES_SERV_ENCERRADAS)

  # ---- 4) Datas da situação inconsistentes ----

  r4 <- datas_situacao_inconsistentes(d, hoje)

  bind_rows(
    montar_inconsistencias_pessoas(r1, VERIFICACAO_SERV_DEFINITIVA),
    montar_inconsistencias_pessoas(r2, VERIFICACAO_SERV_CARGO),
    montar_inconsistencias_pessoas(r3, VERIFICACAO_SERV_LOTACAO),
    montar_inconsistencias_pessoas(r4, VERIFICACAO_SERV_DATAS)
  )

}

# Lotação ATUAL das pessoas (último registro de cada CPF, em situação
# não encerrada), com a chave usada para casar com a base de Serventias.
# Usada pelo módulo Serventias (verificação "Serventia inativa com
# lotação ativa").
lotacoes_ativas_pessoas <- function(df, regex_orgao, encerradas) {

  vazio <- tibble(CPF = character(0), Nome = character(0), situacao = character(0),
                  orgao = character(0), chave = character(0))

  d <- preparar_registros_pessoas(df, regex_orgao)

  if (is.null(d) || nrow(d) == 0) {
    return(vazio)
  }

  d %>%
    ultimo_registro_por_cpf() %>%
    filter(!is.na(situacao) & situacao != "" & !(situacao %in% encerradas)) %>%
    filter(!is.na(orgao) & orgao != "") %>%
    # as.character(): sem nenhuma linha, chave_lotacao() (ifelse) devolve
    # logical(0), e o bind_rows() de magistrados + servidores no módulo
    # Serventias quebrava a sessão.
    transmute(CPF, Nome, situacao, orgao, chave = as.character(chave_lotacao(orgao)))

}

# Registros de Pessoal e Auxiliar que alimentam as verificações e o
# cruzamento com Serventias: a BASE (histórico completo, com data de
# saída), se tiver sido enviada; senão, os arquivos de alertas.
registros_pessoal_empresa <- function(base, alertas) {
  if (!is.null(base) && nrow(base) > 0) base else alertas
}

# Lotações atuais dos servidores da empresa (base de Pessoal e Auxiliar
# ou, sem ela, arquivos de alertas).
carregar_lotacoes_servidores_empresa <- function(empresa) {
  tryCatch(
    lotacoes_ativas_pessoas(
      registros_pessoal_empresa(
        carregar_base_pessoal_empresa(empresa),
        carregar_alertas(caminho_alertas(empresa))
      ),
      REGEX_COL_ORGAO_SERVIDOR,
      SITUACOES_SERV_ENCERRADAS
    ),
    error = function(e) lotacoes_ativas_pessoas(NULL, REGEX_COL_ORGAO_SERVIDOR, character(0))
  )
}

# =====================================================
# BASE DE DADOS — PESSOAL E AUXILIAR
# -----------------------------------------------------
# Cadastro do Quadro de Pessoal e Auxiliar do MPM/CNJ (histórico de
# situações de cada CPF), num arquivo próprio — fora da pasta de alertas,
# que consolida TODOS os arquivos dela:
#
#   <PASTA_ALERTAS>/<EMPRESA>/pessoal_auxiliar/pessoal_auxiliar.xlsx  (ou .csv)
#
# No upload, o tipo de cada arquivo é identificado pelas colunas (ver
# identificar_tipo_arquivo_pessoal()): com colunas "Alerta: ..." ele vai
# para a pasta de alertas (com o nome original saneado); sem elas, é a
# base e é gravado com o nome fixo acima (mantendo a extensão).
# =====================================================

SUBPASTA_PESSOAL <- "pessoal_auxiliar"
RADICAL_BASE_PESSOAL <- "pessoal_auxiliar"

# Arquivo de ENVIO ao MPM (o que o órgão envia, antes do processamento
# do CNJ): mesmas colunas da base, então é reconhecido pelo NOME — que
# precisa conter "envio" (ex.: quadro_pessoal_e_auxiliar_envio.xlsx).
# Gravado com o nome fixo abaixo, na mesma pasta da base:
#
#   <PASTA_ALERTAS>/<EMPRESA>/pessoal_auxiliar/pessoal_auxiliar_envio.xlsx (ou .csv)
RADICAL_ENVIO_PESSOAL <- "pessoal_auxiliar_envio"
REGEX_NOME_ARQUIVO_ENVIO <- "(^|_)envio(_|$)"

# "Órgão de lotação do(a) Servidor(a) ou Auxiliar" — distingue os
# arquivos de Pessoal e Auxiliar dos de Magistrados ("... do
# magistrado(a)"), que também têm CPF.
REGEX_ASSINATURA_SERVIDOR <- "^orgao de lotacao do\\(?a?\\)? ?servidor"

REGEX_COL_STATUS_PESSOAL <- "^status$"
REGEX_COL_RACA_PESSOAL <- "^raca"
REGEX_COL_AREA_ATUACAO <- "^area de atuacao"
REGEX_COL_EXCLUSAO_ERRO <- "^exclusao de registro"

# Opções de agrupamento do gráfico da base (rótulo -> regex da coluna).
AGRUPAMENTOS_BASE_PESSOAL <- c(
  "Cargo"                       = REGEX_COL_CARGO,
  "Situação profissional atual" = REGEX_COL_SITUACAO,
  "Órgão de lotação"            = REGEX_COL_ORGAO_SERVIDOR,
  "Área de atuação"             = REGEX_COL_AREA_ATUACAO,
  "Sexo"                        = REGEX_COL_SEXO,
  "Raça / Cor"                  = REGEX_COL_RACA_PESSOAL,
  "Naturalidade"                = REGEX_COL_NATURALIDADE,
  "Status"                      = REGEX_COL_STATUS_PESSOAL
)

pasta_pessoal <- function(empresa) {
  caminho_alertas(empresa, SUBPASTA_PESSOAL)
}

# Arquivos da base (ou, com `radical` = RADICAL_ENVIO_PESSOAL, do envio)
# na pasta, na ordem de prioridade (.xlsx antes de .csv).
arquivos_base_pessoal <- function(pasta, radical = RADICAL_BASE_PESSOAL) {

  if (is.null(pasta) || !dir.exists(pasta)) {
    return(character(0))
  }

  existentes <- list.files(pasta, full.names = TRUE)

  ok <- extensao_arquivo(existentes) %in% EXTENSOES_PLANILHA &
    normalizar_nome_arquivo(existentes) == radical

  achados <- existentes[ok]
  achados[order(match(extensao_arquivo(achados), EXTENSOES_PLANILHA))]

}

arquivo_base_pessoal <- function(pasta, radical = RADICAL_BASE_PESSOAL) {
  achados <- arquivos_base_pessoal(pasta, radical)
  if (length(achados) > 0) achados[1] else NULL
}

arquivos_envio_pessoal <- function(pasta) {
  arquivos_base_pessoal(pasta, RADICAL_ENVIO_PESSOAL)
}

carregar_base_pessoal <- function(arquivo) {

  if (is.null(arquivo) || !file.exists(arquivo)) {
    return(data.frame())
  }

  # Lido e limpo uma vez por processo e reaproveitado por todas as sessões (e pelos outros módulos e pelo Painel) enquanto o arquivo não mudar — ver com_cache_arquivo() em R/utils.R.
  com_cache_arquivo(arquivo, "base_pessoal", function() {
    limpar_dados_lidos(ler_arquivo_dados(arquivo, aba_preferida = "pessoal_auxiliar"))
  })

}

carregar_base_pessoal_empresa <- function(empresa) {
  tryCatch(
    carregar_base_pessoal(arquivo_base_pessoal(pasta_pessoal(empresa))),
    error = function(e) data.frame()
  )
}

carregar_envio_pessoal_empresa <- function(empresa) {
  tryCatch(
    carregar_base_pessoal(arquivo_base_pessoal(pasta_pessoal(empresa), RADICAL_ENVIO_PESSOAL)),
    error = function(e) data.frame()
  )
}

# Tipo de um arquivo enviado em "Gerenciar Arquivos":
#   - sem CPF ou sem "Órgão de lotação do(a) Servidor(a)..."   -> NA
#   - com colunas "Alerta..."/"Conflito..."                     -> alertas
#   - sem colunas de alerta, com "envio" no nome do arquivo     -> envio
#   - sem colunas de alerta                                     -> base
identificar_tipo_arquivo_pessoal <- function(df, nome_arquivo = NULL) {

  if (is.null(df) || ncol(df) == 0) {
    return(NA_character_)
  }

  names(df) <- limpar_cabecalho(names(df))

  if (is.null(coluna_por_regex(df, REGEX_COL_CPF)) ||
      is.null(coluna_por_regex(df, REGEX_ASSINATURA_SERVIDOR))) {
    return(NA_character_)
  }

  tem_alertas <- any(str_starts(names(df), "Alerta") | str_starts(names(df), "Conflito"))

  if (tem_alertas) {
    return("alertas")
  }

  eh_envio <- !is.null(nome_arquivo) && length(nome_arquivo) == 1 && !is.na(nome_arquivo) &&
    str_detect(normalizar_nome_arquivo(nome_arquivo), REGEX_NOME_ARQUIVO_ENVIO)

  if (eh_envio) "envio" else "base"

}

# =====================================================
# NOVO ENVIO — CONFLITO "DUPLICIDADE NO PRÓPRIO TRIBUNAL"
# -----------------------------------------------------
# Para cada CPF com o conflito "Duplicidade no próprio Tribunal" nos
# arquivos de alertas, gera DUAS linhas, nesta ordem:
#
#   1) o registro da BASE DE DADOS (o mais recente com Data de início
#      anterior à do envio), igual ao da base, apenas com a inclusão da
#      "Data de saída da situação" = "Data de início da situação" do
#      registro do ENVIO — encerra a situação anterior;
#   2) o registro do ENVIO, exatamente como está no arquivo de envio
#      (o mais recente do CPF, se houver mais de um).
#
# As colunas são as do arquivo de envio, na mesma ordem e com o mesmo
# cabeçalho; colunas do envio que a base não tem ficam vazias. Os
# valores saem como estão nos arquivos (códigos de Cargo, Situação etc.
# — nunca a nomenclatura da tela). Datas que o Excel guardou como número
# serial saem em dd/mm/aaaa, o formato do MPM.
#
# Só entram os CPFs cujo registro da base tem Situação profissional
# atual 1, 2 ou 3 (Cargo de chefia; Outros cargos em comissão ou funções
# comissionadas; Não exerce cargo em comissão ou função comissionada —
# SITUACOES_NOVO_ENVIO): as situações em exercício, que o novo envio
# encerra. Os demais CPFs não geram linhas.
#
# Devolve list(dados, cpfs_conflito, sem_envio, sem_base,
# fora_situacao).
# =====================================================

SITUACOES_NOVO_ENVIO <- c("1", "2", "3")

REGEX_CONFLITO_DUPLICIDADE_TRIBUNAL <- "^conflito.*duplicidade.*proprio tribunal"

REGEX_COLS_DATA_ENVIO <- "^data "

# CPF só com dígitos e 11 posições, para casar CPFs entre arquivos (o
# Excel tira o zero à esquerda: "1001715594" = "01001715594").
chave_cpf <- function(x) {
  d <- gsub("\\D", "", as.character(x))
  d[is.na(x)] <- ""
  ifelse(nchar(d) > 0 & nchar(d) < 11, str_pad(d, 11, pad = "0"), d)
}

# "45901" (número serial do Excel) -> "01/09/2025"; o resto fica igual.
data_formato_envio <- function(x) {
  x <- as.character(x)
  serial <- !is.na(x) & str_detect(x, "^\\d{5}(\\.\\d+)?$")
  x[serial] <- format(as.Date(floor(as.numeric(x[serial])), origin = "1899-12-30"), "%d/%m/%Y")
  x
}

novo_envio_vazio <- function(colunas = character(0)) {
  df <- as.data.frame(
    stats::setNames(replicate(length(colunas), character(0), simplify = FALSE), colunas),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  list(dados = df, cpfs_conflito = character(0), sem_envio = character(0), sem_base = character(0),
       fora_situacao = character(0))
}

# Cartão "Gerar Novo Envio" da aba Envio > Gerar arquivo CSV, com o
# mesmo visual em Pessoal e Auxiliar, Magistrados e Serventias. O botão
# baixa ns("novo_envio_download") e fica desabilitado sem linhas.
ui_novo_envio <- function(ns, pode_gerar, descricao, resumo) {

  div(
    class = "mt-4 pt-3 border-top",
    style = "max-width: 640px;",

    div(class = "al-modal-secao-titulo", icon("paper-plane", class = "text-primary"), "Gerar Novo Envio"),

    p(class = "text-muted", descricao),

    div(class = "mb-2", style = "font-size: .85rem;", resumo),

    if (pode_gerar) {
      downloadButton(
        ns("novo_envio_download"),
        "Gerar Novo Envio",
        icon = icon("paper-plane"),
        class = "btn btn-primary btn-acao"
      )
    } else {
      tags$button(
        type = "button",
        class = "btn btn-primary btn-acao",
        disabled = "disabled",
        icon("paper-plane", class = "me-2"),
        "Gerar Novo Envio"
      )
    }
  )

}

gerar_novo_envio_duplicidade <- function(envio, base, alertas) {

  if (is.null(envio) || ncol(envio) == 0) {
    return(novo_envio_vazio())
  }

  colunas <- names(envio)
  vazio <- novo_envio_vazio(colunas)

  if (is.null(alertas) || nrow(alertas) == 0 || nrow(envio) == 0) {
    return(vazio)
  }

  # ---- CPFs com o conflito ----
  nomes_alertas <- normalizar_nome_coluna(names(alertas))
  cols_conflito <- names(alertas)[str_detect(nomes_alertas, REGEX_CONFLITO_DUPLICIDADE_TRIBUNAL)]
  col_cpf_alertas <- coluna_por_regex(alertas, REGEX_COL_CPF)

  if (length(cols_conflito) == 0 || is.null(col_cpf_alertas)) {
    return(vazio)
  }

  tem_conflito <- Reduce(`|`, lapply(cols_conflito, function(c) {
    v <- alertas[[c]]
    !is.na(v) & str_trim(v) != ""
  }))

  cpfs <- unique(chave_cpf(alertas[[col_cpf_alertas]][tem_conflito]))
  cpfs <- cpfs[cpfs != ""]

  if (length(cpfs) == 0) {
    return(vazio)
  }

  vazio$cpfs_conflito <- cpfs

  # ---- colunas usadas ----
  col_cpf_envio <- coluna_por_regex(envio, REGEX_COL_CPF)
  col_inicio_envio <- coluna_por_regex(envio, REGEX_COL_INICIO_SITUACAO)
  col_saida_envio <- coluna_por_regex(envio, REGEX_COL_SAIDA_SITUACAO)

  if (is.null(col_cpf_envio) || is.null(col_inicio_envio)) {
    vazio$sem_envio <- cpfs
    return(vazio)
  }

  base <- remover_registros_excluidos(base)
  col_cpf_base <- if (!is.null(base) && nrow(base) > 0) coluna_por_regex(base, REGEX_COL_CPF)
  col_inicio_base <- if (!is.null(col_cpf_base)) coluna_por_regex(base, REGEX_COL_INICIO_SITUACAO)
  col_situacao_base <- if (!is.null(col_cpf_base)) coluna_por_regex(base, REGEX_COL_SITUACAO)

  # Coluna da base que corresponde a cada coluna do envio (pelo nome
  # normalizado: sem acento, maiúsculas ou espaços extras).
  correspondencia <- if (!is.null(col_cpf_base)) {
    names(base)[match(normalizar_nome_coluna(colunas), normalizar_nome_coluna(names(base)))]
  } else {
    rep(NA_character_, length(colunas))
  }

  chave_envio <- chave_cpf(envio[[col_cpf_envio]])
  inicio_envio <- converter_data(envio[[col_inicio_envio]])
  chave_base <- if (!is.null(col_cpf_base)) chave_cpf(base[[col_cpf_base]]) else character(0)
  inicio_base <- if (!is.null(col_inicio_base)) converter_data(base[[col_inicio_base]]) else as.Date(character(0))

  como_texto <- function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    v
  }

  linhas <- list()
  sem_envio <- character(0)
  sem_base <- character(0)
  fora_situacao <- character(0)

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

    # Registro da base: o mais recente com início ANTES do início do
    # envio (a situação que o envio encerra).
    i_base <- which(chave_base == cpf)
    if (length(i_base) > 0 && !is.na(data_ini_env)) {
      i_base <- i_base[!is.na(inicio_base[i_base]) & inicio_base[i_base] < data_ini_env]
    } else {
      i_base <- integer(0)
    }

    if (length(i_base) == 0) {
      sem_base <- c(sem_base, cpf)
      next
    }

    i_base <- i_base[order(inicio_base[i_base], i_base)][length(i_base)]

    # Só situações 1, 2 ou 3 na base (ver SITUACOES_NOVO_ENVIO).
    situacao_base <- if (is.null(col_situacao_base)) NA_character_ else codigo_limpo(base[[col_situacao_base]][i_base])
    if (is.na(situacao_base) || !(situacao_base %in% SITUACOES_NOVO_ENVIO)) {
      fora_situacao <- c(fora_situacao, cpf)
      next
    }

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
       fora_situacao = fora_situacao)

}

# Remove os registros com "Exclusão de registro por erro" = S/Sim (o
# próprio MPM marca assim os registros enviados por engano). Sem a
# coluna, devolve o data.frame como veio.
remover_registros_excluidos <- function(df) {

  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  col <- coluna_por_regex(df, REGEX_COL_EXCLUSAO_ERRO)

  if (is.null(col)) {
    return(df)
  }

  excluido <- normalizar_nome_coluna(df[[col]]) %in% c("s", "sim")
  df[!excluido, , drop = FALSE]

}

# Código da serventia -> nome, a partir da base de Serventias da empresa.
# Vetor nomeado vazio se a base não tiver sido enviada.
nomes_serventias <- function(serventias) {

  if (is.null(serventias) || nrow(serventias) == 0 ||
      !exists("REGEX_COL_CODIGO_SERVENTIA") || !exists("REGEX_COL_NOME_SERVENTIA")) {
    return(character(0))
  }

  col_cod <- coluna_por_regex(serventias, REGEX_COL_CODIGO_SERVENTIA)
  col_nome <- coluna_por_regex(serventias, REGEX_COL_NOME_SERVENTIA)

  if (is.null(col_cod) || is.null(col_nome)) {
    return(character(0))
  }

  mapa <- as.character(serventias[[col_nome]])
  names(mapa) <- codigo_limpo(serventias[[col_cod]])
  mapa <- mapa[!is.na(names(mapa)) & names(mapa) != "" & !is.na(mapa)]
  mapa[!duplicated(names(mapa))]

}

# "32" -> "32 - 2ª VARA CÍVEL DE ...", quando o código existir no mapa.
# Valores que não são um código conhecido ficam como vieram.
rotular_orgaos <- function(valores, mapa) {

  if (length(mapa) == 0) {
    return(valores)
  }

  cod <- codigo_limpo(valores)
  nome <- unname(mapa[cod])

  ifelse(!is.na(nome), paste0(cod, " - ", nome), valores)

}

# =====================================================
# CONSOLIDA ALERTA(S)/CONFLITO(S) DETECTADO(S) — SÓ PARA A TABELA
# -----------------------------------------------------
# O arquivo original traz uma coluna "Alerta: <tipo>" ou
# "Conflito: <tipo>" para cada tipo possível (a maioria vazia na
# maior parte das linhas) — o que deixa a tabela com dezenas de
# colunas. Esta função mantém as colunas fixas até "Órgão de lotação
# do(a) Servidor(a) ou Auxiliar" e substitui:
#   - todas as colunas "Alerta: ..." por uma única "Alerta(s) Detectado";
#   - todas as colunas "Conflito: ..." por uma única "Conflito(s)
#     Detectado", logo em seguida.
# Para cada uma que tiver conteúdo naquela linha, junta o texto depois
# dos dois-pontos do nome da coluna com o conteúdo da célula (ex.:
# "Data Nascimento: Data de nascimento não corresponde..."); se a
# linha tiver mais de um alerta (ou mais de um conflito), cada um vira
# um trecho separado por " | " dentro da mesma célula.
#
# Usada só para o que é exibido na aba Tabela (e no CSV gerado a
# partir dela) — dados(), dados_filtrados() e o gráfico continuam
# trabalhando com as colunas originais, sem nenhuma alteração de
# regra.
# =====================================================

# Junta, linha a linha, o conteúdo das colunas informadas em um único
# texto por linha ("<sufixo depois de :>: <valor>", vários trechos
# separados por " | " quando mais de uma coluna tiver conteúdo na
# mesma linha). Usada por consolidar_alertas_tabela() para Alerta e
# Conflito separadamente.
consolidar_colunas_por_linha <- function(df, colunas) {
  
  if (length(colunas) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  
  # Vetorizado por COLUNA (poucas dezenas) em vez de apply() por LINHA
  # (milhares): mesmo resultado, ~80x mais rápido em arquivos grandes.
  sufixos <- str_trim(str_remove(colunas, "^[^:]*:"))

  resultado <- rep(NA_character_, nrow(df))

  for (j in seq_along(colunas)) {

    valores <- str_trim(as.character(df[[colunas[j]]]))
    preenchido <- !is.na(valores) & valores != ""

    if (!any(preenchido)) {
      next
    }

    trecho <- paste0(sufixos[j], ": ", valores[preenchido])
    atual <- resultado[preenchido]

    resultado[preenchido] <- ifelse(is.na(atual), trecho, paste0(atual, " | ", trecho))

  }

  resultado
  
}

consolidar_alertas_tabela <- function(df) {
  
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }
  
  coluna_orgao <- "Órgão de lotação do(a) Servidor(a) ou Auxiliar"
  idx_orgao <- which(names(df) == coluna_orgao)
  
  if (length(idx_orgao) == 0) {
    # Layout inesperado (coluna não encontrada) — não quebra a tela,
    # só não consolida nada e mostra os dados como vieram.
    return(df)
  }
  
  colunas_fixas <- names(df)[seq_len(idx_orgao[1])]
  
  colunas_alerta <- colunas_por_prefixo(df, "Alerta")
  colunas_conflito <- colunas_por_prefixo(df, "Conflito")
  
  df_exibicao <- df[colunas_fixas]
  df_exibicao[["Alerta(s) Detectado"]] <- consolidar_colunas_por_linha(df, colunas_alerta)
  df_exibicao[["Conflito(s) Detectado"]] <- consolidar_colunas_por_linha(df, colunas_conflito)
  
  df_exibicao
  
}

# =====================================================
# ABA DE CADASTRO (Base de Dados / Envio - Pessoal e Auxiliar)
# -----------------------------------------------------
# As duas abas têm os mesmos filtros (Status, CPF, Nome, Cargo,
# Situação, Órgão de lotação e Detalhe), gráfico e CSV. `p` é o prefixo
# dos ids ("base" ou "envio": base_status, envio_status...). O lado do
# servidor é montado por servidor_aba_cadastro_pessoal(), dentro de
# mod_alertas_server().
# =====================================================

ui_aba_cadastro_pessoal <- function(ns, p, titulo, icone) {

  id <- function(x) ns(paste0(p, "_", x))

  tabPanel(
    title = tagList(icon(icone, class = "me-1"), titulo),
    value = p,

    div(
      class = "al-card",
      # Bloqueado (ver handler JS "bloquear-filtros", em app.R) enquanto
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
          # Começa em "Ativo"; se o arquivo não tiver coluna Status,
          # volta para "Todos".
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
        column(
          4,
          selectInput(id("cargo"), "Cargo", choices = c("Todos"), width = "100%")
        ),
        column(
          4,
          selectInput(
            id("situacao"), "Situação profissional atual",
            choices = c("Todos"), width = "100%"
          )
        ),
        column(
          4,
          selectInput(
            id("orgao"), "Órgão de lotação",
            choices = c("Todos"), width = "100%"
          )
        )
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
                choices = names(AGRUPAMENTOS_BASE_PESSOAL),
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

# =====================================================
# UI
# =====================================================

mod_alertas_ui <- function(id) {

  ns <- NS(id)

  tagList(

    # ===================================================
    # ESTILO ENTERPRISE (escopo deste módulo)
    # ===================================================

    tags$head(
      tags$style(HTML(sprintf("

        #%1$s .al-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 1rem;
          margin-bottom: 1.5rem;
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

        /* Abas principais (Base de Dados / Alertas / Inconsistências) */
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

        /* Garante que a tabela ocupe 100%% da largura disponível —
           autoWidth do DT, combinado com scrollX, às vezes não
           estica sozinho até a borda do container. */
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
      # CABEÇALHO - TÍTULO + AÇÕES (valem para as três abas)
      # =================================================

      div(
        class = "al-header",

        div(
          class = "al-titulo",
          div(class = "al-titulo-icone", icon("users")),
          tags$h4("Pessoal e Auxiliar")
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
          # ABA 1 — BASE DE DADOS - PESSOAL E AUXILIAR
          # -------------------------------------------------
          # Mesma estrutura da base de Magistrados: a tabela mostra as
          # colunas do arquivo, na mesma ordem (Cargo e Situação por
          # extenso; Órgão como "código - nome da serventia", quando a
          # base de Serventias tiver sido enviada).
          # =============================================

          ui_aba_cadastro_pessoal(
            ns, "base",
            titulo = "Base de Dados",
            icone = "database"
          ),

          # =============================================
          # ABA 2 — ENVIO - PESSOAL E AUXILIAR
          # -------------------------------------------------
          # Arquivo que o órgão envia ao MPM (pessoal_auxiliar_envio),
          # com os mesmos filtros, gráfico e CSV da base.
          # =============================================

          ui_aba_cadastro_pessoal(
            ns, "envio",
            titulo = "Envio",
            icone = "paper-plane"
          ),

          # =============================================
          # ABA 3 — ALERTAS - PESSOAL E AUXILIAR
          # -------------------------------------------------
          # "Alerta" e "Conflito" se excluem mutuamente (ver
          # observeEvent(input$alerta)/observeEvent(input$conflito) no
          # server) — escolher um dos dois volta o outro para "Todos".
          # =============================================

          tabPanel(
            title = tagList(icon("triangle-exclamation", class = "me-1"), "Alertas"),
            value = "alertas",

            div(
              class = "al-card",
              id = ns("alertas_filtros"),

              div(class = "al-card-titulo", "Filtros"),

              div(
                class = "al-filtros-aviso",
                icon("lock", class = "me-2"),
                "Filtros indisponíveis: não há arquivos de alertas para exibir. Envie-os pelo botão \"Gerenciar Arquivos\"."
              ),

              fluidRow(
                column(3, selectInput(ns("alerta"), "Alerta", choices = c("Todos"), width = "100%")),
                column(3, selectInput(ns("conflito"), "Conflito", choices = c("Todos"), width = "100%")),
                column(3, textInput(ns("cpf"), "CPF", width = "100%")),
                column(3, textInput(ns("nome"), "Nome", width = "100%"))
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
                tabPanel(tagList(icon("table", class = "me-1"), "Tabela"), DTOutput(ns("tabela"))),
                tabPanel(
                  tagList(icon("chart-column", class = "me-1"), "Gráfico"),

                  # 1) Tipo de Alerta/Conflito (echarts4r) — em um uiOutput
                  #    porque a altura acompanha a quantidade de barras.
                  uiOutput(ns("grafico_ui")),

                  tags$hr(class = "my-4"),

                  # 2) Registros por Cargo (ggiraph)
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
          # ABA 4 — INCONSISTÊNCIAS - PESSOAL E AUXILIAR
          # -------------------------------------------------
          # Verificações automáticas (ver
          # verificar_inconsistencias_servidores()) sobre a base — ou,
          # sem ela, sobre os arquivos de alertas; apresentação pelo
          # submódulo mod_inconsistencias.R.
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
              verificacoes = VERIFICACOES_SERVIDORES,
              filtros_texto = FILTROS_TEXTO_INC_SERVIDORES
            )
          )
        )
      )

    )

  )

}

# Filtros de texto da aba Inconsistências (rótulo -> coluna).
FILTROS_TEXTO_INC_SERVIDORES <- c("CPF" = "CPF", "Nome" = "Nome")

# =====================================================
# SERVER
# =====================================================

# =====================================================
# SERVIDOR DA ABA DE CADASTRO (Base de Dados / Envio)
# -----------------------------------------------------
# Chamado dentro de mod_alertas_server(). `dados` é o reactiveVal com o
# arquivo lido; `p`, o prefixo dos ids da UI (ver
# ui_aba_cadastro_pessoal()). Devolve a versão de exibição, a filtrada e
# a função que atualiza os combos.
# =====================================================

servidor_aba_cadastro_pessoal <- function(input, output, session, p, dados, mapa_orgaos,
                                          msg_sem_dados, nome_csv, extra_csv = NULL) {

  ns <- session$ns
  id <- function(x) paste0(p, "_", x)
  valor <- function(x) input[[id(x)]]

  MSG_SEM_RESULTADO <- "Nenhum registro encontrado com os filtros aplicados."

  # Mesmas colunas e ordem do arquivo; Cargo e Situação por extenso e o
  # Órgão como "código - nome da serventia" (se a base de Serventias
  # tiver sido enviada). Filtros, Detalhe, gráfico e CSV usam esta
  # versão.
  exibicao <- reactive({
    df <- traduzir_codigos_tabela(dados())
    col_orgao <- coluna_por_regex(df, REGEX_COL_ORGAO_SERVIDOR)
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

    status <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_STATUS_PESSOAL))
    cargos <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_CARGO))
    situacoes <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_SITUACAO))
    orgaos <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_ORGAO_SERVIDOR))

    status_atual <- if (length(status_anterior) == 0) "Ativo" else valor("status")
    status_anterior <<- status

    updateSelectInput(
      session, id("status"),
      choices = c("Todos", status),
      selected = selecao_valida(status_atual, status, padrao = "Ativo")
    )

    updateSelectInput(
      session, id("cargo"),
      choices = c("Todos", cargos),
      selected = selecao_valida(valor("cargo"), cargos)
    )

    updateSelectInput(
      session, id("situacao"),
      choices = c("Todos", situacoes),
      selected = selecao_valida(valor("situacao"), situacoes)
    )

    updateSelectInput(
      session, id("orgao"),
      choices = c("Todos", orgaos),
      selected = selecao_valida(valor("orgao"), orgaos)
    )

  }

  # Status volta ao padrão "Ativo" (não para "Todos").
  observeEvent(valor("limpar_filtros"), {
    status <- opcoes_combo(valores_coluna_regex(exibicao(), REGEX_COL_STATUS_PESSOAL))
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

    filtrar_texto <- function(txt, regex) {
      txt <- str_trim(txt %||% "")
      col <- coluna_por_regex(df, regex)
      if (txt != "" && !is.null(col)) {
        manter <<- manter & contem_texto(df[[col]], txt)
      }
    }

    filtrar_combo(valor("status"), REGEX_COL_STATUS_PESSOAL)
    filtrar_combo(valor("cargo"), REGEX_COL_CARGO)
    filtrar_combo(valor("situacao"), REGEX_COL_SITUACAO)
    filtrar_combo(valor("orgao"), REGEX_COL_ORGAO_SERVIDOR)
    cpf <- str_trim(cpf_d() %||% "")
    col_cpf <- coluna_por_regex(df, REGEX_COL_CPF)
    if (cpf != "" && !is.null(col_cpf)) {
      manter <- manter & contem_cpf(df[[col_cpf]], cpf)
    }
    filtrar_texto(nome_d(), REGEX_COL_NOME)

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
    regex <- AGRUPAMENTOS_BASE_PESSOAL[[agrupar]]
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
          "<b>%s</b><br/>%s registro(s)<br/>%s pessoa(s) (CPF distintos)",
          htmltools::htmlEscape(Grupo),
          format(Quantidade, big.mark = ".", decimal.mark = ","),
          format(Pessoas, big.mark = ".", decimal.mark = ",")
        )
      )

    grafico_barras_girafe(
      exibido,
      titulo = sprintf("Quantidade de Registros por %s", r$agrupar),
      subtitulo = sprintf("Exibindo %d de %d grupo(s)", nrow(exibido), total)
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

  list(exibicao = exibicao, filtrados = filtrados, atualizar_combos = atualizar_combos,
       msg_sem_dados = msg_sem_dados)

}

mod_alertas_server <- function(id, ativo = reactive(TRUE), empresa = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Começam vazios: a empresa (distro) só é conhecida depois do login,
    # então o carregamento real acontece no observeEvent(empresa())
    # logo abaixo — nunca aqui na inicialização do módulo.
    dados <- reactiveVal(data.frame())       # arquivos de alertas (pasta alertas_csv)
    dados_base <- reactiveVal(data.frame())  # pessoal_auxiliar.xlsx/.csv
    dados_envio <- reactiveVal(data.frame()) # pessoal_auxiliar_envio.xlsx/.csv

    # Base de Serventias da empresa (módulo Serventias) — usada na
    # verificação "Lotação em serventia inativa" e para mostrar o nome
    # da serventia ao lado do código do órgão. Mudanças nela entram aqui
    # no "Atualizar Dados" ou na troca de empresa.
    base_serventias <- reactiveVal(data.frame())

    empresa_definida <- function() {
      e <- empresa()
      !is.null(e) && !is.na(e) && trimws(e) != ""
    }

    recarregar <- function() {
      dados(carregar_alertas(caminho_alertas(empresa())))
      dados_base(carregar_base_pessoal_empresa(empresa()))
      dados_envio(carregar_envio_pessoal_empresa(empresa()))
      base_serventias(carregar_base_serventias_empresa(empresa()))
    }

    # ----------------------------------------
    # CONTADOR DO CAMPO DE UPLOAD (modal "Gerenciar Arquivos")
    # -----------------------------------------------------
    # O fileInput() do modal usa um ID novo (ns("upload_arquivos_N"))
    # toda vez que o modal é aberto, em vez de um ID fixo. Sem isso, o
    # Shiny não garante que a seleção de arquivos do input$ fique vazia
    # ao reabrir o modal — o valor antigo pode continuar "vivo" no
    # servidor mesmo com o campo aparentando estar limpo na tela,
    # fazendo "Enviar para Pasta" reenviar os arquivos de uma seleção
    # anterior mesmo sem nada escolhido dessa vez.
    # ----------------------------------------
    contador_upload <- reactiveVal(0)

    id_upload_atual <- function() {
      paste0("upload_arquivos_", contador_upload())
    }

    entrada_upload_atual <- function() {
      input[[id_upload_atual()]]
    }

    # ----------------------------------------
    # CARREGA DADOS DA EMPRESA ATUAL
    # ----------------------------------------

    # Carga SOB DEMANDA: só quando o módulo é aberto pela primeira vez
    # para a empresa da sessão (ou quando a empresa muda com ele aberto),
    # em vez de logo no login. Com a empresa vazia (logout), os dados
    # são descartados.
    empresa_carregada <- reactiveVal(NULL)

    observeEvent(list(empresa(), ativo()), {

      if (!empresa_definida()) {
        if (!is.null(empresa_carregada())) {
          dados(data.frame())
          dados_base(data.frame())
          dados_envio(data.frame())
          base_serventias(data.frame())
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
    # EXIBIÇÃO DA BASE (códigos -> nomenclatura)
    # -------------------------------------------------
    # Mesmas colunas e ordem do arquivo; Cargo e Situação por extenso e
    # o Órgão como "código - nome da serventia" (se a base de Serventias
    # tiver sido enviada). Filtros, Detalhe, gráfico e CSV usam esta
    # versão.
    # =================================================

    mapa_orgaos <- reactive({
      nomes_serventias(base_serventias())
    })

    # Abas "Base de Dados" e "Envio" (mesmos filtros, gráfico e CSV) —
    # ver servidor_aba_cadastro_pessoal().
    aba_base <- servidor_aba_cadastro_pessoal(
      input, output, session, "base",
      dados = dados_base,
      mapa_orgaos = mapa_orgaos,
      msg_sem_dados = paste0(
        "Não existe arquivo da base de Pessoal e Auxiliar (pessoal_auxiliar.xlsx ou .csv)",
        " para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviá-lo."
      ),
      nome_csv = "base-pessoal-auxiliar"
    )

    aba_envio <- servidor_aba_cadastro_pessoal(
      input, output, session, "envio",
      dados = dados_envio,
      mapa_orgaos = mapa_orgaos,
      msg_sem_dados = paste0(
        "Não existe arquivo de envio de Pessoal e Auxiliar (pessoal_auxiliar_envio.xlsx ou .csv)",
        " para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviá-lo",
        " (o nome do arquivo precisa conter \"envio\")."
      ),
      nome_csv = "envio-pessoal-auxiliar",
      extra_csv = function() uiOutput(ns("novo_envio_ui"))
    )

    base_exibicao <- aba_base$exibicao
    dados_base_filtrados <- aba_base$filtrados

    # ---- Envio > Gerar arquivo CSV > "Gerar Novo Envio" ----
    # Ver gerar_novo_envio_duplicidade(). Usa os arquivos como foram
    # lidos (sem a tradução de códigos da tela) e todos os registros (os
    # filtros da aba não se aplicam).
    novo_envio <- reactive({
      gerar_novo_envio_duplicidade(dados_envio(), dados_base(), dados())
    })

    output$novo_envio_ui <- renderUI({

      r <- novo_envio()
      n_pessoas <- nrow(r$dados) / 2
      pode_gerar <- nrow(r$dados) > 0

      div(
        class = "mt-4 pt-3 border-top",
        style = "max-width: 640px;",

        div(class = "al-modal-secao-titulo", icon("paper-plane", class = "text-primary"), "Gerar Novo Envio"),

        p(
          class = "text-muted",
          "Para cada CPF com o conflito ", tags$b("Duplicidade no próprio Tribunal"),
          " nos arquivos de Alertas cujo registro na Base de Dados tem ", tags$b("Situação profissional atual 1, 2 ou 3"),
          ", gera duas linhas, com as mesmas colunas do arquivo de envio: ",
          "o registro da Base de Dados apenas com a inclusão da ", tags$b("Data de saída da situação"),
          " igual à Data de início da situação do envio, e logo abaixo o registro do envio, sem alteração. ",
          "Os valores saem como nos arquivos (códigos, não os textos da tela). Os filtros desta aba não se aplicam."
        ),

        div(
          class = "mb-2",
          style = "font-size: .85rem;",
          if (length(r$cpfs_conflito) == 0) {
            "Nenhum CPF com esse conflito nos arquivos de alertas."
          } else {
            tagList(
              sprintf("%d CPF(s) com o conflito; %d no novo envio (%d linhas).",
                      length(r$cpfs_conflito), n_pessoas, nrow(r$dados)),
              if (length(r$sem_envio) > 0) {
                div(class = "text-warning", sprintf("%d CPF(s) não estão no arquivo de envio.", length(r$sem_envio)))
              },
              if (length(r$sem_base) > 0) {
                div(class = "text-warning", sprintf(
                  "%d CPF(s) sem registro na Base de Dados com início anterior ao do envio.", length(r$sem_base)
                ))
              },
              if (length(r$fora_situacao) > 0) {
                div(class = "text-muted", sprintf(
                  "%d CPF(s) fora do novo envio: na Base de Dados, a situação profissional atual não é 1, 2 ou 3.",
                  length(r$fora_situacao)
                ))
              }
            )
          }
        ),

        if (pode_gerar) {
          downloadButton(
            ns("novo_envio_download"),
            "Gerar Novo Envio",
            icon = icon("paper-plane"),
            class = "btn btn-primary btn-acao"
          )
        } else {
          tags$button(
            type = "button",
            class = "btn btn-primary btn-acao",
            disabled = "disabled",
            icon("paper-plane", class = "me-2"),
            "Gerar Novo Envio"
          )
        }
      )

    })

    output$novo_envio_download <- downloadHandler(

      filename = function() {
        paste0("novo-envio-pessoal-auxiliar-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
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
    envio_exibicao <- aba_envio$exibicao
    dados_envio_filtrados <- aba_envio$filtrados

    atualizar_combos_base <- function() {
      aba_base$atualizar_combos()
      aba_envio$atualizar_combos()
    }

    # Combos da aba Alertas (Alerta / Conflito), preservando a seleção
    # atual quando ainda fizer sentido.
    atualizar_combos <- function() {

      df <- dados()

      colunas_alerta <- colunas_com_dados(df, colunas_por_prefixo(df, "Alerta"))
      colunas_conflito <- colunas_com_dados(df, colunas_por_prefixo(df, "Conflito"))

      updateSelectInput(
        session,
        "alerta",
        choices = c("Todos", colunas_alerta),
        selected = selecao_valida(input$alerta, colunas_alerta)
      )

      updateSelectInput(
        session,
        "conflito",
        choices = c("Todos", colunas_conflito),
        selected = selecao_valida(input$conflito, colunas_conflito)
      )

    }

    atualizar_todos_combos <- function() {
      atualizar_combos_base()
      atualizar_combos()
    }

    observeEvent(list(dados(), base_exibicao(), envio_exibicao(), ativo()), {
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
      updateTextInput(session, "cpf", value = "")
      updateTextInput(session, "nome", value = "")
    })

    # ----------------------------------------
    # EXCLUSÃO MÚTUA (Alerta x Conflito)
    # ----------------------------------------

    observeEvent(input$alerta, {
      req(input$alerta)

      if (input$alerta != "Todos" &&
          !is.null(input$conflito) &&
          input$conflito != "Todos") {
        updateSelectInput(session, "conflito", selected = "Todos")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$conflito, {
      req(input$conflito)

      if (input$conflito != "Todos" &&
          !is.null(input$alerta) &&
          input$alerta != "Todos") {
        updateSelectInput(session, "alerta", selected = "Todos")
      }
    }, ignoreInit = TRUE)

    # =================================================
    # GERENCIAR ARQUIVOS (comum às três abas)
    # -------------------------------------------------
    # Lista a base, o envio e os arquivos de alertas (cada um com a data
    # de atualização e seu botão Apagar) e envia um ou vários arquivos de
    # uma vez. O tipo de cada arquivo é identificado pelas colunas e, para
    # o envio, pelo nome (identificar_tipo_arquivo_pessoal):
    #   - base    -> <EMPRESA>/pessoal_auxiliar/pessoal_auxiliar.<ext>
    #               (substitui a anterior);
    #   - envio   -> <EMPRESA>/pessoal_auxiliar/pessoal_auxiliar_envio.<ext>
    #               (substitui o anterior);
    #   - alertas -> <EMPRESA>/alertas_csv/<nome original saneado>
    #               (vários arquivos; podem se somar aos existentes).
    # =================================================

    arquivos_alertas_existentes <- function() {
      cam <- caminho_alertas(empresa())
      if (is.null(cam) || !dir.exists(cam)) character(0) else list.files(cam, full.names = TRUE)
    }

    linha_arquivo <- function(nome, info, id_apagar = NULL, icone = "file-csv", aviso = NULL) {
      div(
        class = "srv-arquivo-linha",
        div(
          div(class = "srv-arquivo-nome", icon(icone, class = "me-1"), nome),
          div(class = "srv-arquivo-info", info, aviso)
        ),
        if (!is.null(id_apagar)) {
          actionButton(
            ns(id_apagar),
            tagList(icon("trash", class = "me-1"), "Apagar"),
            class = "btn btn-outline-danger btn-sm"
          )
        }
      )
    }

    # Data/hora da última atualização (mais recente, se forem vários).
    data_atualizacao <- function(arquivos) {
      format(max(file.info(arquivos)$mtime), "%d/%m/%Y %H:%M")
    }

    # Linha da base ou do envio: nome, data de atualização (ou "ainda não
    # enviado"), botão Apagar e aviso se houver mais de um na pasta.
    linha_cadastro <- function(arquivos, radical, rotulo, id_apagar) {
      tem <- length(arquivos) > 0
      linha_arquivo(
        nome = if (tem) basename(arquivos[1]) else paste0(radical, ".xlsx / .csv"),
        info = paste0(
          rotulo, " — ",
          if (tem) sprintf("atualizado em %s", data_atualizacao(arquivos[1])) else "ainda não enviado"
        ),
        id_apagar = if (tem) id_apagar,
        icone = if (tem && extensao_arquivo(arquivos[1]) == "xlsx") "file-excel" else "file-csv",
        aviso = if (length(arquivos) > 1) {
          tags$div(
            class = "text-warning",
            sprintf("também na pasta (ignorado): %s", paste(basename(arquivos[-1]), collapse = ", "))
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

      bases <- arquivos_base_pessoal(pasta_pessoal(empresa()))
      envios <- arquivos_envio_pessoal(pasta_pessoal(empresa()))
      alertas <- arquivos_alertas_existentes()

      # ID novo do fileInput a cada abertura — ver contador_upload().
      contador_upload(contador_upload() + 1)

      showModal(modalDialog(
        title = div(
          style = "position:relative; padding-right:28px;",
          icon("folder-open", class = "me-2"),
          sprintf("Gerenciar Arquivos de Pessoal e Auxiliar — %s", empresa()),

          # X próprio no canto superior direito do título. Usa
          # Shiny.setInputValue() diretamente (em vez do fechamento
          # padrão do Bootstrap) para funcionar com qualquer versão do
          # Bootstrap usada pelo tema.
          tags$button(
            type = "button",
            class = "btn-close",
            style = "position:absolute; top:2px; right:0;",
            `aria-label` = "Fechar",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
              ns("fechar_modal_alertas")
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

          linha_cadastro(bases, RADICAL_BASE_PESSOAL, "Base de Dados - Pessoal e Auxiliar", "apagar_base"),

          linha_cadastro(envios, RADICAL_ENVIO_PESSOAL, "Envio - Pessoal e Auxiliar", "apagar_envio"),

          linha_arquivo(
            nome = if (length(alertas) > 0) {
              sprintf("%d arquivo(s) de alertas", length(alertas))
            } else {
              "Arquivos de alertas"
            },
            info = if (length(alertas) > 0) {
              tagList(
                sprintf("Alertas - Pessoal e Auxiliar — atualizado(s) em %s", data_atualizacao(alertas)),
                tags$ul(
                  class = "mb-0 ps-3",
                  lapply(alertas, function(a) {
                    tags$li(sprintf("%s — atualizado em %s", basename(a), data_atualizacao(a)))
                  })
                )
              )
            } else {
              "Alertas - Pessoal e Auxiliar — nenhum arquivo enviado"
            },
            id_apagar = if (length(alertas) > 0) "apagar_pasta"
          )
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
            "Selecione a base do Quadro de Pessoal e Auxiliar, o arquivo de envio e/ou um ou mais ",
            "arquivos de alertas gerados pelo MPM, em ", tags$b(".xlsx"), " ou ", tags$b(".csv"),
            " (limite de ", MAX_UPLOAD_MB, " MB no total). O tipo é identificado assim: ",
            "com colunas \"Alerta: ...\" o arquivo vai para os alertas; sem elas, se o nome do arquivo ",
            "tiver ", tags$b("envio"), ", é gravado como ", tags$code(RADICAL_ENVIO_PESSOAL),
            "; senão, como ", tags$code(RADICAL_BASE_PESSOAL),
            ". A base e o envio substituem o arquivo anterior do mesmo tipo."
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

        footer = actionButton(ns("fechar_modal_alertas"), "Fechar")

      ))

    }

    observeEvent(input$gerenciar_arquivos, abrir_gerenciar_arquivos())

    # =================================================
    # SEM ARQUIVO: JANELA DE AVISO E FILTROS BLOQUEADOS
    # -------------------------------------------------
    # Nas abas Base de Dados, Envio e Alertas, quando não há arquivo:
    #   - ao selecionar a aba (ou abrir o módulo nela), abre uma janela
    #     com o aviso e o botão "Gerenciar Arquivos";
    #   - os filtros (e os controles do gráfico) ficam bloqueados, até
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

    # Fecha o modal "Gerenciar Arquivos" — usado tanto pelo X do
    # cabeçalho quanto pelo botão "Fechar" do rodapé.
    observeEvent(input$fechar_modal_alertas, {
      removeModal()
      session$sendCustomMessage("limpar-modal-backdrop", list())
    })

    # ----------------------------------------
    # APAGAR (base ou alertas, com confirmação)
    # ----------------------------------------

    tipo_para_apagar <- reactiveVal(NULL)

    # Arquivos de cada tipo na pasta da empresa.
    existentes_do_tipo <- function(tipo) {
      switch(
        tipo,
        base = arquivos_base_pessoal(pasta_pessoal(empresa())),
        envio = arquivos_envio_pessoal(pasta_pessoal(empresa())),
        arquivos_alertas_existentes()
      )
    }

    pedir_confirmacao_apagar <- function(tipo) {
      req(empresa())

      existentes <- existentes_do_tipo(tipo)

      if (length(existentes) == 0) {
        showNotification("A pasta já está vazia.", type = "warning")
        return(invisible(NULL))
      }

      tipo_para_apagar(tipo)

      # Fecha o modal "Gerenciar Arquivos" ANTES de abrir a
      # confirmação — trocar um modal por outro na hora pode deixar o
      # backdrop do Bootstrap "grudado" na tela.
      removeModal()

      showModal(modalDialog(
        title = "Confirmar exclusão",
        if (tipo %in% c("base", "envio")) {
          sprintf(
            "Tem certeza que deseja apagar %s de Pessoal e Auxiliar (%s) da empresa %s? Esta ação não pode ser desfeita.",
            if (tipo == "base") "a base" else "o arquivo de envio",
            paste(basename(existentes), collapse = " e "), empresa()
          )
        } else {
          sprintf(
            "Tem certeza que deseja apagar os %d arquivo(s) de alertas da empresa %s? Esta ação não pode ser desfeita.",
            length(existentes), empresa()
          )
        },
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirmar_apagar_pasta"), "Apagar", class = "btn-danger")
        )
      ))
    }

    observeEvent(input$apagar_base, pedir_confirmacao_apagar("base"))
    observeEvent(input$apagar_envio, pedir_confirmacao_apagar("envio"))
    observeEvent(input$apagar_pasta, pedir_confirmacao_apagar("alertas"))

    observeEvent(input$confirmar_apagar_pasta, {
      removeModal()
      session$sendCustomMessage("limpar-modal-backdrop", list())

      req(empresa(), tipo_para_apagar())

      tipo <- tipo_para_apagar()
      tipo_para_apagar(NULL)

      inicio <- Sys.time()
      existentes <- existentes_do_tipo(tipo)

      if (length(existentes) == 0) {
        showNotification("A pasta já está vazia.", type = "warning")
        return()
      }

      removidos <- tryCatch(
        file.remove(existentes),
        error = function(e) {
          showNotification(sprintf("Erro ao apagar arquivos: %s", conditionMessage(e)), type = "error")
          NULL
        }
      )

      if (is.null(removidos)) {
        return()
      }

      if (all(removidos)) {
        showNotification(
          sprintf("%d arquivo(s) apagado(s) em %.2fs.", length(existentes), tempo_decorrido(inicio)),
          type = "message"
        )
      } else {
        showNotification(
          sprintf(
            "%d de %d arquivo(s) não puderam ser apagados (verifique se estão abertos em outro programa).",
            sum(!removidos), length(removidos)
          ),
          type = "error"
        )
      }

      tryCatch({
        recarregar()
        atualizar_todos_combos()
      }, error = function(e) {
        showNotification(
          sprintf("Arquivos apagados, mas houve erro ao recarregar as tabelas: %s", conditionMessage(e)),
          type = "error"
        )
      })
    })

    # ----------------------------------------
    # UPLOAD (base, envio e/ou alertas; ver identificar_tipo_arquivo_pessoal())
    # ----------------------------------------

    # Arquivos já validados, aguardando a confirmação.
    upload_pendente <- reactiveVal(NULL)

    # Grava os arquivos de `pendente`. Os novos são gravados ANTES de
    # qualquer arquivo antigo ser apagado (gravar_arquivo_substituindo()
    # em R/utils.R), então uma falha no meio não deixa a empresa sem
    # dados. `apagar_alertas_existentes`: remove, depois de gravar os
    # novos, os arquivos de alertas que já estavam na pasta.
    processar_upload <- function(pendente, apagar_alertas_existentes = FALSE) {
      req(pendente, empresa())

      inicio <- Sys.time()

      removeModal()
      session$sendCustomMessage("limpar-modal-backdrop", list())

      gravados <- character(0)
      falhas <- character(0)

      # ---- base e envio (nome fixo; substituem o anterior) ----
      for (tipo in c("base", "envio")) {
        arq <- pendente[pendente$tipo == tipo, , drop = FALSE]

        if (nrow(arq) == 1) {
          pasta <- pasta_pessoal(empresa())
          radical <- if (tipo == "base") RADICAL_BASE_PESSOAL else RADICAL_ENVIO_PESSOAL
          destino <- file.path(pasta, paste0(radical, ".", arq$extensao[1]))

          if (gravar_arquivo_substituindo(arq$datapath[1], destino, arquivos_base_pessoal(pasta, radical))) {
            gravados <- c(gravados, sprintf("%s → %s", arq$name[1], basename(destino)))
          } else {
            falhas <- c(falhas, arq$name[1])
          }
        }
      }

      # ---- alertas ----
      alertas <- pendente[pendente$tipo == "alertas", , drop = FALSE]

      if (nrow(alertas) > 0) {
        cam <- caminho_alertas(empresa())
        anteriores <- list.files(cam, full.names = TRUE)
        destinos <- file.path(cam, alertas$nome_seguro)

        ok <- vapply(
          seq_len(nrow(alertas)),
          function(i) gravar_arquivo_substituindo(alertas$datapath[i], destinos[i]),
          logical(1)
        )

        gravados <- c(gravados, alertas$nome_seguro[ok])
        falhas <- c(falhas, alertas$name[!ok])

        # Só apaga os antigos se TODOS os novos foram gravados.
        if (apagar_alertas_existentes && all(ok)) {
          antigos <- setdiff(
            normalizePath(anteriores, mustWork = FALSE),
            normalizePath(destinos, mustWork = FALSE)
          )
          if (length(antigos) > 0) {
            file.remove(antigos[file.exists(antigos)])
          }
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

      if (length(falhas) > 0) {
        showNotification(
          sprintf(
            "Não foi possível gravar: %s. Os arquivos que já estavam na pasta foram mantidos.",
            paste(falhas, collapse = ", ")
          ),
          type = "error",
          duration = 15
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

      # O nome vem do navegador e pode trazer "../" — sem saneamento,
      # um arquivo de alertas poderia ser gravado fora da pasta da
      # empresa (ver nome_arquivo_seguro(), em R/utils.R).
      nomes_seguros <- nome_arquivo_seguro(up$name, EXTENSOES_PLANILHA)

      tipos <- vapply(
        seq_len(nrow(up)),
        function(i) {
          if (is.na(nomes_seguros[i]) || !(extensoes[i] %in% EXTENSOES_PLANILHA)) {
            return(NA_character_)
          }
          identificar_tipo_arquivo_pessoal(
            ler_arquivo_dados(up$datapath[i], extensoes[i], up$name[i], n_max = 5),
            nome_arquivo = up$name[i]
          )
        },
        character(1)
      )

      invalidos <- basename(up$name[is.na(tipos)])

      if (length(invalidos) > 0) {
        showNotification(
          sprintf(
            "Não reconhecido(s) como base, envio ou alertas de Pessoal e Auxiliar em .xlsx ou .csv (ignorado[s]): %s.",
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

      if (sum(tipos[validos] == "base") > 1) {
        showNotification(
          "Foram selecionadas duas bases de Pessoal e Auxiliar. Envie no máximo uma base por vez.",
          type = "error",
          duration = 12
        )
        return(invisible(NULL))
      }

      if (sum(tipos[validos] == "envio") > 1) {
        showNotification(
          "Foram selecionados dois arquivos de envio de Pessoal e Auxiliar. Envie no máximo um por vez.",
          type = "error",
          duration = 12
        )
        return(invisible(NULL))
      }

      pendente <- data.frame(
        name = up$name[validos],
        nome_seguro = nomes_seguros[validos],
        datapath = up$datapath[validos],
        tipo = tipos[validos],
        extensao = extensoes[validos],
        stringsAsFactors = FALSE
      )

      upload_pendente(pendente)

      tem_base_nova <- any(pendente$tipo == "base")
      tem_envio_novo <- any(pendente$tipo == "envio")
      tem_alertas_novos <- any(pendente$tipo == "alertas")

      bases_existentes <- arquivos_base_pessoal(pasta_pessoal(empresa()))
      envios_existentes <- arquivos_envio_pessoal(pasta_pessoal(empresa()))
      alertas_existentes <- arquivos_alertas_existentes()

      substitui_base <- tem_base_nova && length(bases_existentes) > 0
      substitui_envio <- tem_envio_novo && length(envios_existentes) > 0
      soma_alertas <- tem_alertas_novos && length(alertas_existentes) > 0

      if (!substitui_base && !substitui_envio && !soma_alertas) {
        processar_upload(pendente)
        return(invisible(NULL))
      }

      # Fecha o modal "Gerenciar Arquivos" ANTES de abrir a confirmação
      # (mesma correção do Apagar). O valor do fileInput continua
      # acessível em upload_pendente().
      removeModal()

      showModal(modalDialog(
        title = "Arquivos existentes na pasta",
        tagList(
          if (substitui_base) {
            p(sprintf(
              "A base atual (%s) será substituída pela nova.",
              paste(basename(bases_existentes), collapse = ", ")
            ))
          },
          if (substitui_envio) {
            p(sprintf(
              "O arquivo de envio atual (%s) será substituído pelo novo.",
              paste(basename(envios_existentes), collapse = ", ")
            ))
          },
          if (soma_alertas) {
            p(sprintf(
              "A pasta de alertas da empresa %s já contém %d arquivo(s). Deseja apagar os existentes depois de enviar os novos, ou manter os dois conjuntos?",
              empresa(), length(alertas_existentes)
            ))
          }
        ),
        footer = tagList(
          modalButton("Cancelar"),
          if (soma_alertas) {
            tagList(
              actionButton(ns("manter_existentes"), "Manter Existentes"),
              actionButton(ns("apagar_existentes"), "Apagar e Enviar", class = "btn-danger")
            )
          } else {
            actionButton(ns("manter_existentes"), "Substituir", class = "btn-danger")
          }
        )
      ))
    })

    observeEvent(input$apagar_existentes, {
      processar_upload(upload_pendente(), apagar_alertas_existentes = TRUE)
    })

    observeEvent(input$manter_existentes, {
      processar_upload(upload_pendente(), apagar_alertas_existentes = FALSE)
    })

    # =================================================
    # ABA 3 — ALERTAS - PESSOAL E AUXILIAR
    # =================================================

    # ----------------------------------------
    # FILTROS
    # ----------------------------------------
    
    dados_filtrados <- reactive({
      req(dados())
      df <- dados()
      
      # Busca literal (um "(" ou "." digitado não vira expressão
      # regular), sem diferenciar maiúsculas/minúsculas nem acentos.
      if (!is.null(input$cpf) && str_trim(input$cpf) != "" && "CPF" %in% names(df)) {
        df <- df[contem_cpf(df$CPF, input$cpf), , drop = FALSE]
      }
      
      if (!is.null(input$nome) && str_trim(input$nome) != "" && "Nome" %in% names(df)) {
        df <- df[contem_texto(df$Nome, input$nome), , drop = FALSE]
      }
      
      if (!is.null(input$alerta) && input$alerta != "Todos" && input$alerta %in% names(df)) {
        df <- df %>%
          filter(!is.na(.data[[input$alerta]]) & str_trim(.data[[input$alerta]]) != "")
      }
      
      if (!is.null(input$conflito) && input$conflito != "Todos" && input$conflito %in% names(df)) {
        df <- df %>%
          filter(!is.na(.data[[input$conflito]]) & str_trim(.data[[input$conflito]]) != "")
      }
      
      df
    })
    
    # ----------------------------------------
    # TABELA (colunas fixas + Alerta(s) Detectado consolidado)
    # -----------------------------------------------------
    # Só afeta a apresentação: dados_filtrados() continua com as
    # colunas originais (Alerta: X / Conflito: X), usadas pelos
    # filtros Alerta/Conflito acima e pelo gráfico logo abaixo, sem
    # nenhuma alteração de regra.
    # ----------------------------------------
    
    # Também troca o código pela nomenclatura em "Situação Profissional
    # Atual" e "Cargo" (traduzir_codigos_tabela) — isso vale para a
    # tabela e para o CSV gerado a partir dela.
    tabela_exibicao <- reactive({
      dados_filtrados() %>%
        consolidar_alertas_tabela() %>%
        traduzir_codigos_tabela()
    })
    
    output$tabela <- renderDT({
      df <- tabela_exibicao()
      
      shiny::validate(need(nrow(df) > 0, mensagem_sem_linhas()))
      
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
    
    # ----------------------------------------
    # GRÁFICOS
    # -----------------------------------------------------
    # Os dois gráficos usam dados_filtrados() (mesmos filtros da
    # tabela) e só mudam a apresentação:
    #   1) Registros por tipo de Alerta/Conflito — echarts4r
    #      (colunas originais "Alerta: X" / "Conflito: X").
    #   2) Registros por Cargo — ggiraph (nomenclatura do cargo; ver
    #      traduzir_codigos_tabela()).
    # Cada resumo é um reactive() que devolve list(resumo, mensagem):
    # quando não há o que desenhar, `resumo` é NULL e `mensagem` traz o
    # aviso mostrado no lugar do gráfico (via shiny::validate()).
    # ----------------------------------------
    
    MSG_SEM_ARQUIVOS <- paste0(
      "Não existem arquivos de alertas para processamento. Utilize o botão ",
      "\"Gerenciar Arquivos\" para enviá-los."
    )

    # Distingue "não há arquivos de alertas" de "há dados, mas os filtros
    # não retornaram nada" (antes as duas situações mostravam o aviso de
    # "não existem arquivos").
    # Sem arquivos, devolve FALSE: a tabela e os gráficos ficam vazios
    # (shiny::validate() sem mensagem) e o aviso MSG_SEM_ARQUIVOS aparece
    # numa janela ao selecionar a aba (mostrar_alerta_sem_arquivo()).
    mensagem_sem_linhas <- function() {
      if (nrow(dados()) == 0) FALSE else "Nenhum registro encontrado para os filtros aplicados."
    }
    
    resumo_tipos <- reactive({
      df <- dados_filtrados()
      
      if (nrow(df) == 0) {
        return(list(resumo = NULL, mensagem = mensagem_sem_linhas()))
      }
      
      colunas_tipo <- c(
        colunas_por_prefixo(df, "Alerta"),
        colunas_por_prefixo(df, "Conflito")
      )
      
      if (length(colunas_tipo) == 0) {
        return(list(
          resumo = NULL,
          mensagem = "Nenhuma coluna de Alerta/Conflito encontrada nos arquivos carregados."
        ))
      }
      
      resumo <- map_dfr(colunas_tipo, function(col) {
        valores <- df[[col]]
        qtd <- sum(!is.na(valores) & str_trim(valores) != "")
        tibble(Tipo = col, Quantidade = qtd)
      }) %>%
        filter(Quantidade > 0) %>%
        arrange(Quantidade)   # crescente: com o eixo invertido, a maior fica no topo
      
      if (nrow(resumo) == 0) {
        return(list(
          resumo = NULL,
          mensagem = "Nenhum alerta/conflito encontrado nos dados atuais."
        ))
      }
      
      list(resumo = resumo, mensagem = NULL)
    })
    
    resumo_cargos <- reactive({
      df <- dados_filtrados()
      
      if (nrow(df) == 0) {
        return(list(resumo = NULL, mensagem = mensagem_sem_linhas()))
      }
      
      # Troca o código pela nomenclatura (CARGO_SERVIDOR).
      df <- traduzir_codigos_tabela(df)
      
      col_cargo <- which(normalizar_nome_coluna(names(df)) == "cargo")
      
      if (length(col_cargo) == 0) {
        return(list(
          resumo = NULL,
          mensagem = "Coluna \"Cargo\" não encontrada nos arquivos carregados."
        ))
      }
      
      valores <- str_trim(as.character(df[[col_cargo[1]]]))
      valores <- ifelse(is.na(valores) | valores == "", "(Não informado)", valores)
      
      resumo <- tibble(Cargo = valores) %>%
        count(Cargo, name = "Quantidade") %>%
        arrange(Quantidade)
      
      list(resumo = resumo, mensagem = NULL)
    })
    
    # ---- 1) Tipo de Alerta/Conflito (echarts4r) ----
    
    # A altura acompanha o número de barras, para os rótulos não se
    # sobreporem quando há muitos tipos de alerta.
    output$grafico_ui <- renderUI({
      r <- resumo_tipos()
      n <- if (is.null(r$resumo)) 0 else nrow(r$resumo)
      altura <- if (n == 0) 120 else max(320, 34 * n + 110)
      
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
        e_color("#2C7FB8") %>%
        e_title("Quantidade de Registros por Tipo de Alerta/Conflito") %>%
        e_tooltip(trigger = "item") %>%
        e_legend(show = FALSE) %>%
        e_grid(containLabel = TRUE, left = "2%", right = "8%") %>%
        e_toolbox_feature(feature = "saveAsImage")
    })
    
    # ---- 2) Registros por Cargo (ggiraph) ----
    
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
        geom_col_interactive(
          aes(tooltip = dica, data_id = Cargo),
          fill = "#2C7FB8"
        ) +
        geom_text(aes(label = Quantidade), hjust = -0.2) +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(
          title = "Quantidade de Registros por Cargo",
          x = "",
          y = "Quantidade"
        ) +
        theme_minimal()
      
      girafe(
        ggobj = p,
        width_svg = 9,
        height_svg = max(3, 0.45 * nrow(resumo) + 1.5),
        options = list(
          opts_hover(css = "fill:#0d6efd;cursor:pointer;"),
          opts_sizing(rescale = TRUE, width = 1)
        )
      )
    })
    
    # ==============================================
    # GERAR ARQUIVO CSV
    #
    # O botão só fica disponível quando a Tabela tem
    # dados a exibir (mesmos dados/filtros da aba
    # "Tabela"). Nome do arquivo: "alertas-" +
    # ano/mês/dia + hora/minuto da geração.
    # ==============================================
    
    output$csv_ui <- renderUI({
      df <- tabela_exibicao()
      ui_csv_dados(ns, "download_csv", !is.null(df) && nrow(df) > 0)
    })
    
    output$download_csv <- downloadHandler(
      
      filename = function() {
        paste0(
          "alertas-",
          format(Sys.time(), "%Y%m%d%H%M"),
          ".csv"
        )
      },
      
      content = function(file) {
        
        inicio <- Sys.time()
        
        df <- tabela_exibicao()
        
        # Além dos filtros da aplicação (CPF/Nome/Alerta/Conflito,
        # já aplicados em tabela_exibicao()), respeita também o que
        # está sendo exibido dentro do próprio objeto DT: o campo
        # "Search" (busca global) e os filtros de coluna
        # (filter = "top"). Como output$tabela é renderizada com
        # processamento no servidor (padrão do renderDT), o DT
        # expõe automaticamente input$tabela_rows_all — os índices
        # das linhas de tabela_exibicao() que sobrevivem a essa
        # busca/filtro, em todas as páginas.
        linhas_visiveis <- input$tabela_rows_all
        
        if (!is.null(linhas_visiveis)) {
          df <- df[linhas_visiveis, , drop = FALSE]
        }
        
        readr::write_excel_csv2(
          df,
          file,
          na = ""
        )
        
        showNotification(
          sprintf("Arquivo CSV gerado em %.2fs.", tempo_decorrido(inicio)),
          type = "message"
        )
        
      }
      
    )
    
    # ==============================================
    # ABA 4 — INCONSISTÊNCIAS - PESSOAL E AUXILIAR
    # -----------------------------------------------
    # Calculada sobre a BASE de Pessoal e Auxiliar (histórico completo,
    # com data de saída), quando ela tiver sido enviada; sem ela, sobre
    # os arquivos de alertas (registros_pessoal_empresa()). Não depende
    # dos filtros das outras abas. A tabela mostra Cargo e Situação por
    # extenso e o Órgão como "código - nome da serventia".
    # ==============================================

    usa_base <- reactive({
      nrow(dados_base()) > 0
    })

    registros_inc <- reactive({
      registros_pessoal_empresa(dados_base(), dados())
    })

    inconsistencias <- reactive({
      verificar_inconsistencias_servidores(registros_inc(), base_serventias())
    })

    inconsistencias_exibicao <- reactive({
      df <- traduzir_codigos_tabela(inconsistencias())
      if (nrow(df) > 0) {
        df$`Órgão de lotação` <- rotular_orgaos(df$`Órgão de lotação`, mapa_orgaos())
      }
      df
    })

    mod_inconsistencias_server(
      "inc",
      dados = inconsistencias_exibicao,
      verificacoes = reactive({
        sem_serventias <- nrow(base_serventias()) == 0
        lista <- list(
          list(
            descricao = paste0(
              "Aposentadoria, falecimento, exoneração ou demissão (situações 7 a 10) com data de saída ",
              "ou seguidas de nova situação ATIVA do mesmo CPF (após o falecimento, qualquer registro). ",
              "Alta: falecimento, ou aposentadoria com retorno/duração de até ",
              LIMITE_DIAS_SITUACAO_CURTA, " dias."
            )
          ),
          list(
            descricao = paste0(
              "Estagiário, terceirizado, juiz leigo, conciliador, aprendiz, voluntário ou residente em cargo ",
              "de chefia/comissão ou aposentado; comissionado sem vínculo sem cargo em comissão; cedido ou ",
              "comissionado como aposentado (Média)."
            )
          ),
          list(
            descricao = "Último registro do CPF, em situação não encerrada, lotado em serventia que não está ativa na base de Serventias.",
            indisponivel = sem_serventias
          ),
          list(
            descricao = "Data de início no futuro ou saída anterior ao início (Alta); data em formato não reconhecido (Média)."
          )
        )
        setNames(lista, VERIFICACOES_SERVIDORES)
      }),
      mensagem_sem_base = reactive({
        df <- registros_inc()
        if (nrow(df) == 0) {
          paste0(
            "Não existem arquivos para processamento. Envie a base de Pessoal e Auxiliar (ou os arquivos ",
            "de alertas) pelo botão \"Gerenciar Arquivos\"."
          )
        } else if (is.null(coluna_por_regex(df, REGEX_COL_CPF)) ||
                   is.null(coluna_por_regex(df, REGEX_COL_CARGO)) ||
                   is.null(coluna_por_regex(df, REGEX_COL_SITUACAO))) {
          "Os arquivos carregados não têm as colunas CPF, Cargo e Situação profissional atual, necessárias às verificações."
        }
      }),
      avisos = reactive({
        tagList(
          if (!usa_base() && nrow(dados()) > 0) {
            div(
              class = "alert alert-info py-2",
              style = "font-size: .85rem;",
              icon("circle-info", class = "me-1"),
              "As verificações estão usando os arquivos de alertas, porque a base de Pessoal e Auxiliar ",
              "não foi enviada. Com a base (histórico completo, com data de saída), elas ficam mais precisas."
            )
          },
          if (nrow(base_serventias()) == 0) {
            div(
              class = "alert alert-warning py-2",
              style = "font-size: .85rem;",
              icon("circle-info", class = "me-1"),
              "A base de Serventias não foi encontrada: a lotação em serventia inativa não pôde ser ",
              "verificada. Envie-a no módulo Serventias e clique em \"Atualizar Dados\"."
            )
          }
        )
      }),
      col_entidade = "CPF",
      rotulo_entidade = "servidor(es)",
      filtros_texto = FILTROS_TEXTO_INC_SERVIDORES,
      prefixo_csv = "inconsistencias-pessoal-auxiliar"
    )

  })
}
