# =====================================================
# modules/mod_alertas.R
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
# Cada empresa (distro) tem sua própria subpasta de arquivos de
# alertas:
#   <PASTA_ALERTAS>/<EMPRESA>/alertas_csv
# PASTA_ALERTAS (env var, default "data") define só o diretório raiz;
# a empresa é resolvida em tempo de execução a partir do usuário
# autenticado (ver parâmetro `empresa` de mod_alertas_server(),
# abaixo) — por isso o caminho NÃO é uma constante fixa calculada uma
# única vez ao carregar o módulo (isso seria compartilhado entre TODAS
# as sessões do app, já que rodam no mesmo processo R).
# =====================================================

PASTA_ALERTAS_BASE <- Sys.getenv("PASTA_ALERTAS", unset = "data")

# Monta (e garante que existe) a pasta de alertas da empresa
# informada. Retorna NULL se `empresa` não estiver definida (ex.:
# sessão ainda não autenticada).
caminho_alertas <- function(empresa) {
  
  if (is.null(empresa) || is.na(empresa) || trimws(empresa) == "") {
    return(NULL)
  }
  
  caminho <- file.path(PASTA_ALERTAS_BASE, trimws(empresa), "alertas_csv")
  
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
# LEITURA DE UM ARQUIVO
# -----------------------------------------------------
# Tenta primeiro como CSV separado por ";" (padrão de exportação
# brasileiro/Excel); se resultar em uma única coluna (sinal de que o
# delimitador está errado), tenta de novo com detecção automática do
# delimitador.
#
# Fica dentro de um tryCatch: em vez de travar o carregamento inteiro
# por causa de um arquivo ilegível (corrompido, formato inesperado),
# avisa qual arquivo deu problema e pula só ele — os demais continuam
# sendo processados normalmente.
# =====================================================

ler_arquivo_alertas <- function(arquivo) {
  
  resultado <- tryCatch({
    
    df <- read_csv2(
      arquivo,
      show_col_types = FALSE,
      locale = locale(
        encoding = "UTF-8",
        decimal_mark = ",",
        grouping_mark = "."
      ),
      col_types = cols(.default = col_character())
    )
    
    if (ncol(df) <= 1) {
      
      df <- read_delim(
        arquivo,
        delim = NULL,
        show_col_types = FALSE,
        locale = locale(encoding = "UTF-8"),
        col_types = cols(.default = col_character())
      )
      
    }
    
    df
    
  }, error = function(e) {
    
    msg <- paste0(
      "Não foi possível ler o arquivo ", basename(arquivo), ": ",
      conditionMessage(e), ". Esse arquivo foi ignorado."
    )
    
    warning(msg)
    showNotification(msg, type = "warning", duration = 15)
    
    NULL
    
  })
  
  resultado
  
}

# =====================================================
# CARREGAMENTO
# -----------------------------------------------------
# Consolida todos os CSVs da pasta de alertas da empresa. Arquivos com
# colunas diferentes entre si (ex.: um trouxe uma coluna "Alerta X" que
# o outro não tem) são combinados sem problema — bind_rows() preenche
# com NA o que faltar em cada um.
# =====================================================

carregar_alertas <- function(caminho) {
  
  if (is.null(caminho)) {
    return(data.frame())
  }
  
  arquivos <- list.files(
    caminho,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  if (length(arquivos) == 0) {
    warning("Nenhum arquivo encontrado em: ", caminho)
    return(data.frame())
  }
  
  resultados <- map(arquivos, ler_arquivo_alertas)
  
  # Remove arquivos ignorados por ler_arquivo_alertas() (erro de
  # leitura — ver tryCatch lá dentro).
  resultados <- Filter(Negate(is.null), resultados)
  
  if (length(resultados) == 0) {
    return(data.frame())
  }
  
  bind_rows(resultados) %>%
    distinct()
  
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
# CÓDIGO -> NOMENCLATURA (Situação Profissional Atual e Cargo)
# -----------------------------------------------------
# No arquivo do Quadro de Pessoal e Auxiliar (servidores) do CNJ/MPM,
# as colunas "Situação Profissional Atual" e "Cargo" vêm como código
# numérico. A correspondência código -> nomenclatura fica em duas
# tabelas do banco SQLite da aplicação (o mesmo faroljus_lite.db do
# controle de acesso, conexão `con` do app.R):
#
#   - tab_situacao_profissional_servidor (codigo, nomenclatura)
#   - tab_cargo_servidor                 (codigo, nomenclatura)
#
# Os valores seguem o Manual de Preenchimento do MPM (Quadro de Pessoal
# e Auxiliar). As constantes SEMENTE_* logo abaixo são só a carga
# inicial: garantir_tabelas_auxiliares() cria as tabelas se não
# existirem e insere os códigos que faltarem (INSERT OR IGNORE — nunca
# sobrescreve uma nomenclatura já existente, então ajustes feitos
# direto no banco são preservados). Na hora de exibir, a leitura é
# sempre feita no banco.
#
# É só para exibição: a tabela e o CSV gerado a partir dela mostram a
# nomenclatura, enquanto dados(), dados_filtrados() e o gráfico
# continuam com os valores originais.
#
# Obs.: na Situação Profissional Atual não existem os códigos 4 e 5
# (a numeração pula de 3 para 6) — é assim no manual.
# =====================================================

TAB_SITUACAO_PROFISSIONAL <- "tab_situacao_profissional_servidor"
TAB_CARGO <- "tab_cargo_servidor"

SEMENTE_SITUACAO_PROFISSIONAL_SERVIDOR <- c(
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

SEMENTE_CARGO_SERVIDOR <- c(
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

# Cria (se preciso) as tabelas auxiliares no SQLite e insere os códigos
# que ainda não existirem. Idempotente: pode rodar a cada início da
# aplicação/sessão. Um banco novo/vazio (ex.: primeiro deploy no
# shinyapps.io) ganha as tabelas já preenchidas.
garantir_tabelas_auxiliares <- function(con) {
  
  criar_e_semear <- function(tabela, semente) {
    
    DBI::dbExecute(
      con,
      sprintf(
        paste0(
          "CREATE TABLE IF NOT EXISTS %s (",
          "codigo INTEGER PRIMARY KEY, ",
          "nomenclatura TEXT NOT NULL)"
        ),
        tabela
      )
    )
    
    for (i in seq_along(semente)) {
      DBI::dbExecute(
        con,
        sprintf(
          "INSERT OR IGNORE INTO %s (codigo, nomenclatura) VALUES (?, ?)",
          tabela
        ),
        params = list(as.integer(names(semente)[i]), unname(semente[i]))
      )
    }
    
  }
  
  criar_e_semear(TAB_SITUACAO_PROFISSIONAL, SEMENTE_SITUACAO_PROFISSIONAL_SERVIDOR)
  criar_e_semear(TAB_CARGO, SEMENTE_CARGO_SERVIDOR)
  
  invisible(TRUE)
  
}

# Lê uma tabela auxiliar do SQLite e devolve um vetor nomeado
# (nome = código como texto, valor = nomenclatura), no formato esperado
# por codigo_para_nomenclatura(). Se a conexão não existir ou a leitura
# falhar, devolve um vetor vazio — a tabela do app continua funcionando,
# só mostrando os códigos como vieram no arquivo.
ler_tabela_auxiliar <- function(con, tabela) {
  
  if (is.null(con)) {
    return(character(0))
  }
  
  tryCatch({
    
    ref <- DBI::dbGetQuery(
      con,
      sprintf("SELECT codigo, nomenclatura FROM %s", tabela)
    )
    
    setNames(as.character(ref$nomenclatura), as.character(ref$codigo))
    
  }, error = function(e) {
    
    warning("Não foi possível ler a tabela ", tabela, ": ", conditionMessage(e))
    character(0)
    
  })
  
}

# Troca, em um vetor de valores, o código pela nomenclatura da tabela
# informada. Aceita o código "puro" com ou sem zero à esquerda e com
# espaços ("6", "06", " 6 ", "6.0"). O que não for um código conhecido
# (valor vazio, já por extenso ou código fora da tabela) é mantido como
# veio, para não esconder dado inesperado.
codigo_para_nomenclatura <- function(valores, tabela) {
  
  valores_chr <- as.character(valores)
  
  codigos <- str_match(
    valores_chr,
    "^\\s*0*(\\d+)(?:[.,]0+)?\\s*$"
  )[, 2]
  
  nomes <- unname(tabela[codigos])
  
  ifelse(is.na(nomes), valores_chr, nomes)
  
}

# Normaliza o nome de uma coluna para comparação (sem acento, minúsculo,
# sem espaços nas pontas) — tolera pequenas variações de grafia/acentuação
# no cabeçalho do CSV.
normalizar_nome_coluna <- function(x) {
  str_to_lower(str_trim(stringi::stri_trans_general(x, "Latin-ASCII")))
}

# Aplica a conversão código -> nomenclatura nas colunas "Situação
# Profissional Atual" e "Cargo" do data.frame (se existirem), lendo as
# tabelas auxiliares do SQLite (`con`). Só para exibição na tabela/CSV.
# Sem `con` (ou sem as tabelas), os códigos são mantidos como vieram.
traduzir_codigos_tabela <- function(df, con = NULL) {
  
  if (is.null(df) || nrow(df) == 0 || is.null(con)) {
    return(df)
  }
  
  situacoes <- ler_tabela_auxiliar(con, TAB_SITUACAO_PROFISSIONAL)
  cargos <- ler_tabela_auxiliar(con, TAB_CARGO)
  
  nomes_norm <- normalizar_nome_coluna(names(df))
  
  col_situacao <- which(str_starts(nomes_norm, "situacao profissional"))
  col_cargo <- which(nomes_norm == "cargo")
  
  for (i in col_situacao) {
    df[[i]] <- codigo_para_nomenclatura(df[[i]], situacoes)
  }
  
  for (i in col_cargo) {
    df[[i]] <- codigo_para_nomenclatura(df[[i]], cargos)
  }
  
  df
  
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
  
  matriz <- as.matrix(df[colunas])
  
  apply(matriz, 1, function(linha) {
    
    preenchidos <- !is.na(linha) & str_trim(linha) != ""
    
    if (!any(preenchidos)) {
      return(NA_character_)
    }
    
    sufixos <- str_trim(str_remove(colunas[preenchidos], "^[^:]*:"))
    valores <- str_trim(linha[preenchidos])
    
    paste0(sufixos, ": ", valores, collapse = " | ")
    
  })
  
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

        /* Garante que a tabela ocupe 100%% da largura disponível —
           autoWidth do DT, combinado com scrollX, às vezes não
           estica sozinho até a borda do container. */
        #%1$s .dataTables_wrapper,
        #%1$s table.dataTable {
          width: 100%% !important;
        }

      ", id)))
    ),
    
    div(
      
      id = id,
      
      # =================================================
      # CABEÇALHO - TÍTULO + AÇÕES (mesma linha)
      # =================================================
      
      div(
        class = "al-header",
        
        div(
          class = "al-titulo",
          div(class = "al-titulo-icone", icon("triangle-exclamation")),
          tags$h4("Alertas — Quadro Pessoal e Auxiliar")
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
      
      # =================================================
      # FILTROS - EM LINHA, LOGO ABAIXO DO CABEÇALHO
      # -------------------------------------------------
      # "Alerta" e "Conflito" se excluem mutuamente (ver
      # observeEvent(input$alerta)/observeEvent(input$conflito) no
      # server) — escolher um dos dois volta o outro para "Todos".
      # =================================================
      
      div(
        class = "al-card",
        
        div(class = "al-card-titulo", "Filtros"),
        
        fluidRow(
          column(3, selectInput(ns("alerta"), "Alerta", choices = c("Todos"))),
          column(3, selectInput(ns("conflito"), "Conflito", choices = c("Todos"))),
          column(3, textInput(ns("cpf"), "CPF")),
          column(3, textInput(ns("nome"), "Nome"))
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
      
      # =================================================
      # CONTEÚDO - TABELA / GRÁFICO / CSV
      # =================================================
      
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
      
    )
    
  )
  
}

# =====================================================
# SERVER
# =====================================================

mod_alertas_server <- function(id, ativo = reactive(TRUE), empresa = reactive(NULL), con = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Garante as tabelas auxiliares (Situação Profissional / Cargo) no
    # SQLite do app — a mesma conexão `con` usada no controle de acesso
    # (usuarios_totp). Idempotente. Um erro aqui não pode derrubar o
    # módulo: sem as tabelas, a exibição só mostra os códigos originais.
    if (!is.null(con)) {
      tryCatch(
        garantir_tabelas_auxiliares(con),
        error = function(e) {
          warning("Não foi possível preparar as tabelas auxiliares: ", conditionMessage(e))
        }
      )
    }
    
    # Começa vazio: a empresa (distro) só é conhecida depois do login,
    # então o carregamento real acontece no observeEvent(empresa())
    # logo abaixo — nunca aqui na inicialização do módulo.
    dados <- reactiveVal(data.frame())
    
    # ----------------------------------------
    # CONTADOR DO CAMPO DE UPLOAD (modal "Gerenciar Arquivos")
    # -----------------------------------------------------
    # O fileInput() do modal usa um ID novo (ns("upload_arquivos_N"))
    # toda vez que o modal é aberto, em vez de um ID fixo. Sem isso, o
    # Shiny não garante que a seleção de arquivos do input$ fique vazia
    # ao reabrir o modal — o valor antigo pode continuar "vivo" no
    # servidor mesmo com o campo aparentando estar limpo na tela,
    # fazendo "Enviar para Pasta" reenviar os arquivos de uma seleção
    # anterior mesmo sem nada escolhido dessa vez. Um ID novo a cada
    # abertura elimina esse problema de vez.
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
    # -----------------------------------------
    # Dispara na primeira empresa disponível (login) e sempre que ela
    # mudar (ex.: logout seguido de novo login de outra empresa, na
    # mesma sessão do navegador — este módulo não é recriado a cada
    # login).
    # ----------------------------------------
    
    observeEvent(empresa(), {
      req(empresa())
      
      dados(
        carregar_alertas(
          caminho_alertas(empresa())
        )
      )
    }, ignoreInit = FALSE)
    
    # ----------------------------------------
    # ATUALIZA OS COMBOS (Alerta / Conflito)
    # — reage a dados() E a ativo(), preservando a seleção atual quando
    # ainda fizer sentido (mesmo padrão dos outros módulos).
    # ----------------------------------------
    
    atualizar_combos <- function() {
      
      df <- dados()
      
      alerta_atual <- input$alerta
      conflito_atual <- input$conflito
      
      colunas_alerta <- colunas_com_dados(df, colunas_por_prefixo(df, "Alerta"))
      colunas_conflito <- colunas_com_dados(df, colunas_por_prefixo(df, "Conflito"))
      
      alerta_selecionado <- if (!is.null(alerta_atual) && alerta_atual %in% colunas_alerta) {
        alerta_atual
      } else {
        "Todos"
      }
      
      conflito_selecionado <- if (!is.null(conflito_atual) && conflito_atual %in% colunas_conflito) {
        conflito_atual
      } else {
        "Todos"
      }
      
      updateSelectInput(
        session,
        "alerta",
        choices = c("Todos", colunas_alerta),
        selected = alerta_selecionado
      )
      
      updateSelectInput(
        session,
        "conflito",
        choices = c("Todos", colunas_conflito),
        selected = conflito_selecionado
      )
      
    }
    
    observeEvent(list(dados(), ativo()), {
      req(ativo())
      atualizar_combos()
    }, ignoreInit = FALSE)
    
    observeEvent(input$atualizar, {
      req(empresa())
      inicio <- Sys.time()
      
      dados(carregar_alertas(caminho_alertas(empresa())))
      atualizar_combos()
      
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
    
    # ----------------------------------------
    # MODAL "GERENCIAR ARQUIVOS"
    #
    # Reúne, em uma única janela, as opções
    # "Apagar Arquivos da Pasta" e "Enviar para Pasta",
    # acessadas pelo botão ao lado de "Atualizar Dados".
    # ----------------------------------------
    
    observeEvent(input$gerenciar_arquivos, {
      
      req(empresa())
      
      cam <- caminho_alertas(empresa())
      
      total_arquivos <- if (!is.null(cam) && dir.exists(cam)) {
        length(list.files(cam))
      } else {
        0
      }
      
      # ID novo do fileInput a cada abertura — ver comentário em
      # contador_upload(), acima.
      contador_upload(contador_upload() + 1)
      
      showModal(modalDialog(
        title = div(
          style = "position:relative; padding-right:28px;",
          icon("folder-open", class = "me-2"),
          sprintf("Gerenciar Arquivos de Alertas — %s", empresa()),
          
          # X próprio no canto superior direito do título. Usa
          # Shiny.setInputValue() diretamente (em vez do fechamento
          # padrão do Bootstrap via data-dismiss/data-bs-dismiss) para
          # garantir que funcione independentemente da versão do
          # Bootstrap usada pelo tema — o mesmo observeEvent de baixo
          # cuida do fechamento, junto com o botão "Fechar" do rodapé.
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
            icon("trash", class = "text-danger"),
            "Apagar arquivos da pasta"
          ),
          
          div(
            class = "al-modal-secao-desc",
            if (total_arquivos > 0) {
              sprintf("A pasta contém atualmente %d arquivo(s).", total_arquivos)
            } else {
              "A pasta de alertas está vazia."
            }
          ),
          
          actionButton(
            ns("apagar_pasta"),
            tagList(icon("trash", class = "me-2"), "Apagar Arquivos da Pasta"),
            class = "btn btn-danger w-100"
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
            "Selecione um ou mais arquivos CSV de alertas (quadro ",
            "pessoal e auxiliar) — cabem vários de uma vez (limite de ",
            MAX_UPLOAD_MB, " MB no total por envio)."
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
            accept = c(".csv", "text/csv"),
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
      
    })
    
    # Fecha o modal "Gerenciar Arquivos" — usado tanto pelo X do
    # cabeçalho quanto pelo botão "Fechar" do rodapé (ver acima). Não é
    # preciso limpar o fileInput aqui: o próximo input$gerenciar_arquivos
    # já gera um ID novo para ele (ver contador_upload(), no início do
    # server), então a seleção anterior nunca reaparece.
    observeEvent(input$fechar_modal_alertas, {
      removeModal()
      session$sendCustomMessage("limpar-modal-backdrop", list())
    })
    
    # ----------------------------------------
    # APAGAR ARQUIVOS DA PASTA (BOTÃO INDEPENDENTE)
    # ----------------------------------------
    
    apagar_pasta_alertas <- function() {
      inicio <- Sys.time()
      
      cam <- if (!is.null(empresa())) caminho_alertas(empresa()) else NULL
      
      if (is.null(cam) || !dir.exists(cam)) {
        showNotification(
          "Não foi possível determinar a pasta de alertas da empresa atual.",
          type = "error"
        )
        return(invisible(NULL))
      }
      
      arquivos_atuais <- list.files(cam, full.names = TRUE)
      
      if (length(arquivos_atuais) == 0) {
        showNotification("A pasta já está vazia.", type = "warning")
        return(invisible(NULL))
      }
      
      resultado <- tryCatch({
        removidos <- file.remove(arquivos_atuais)
        list(ok = TRUE, removidos = removidos)
      }, error = function(e) {
        list(ok = FALSE, erro = conditionMessage(e))
      })
      
      if (!resultado$ok) {
        showNotification(
          sprintf("Erro ao apagar arquivos: %s", resultado$erro),
          type = "error"
        )
        return(invisible(NULL))
      }
      
      removidos <- resultado$removidos
      if (all(removidos)) {
        showNotification(
          sprintf(
            "%d arquivo(s) apagado(s) da pasta em %.2fs.",
            length(arquivos_atuais),
            tempo_decorrido(inicio)
          ),
          type = "message"
        )
      } else {
        showNotification(
          sprintf(
            "%d de %d arquivo(s) não puderam ser apagados (verifique se estão abertos em outro programa).",
            sum(!removidos),
            length(removidos)
          ),
          type = "error"
        )
      }
      
      tryCatch({
        dados(carregar_alertas(cam))
        atualizar_combos()
      }, error = function(e) {
        showNotification(
          sprintf("Arquivos apagados, mas houve erro ao recarregar a tabela: %s", conditionMessage(e)),
          type = "error"
        )
      })
    }
    
    observeEvent(input$apagar_pasta, {
      req(empresa())
      
      cam <- caminho_alertas(empresa())
      
      if (is.null(cam) || !dir.exists(cam)) {
        showNotification(
          "Não foi possível determinar a pasta de alertas da empresa atual.",
          type = "error"
        )
        return()
      }
      
      arquivos_existentes <- list.files(cam)
      
      if (length(arquivos_existentes) == 0) {
        showNotification("A pasta já está vazia.", type = "warning")
        return()
      }
      
      # Fecha o modal "Gerenciar Arquivos" (de onde este botão foi
      # clicado) ANTES de abrir o de confirmação, em vez de deixar o
      # showModal() abaixo substituir um modal ainda aberto na hora —
      # essa troca instantânea de modal por modal é a causa mais
      # provável de a tela travar (backdrop do Bootstrap ficando
      # "grudado") especificamente nessa ação.
      removeModal()
      
      showModal(modalDialog(
        title = "Confirmar exclusão",
        sprintf(
          "Tem certeza que deseja apagar os %d arquivo(s) da pasta de alertas da empresa %s? Esta ação não pode ser desfeita.",
          length(arquivos_existentes),
          empresa()
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirmar_apagar_pasta"), "Apagar", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirmar_apagar_pasta, {
      removeModal()
      session$sendCustomMessage("limpar-modal-backdrop", list())
      apagar_pasta_alertas()
    })
    
    # ----------------------------------------
    # UPLOAD DE ARQUIVOS PARA A PASTA DA EMPRESA ATUAL
    # ----------------------------------------
    
    # Copia os arquivos enviados para a pasta de alertas da empresa
    # atual (caminho_alertas(empresa())), apagando os existentes antes
    # se `apagar_existentes = TRUE`.
    #
    # Fecha o modal "Gerenciar Arquivos" logo depois de copiar os
    # arquivos (rápido) — ANTES de recarregar a tabela, que pode
    # demorar com muitos arquivos. Fazer isso na ordem inversa deixava
    # o modal parado na tela, sem retorno visual, dando a impressão de
    # que a aplicação tinha travado. O recarregamento fica dentro de
    # um tryCatch: um erro inesperado ali não pode travar esta
    # observeEvent nem a sessão.
    processar_upload <- function(arquivos_upload, apagar_existentes = FALSE) {
      req(arquivos_upload)
      req(empresa())
      
      cam <- caminho_alertas(empresa())
      req(cam)
      
      inicio <- Sys.time()
      
      if (apagar_existentes) {
        arquivos_atuais <- list.files(cam, full.names = TRUE)
        if (length(arquivos_atuais) > 0) {
          file.remove(arquivos_atuais)
        }
      }
      
      destinos <- file.path(cam, arquivos_upload$name)
      copiados <- file.copy(arquivos_upload$datapath, destinos, overwrite = TRUE)
      
      if (all(copiados)) {
        showNotification(
          sprintf(
            "%d arquivo(s) enviado(s) com sucesso em %.2fs. Atualizando a tabela...",
            nrow(arquivos_upload),
            tempo_decorrido(inicio)
          ),
          type = "message"
        )
      } else {
        showNotification(
          sprintf(
            "%d de %d arquivo(s) não puderam ser copiados.",
            sum(!copiados),
            length(copiados)
          ),
          type = "error"
        )
      }
      
      removeModal()
      session$sendCustomMessage("limpar-modal-backdrop", list())
      
      resultado <- tryCatch(
        carregar_alertas(cam),
        error = function(e) {
          showNotification(
            sprintf("Arquivos enviados, mas houve erro ao recarregar a tabela: %s", conditionMessage(e)),
            type = "error",
            duration = 15
          )
          NULL
        }
      )
      
      if (!is.null(resultado)) {
        dados(resultado)
        atualizar_combos()
      }
    }
    
    observeEvent(input$enviar_arquivos, {
      req(entrada_upload_atual())
      req(empresa())
      
      cam <- caminho_alertas(empresa())
      req(cam)
      
      arquivos_existentes <- list.files(cam)
      
      if (length(arquivos_existentes) > 0) {
        
        # Fecha o modal "Gerenciar Arquivos" ANTES de abrir a
        # confirmação, em vez de deixar o showModal() abaixo
        # substituir um modal ainda aberto na hora — mesma correção de
        # observeEvent(input$apagar_pasta). O valor já selecionado no
        # fileInput (entrada_upload_atual()) continua acessível depois
        # disso: o Shiny mantém o último valor recebido de um input
        # mesmo com o elemento fora da tela.
        removeModal()
        
        showModal(modalDialog(
          title = "Arquivos existentes na pasta",
          sprintf(
            "A pasta de alertas da empresa %s já contém %d arquivo(s). Deseja apagar os arquivos existentes antes de enviar os novos, ou manter os dois conjuntos?",
            empresa(),
            length(arquivos_existentes)
          ),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("manter_existentes"), "Manter Existentes"),
            actionButton(ns("apagar_existentes"), "Apagar e Enviar", class = "btn-danger")
          )
        ))
      } else {
        processar_upload(entrada_upload_atual(), apagar_existentes = FALSE)
      }
    })
    
    # Não chama removeModal() aqui: processar_upload() já fecha o modal
    # "Gerenciar Arquivos" ao final (ver comentário na definição da
    # função, acima). Fechar aqui TAMBÉM causava duas chamadas de
    # removeModal() em sequência rápida, o que podia deixar o backdrop
    # do Bootstrap "grudado" na tela, bloqueando cliques.
    observeEvent(input$apagar_existentes, {
      processar_upload(entrada_upload_atual(), apagar_existentes = TRUE)
    })
    
    observeEvent(input$manter_existentes, {
      processar_upload(entrada_upload_atual(), apagar_existentes = FALSE)
    })
    
    # ----------------------------------------
    # FILTROS
    # ----------------------------------------
    
    dados_filtrados <- reactive({
      req(dados())
      df <- dados()
      
      if (!is.null(input$cpf) && input$cpf != "" && "CPF" %in% names(df)) {
        df <- df %>%
          filter(str_detect(
            str_to_upper(as.character(CPF)),
            str_to_upper(input$cpf)
          ))
      }
      
      if (!is.null(input$nome) && input$nome != "" && "Nome" %in% names(df)) {
        df <- df %>%
          filter(str_detect(str_to_upper(Nome), str_to_upper(input$nome)))
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
        traduzir_codigos_tabela(con)
    })
    
    output$tabela <- renderDT({
      df <- tabela_exibicao()
      
      shiny::validate(
        need(
          nrow(df) > 0,
          "Não existem arquivos para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviar os arquivos de alertas."
        )
      )
      
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
    #   2) Registros por Cargo — ggiraph (nomenclatura do cargo,
    #      lida das tabelas auxiliares do SQLite; ver
    #      traduzir_codigos_tabela()).
    # Cada resumo é um reactive() que devolve list(resumo, mensagem):
    # quando não há o que desenhar, `resumo` é NULL e `mensagem` traz o
    # aviso mostrado no lugar do gráfico (via shiny::validate()).
    # ----------------------------------------
    
    MSG_SEM_ARQUIVOS <- paste0(
      "Não existem arquivos para processamento. Utilize o botão ",
      "\"Gerenciar Arquivos\" para enviar os arquivos de alertas."
    )
    
    resumo_tipos <- reactive({
      df <- dados_filtrados()
      
      if (nrow(df) == 0) {
        return(list(resumo = NULL, mensagem = MSG_SEM_ARQUIVOS))
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
        return(list(resumo = NULL, mensagem = MSG_SEM_ARQUIVOS))
      }
      
      # Troca o código pela nomenclatura (tabelas auxiliares no SQLite).
      df <- traduzir_codigos_tabela(df, con)
      
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
      
      tem_dados <- !is.null(df) && nrow(df) > 0
      
      div(
        class = "mt-4",
        style = "max-width: 420px;",
        
        p(
          class = "text-muted",
          "Gera um arquivo .csv com os dados exibidos na aba \"Tabela\" (respeitando os filtros aplicados e a busca da tabela)."
        ),
        
        if (tem_dados) {
          
          downloadButton(
            ns("download_csv"),
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
    
  })
}