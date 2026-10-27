# =====================================================
# modules/mod_matricula.R
# =====================================================

library(shiny)
library(DT)
library(DBI)

# =====================================================
# UI
# =====================================================

mod_matricula_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        12,
        h3("Atribuição de Matrícula por Nome"),
        p("Faça o upload do arquivo CSV contendo a coluna 'Nome' para consultar e acrescentar a Matrícula."),
        
        fileInput(
          ns("arquivo_csv"),
          "Selecione o arquivo CSV",
          accept = c(".csv", "text/csv", "text/comma-separated-values")
        ),
        
        actionButton(
          ns("processar"),
          "Processar Arquivo",
          class = "btn btn-primary",
          icon = icon("play")
        ),
        
        downloadButton(
          ns("download_csv"),
          "Baixar CSV Processado",
          class = "btn btn-success ms-2"
        )
      )
    ),
    
    br(),
    
    fluidRow(
      column(
        12,
        verbatimTextOutput(ns("info"))
      )
    ),
    
    fluidRow(
      column(
        12,
        DTOutput(ns("resultado"))
      )
    )
  )
}

# =====================================================
# SERVER
# =====================================================

# Quantos nomes vão em cada consulta "WHERE Nome IN (?, ?, ...)". Um
# lote grande demais esbarra no limite de parâmetros do driver; um lote
# pequeno demais volta a fazer muitas idas ao banco.
MATRICULA_TAMANHO_LOTE <- 500L

# Busca a matrícula de cada nome no IRIS da empresa informada.
#
# Antes: uma consulta POR LINHA do CSV (milhares de idas ao IRIS), com o
# nome concatenado na SQL ("... AND Nome = '<nome>'") — injeção de SQL
# protegida só pela troca de aspas — e conectar_banco() sem a empresa
# (que sempre falhava). Agora: nomes distintos, consultas em lotes com
# parâmetros (?) e UMA conexão para tudo, fechada ao final mesmo em caso
# de erro.
buscar_matriculas_iris <- function(distro, nomes, ao_avancar = NULL) {
  
  # Chave de comparação: sem espaços nas pontas e em maiúsculas — no
  # IRIS, campos texto usam por padrão a collation SQLUPPER, então o banco
  # casa "Maria" com "MARIA" e devolve o nome como está gravado.
  chave <- function(x) toupper(trimws(as.character(x)))
  
  nomes <- trimws(as.character(nomes))
  distintos <- unique(nomes[!is.na(nomes) & nomes != ""])
  
  mapa <- setNames(rep(NA_character_, length(unique(chave(distintos)))), unique(chave(distintos)))
  
  if (length(distintos) == 0) {
    return(rep(NA_character_, length(nomes)))
  }
  
  con_iris <- conectar_banco(distro)
  on.exit(try(DBI::dbDisconnect(con_iris), silent = TRUE), add = TRUE)
  
  lotes <- split(distintos, ceiling(seq_along(distintos) / MATRICULA_TAMANHO_LOTE))
  
  for (i in seq_along(lotes)) {
    
    lote <- lotes[[i]]
    
    sql <- paste0(
      "SELECT Nome, Matricula FROM RHCadServidor ",
      "WHERE Financeiro->CalculaFolha = 1 AND Nome IN (",
      paste(rep("?", length(lote)), collapse = ", "),
      ")"
    )
    
    res <- do.call(DBI::dbGetQuery, c(list(con_iris, sql), as.list(lote)))
    
    if (nrow(res) > 0) {
      res <- res[!is.na(res$Matricula), , drop = FALSE]
      # Mesmo critério de antes: a primeira matrícula encontrada.
      res <- res[!duplicated(chave(res$Nome)), , drop = FALSE]
      encontrados <- chave(res$Nome) %in% names(mapa)
      mapa[chave(res$Nome)[encontrados]] <- as.character(res$Matricula[encontrados])
    }
    
    if (is.function(ao_avancar)) {
      ao_avancar(i, length(lotes))
    }
    
  }
  
  unname(mapa[chave(nomes)])
  
}

mod_matricula_server <- function(id, empresa = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      
      dados_processados <- reactiveVal(NULL)
      
      # ---------------------------------------------
      # PROCESSAMENTO DO CSV E CONSULTA SQL
      # ---------------------------------------------
      observeEvent(input$processar, {
        req(input$arquivo_csv)
        
        distro <- empresa()
        
        if (is.null(distro) || is.na(distro) || trimws(distro) == "") {
          showNotification(
            "Nenhuma empresa definida para esta sessão. Faça login novamente.",
            type = "error"
          )
          return()
        }
        
        tryCatch({
          # 1. Ler arquivo CSV enviado
          dados <- read.csv(
            input$arquivo_csv$datapath,
            stringsAsFactors = FALSE,
            encoding = "UTF-8"
          )
          
          # Tratamento caso use ponto e vírgula como separador
          if (ncol(dados) == 1 && grepl(";", colnames(dados)[1])) {
            dados <- read.csv2(
              input$arquivo_csv$datapath,
              stringsAsFactors = FALSE,
              encoding = "UTF-8"
            )
          }
          
          # Validação da coluna Nome
          if (!"Nome" %in% colnames(dados)) {
            showNotification(
              "O arquivo CSV deve conter uma coluna com o cabeçalho 'Nome'.",
              type = "error"
            )
            return()
          }
          
          # 2. Consulta ao IRIS (lotes parametrizados, uma conexão)
          withProgress(message = "Consultando banco de dados...", value = 0, {
            dados$Matricula <- buscar_matriculas_iris(
              distro,
              dados$Nome,
              ao_avancar = function(i, n) {
                setProgress(i / n, detail = paste("Lote", i, "de", n))
              }
            )
          })
          
          dados_processados(dados)
          
          showNotification(
            paste("Processamento concluído com sucesso!", nrow(dados), "registros processados."),
            type = "message"
          )
          
        }, error = function(e) {
          dados_processados(NULL)
          # Detalhe técnico (URL/usuário do IRIS, mensagem do driver) só no
          # log do servidor.
          warning("Erro ao consultar matrículas no IRIS: ", conditionMessage(e))
          showNotification(
            "Erro ao processar o arquivo ou consultar o banco. Verifique o arquivo e tente novamente.",
            type = "error",
            duration = NULL
          )
        })
      })
      
      # ---------------------------------------------
      # INFORMAÇÕES
      # ---------------------------------------------
      output$info <- renderText({
        req(dados_processados())
        
        total <- nrow(dados_processados())
        encontrados <- sum(!is.na(dados_processados()$Matricula))
        
        paste0(
          "Total de Registros: ", total, "\n",
          "Matrículas Encontradas: ", encontrados, "\n",
          "Matrículas Não Encontradas: ", total - encontrados
        )
      })
      
      # ---------------------------------------------
      # EXIBIÇÃO DA TABELA
      # ---------------------------------------------
      output$resultado <- renderDT({
        req(dados_processados())
        
        datatable(
          dados_processados(),
          filter = "top",
          rownames = FALSE,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            autoWidth = TRUE
          )
        )
      })
      
      # ---------------------------------------------
      # DOWNLOAD DO CSV RESULTANTE
      # ---------------------------------------------
      output$download_csv <- downloadHandler(
        filename = function() {
          paste0("resultado_matriculas_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
          write.csv(
            dados_processados(),
            file,
            row.names = FALSE,
            fileEncoding = "UTF-8"
          )
        }
      )
    }
  )
}