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

mod_matricula_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      dados_processados <- reactiveVal(NULL)
      
      # ---------------------------------------------
      # PROCESSAMENTO DO CSV E CONSULTA SQL
      # ---------------------------------------------
      observeEvent(input$processar, {
        req(input$arquivo_csv)
        
        tryCatch({
          # 1. Ler arquivo CSV uploaded
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
          
          # Conectar ao banco IRIS Intersystems usando a função padrão da aplicação
          con <- conectar_banco()
          on.exit(dbDisconnect(con), add = TRUE)
          
          # 2. Inicializa a nova coluna Matricula
          dados$Matricula <- NA_character_
          
          # 3. Iteração para buscar a matrícula de cada registro
          withProgress(message = 'Consultando banco de dados...', value = 0, {
            n <- nrow(dados)
            
            for (i in seq_len(n)) {
              nome_servidor <- dados$Nome[i]
              
              if (!is.na(nome_servidor) && trimws(nome_servidor) != "") {
                # Tratamento de aspas simples para prevenção de erro SQL
                nome_escapado <- gsub("'", "''", nome_servidor)
                
                # Query exigida para o IRIS Intersystems
                query <- paste0(
                  "SELECT Matricula FROM RHCadServidor WHERE Financeiro->CalculaFolha = 1 AND Nome = '",
                  nome_escapado,
                  "'"
                )
                
                res <- dbGetQuery(con, query)
                
                if (nrow(res) > 0 && !is.na(res$Matricula[1])) {
                  dados$Matricula[i] <- as.character(res$Matricula[1])
                }
              }
              
              incProgress(1 / n, detail = paste("Processando registro", i, "de", n))
            }
          })
          
          dados_processados(dados)
          
          showNotification(
            paste("Processamento concluído com sucesso!", nrow(dados), "registros processados."),
            type = "message"
          )
          
        }, error = function(e) {
          dados_processados(NULL)
          showNotification(
            paste("Erro ao processar:", e$message),
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