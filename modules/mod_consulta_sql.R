# =====================================================
# modules/mod_consulta_sql.R
# =====================================================

library(shiny)
library(DT)
library(DBI)

# =====================================================
# VALIDAÇÃO SQL
# =====================================================

validar_sql <- function(sql){

  if(
    is.null(sql) ||
    trimws(sql) == ""
  ){
    return(
      list(
        valido = FALSE,
        mensagem = "Informe uma consulta SQL."
      )
    )
  }

  sql_upper <- toupper(trimws(sql))

  comandos_proibidos <- c(
    "DELETE",
    "UPDATE",
    "INSERT",
    "DROP",
    "ALTER",
    "TRUNCATE",
    "CREATE",
    "GRANT",
    "REVOKE",
    "MERGE",
    "CALL",
    "EXEC",
    "EXECUTE"
  )

  for(cmd in comandos_proibidos){

    if(
      grepl(
        paste0("\\b", cmd, "\\b"),
        sql_upper
      )
    ){

      return(
        list(
          valido = FALSE,
          mensagem = paste(
            "Comando não permitido:",
            cmd
          )
        )
      )

    }

  }

  if(
    !startsWith(
      sql_upper,
      "SELECT"
    )
  ){

    return(
      list(
        valido = FALSE,
        mensagem = "Somente consultas SELECT são permitidas."
      )
    )

  }

  list(
    valido = TRUE,
    mensagem = NULL
  )

}

# =====================================================
# UI
# =====================================================

mod_consulta_sql_ui <- function(id){

  ns <- NS(id)

  fluidPage(

    fluidRow(

      column(

        12,

        h3("Consulta SQL"),

        p(
          "Somente consultas SELECT são permitidas."
        ),

        textAreaInput(
          ns("sql"),
          "Consulta SQL",
          rows = 12,
          width = "100%",
          placeholder =
            "Exemplo:\nSELECT TOP 100 * FROM RHCADSERVIDOR"
        ),

        actionButton(
          ns("executar"),
          "Executar Consulta",
          class = "btn btn-primary"
        )

      )

    ),

    br(),

    fluidRow(

      column(

        12,

        verbatimTextOutput(
          ns("info")
        )

      )

    ),

    fluidRow(

      column(

        12,

        DTOutput(
          ns("resultado")
        )

      )

    )

  )

}

# =====================================================
# SERVER
# =====================================================

mod_consulta_sql_server <- function(
  id
){

  moduleServer(

    id,

    function(
      input,
      output,
      session
    ){

      resultado <- reactiveVal(NULL)

      quantidade <- reactiveVal(0)

      # ---------------------------------------------
      # EXECUTAR CONSULTA
      # ---------------------------------------------

      observeEvent(
        input$executar,
        {

          validacao <- validar_sql(
            input$sql
          )

          if(!validacao$valido){

            showNotification(
              validacao$mensagem,
              type = "error"
            )

            return()

          }

          tryCatch({

            con <- conectar_banco()

            on.exit(
              dbDisconnect(con),
              add = TRUE
            )

            dados <- dbGetQuery(
              con,
              input$sql
            )

            resultado(dados)

            quantidade(
              nrow(dados)
            )

            showNotification(
              paste(
                "Consulta executada com sucesso.",
                nrow(dados),
                "registro(s) retornado(s)."
              ),
              type = "message"
            )

          },

          error = function(e){

            resultado(NULL)

            quantidade(0)

            showNotification(
              e$message,
              type = "error",
              duration = NULL
            )

          })

        }

      )

      # ---------------------------------------------
      # INFORMAÇÕES DA CONSULTA
      # ---------------------------------------------

      output$info <- renderText({

        req(resultado())

        paste(
          "Quantidade de registros:",
          format(
            quantidade(),
            big.mark = ".",
            decimal.mark = ","
          )
        )

      })

      # ---------------------------------------------
      # TABELA
      # ---------------------------------------------

      output$resultado <- renderDT({

        req(resultado())

        datatable(

          resultado(),

          filter = "top",

          rownames = FALSE,

          options = list(

            pageLength = 20,

            lengthMenu = c(
              10,
              20,
              50,
              100,
              500
            ),

            scrollX = TRUE,

            autoWidth = TRUE

          )

        )

      })

    }

  )

}