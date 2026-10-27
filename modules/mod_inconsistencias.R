# =====================================================
# modules/mod_inconsistencias.R
# -----------------------------------------------------
# Submódulo da aba "Inconsistências" — a mesma apresentação nos três
# módulos de dados (Quadro de Pessoal, Serventias e Magistrados):
#
#   - um cartão por verificação, com a quantidade de pessoas/serventias
#     apontadas (ou "OK") e a descrição da regra;
#   - avisos opcionais (ex.: base de Serventias ausente);
#   - filtros Verificação, Gravidade e campos de texto configuráveis;
#   - Tabela (Gravidade colorida) e Gerar arquivo CSV;
#   - um contador (badge) para o título da aba.
#
# Cada módulo calcula as próprias verificações (ex.:
# verificar_inconsistencias_servidores(), em mod_pessoal_auxiliar.R) e entrega o
# resultado já pronto para exibição; este submódulo só apresenta.
# O resultado precisa ter as colunas "Verificação", "Gravidade"
# ("Alta"/"Média") e "Motivo", além da coluna que identifica a entidade
# (ex.: CPF, Código Serventia).
#
# Uso (dentro de um módulo):
#   UI:     tabPanel(title = tagList("Inconsistências", mod_inconsistencias_badge_ui(ns("inc"))),
#                    mod_inconsistencias_ui(ns("inc"), verificacoes = c(...), filtros_texto = c(...)))
#   SERVER: mod_inconsistencias_server("inc", dados = reactive(...), ...)
#
# DEPENDÊNCIA: usa ui_csv_dados(), tabela_dt(), gravar_csv_tabela(),
# contem_texto() e tempo_decorrido(), de mod_pessoal_auxiliar.R.
# =====================================================

library(shiny)
library(dplyr)
library(stringr)
library(DT)

# Contador de entidades apontadas — vai no título da aba.
mod_inconsistencias_badge_ui <- function(id) {
  uiOutput(NS(id, "badge"), inline = TRUE)
}

# `verificacoes`: títulos das verificações (opções do filtro).
# `filtros_texto`: rótulo -> nome da coluna, para filtros "contém".
mod_inconsistencias_ui <- function(id, verificacoes, filtros_texto = character(0)) {

  ns <- NS(id)

  n_texto <- length(filtros_texto)
  largura_texto <- if (n_texto == 0) 0 else max(2, floor(5 / n_texto))

  tagList(

    tags$head(
      tags$style(HTML("

        .inc-modulo .inc-resumo {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
          gap: 1rem;
          margin-bottom: 1.5rem;
        }

        .inc-modulo .inc-card {
          background: #fff;
          border: 1px solid rgba(0,0,0,.06);
          border-left: 4px solid #198754;
          border-radius: .9rem;
          box-shadow: 0 .2rem .6rem rgba(15,23,42,.05);
          padding: 1rem 1.25rem;
        }

        .inc-modulo .inc-card.inc-alerta {
          border-left-color: #dc3545;
        }

        .inc-modulo .inc-card.inc-indisponivel {
          border-left-color: #adb5bd;
        }

        .inc-modulo .inc-card-titulo {
          font-weight: 600;
          margin-bottom: .25rem;
        }

        .inc-modulo .inc-card-numero {
          font-size: 1.6rem;
          font-weight: 700;
          line-height: 1.2;
        }

        .inc-modulo .inc-card-desc {
          font-size: .8rem;
          color: #6c757d;
          margin-top: .35rem;
        }

        .inc-badge {
          margin-left: .4rem;
          font-size: .7rem;
          vertical-align: middle;
        }

      "))
    ),

    div(
      class = "inc-modulo",

      uiOutput(ns("resumo")),

      div(
        class = "al-card",

        div(class = "al-card-titulo", "Filtros"),

        fluidRow(
          column(
            5,
            selectInput(
              ns("verificacao"), "Verificação",
              choices = c("Todas", verificacoes),
              width = "100%"
            )
          ),
          column(
            2,
            selectInput(
              ns("gravidade"), "Gravidade",
              choices = c("Todas", "Alta", "Média"),
              width = "100%"
            )
          ),
          lapply(seq_len(n_texto), function(i) {
            column(
              largura_texto,
              textInput(ns(paste0("texto_", i)), names(filtros_texto)[i], width = "100%")
            )
          })
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
            tagList(icon("file-csv", class = "me-1"), "Gerar arquivo CSV"),
            uiOutput(ns("csv_ui"))
          )
        )
      )
    )
  )

}

# Argumentos:
#   dados          reactive: tibble das inconsistências, já para exibição.
#   verificacoes   lista nomeada (ou reactive que a devolve): título ->
#                  descrição (texto ou tag) de cada verificação, na ordem
#                  dos cartões. Uma descrição pode ser list(descricao =,
#                  indisponivel = TRUE) para marcar a verificação como não
#                  executada (ex.: falta a base de Serventias).
#   mensagem_sem_base reactive: NULL ou texto exibido no lugar dos
#                  cartões/tabela quando não há dados de origem.
#   avisos         reactive: NULL ou tags exibidas abaixo dos cartões.
#   col_entidade   coluna que identifica a entidade (contagem distinta).
#   rotulo_entidade texto da contagem: "servidor(es)", "serventia(s)"...
#   filtros_texto  rótulo -> coluna (os mesmos passados para a UI).
#   prefixo_csv    início do nome do arquivo gerado.
mod_inconsistencias_server <- function(id, dados, verificacoes,
                                       mensagem_sem_base = reactive(NULL),
                                       avisos = reactive(NULL),
                                       col_entidade = "CPF",
                                       rotulo_entidade = "registro(s)",
                                       filtros_texto = character(0),
                                       prefixo_csv = "inconsistencias") {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    valor_verificacoes <- function() {
      if (is.reactive(verificacoes)) verificacoes() else verificacoes
    }

    contar_entidades <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(0L)
      }
      if (col_entidade %in% names(df)) n_distinct(df[[col_entidade]]) else nrow(df)
    }

    output$badge <- renderUI({
      n <- contar_entidades(dados())
      if (n == 0) {
        return(NULL)
      }
      tags$span(class = "badge rounded-pill bg-danger inc-badge", n)
    })

    output$resumo <- renderUI({

      msg <- mensagem_sem_base()

      if (!is.null(msg)) {
        return(div(class = "alert alert-secondary", msg))
      }

      inc <- dados()
      verif <- valor_verificacoes()

      cartao <- function(titulo, info) {

        indisponivel <- is.list(info) && isTRUE(info$indisponivel)
        descricao <- if (is.list(info) && !inherits(info, "shiny.tag") && !inherits(info, "shiny.tag.list")) {
          info$descricao
        } else {
          info
        }

        x <- inc[inc$`Verificação` == titulo, , drop = FALSE]
        n <- contar_entidades(x)

        classe <- if (indisponivel) "inc-indisponivel" else if (n > 0) "inc-alerta" else ""

        div(
          class = paste("inc-card", classe),
          div(class = "inc-card-titulo", titulo),
          div(
            class = "inc-card-numero",
            if (indisponivel) {
              tagList(icon("circle-minus", class = "text-secondary me-2"), "Não verificado")
            } else if (n == 0) {
              tagList(icon("circle-check", class = "text-success me-2"), "OK")
            } else {
              sprintf("%d %s", n, rotulo_entidade)
            }
          ),
          if (!indisponivel && n > 0) {
            div(
              class = "inc-card-desc",
              sprintf("%d registro(s) — %d de gravidade Alta", nrow(x), sum(x$Gravidade == "Alta"))
            )
          },
          div(class = "inc-card-desc", descricao)
        )

      }

      tagList(
        div(
          class = "inc-resumo",
          lapply(names(verif), function(t) cartao(t, verif[[t]]))
        ),
        avisos()
      )

    })

    # Textos dos filtros (CPF, Nome...), esperando o usuário parar de
    # digitar (400 ms) antes de filtrar.
    textos_d <- debounce(reactive({
      vapply(seq_along(filtros_texto), function(i) {
        str_trim(input[[paste0("texto_", i)]] %||% "")
      }, character(1))
    }), 400)

    filtradas <- reactive({

      df <- dados()

      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }

      manter <- rep(TRUE, nrow(df))

      if (!is.null(input$verificacao) && input$verificacao != "Todas") {
        manter <- manter & df$`Verificação` == input$verificacao
      }

      if (!is.null(input$gravidade) && input$gravidade != "Todas") {
        manter <- manter & df$Gravidade == input$gravidade
      }

      for (i in seq_along(filtros_texto)) {
        texto <- textos_d()[i]
        coluna <- filtros_texto[[i]]
        if (texto != "" && coluna %in% names(df)) {
          manter <- manter & contem_texto(df[[coluna]], texto)
        }
      }

      df[manter, , drop = FALSE]

    })

    observeEvent(input$limpar_filtros, {
      updateSelectInput(session, "verificacao", selected = "Todas")
      updateSelectInput(session, "gravidade", selected = "Todas")
      for (i in seq_along(filtros_texto)) {
        updateTextInput(session, paste0("texto_", i), value = "")
      }
    })

    output$tabela <- renderDT({

      msg <- mensagem_sem_base()
      shiny::validate(need(is.null(msg), msg))
      shiny::validate(need(nrow(dados()) > 0, "Nenhuma inconsistência encontrada."))

      df <- filtradas()

      shiny::validate(need(nrow(df) > 0, "Nenhuma inconsistência corresponde aos filtros aplicados."))

      tabela_dt(
        df,
        columnDefs = list(list(width = "420px", targets = which(names(df) == "Motivo") - 1))
      ) %>%
        formatStyle(
          "Gravidade",
          color = styleEqual(c("Alta", "Média"), c("#dc3545", "#b58105")),
          fontWeight = "bold"
        )

    })

    output$csv_ui <- renderUI({
      df <- filtradas()
      ui_csv_dados(ns, "download_csv", !is.null(df) && nrow(df) > 0)
    })

    output$download_csv <- downloadHandler(

      filename = function() {
        paste0(prefixo_csv, "-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
      },

      content = function(file) {
        inicio <- Sys.time()

        gravar_csv_tabela(filtradas(), file, input$tabela_rows_all)

        showNotification(
          sprintf("Arquivo CSV gerado em %.2fs.", tempo_decorrido(inicio)),
          type = "message"
        )
      }
    )

  })
}
