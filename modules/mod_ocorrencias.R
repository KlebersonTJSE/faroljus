# =====================================================
# modules/mod_ocorrencias.R
# =====================================================

library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(DT)

# =====================================================
# CONFIGURAÇÃO
# =====================================================

CAMINHO_OCORRENCIAS <- Sys.getenv("PASTA_OCORRENCIAS")

if (nzchar(CAMINHO_OCORRENCIAS) && !dir.exists(CAMINHO_OCORRENCIAS)) {
  dir.create(CAMINHO_OCORRENCIAS, recursive = TRUE, showWarnings = FALSE)
}

# =====================================================
# LEITURA E TRATAMENTO
# =====================================================

ler_tratar_csv <- function(arquivo) {
  
  df <- read_csv(
    
    arquivo,
    
    show_col_types =
      FALSE
    
  )
  
  arquivo_identificado <-
    FALSE
  
  arquivo_lc <-
    
    tolower(
      
      basename(
        arquivo
      )
      
    )
  
  if (
    
    str_detect(
      arquivo_lc,
      "inconsistentes"
    )
    
  ) {
    
    arquivo_identificado <-
      TRUE
    
    df <- df %>%
      
      mutate(
        
        `ID Evento` =
          `ID`,
        
        `Código Evento` =
          `Cod. Evento`,
        
        `Ocorrência(s)` =
          `Inconsistência(s)`,
        
        `Situação` =
          `Situação`,
        
        `Período` =
          NA_character_
        
      )
    
  } else if (
    
    str_detect(
      arquivo_lc,
      "rejeitados"
    )
    
  ) {
    
    arquivo_identificado <-
      TRUE
    
    df <- df %>%
      
      mutate(
        
        `ID Evento` =
          `ID Evento Fila`,
        
        `Código Evento` =
          `Código Evento`,
        
        `Ocorrência(s)` =
          `Ocorrência`,
        
        `Situação` =
          NA_character_,
        
        `Período` =
          `Período`
        
      )
    
  }
  
  if (
    
    !arquivo_identificado
    
  ) {
    
    warning(
      
      paste(
        
        "Arquivo não identificado:",
        arquivo
        
      )
      
    )
    
    return(
      data.frame()
    )
    
  }
  
  df %>%
    
    mutate(
      
      Matricula = str_extract(
        
        `Detalhe`,
        
        "^[^ ]+"
        
      ),
      
      Nome = str_replace(
        
        `Detalhe`,
        
        "^[^ ]+ [^ ]+\\s*",
        
        ""
        
      ),
      
      `Código Evento` =
        as.character(
          `Código Evento`
        )
      
    ) %>%
    
    select(
      
      `ID Evento`,
      
      `Código Evento`,
      
      Matricula,
      
      Nome,
      
      `Ocorrência(s)`,
      
      `Situação`,
      
      `Período`
      
    )
  
}

# =====================================================
# CARREGAMENTO
# =====================================================

carregar_ocorrencias <- function() {
  
  arquivos <- list.files(
    
    path =
      CAMINHO_OCORRENCIAS,
    
    pattern =
      "^relat_.*\\.csv$",
    
    full.names =
      TRUE
    
  )
  
  if (
    
    length(arquivos) == 0
    
  ) {
    
    warning(
      
      "Nenhum arquivo encontrado em: ",
      
      CAMINHO_OCORRENCIAS
      
    )
    
    return(
      data.frame()
    )
    
  }
  
  map_dfr(
    
    arquivos,
    
    ler_tratar_csv
    
  )
  
}

# =====================================================
# UI
# =====================================================

mod_ocorrencias_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # ===================================================
    # ESTILO ENTERPRISE (escopo deste módulo)
    # ===================================================
    
    tags$head(
      tags$style(HTML(sprintf("

        #%1$s .oc-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 1rem;
          margin-bottom: 1.5rem;
        }

        #%1$s .oc-titulo {
          display: flex;
          align-items: center;
          gap: .65rem;
        }

        #%1$s .oc-titulo-icone {
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

        #%1$s .oc-titulo h4 {
          margin: 0;
          font-weight: 700;
          letter-spacing: -.01em;
        }

        #%1$s .oc-acoes {
          display: flex;
          gap: .6rem;
          flex-wrap: wrap;
        }

        #%1$s .btn-acao {
          font-weight: 600;
          border-radius: .6rem;
          padding: .55rem 1.1rem;
        }

        #%1$s .oc-card {
          background: #fff;
          border: 1px solid rgba(0,0,0,.06);
          border-radius: .9rem;
          box-shadow: 0 .2rem .6rem rgba(15,23,42,.05);
          padding: 1.25rem 1.25rem .5rem 1.25rem;
          margin-bottom: 1.5rem;
        }

        #%1$s .oc-card-titulo {
          font-weight: 600;
          font-size: .8rem;
          text-transform: uppercase;
          letter-spacing: .04em;
          color: #6c757d;
          margin-bottom: .9rem;
        }

        #%1$s .oc-conteudo {
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

        .oc-modal-secao-titulo {
          font-weight: 600;
          font-size: 1rem;
          display: flex;
          align-items: center;
          gap: .5rem;
          margin-bottom: .35rem;
        }

        .oc-modal-secao-desc {
          font-size: .82rem;
          color: #6c757d;
          margin-bottom: .85rem;
        }

      ", id)))
    ),
    
    div(
      
      id = id,
      
      # =================================================
      # CABEÇALHO - TÍTULO + AÇÕES (mesma linha)
      # =================================================
      
      div(
        class = "oc-header",
        
        div(
          class = "oc-titulo",
          div(class = "oc-titulo-icone", icon("triangle-exclamation")),
          tags$h4("Consolidação - Inconsistências")
        ),
        
        div(
          class = "oc-acoes",
          
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
      # =================================================
      
      div(
        class = "oc-card",
        
        div(class = "oc-card-titulo", "Filtros"),
        
        fluidRow(
          column(3, selectInput(ns("codigo"), "Código Evento", choices = c("Todos"))),
          column(3, textInput(ns("matricula"), "Matrícula")),
          column(3, textInput(ns("nome"), "Nome")),
          column(3, textInput(ns("ocorrencia"), "Ocorrência"))
        )
        
      ),
      
      # =================================================
      # CONTEÚDO - TABELA / GRÁFICO
      # =================================================
      
      div(
        class = "oc-conteudo",
        
        tabsetPanel(
          tabPanel(tagList(icon("table", class = "me-1"), "Tabela"), DTOutput(ns("tabela"))),
          tabPanel(tagList(icon("chart-column", class = "me-1"), "Gráfico"), plotOutput(ns("grafico")))
        )
        
      )
      
    )
    
  )
  
}

# =====================================================
# SERVER
# =====================================================

mod_ocorrencias_server <- function(
    
  id,
  
  ativo =
    reactive(TRUE)
  
) {
  
  moduleServer(
    
    id,
    
    function(
    
      input,
      
      output,
      
      session
      
    ) {
      
      ns <- session$ns
      
      # ==============================================
      # DADOS
      # ==============================================
      
      dados <- reactiveVal(
        
        carregar_ocorrencias()
        
      )
      
      # ==============================================
      # ATUALIZA O COMBO
      # ==============================================
      
      atualizar_combo_codigo <- function() {
        
        df <- dados()
        
        if (
          
          is.null(df) ||
          
          nrow(df) == 0 ||
          
          !"Código Evento" %in%
          names(df)
          
        ) {
          
          updateSelectInput(
            
            session,
            
            "codigo",
            
            choices =
              "Todos",
            
            selected =
              "Todos"
            
          )
          
          return()
          
        }
        
        codigos <- sort(
          
          unique(
            
            df[[
              "Código Evento"
            ]]
            
          )
          
        )
        
        codigos <-
          
          codigos[
            
            !is.na(codigos) &
              
              codigos != ""
            
          ]
        
        updateSelectInput(
          
          session,
          
          "codigo",
          
          choices = c(
            
            "Todos",
            
            codigos
            
          ),
          
          selected =
            "Todos"
          
        )
        
      }
      
      # ==============================================
      # PRIMEIRA ATIVAÇÃO DA ABA
      # ==============================================
      
      observeEvent(
        
        ativo(),
        
        {
          
          if (
            
            isTRUE(
              ativo()
            )
            
          ) {
            
            atualizar_combo_codigo()
            
          }
          
        },
        
        ignoreInit =
          FALSE
        
      )
      
      # ==============================================
      # ATUALIZAÇÃO MANUAL
      # ==============================================
      
      observeEvent(
        
        input$atualizar,
        
        {
          
          dados(
            
            carregar_ocorrencias()
            
          )
          
          atualizar_combo_codigo()
          
          showNotification(
            
            "Dados atualizados.",
            
            type =
              "message"
            
          )
          
        },
        
        ignoreInit =
          TRUE
        
      )
      
      # ==============================================
      # MODAL "GERENCIAR ARQUIVOS"
      #
      # Reúne, em uma única janela, as opções
      # "Apagar Arquivos da Pasta" e "Enviar para Pasta",
      # apontando para a pasta de ocorrências (ocorrencias_csv).
      # ==============================================
      
      observeEvent(input$gerenciar_arquivos, {
        
        total_arquivos <- if (nzchar(CAMINHO_OCORRENCIAS) && dir.exists(CAMINHO_OCORRENCIAS)) {
          length(list.files(CAMINHO_OCORRENCIAS))
        } else {
          0
        }
        
        showModal(modalDialog(
          title = tagList(icon("folder-open", class = "me-2"), "Gerenciar Arquivos de Ocorrências"),
          size = "m",
          easyClose = TRUE,
          
          div(
            class = "mb-4",
            
            div(
              class = "oc-modal-secao-titulo",
              icon("trash", class = "text-danger"),
              "Apagar arquivos da pasta"
            ),
            
            div(
              class = "oc-modal-secao-desc",
              if (total_arquivos > 0) {
                sprintf("A pasta contém atualmente %d arquivo(s).", total_arquivos)
              } else {
                "A pasta de ocorrências está vazia."
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
              class = "oc-modal-secao-titulo",
              icon("upload", class = "text-primary"),
              "Enviar arquivos para a pasta"
            ),
            
            div(
              class = "oc-modal-secao-desc",
              "Selecione um ou mais arquivos CSV de ocorrências (rejeitados/inconsistências)."
            ),
            
            fileInput(
              ns("upload_arquivos"),
              NULL,
              multiple = TRUE,
              accept = c(".csv", "text/csv")
            ),
            
            actionButton(
              ns("enviar_arquivos"),
              tagList(icon("upload", class = "me-2"), "Enviar para Pasta"),
              class = "btn btn-primary w-100"
            )
            
          ),
          
          footer = modalButton("Fechar")
          
        ))
        
      })
      
      # ----------------------------------------
      # APAGAR ARQUIVOS DA PASTA (BOTÃO INDEPENDENTE)
      # ----------------------------------------
      
      apagar_pasta_ocorrencias <- function() {
        if (!nzchar(CAMINHO_OCORRENCIAS) || !dir.exists(CAMINHO_OCORRENCIAS)) {
          showNotification(
            "PASTA_OCORRENCIAS não está configurada ou não existe.",
            type = "error"
          )
          return(invisible(NULL))
        }
        
        arquivos_atuais <- list.files(CAMINHO_OCORRENCIAS, full.names = TRUE)
        
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
            sprintf("%d arquivo(s) apagado(s) da pasta.", length(arquivos_atuais)),
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
          dados(carregar_ocorrencias())
          atualizar_combo_codigo()
        }, error = function(e) {
          showNotification(
            sprintf("Arquivos apagados, mas houve erro ao recarregar a tabela: %s", conditionMessage(e)),
            type = "error"
          )
        })
      }
      
      observeEvent(input$apagar_pasta, {
        if (!nzchar(CAMINHO_OCORRENCIAS) || !dir.exists(CAMINHO_OCORRENCIAS)) {
          showNotification(
            "PASTA_OCORRENCIAS não está configurada ou não existe.",
            type = "error"
          )
          return()
        }
        
        arquivos_existentes <- list.files(CAMINHO_OCORRENCIAS)
        
        if (length(arquivos_existentes) == 0) {
          showNotification("A pasta já está vazia.", type = "warning")
          return()
        }
        
        showModal(modalDialog(
          title = "Confirmar exclusão",
          sprintf(
            "Tem certeza que deseja apagar os %d arquivo(s) da pasta de ocorrências? Esta ação não pode ser desfeita.",
            length(arquivos_existentes)
          ),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("confirmar_apagar_pasta"), "Apagar", class = "btn-danger")
          )
        ))
      })
      
      observeEvent(input$confirmar_apagar_pasta, {
        removeModal()
        apagar_pasta_ocorrencias()
      })
      
      # ----------------------------------------
      # UPLOAD DE ARQUIVOS PARA CAMINHO_OCORRENCIAS
      # ----------------------------------------
      
      # Copia os arquivos enviados para CAMINHO_OCORRENCIAS,
      # apagando os existentes antes se `apagar_existentes = TRUE`.
      processar_upload <- function(arquivos_upload, apagar_existentes = FALSE) {
        req(arquivos_upload)
        
        if (apagar_existentes) {
          arquivos_atuais <- list.files(CAMINHO_OCORRENCIAS, full.names = TRUE)
          if (length(arquivos_atuais) > 0) {
            file.remove(arquivos_atuais)
          }
        }
        
        destinos <- file.path(CAMINHO_OCORRENCIAS, arquivos_upload$name)
        copiados <- file.copy(arquivos_upload$datapath, destinos, overwrite = TRUE)
        
        if (all(copiados)) {
          showNotification(
            sprintf("%d arquivo(s) enviado(s) com sucesso.", nrow(arquivos_upload)),
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
        
        dados(carregar_ocorrencias())
        atualizar_combo_codigo()
      }
      
      observeEvent(input$enviar_arquivos, {
        req(input$upload_arquivos)
        
        arquivos_existentes <- list.files(CAMINHO_OCORRENCIAS)
        
        if (length(arquivos_existentes) > 0) {
          showModal(modalDialog(
            title = "Arquivos existentes na pasta",
            sprintf(
              "A pasta de ocorrências já contém %d arquivo(s). Deseja apagar os arquivos existentes antes de enviar os novos, ou manter os dois conjuntos?",
              length(arquivos_existentes)
            ),
            footer = tagList(
              modalButton("Cancelar"),
              actionButton(ns("manter_existentes"), "Manter Existentes"),
              actionButton(ns("apagar_existentes"), "Apagar e Enviar", class = "btn-danger")
            )
          ))
        } else {
          processar_upload(input$upload_arquivos, apagar_existentes = FALSE)
        }
      })
      
      observeEvent(input$apagar_existentes, {
        removeModal()
        processar_upload(input$upload_arquivos, apagar_existentes = TRUE)
      })
      
      observeEvent(input$manter_existentes, {
        removeModal()
        processar_upload(input$upload_arquivos, apagar_existentes = FALSE)
      })
      
      # ==============================================
      # FILTROS
      # ==============================================
      
      dados_filtrados <- reactive({
        
        req(
          dados()
        )
        
        df <- dados()
        
        if (
          
          !is.null(
            input$matricula
          ) &&
          
          input$matricula != ""
          
        ) {
          
          df <- df %>%
            
            filter(
              
              str_detect(
                
                str_to_upper(
                  Matricula
                ),
                
                str_to_upper(
                  input$matricula
                )
                
              )
              
            )
          
        }
        
        if (
          
          !is.null(
            input$nome
          ) &&
          
          input$nome != ""
          
        ) {
          
          df <- df %>%
            
            filter(
              
              str_detect(
                
                str_to_upper(
                  Nome
                ),
                
                str_to_upper(
                  input$nome
                )
                
              )
              
            )
          
        }
        
        if (
          
          !is.null(
            input$ocorrencia
          ) &&
          
          input$ocorrencia != ""
          
        ) {
          
          df <- df %>%
            
            filter(
              
              str_detect(
                
                str_to_upper(
                  `Ocorrência(s)`
                ),
                
                str_to_upper(
                  input$ocorrencia
                )
                
              )
              
            )
          
        }
        
        if (
          
          !is.null(
            input$codigo
          ) &&
          
          input$codigo != "Todos"
          
        ) {
          
          df <- df %>%
            
            filter(
              
              `Código Evento` ==
                input$codigo
              
            )
          
        }
        
        df
        
      })
      
      # ==============================================
      # TABELA
      # ==============================================
      
      output$tabela <- renderDT({
        
        datatable(
          
          dados_filtrados(),
          
          filter =
            "top",
          
          rownames =
            FALSE,
          
          options =
            list(
              
              pageLength =
                20,
              
              scrollX =
                TRUE,
              
              autoWidth =
                TRUE
              
            )
          
        )
        
      })
      
      # ==============================================
      # GRÁFICO
      # ==============================================
      
      output$grafico <- renderPlot({
        
        df <-
          dados_filtrados()
        
        req(
          nrow(df) > 0
        )
        
        df %>%
          
          count(
            
            `Código Evento`
            
          ) %>%
          
          ggplot(
            
            aes(
              
              x =
                as.factor(
                  `Código Evento`
                ),
              
              y =
                n
              
            )
            
          ) +
          
          geom_col(
            
            fill =
              "#2C7FB8"
            
          ) +
          
          geom_text(
            
            aes(
              label =
                n
            ),
            
            vjust =
              -0.3
            
          ) +
          
          labs(
            
            title =
              "Quantidade por Código Evento",
            
            x =
              "Código Evento",
            
            y =
              "Quantidade"
            
          ) +
          
          theme_minimal()
        
      })
      
    }
    
  )
  
}

# # =====================================================
# # modules/mod_ocorrencias.R
# # =====================================================
# 
# library(shiny)
# library(readr)
# library(dplyr)
# library(purrr)
# library(stringr)
# library(ggplot2)
# library(DT)
# 
# # =====================================================
# # CONFIGURAÇÃO
# # =====================================================
# 
# CAMINHO_OCORRENCIAS <- Sys.getenv("PASTA_OCORRENCIAS")
# 
# # =====================================================
# # LEITURA E TRATAMENTO
# # =====================================================
# 
# ler_tratar_csv <- function(arquivo) {
#   
#   df <- read_csv(
#     
#     arquivo,
#     
#     show_col_types =
#       FALSE
#     
#   )
#   
#   arquivo_identificado <-
#     FALSE
#   
#   arquivo_lc <-
#     
#     tolower(
#       
#       basename(
#         arquivo
#       )
#       
#     )
#   
#   if (
#     
#     str_detect(
#       arquivo_lc,
#       "inconsistentes"
#     )
#     
#   ) {
#     
#     arquivo_identificado <-
#       TRUE
#     
#     df <- df %>%
#       
#       mutate(
#         
#         `ID Evento` =
#           `ID`,
#         
#         `Código Evento` =
#           `Cod. Evento`,
#         
#         `Ocorrência(s)` =
#           `Inconsistência(s)`,
#         
#         `Situação` =
#           `Situação`,
#         
#         `Período` =
#           NA_character_
#         
#       )
#     
#   } else if (
#     
#     str_detect(
#       arquivo_lc,
#       "rejeitados"
#     )
#     
#   ) {
#     
#     arquivo_identificado <-
#       TRUE
#     
#     df <- df %>%
#       
#       mutate(
#         
#         `ID Evento` =
#           `ID Evento Fila`,
#         
#         `Código Evento` =
#           `Código Evento`,
#         
#         `Ocorrência(s)` =
#           `Ocorrência`,
#         
#         `Situação` =
#           NA_character_,
#         
#         `Período` =
#           `Período`
#         
#       )
#     
#   }
#   
#   if (
#     
#     !arquivo_identificado
#     
#   ) {
#     
#     warning(
#       
#       paste(
#         
#         "Arquivo não identificado:",
#         arquivo
#         
#       )
#       
#     )
#     
#     return(
#       data.frame()
#     )
#     
#   }
#   
#   df %>%
#     
#     mutate(
#       
#       Matricula = str_extract(
#         
#         `Detalhe`,
#         
#         "^[^ ]+"
#         
#       ),
#       
#       Nome = str_replace(
#         
#         `Detalhe`,
#         
#         "^[^ ]+ [^ ]+\\s*",
#         
#         ""
#         
#       ),
#       
#       `Código Evento` =
#         as.character(
#           `Código Evento`
#         )
#       
#     ) %>%
#     
#     select(
#       
#       `ID Evento`,
#       
#       `Código Evento`,
#       
#       Matricula,
#       
#       Nome,
#       
#       `Ocorrência(s)`,
#       
#       `Situação`,
#       
#       `Período`
#       
#     )
#   
# }
# 
# # =====================================================
# # CARREGAMENTO
# # =====================================================
# 
# carregar_ocorrencias <- function() {
#   
#   arquivos <- list.files(
#     
#     path =
#       CAMINHO_OCORRENCIAS,
#     
#     pattern =
#       "^relat_.*\\.csv$",
#     
#     full.names =
#       TRUE
#     
#   )
#   
#   if (
#     
#     length(arquivos) == 0
#     
#   ) {
#     
#     warning(
#       
#       "Nenhum arquivo encontrado em: ",
#       
#       CAMINHO_OCORRENCIAS
#       
#     )
#     
#     return(
#       data.frame()
#     )
#     
#   }
#   
#   map_dfr(
#     
#     arquivos,
#     
#     ler_tratar_csv
#     
#   )
#   
# }
# 
# # =====================================================
# # UI
# # =====================================================
# 
# mod_ocorrencias_ui <- function(id) {
#   ns <- NS(id)
#   fluidPage(titlePanel("Consolidação - Inconsistencias"),
#             sidebarLayout(
#             sidebarPanel(
#               actionButton(ns("atualizar"),
#                            "Atualizar Dados",
#           
#           class =
#             "btn btn-secondary"
#           
#         ),
#         
#         br(),
#         br(),
#         
#         selectInput(
#           
#           ns("codigo"),
#           
#           "Código Evento",
#           
#           choices =
#             c("Todos")
#           
#         ),
#         
#         textInput(
#           
#           ns("matricula"),
#           
#           "Matrícula"
#           
#         ),
#         
#         textInput(
#           
#           ns("nome"),
#           
#           "Nome"
#           
#         ),
#         
#         textInput(
#           
#           ns("ocorrencia"),
#           
#           "Ocorrência"
#           
#         )
#         
#       ),
#       
#       mainPanel(
#         
#         tabsetPanel(
#           
#           tabPanel(
#             
#             "Tabela",
#             
#             DTOutput(
#               ns("tabela")
#             )
#             
#           ),
#           
#           tabPanel(
#             
#             "Gráfico",
#             
#             plotOutput(
#               ns("grafico")
#             )
#             
#           )
#           
#         )
#         
#       )
#       
#     )
#     
#   )
#   
# }
# 
# # =====================================================
# # SERVER
# # =====================================================
# 
# mod_ocorrencias_server <- function(
#     
#   id,
#   
#   ativo =
#     reactive(TRUE)
#   
# ) {
#   
#   moduleServer(
#     
#     id,
#     
#     function(
#     
#       input,
#       
#       output,
#       
#       session
#       
#     ) {
#       
#       # ==============================================
#       # DADOS
#       # ==============================================
#       
#       dados <- reactiveVal(
#         
#         carregar_ocorrencias()
#         
#       )
#       
#       # ==============================================
#       # ATUALIZA O COMBO
#       # ==============================================
#       
#       atualizar_combo_codigo <- function() {
#         
#         df <- dados()
#         
#         if (
#           
#           is.null(df) ||
#           
#           nrow(df) == 0 ||
#           
#           !"Código Evento" %in%
#           names(df)
#           
#         ) {
#           
#           updateSelectInput(
#             
#             session,
#             
#             "codigo",
#             
#             choices =
#               "Todos",
#             
#             selected =
#               "Todos"
#             
#           )
#           
#           return()
#           
#         }
#         
#         codigos <- sort(
#           
#           unique(
#             
#             df[[
#               "Código Evento"
#             ]]
#             
#           )
#           
#         )
#         
#         codigos <-
#           
#           codigos[
#             
#             !is.na(codigos) &
#               
#               codigos != ""
#             
#           ]
#         
#         updateSelectInput(
#           
#           session,
#           
#           "codigo",
#           
#           choices = c(
#             
#             "Todos",
#             
#             codigos
#             
#           ),
#           
#           selected =
#             "Todos"
#           
#         )
#         
#       }
#       
#       # ==============================================
#       # PRIMEIRA ATIVAÇÃO DA ABA
#       # ==============================================
#       
#       observeEvent(
#         
#         ativo(),
#         
#         {
#           
#           if (
#             
#             isTRUE(
#               ativo()
#             )
#             
#           ) {
#             
#             atualizar_combo_codigo()
#             
#           }
#           
#         },
#         
#         ignoreInit =
#           FALSE
#         
#       )
#       
#       # ==============================================
#       # ATUALIZAÇÃO MANUAL
#       # ==============================================
#       
#       observeEvent(
#         
#         input$atualizar,
#         
#         {
#           
#           dados(
#             
#             carregar_ocorrencias()
#             
#           )
#           
#           atualizar_combo_codigo()
#           
#           showNotification(
#             
#             "Dados atualizados.",
#             
#             type =
#               "message"
#             
#           )
#           
#         },
#         
#         ignoreInit =
#           TRUE
#         
#       )
#       
#       # ==============================================
#       # FILTROS
#       # ==============================================
#       
#       dados_filtrados <- reactive({
#         
#         req(
#           dados()
#         )
#         
#         df <- dados()
#         
#         if (
#           
#           !is.null(
#             input$matricula
#           ) &&
#           
#           input$matricula != ""
#           
#         ) {
#           
#           df <- df %>%
#             
#             filter(
#               
#               str_detect(
#                 
#                 str_to_upper(
#                   Matricula
#                 ),
#                 
#                 str_to_upper(
#                   input$matricula
#                 )
#                 
#               )
#               
#             )
#           
#         }
#         
#         if (
#           
#           !is.null(
#             input$nome
#           ) &&
#           
#           input$nome != ""
#           
#         ) {
#           
#           df <- df %>%
#             
#             filter(
#               
#               str_detect(
#                 
#                 str_to_upper(
#                   Nome
#                 ),
#                 
#                 str_to_upper(
#                   input$nome
#                 )
#                 
#               )
#               
#             )
#           
#         }
#         
#         if (
#           
#           !is.null(
#             input$ocorrencia
#           ) &&
#           
#           input$ocorrencia != ""
#           
#         ) {
#           
#           df <- df %>%
#             
#             filter(
#               
#               str_detect(
#                 
#                 str_to_upper(
#                   `Ocorrência(s)`
#                 ),
#                 
#                 str_to_upper(
#                   input$ocorrencia
#                 )
#                 
#               )
#               
#             )
#           
#         }
#         
#         if (
#           
#           !is.null(
#             input$codigo
#           ) &&
#           
#           input$codigo != "Todos"
#           
#         ) {
#           
#           df <- df %>%
#             
#             filter(
#               
#               `Código Evento` ==
#                 input$codigo
#               
#             )
#           
#         }
#         
#         df
#         
#       })
#       
#       # ==============================================
#       # TABELA
#       # ==============================================
#       
#       output$tabela <- renderDT({
#         
#         datatable(
#           
#           dados_filtrados(),
#           
#           filter =
#             "top",
#           
#           rownames =
#             FALSE,
#           
#           options =
#             list(
#               
#               pageLength =
#                 20,
#               
#               scrollX =
#                 TRUE,
#               
#               autoWidth =
#                 TRUE
#               
#             )
#           
#         )
#         
#       })
#       
#       # ==============================================
#       # GRÁFICO
#       # ==============================================
#       
#       output$grafico <- renderPlot({
#         
#         df <-
#           dados_filtrados()
#         
#         req(
#           nrow(df) > 0
#         )
#         
#         df %>%
#           
#           count(
#             
#             `Código Evento`
#             
#           ) %>%
#           
#           ggplot(
#             
#             aes(
#               
#               x =
#                 as.factor(
#                   `Código Evento`
#                 ),
#               
#               y =
#                 n
#               
#             )
#             
#           ) +
#           
#           geom_col(
#             
#             fill =
#               "#2C7FB8"
#             
#           ) +
#           
#           geom_text(
#             
#             aes(
#               label =
#                 n
#             ),
#             
#             vjust =
#               -0.3
#             
#           ) +
#           
#           labs(
#             
#             title =
#               "Quantidade por Código Evento",
#             
#             x =
#               "Código Evento",
#             
#             y =
#               "Quantidade"
#             
#           ) +
#           
#           theme_minimal()
#         
#       })
#       
#     }
#     
#   )
#   
# }