# =====================================================
# modules/mod_rejeitados.R
# =====================================================

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(DT)
library(ggplot2)

# =====================================================
# CONFIGURAÇÃO
# =====================================================

CAMINHO_REJEITADOS <- Sys.getenv("PASTA_REJEITADOS")

if (nzchar(CAMINHO_REJEITADOS) && !dir.exists(CAMINHO_REJEITADOS)) {
    dir.create(CAMINHO_REJEITADOS, recursive = TRUE, showWarnings = FALSE)
}

# =====================================================
# TEMPO DE PROCESSAMENTO
# =====================================================

tempo_decorrido <- function(inicio) {
    round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 2)
}

# =====================================================
# EXTRAI PERÍODO
# =====================================================

extrair_periodo_rejeitado <- function(nome_arquivo) {
    base <- basename(nome_arquivo)
    achado <- str_extract(base, "\\d{6}")
    achado
}

# =====================================================
# EXTRAI MATRÍCULA E NOME
# =====================================================

extrair_matricula <- function(texto){
    str_extract(
        texto,
        "\\d+"
    )
}

extrair_nome <- function(texto){
    texto %>%
        str_remove("^\\d+") %>%
        str_remove("^\\s*[-:]?\\s*") %>%
        str_trim()
}

processar_rejeitado <- function(arquivo){
    df <- read_csv(
        arquivo,
        col_types = cols(.default = "c")
    )
    df %>%
        mutate(
            Matricula =
                extrair_matricula(
                    Detalhe
                ),
            Nome =
                extrair_nome(
                    Detalhe
                ),
            Periodo = formatar_periodo(`Período`)
        ) %>%
        transmute(
            `ID Evento` =
                `ID Evento Fila`,
            `Código Evento` =
                `Código Evento`,
            Periodo,
            Matricula,
            Nome,
            `Ocorrência(s)` =
                Ocorrência
        )
}

# =====================================================
# UI
# =====================================================

mod_rejeitados_ui <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        
        # ===================================================
        # ESTILO ENTERPRISE (escopo deste módulo)
        # ===================================================
        
        tags$head(
            tags$style(HTML(sprintf("

        #%1$s .rj-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 1rem;
          margin-bottom: 1.5rem;
        }

        #%1$s .rj-titulo {
          display: flex;
          align-items: center;
          gap: .65rem;
        }

        #%1$s .rj-titulo-icone {
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

        #%1$s .rj-titulo h4 {
          margin: 0;
          font-weight: 700;
          letter-spacing: -.01em;
        }

        #%1$s .rj-acoes {
          display: flex;
          gap: .6rem;
          flex-wrap: wrap;
        }

        #%1$s .btn-acao {
          font-weight: 600;
          border-radius: .6rem;
          padding: .55rem 1.1rem;
        }

        #%1$s .rj-card {
          background: #fff;
          border: 1px solid rgba(0,0,0,.06);
          border-radius: .9rem;
          box-shadow: 0 .2rem .6rem rgba(15,23,42,.05);
          padding: 1.25rem 1.25rem .5rem 1.25rem;
          margin-bottom: 1.5rem;
        }

        #%1$s .rj-card-titulo {
          font-weight: 600;
          font-size: .8rem;
          text-transform: uppercase;
          letter-spacing: .04em;
          color: #6c757d;
          margin-bottom: .9rem;
        }

        #%1$s .rj-conteudo {
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

        .rj-modal-secao-titulo {
          font-weight: 600;
          font-size: 1rem;
          display: flex;
          align-items: center;
          gap: .5rem;
          margin-bottom: .35rem;
        }

        .rj-modal-secao-desc {
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
                class = "rj-header",
                
                div(
                    class = "rj-titulo",
                    div(class = "rj-titulo-icone", icon("file-circle-exclamation")),
                    tags$h4("Consolidação - Arquivos Rejeitados")
                ),
                
                div(
                    class = "rj-acoes",
                    
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
                class = "rj-card",
                
                div(class = "rj-card-titulo", "Filtros"),
                
                fluidRow(
                    column(2, selectInput(ns("periodo"), "Período", choices = c("Todos"))),
                    column(2, selectInput(ns("codigo_evento"), "Código Evento", choices = c("Todos"))),
                    column(2, textInput(ns("matricula"), "Matrícula")),
                    column(3, textInput(ns("nome"), "Nome")),
                    column(3, textInput(ns("ocorrencia"), "Ocorrência(s)"))
                )
                
            ),
            
            # =================================================
            # CONTEÚDO - TABELA / GRÁFICO
            # =================================================
            
            div(
                class = "rj-conteudo",
                
                tabsetPanel(
                    tabPanel(tagList(icon("table", class = "me-1"), "Tabela"), DTOutput(ns("tabela"))),
                    tabPanel(tagList(icon("chart-column", class = "me-1"), "Gráfico"), plotOutput(ns("grafico"))),
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

mod_rejeitados_server <- function(id, ativo = reactive(TRUE)) {
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        dados <- reactiveVal(carregar_rejeitados())
        
        # ----------------------------------------
        # COMBO Periodo — reage a dados() E a ativo()
        # ----------------------------------------
        
        observeEvent(list(dados(), ativo()), {
            req(ativo())
            req(dados())
            df <- dados()
            
            # -----------------------------------------
            # PRESERVA VALORES ATUAIS
            #
            # Evita que os combos voltem para "Todos"
            # toda vez que o usuário volta para esta aba
            # (ativo() vira TRUE de novo) sem que os dados
            # tenham mudado de fato.
            # -----------------------------------------
            
            periodo_atual <- input$periodo
            codigo_atual <- input$codigo_evento
            
            periodos <- if (nrow(df) > 0 &&
                            "Periodo" %in% names(df)) {
                sort(unique(df$Periodo))
            } else {
                character(0)
            }
            codigos_evento <- if (nrow(df) > 0 &&
                                  "Código Evento" %in% names(df)) {
                sort(unique(df$`Código Evento`))
            } else {
                character(0)
            }
            
            periodo_selecionado <- if (!is.null(periodo_atual) &&
                                       periodo_atual %in% periodos) {
                periodo_atual
            } else {
                "Todos"
            }
            
            codigo_selecionado <- if (!is.null(codigo_atual) &&
                                      codigo_atual %in% codigos_evento) {
                codigo_atual
            } else {
                "Todos"
            }
            
            updateSelectInput(session,
                              "periodo",
                              choices = c("Todos", periodos),
                              selected = periodo_selecionado)
            updateSelectInput(
                session,
                "codigo_evento",
                choices = c("Todos", codigos_evento),
                selected = codigo_selecionado
            )
        }, ignoreInit = FALSE)
        
        observeEvent(input$atualizar, {
            inicio <- Sys.time()
            
            dados(carregar_rejeitados())
            
            showNotification(
                sprintf("Dados atualizados em %.2fs.", tempo_decorrido(inicio)),
                type = "message"
            )
        })
        
        # ----------------------------------------
        # MODAL "GERENCIAR ARQUIVOS"
        #
        # Reúne, em uma única janela, as opções
        # "Apagar Arquivos da Pasta" e "Enviar para Pasta",
        # acessadas pelo botão ao lado de "Atualizar Dados".
        # ----------------------------------------
        
        observeEvent(input$gerenciar_arquivos, {
            
            total_arquivos <- if (nzchar(CAMINHO_REJEITADOS) && dir.exists(CAMINHO_REJEITADOS)) {
                length(list.files(CAMINHO_REJEITADOS))
            } else {
                0
            }
            
            showModal(modalDialog(
                title = tagList(icon("folder-open", class = "me-2"), "Gerenciar Arquivos Rejeitados"),
                size = "m",
                easyClose = TRUE,
                
                div(
                    class = "mb-4",
                    
                    div(
                        class = "rj-modal-secao-titulo",
                        icon("trash", class = "text-danger"),
                        "Apagar arquivos da pasta"
                    ),
                    
                    div(
                        class = "rj-modal-secao-desc",
                        if (total_arquivos > 0) {
                            sprintf("A pasta contém atualmente %d arquivo(s).", total_arquivos)
                        } else {
                            "A pasta de rejeitados está vazia."
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
                        class = "rj-modal-secao-titulo",
                        icon("upload", class = "text-primary"),
                        "Enviar arquivos para a pasta"
                    ),
                    
                    div(
                        class = "rj-modal-secao-desc",
                        "Selecione um ou mais arquivos CSV de eventos rejeitados."
                    ),
                    
                    fileInput(
                        ns("upload_arquivos"),
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
                
                footer = modalButton("Fechar")
                
            ))
            
        })
        
        # ----------------------------------------
        # APAGAR ARQUIVOS DA PASTA (BOTÃO INDEPENDENTE)
        # ----------------------------------------
        
        apagar_pasta_rejeitados <- function() {
            inicio <- Sys.time()
            
            if (!nzchar(CAMINHO_REJEITADOS) || !dir.exists(CAMINHO_REJEITADOS)) {
                showNotification(
                    "PASTA_REJEITADOS não está configurada ou não existe.",
                    type = "error"
                )
                return(invisible(NULL))
            }
            
            arquivos_atuais <- list.files(CAMINHO_REJEITADOS, full.names = TRUE)
            
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
                dados(carregar_rejeitados())
            }, error = function(e) {
                showNotification(
                    sprintf("Arquivos apagados, mas houve erro ao recarregar a tabela: %s", conditionMessage(e)),
                    type = "error"
                )
            })
        }
        
        observeEvent(input$apagar_pasta, {
            if (!nzchar(CAMINHO_REJEITADOS) || !dir.exists(CAMINHO_REJEITADOS)) {
                showNotification(
                    "PASTA_REJEITADOS não está configurada ou não existe.",
                    type = "error"
                )
                return()
            }
            
            arquivos_existentes <- list.files(CAMINHO_REJEITADOS)
            
            if (length(arquivos_existentes) == 0) {
                showNotification("A pasta já está vazia.", type = "warning")
                return()
            }
            
            showModal(modalDialog(
                title = "Confirmar exclusão",
                sprintf(
                    "Tem certeza que deseja apagar os %d arquivo(s) da pasta de rejeitados? Esta ação não pode ser desfeita.",
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
            apagar_pasta_rejeitados()
        })
        
        # ----------------------------------------
        # UPLOAD DE ARQUIVOS PARA PASTA_REJEITADOS
        # ----------------------------------------
        
        # Copia os arquivos enviados para CAMINHO_REJEITADOS,
        # apagando os existentes antes se `apagar_existentes = TRUE`.
        processar_upload <- function(arquivos_upload, apagar_existentes = FALSE) {
            req(arquivos_upload)
            
            inicio <- Sys.time()
            
            if (apagar_existentes) {
                arquivos_atuais <- list.files(CAMINHO_REJEITADOS, full.names = TRUE)
                if (length(arquivos_atuais) > 0) {
                    file.remove(arquivos_atuais)
                }
            }
            
            destinos <- file.path(CAMINHO_REJEITADOS, arquivos_upload$name)
            copiados <- file.copy(arquivos_upload$datapath, destinos, overwrite = TRUE)
            
            if (all(copiados)) {
                showNotification(
                    sprintf(
                        "%d arquivo(s) enviado(s) com sucesso em %.2fs.",
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
            
            dados(carregar_rejeitados())
        }
        
        observeEvent(input$enviar_arquivos, {
            req(input$upload_arquivos)
            
            arquivos_existentes <- list.files(CAMINHO_REJEITADOS)
            
            if (length(arquivos_existentes) > 0) {
                showModal(modalDialog(
                    title = "Arquivos existentes na pasta",
                    sprintf(
                        "A pasta de rejeitados já contém %d arquivo(s). Deseja apagar os arquivos existentes antes de enviar os novos, ou manter os dois conjuntos?",
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
        
        filtrado <- reactive({
            req(dados())
            df <- dados()
            if (!is.null(input$periodo) &&
                input$periodo != "Todos") {
                df <- df %>%
                    filter(Periodo ==
                               input$periodo)
            }
            if (!is.null(input$codigo_evento) &&
                input$codigo_evento != "Todos") {
                df <- df %>%
                    filter(`Código Evento` ==
                               input$codigo_evento)
            }
            if (!is.null(input$matricula) && input$matricula != "") {
                df <- df %>%
                    filter(str_detect(
                        str_to_upper(Matricula),
                        str_to_upper(input$matricula)
                    ))
            }
            if (!is.null(input$nome) && input$nome != "") {
                df <- df %>%
                    filter(str_detect(str_to_upper(Nome), str_to_upper(input$nome)))
            }
            if (!is.null(input$ocorrencia) && input$ocorrencia != "") {
                df <- df %>%
                    filter(str_detect(
                        str_to_upper(`Ocorrência(s)`),
                        str_to_upper(input$ocorrencia)
                    ))
            }
            df
        })
        output$tabela <- renderDT({
            df <- filtrado()
            
            validate(
                need(
                    nrow(df) > 0,
                    "Não existem arquivos para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviar os arquivos de rejeitados."
                )
            )
            
            datatable(
                df,
                filter = "top",
                rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE)
            )
        })
        output$grafico <- renderPlot({
            df <- filtrado()
            
            validate(
                need(
                    nrow(df) > 0 && "Código Evento" %in% names(df),
                    "Não existem arquivos para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviar os arquivos de rejeitados."
                )
            )
            
            dados_grafico <- df %>%
                count(`Código Evento`, name = "Quantidade") %>%
                arrange(desc(Quantidade))
            ggplot(dados_grafico, aes(x = reorder(`Código Evento`, Quantidade), y = Quantidade)) +
                geom_col(fill = "#E74C3C") +
                geom_text(aes(label = Quantidade), hjust = -0.2) +
                coord_flip() +
                labs(title =
                         "Quantidade de Rejeições por Código de Evento", x =
                         "Código Evento", y =
                         "Quantidade") +
                theme_minimal()
        })
        
        # ==============================================
        # GERAR ARQUIVO CSV
        #
        # O botão só fica disponível quando a Tabela tem
        # dados a exibir (mesmos dados/filtros da aba
        # "Tabela"). Nome do arquivo: "rejeitados-" +
        # ano/mês/dia + hora/minuto da geração.
        # ==============================================
        
        output$csv_ui <- renderUI({
            
            df <- filtrado()
            
            tem_dados <- !is.null(df) && nrow(df) > 0
            
            div(
                class = "mt-4",
                style = "max-width: 420px;",
                
                p(
                    class = "text-muted",
                    "Gera um arquivo .csv com os dados exibidos na aba \"Tabela\" (respeitando os filtros aplicados)."
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
                    "rejeitados-",
                    format(Sys.time(), "%Y%m%d%H%M"),
                    ".csv"
                )
            },
            
            content = function(file) {
                
                inicio <- Sys.time()
                
                df <- filtrado()
                
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