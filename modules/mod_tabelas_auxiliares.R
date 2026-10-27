# =====================================================
# modules/mod_tabelas_auxiliares.R
# -----------------------------------------------------
# Tela "Tabelas Auxiliares" (Configurações > Tabelas Auxiliares).
#
# Mostra as tabelas auxiliares do MPM gravadas no SQLite (cargos e
# situações de magistrados e servidores, sexo, identidade de gênero,
# raça/cor, deficiência, regime de cotas, forma de promoção, área de
# atuação, competências, municípios, tipo e classificação de unidade
# judiciária, entrância) — as mesmas usadas para trocar códigos por
# descrições nas telas. Para cada tabela: onde é usada, de qual arquivo
# veio, quando foi carregada e o conteúdo (com filtro e CSV).
#
# "Gerenciar Arquivos" (mesmo funcionamento dos outros módulos):
#   - lista os arquivos de referência (planilha do MPM e os CSVs de
#     municípios, competências e classificação), indicando se o que vale
#     é o ENVIADO ou o PADRÃO do sistema (pasta referencias/);
#   - envia um ou mais arquivos (.xlsx/.csv) — o tipo é identificado
#     pelo conteúdo (abas da planilha / cabeçalho do CSV) e o arquivo é
#     gravado com o nome fixo em <PASTA_ALERTAS>/referencias/, com
#     confirmação se já existir um enviado do mesmo tipo;
#   - apaga um arquivo enviado (volta a valer o padrão).
# Depois de enviar ou apagar, as tabelas são recarregadas no banco e nas
# telas (ver carregar_tabelas_referencia(), em R/tabelas_referencia.R).
#
# As tabelas valem para TODAS as empresas e sessões, então o Gerenciar
# Arquivos é só para administradores (`pode_gerenciar`, conferido também
# no servidor); os demais usuários só consultam.
# =====================================================

library(shiny)
library(DT)

mod_tabelas_auxiliares_ui <- function(id) {

    ns <- NS(id)

    tagList(

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

        #%1$s .ta-info {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
          gap: .35rem 1.5rem;
          font-size: .88rem;
          color: #495057;
          margin-bottom: 1rem;
        }

        #%1$s .ta-uso {
          font-size: .88rem;
          color: #495057;
          background: #f5f9ff;
          border: 1px solid #dbe7ff;
          border-radius: .6rem;
          padding: .55rem .8rem;
          margin-bottom: 1rem;
        }

        #%1$s .dataTables_wrapper {
          width: 100%% !important;
        }

        #%1$s table.dataTable {
          min-width: 100%%;
        }

        .ta-arquivo-local {
          display: inline-block;
          font-size: .7rem;
          font-weight: 600;
          border-radius: .4rem;
          padding: .05rem .4rem;
          margin-left: .35rem;
        }

        .ta-arquivo-local.enviado { background: #e7f1ff; color: #0d6efd; }
        .ta-arquivo-local.padrao { background: #eef1f4; color: #495057; }

      ", id)))
        ),

        div(
            id = id,

            div(
                class = "al-header",

                div(
                    class = "al-titulo",
                    div(class = "al-titulo-icone", icon("table-list")),
                    tags$h4("Tabelas Auxiliares")
                ),

                div(
                    class = "al-acoes",

                    actionButton(
                        ns("atualizar"),
                        tagList(icon("rotate", class = "me-2"), "Atualizar Dados"),
                        class = "btn btn-outline-primary btn-acao"
                    ),

                    uiOutput(ns("botao_gerenciar"), inline = TRUE)
                )
            ),

            div(
                class = "al-card",

                div(class = "al-card-titulo", "Tabela"),

                fluidRow(
                    column(
                        8,
                        selectInput(ns("tabela"), NULL, choices = character(0), width = "100%")
                    )
                ),

                uiOutput(ns("info_tabela"))
            ),

            div(
                class = "al-conteudo",

                tabsetPanel(
                    tabPanel(
                        tagList(icon("table", class = "me-1"), "Conteúdo"),
                        div(class = "mt-3", DTOutput(ns("conteudo")))
                    ),
                    tabPanel(
                        tagList(icon("list", class = "me-1"), "Todas as tabelas"),
                        div(class = "mt-3", DTOutput(ns("resumo")))
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

mod_tabelas_auxiliares_server <- function(id, con, ativo = reactive(TRUE),
                                          pode_gerenciar = reactive(FALSE)) {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        # Incrementado a cada envio/exclusão/atualização: relê o resumo e
        # o conteúdo do banco.
        versao <- reactiveVal(0)

        resumo <- reactive({
            req(ativo())
            versao()
            listar_tabelas_referencia(con)
        })

        # Lista de tabelas (mantém a escolhida ao atualizar).
        observeEvent(resumo(), {
            r <- resumo()
            escolhas <- setNames(r$tabela, sprintf("%s (%d registro(s))", r$titulo, r$registros))
            atual <- isolate(input$tabela)
            updateSelectInput(
                session, "tabela",
                choices = escolhas,
                selected = if (!is.null(atual) && atual %in% r$tabela) atual else r$tabela[1]
            )
        })

        tabela_atual <- reactive({
            req(ativo(), input$tabela)
            versao()
            ler_tabela_referencia(con, input$tabela)
        })

        formatar_carga <- function(x) {
            dt <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
            ifelse(is.na(dt), "—", format(dt, "%d/%m/%Y %H:%M", tz = FUSO_AUDITORIA))
        }

        rotulo_local <- function(local) {
            if (identical(local, "enviado")) {
                tags$span(class = "ta-arquivo-local enviado", "enviado")
            } else if (identical(local, "padrao")) {
                tags$span(class = "ta-arquivo-local padrao", "padrão do sistema")
            }
        }

        output$info_tabela <- renderUI({
            r <- resumo()
            req(input$tabela)
            l <- r[r$tabela == input$tabela, , drop = FALSE]
            req(nrow(l) == 1)

            tagList(
                div(class = "ta-uso", icon("circle-info", class = "me-1"), tags$b("Onde é usada: "), l$uso),
                div(
                    class = "ta-info",
                    div(tags$b("Tabela no banco: "), tags$code(l$tabela)),
                    div(tags$b("Registros: "), l$registros),
                    div(tags$b("Arquivo de origem: "), if (is.na(l$arquivo)) "—" else l$arquivo, rotulo_local(l$local)),
                    div(tags$b("Carregada em: "), formatar_carga(l$carregado_em))
                )
            )
        })

        nomes_colunas <- c(
            codigo = "Código", descricao = "Descrição", sigla = "Sigla",
            tipo_unidade = "Tipo de unidade", uf = "UF", cod_ibge = "Código IBGE"
        )

        exibicao <- reactive({
            d <- tabela_atual()
            names(d) <- ifelse(names(d) %in% names(nomes_colunas), nomes_colunas[names(d)], names(d))
            d
        })

        output$conteudo <- renderDT({
            d <- exibicao()
            shiny::validate(need(nrow(d) > 0, "Tabela vazia: envie o arquivo pelo botão \"Gerenciar Arquivos\"."))
            tabela_dt(d)
        })

        output$resumo <- renderDT({
            r <- resumo()
            tabela_dt(data.frame(
                Tabela = r$titulo,
                `No banco` = r$tabela,
                Registros = r$registros,
                `Onde é usada` = r$uso,
                `Arquivo de origem` = ifelse(is.na(r$arquivo), "—",
                                             paste0(r$arquivo, ifelse(r$local == "enviado", " (enviado)", " (padrão)"))),
                `Carregada em` = formatar_carga(r$carregado_em),
                check.names = FALSE
            ))
        })

        output$csv_ui <- renderUI({
            ui_csv_dados(ns, "download_csv", nrow(exibicao()) > 0)
        })

        output$download_csv <- downloadHandler(
            filename = function() {
                paste0(sub("^tab_", "", input$tabela %||% "tabela"), "-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
            },
            content = function(file) {
                gravar_csv_tabela(exibicao(), file, input$conteudo_rows_all)
            }
        )

        recarregar <- function(forcar = FALSE) {
            atualizadas <- carregar_tabelas_referencia(con, forcar = forcar)
            versao(versao() + 1)
            atualizadas
        }

        observeEvent(input$atualizar, {
            req(ativo())
            inicio <- Sys.time()
            recarregar()
            showNotification(sprintf("Tabelas atualizadas em %.2fs.", tempo_decorrido(inicio)), type = "message")
        }, ignoreInit = TRUE)

        # =================================================
        # GERENCIAR ARQUIVOS (só administradores)
        # =================================================

        output$botao_gerenciar <- renderUI({
            if (isTRUE(pode_gerenciar())) {
                actionButton(
                    ns("gerenciar_arquivos"),
                    tagList(icon("folder-open", class = "me-2"), "Gerenciar Arquivos"),
                    class = "btn btn-primary btn-acao"
                )
            }
        })

        permitido <- function() {
            isTRUE(ativo()) && isTRUE(pode_gerenciar())
        }

        # ID novo do fileInput a cada abertura do modal (mesmo motivo dos
        # outros módulos: não reenviar uma seleção anterior).
        contador_upload <- reactiveVal(0)
        id_upload_atual <- function() paste0("upload_referencias_", contador_upload())
        entrada_upload_atual <- function() input[[id_upload_atual()]]

        linha_arquivo_modal <- function(chave) {

            info <- ARQUIVOS_REFERENCIA[[chave]]
            enviado <- file.path(PASTA_REFERENCIAS_ENVIADAS, info$arquivo)
            padrao <- file.path(PASTA_REFERENCIAS, info$arquivo)
            tem_enviado <- file.exists(enviado)
            tem_padrao <- file.exists(padrao)

            div(
                class = "srv-arquivo-linha",
                div(
                    div(
                        class = "srv-arquivo-nome",
                        icon(if (tools::file_ext(info$arquivo) == "xlsx") "file-excel" else "file-csv", class = "me-1"),
                        info$arquivo,
                        rotulo_local(if (tem_enviado) "enviado" else if (tem_padrao) "padrao")
                    ),
                    div(
                        class = "srv-arquivo-info",
                        info$rotulo, " — ",
                        if (tem_enviado) {
                            sprintf("enviado em %s", format(file.info(enviado)$mtime, "%d/%m/%Y %H:%M"))
                        } else if (tem_padrao) {
                            "valendo o arquivo padrão do sistema"
                        } else {
                            "ainda não enviado"
                        }
                    )
                ),
                if (tem_enviado) {
                    actionButton(
                        ns(paste0("apagar_", chave)),
                        tagList(icon("trash", class = "me-1"), "Apagar"),
                        class = "btn btn-outline-danger btn-sm",
                        title = "Apaga o arquivo enviado; volta a valer o padrão do sistema"
                    )
                }
            )

        }

        abrir_gerenciar_arquivos <- function() {

            req(permitido())

            contador_upload(contador_upload() + 1)

            showModal(modalDialog(
                title = div(
                    style = "position:relative; padding-right:28px;",
                    icon("folder-open", class = "me-2"),
                    "Gerenciar Arquivos das Tabelas Auxiliares",
                    tags$button(
                        type = "button",
                        class = "btn-close",
                        style = "position:absolute; top:2px; right:0;",
                        `aria-label` = "Fechar",
                        onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
                            ns("fechar_modal")
                        )
                    )
                ),
                size = "m",
                easyClose = TRUE,

                div(
                    class = "mb-4",
                    div(class = "al-modal-secao-titulo", icon("folder", class = "text-secondary"), "Arquivos"),
                    lapply(names(ARQUIVOS_REFERENCIA), linha_arquivo_modal)
                ),

                tags$hr(),

                div(
                    div(class = "al-modal-secao-titulo", icon("upload", class = "text-primary"), "Enviar arquivos"),
                    div(
                        class = "al-modal-secao-desc",
                        "Selecione a planilha ", tags$b("Tabelas auxiliares do MPM"), " (.xlsx, uma aba por tabela) ",
                        "e/ou os CSVs de ", tags$b("municípios"), ", ", tags$b("competências"), " e ",
                        tags$b("tipo/classificação de unidade judiciária"), " (separados por \";\"). ",
                        "O tipo é identificado pelo conteúdo (abas ou cabeçalho), e o arquivo é gravado com o nome ",
                        "padrão, substituindo o enviado anterior do mesmo tipo. As tabelas são recarregadas ",
                        "em seguida e valem para todas as empresas."
                    ),
                    fileInput(
                        ns(id_upload_atual()),
                        NULL,
                        multiple = TRUE,
                        accept = c(".csv", ".xlsx", "text/csv",
                                   "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                        width = "100%",
                        buttonLabel = "Procurar...",
                        placeholder = "Nenhum arquivo selecionado"
                    ),
                    actionButton(
                        ns("enviar_arquivos"),
                        tagList(icon("upload", class = "me-2"), "Enviar"),
                        class = "btn btn-primary w-100"
                    )
                ),

                footer = actionButton(ns("fechar_modal"), "Fechar")
            ))

        }

        observeEvent(input$gerenciar_arquivos, abrir_gerenciar_arquivos(), ignoreInit = TRUE)

        observeEvent(input$fechar_modal, {
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
        }, ignoreInit = TRUE)

        notificar_recarga <- function(atualizadas, inicio, prefixo) {
            titulos <- vapply(TABELAS_REFERENCIA[atualizadas], `[[`, character(1), "titulo")
            showNotification(
                sprintf(
                    "%s em %.2fs. %d tabela(s) recarregada(s)%s",
                    prefixo, tempo_decorrido(inicio), length(atualizadas),
                    if (length(titulos) > 0) paste0(": ", paste(titulos, collapse = ", "), ".") else "."
                ),
                type = "message",
                duration = 10
            )
        }

        # ---- Apagar (só o enviado; o padrão do sistema não é apagado) ----

        tipo_para_apagar <- reactiveVal(NULL)

        pedir_confirmacao_apagar <- function(chave) {
            req(permitido())
            arq <- file.path(PASTA_REFERENCIAS_ENVIADAS, ARQUIVOS_REFERENCIA[[chave]]$arquivo)
            if (!file.exists(arq)) {
                showNotification("Não há arquivo enviado desse tipo.", type = "warning")
                return(invisible(NULL))
            }
            tipo_para_apagar(chave)
            removeModal()
            showModal(modalDialog(
                title = "Confirmar exclusão",
                sprintf(
                    "Apagar o arquivo enviado %s? As tabelas que vêm dele voltam a usar o arquivo padrão do sistema%s.",
                    basename(arq),
                    if (file.exists(file.path(PASTA_REFERENCIAS, basename(arq)))) "" else " (não há padrão: o conteúdo atual do banco é mantido)"
                ),
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(ns("confirmar_apagar"), "Apagar", class = "btn-danger")
                )
            ))
        }

        for (chave in names(ARQUIVOS_REFERENCIA)) {
            local({
                k <- chave
                observeEvent(input[[paste0("apagar_", k)]], pedir_confirmacao_apagar(k), ignoreInit = TRUE)
            })
        }

        observeEvent(input$confirmar_apagar, {
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
            req(permitido(), tipo_para_apagar())

            arq <- file.path(PASTA_REFERENCIAS_ENVIADAS, ARQUIVOS_REFERENCIA[[tipo_para_apagar()]]$arquivo)
            tipo_para_apagar(NULL)
            inicio <- Sys.time()

            if (!isTRUE(file.remove(arq))) {
                showNotification(sprintf("O arquivo %s não pôde ser apagado.", basename(arq)), type = "error")
                return(invisible(NULL))
            }

            notificar_recarga(recarregar(), inicio, sprintf("Arquivo %s apagado", basename(arq)))
        }, ignoreInit = TRUE)

        # ---- Enviar ----

        upload_pendente <- reactiveVal(NULL)

        processar_upload <- function(pendente) {

            req(permitido(), pendente)
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())

            inicio <- Sys.time()
            dir.create(PASTA_REFERENCIAS_ENVIADAS, recursive = TRUE, showWarnings = FALSE)
            gravados <- character(0)

            for (i in seq_len(nrow(pendente))) {
                destino <- file.path(PASTA_REFERENCIAS_ENVIADAS, ARQUIVOS_REFERENCIA[[pendente$tipo[i]]]$arquivo)
                if (gravar_arquivo_substituindo(pendente$datapath[i], destino)) {
                    gravados <- c(gravados, sprintf("%s → %s", pendente$name[i], basename(destino)))
                } else {
                    showNotification(sprintf("O arquivo %s não pôde ser gravado.", pendente$name[i]), type = "error")
                }
            }

            upload_pendente(NULL)

            if (length(gravados) > 0) {
                notificar_recarga(recarregar(), inicio, paste0("Enviado(s): ", paste(gravados, collapse = "; ")))
            }

        }

        observeEvent(input$enviar_arquivos, {

            req(permitido())
            up <- entrada_upload_atual()
            req(up)

            extensoes <- tolower(tools::file_ext(up$name))
            nomes_seguros <- nome_arquivo_seguro(up$name, c("xlsx", "csv"))

            tipos <- vapply(seq_len(nrow(up)), function(i) {
                if (is.na(nomes_seguros[i])) return(NA_character_)
                identificar_arquivo_referencia(up$datapath[i], extensoes[i])
            }, character(1))

            invalidos <- up$name[is.na(tipos)]

            if (length(invalidos) > 0) {
                showNotification(
                    sprintf(
                        paste0("Não reconhecido(s) como tabela auxiliar (ignorado[s]): %s. A planilha precisa ter as abas do MPM ",
                               "(ex.: \"Cargos magistrados\", \"Sexo\", \"Raça_cor\"); os CSVs, os cabeçalhos ",
                               "UF;seq_municipio;nome;cod_ibge, id;descricao ou tipo_unidade;id;descricao."),
                        paste(invalidos, collapse = ", ")
                    ),
                    type = "error",
                    duration = 15
                )
            }

            validos <- !is.na(tipos)
            if (!any(validos)) return(invisible(NULL))

            if (any(duplicated(tipos[validos]))) {
                showNotification("Foram selecionados dois arquivos do mesmo tipo. Envie no máximo um de cada.",
                                 type = "error", duration = 12)
                return(invisible(NULL))
            }

            pendente <- data.frame(name = up$name[validos], datapath = up$datapath[validos],
                                   tipo = tipos[validos], stringsAsFactors = FALSE)
            upload_pendente(pendente)

            existentes <- pendente$tipo[file.exists(file.path(
                PASTA_REFERENCIAS_ENVIADAS,
                vapply(pendente$tipo, function(t) ARQUIVOS_REFERENCIA[[t]]$arquivo, character(1))
            ))]

            if (length(existentes) > 0) {
                removeModal()
                showModal(modalDialog(
                    title = "Arquivo(s) já enviado(s)",
                    sprintf(
                        "Já existe arquivo enviado de: %s. Deseja substituir pelo(s) selecionado(s)?",
                        paste(vapply(existentes, function(t) ARQUIVOS_REFERENCIA[[t]]$arquivo, character(1)), collapse = ", ")
                    ),
                    footer = tagList(
                        modalButton("Cancelar"),
                        actionButton(ns("confirmar_substituir"), "Substituir", class = "btn-danger")
                    )
                ))
            } else {
                processar_upload(pendente)
            }

        }, ignoreInit = TRUE)

        observeEvent(input$confirmar_substituir, {
            processar_upload(upload_pendente())
        }, ignoreInit = TRUE)

    })

}
