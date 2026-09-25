# =====================================================
# modules/mod_declaracao.R
# =====================================================

library(shiny)
library(DBI)
library(rmarkdown)

# =====================================================
# CONSULTA DOS DADOS DO SERVIDOR (TIBBLE) PARA A DECLARAÇÃO
# =====================================================
#
# Retorna uma linha (as.list) com os campos:
#   nome, matricula, cpf, cargo, lotacao, data_ingresso
#
# O filtro utiliza SEMPRE "Matricula =" com o valor
# informado pelo usuário no text box da tela.
# =====================================================

obter_dados_declaracao <- function(matricula){
  
  if(
    is.null(matricula) ||
    trimws(matricula) == ""
  ){
    
    stop("Matrícula não informada.")
    
  }
  
  con <- conectar_banco()
  
  on.exit(
    dbDisconnect(con),
    add = TRUE
  )
  
  sql <- "
    SELECT
    	Nome AS nome,
    	Matricula AS matricula,
    	Pessoa->CPFFormatado AS cpf,
    	CASE 
    		WHEN Funcional->CargoEfetivo IS NOT NULL THEN Funcional->CargoEfetivo->Descricao
    		ELSE Funcional->TipoServidor->Descricao
    	END AS cargo,
    	Funcional->LotacaoContracheque->Descricao AS lotacao,
    	Funcional->DataIngOrgaoFormatada AS data_ingresso,
    	'Tempo_vinculo' AS tempo_vinculo,
    	'Tempo_servico_publico' AS tempo_servico_publico,
    	'Tempo_total_contribuicao' AS tempo_contribuicao,
    	Financeiro->RegimePrev->Descricao AS regime_previdenciario,
    	'R$_Valor_contribuicao' AS valor_contribuicao
    FROM
    	RHCadServidor
    WHERE
    	Matricula = ?
  "
  
  # ---------------------------------------------------
  # Consulta parametrizada (evita injeção de SQL e
  # garante que o WHERE use exatamente o valor informado
  # no text box de matrícula)
  #
  # IMPORTANTE: com o driver JDBC do IRIS/Caché (via
  # RJDBC), o bind SÓ funciona como PreparedStatement
  # quando o parâmetro é passado diretamente na chamada
  # de dbGetQuery/dbSendQuery. Usar dbSendQuery() +
  # dbBind() separadamente faz o driver cair para um
  # Statement comum (sem suporte a "?"), gerando o erro:
  # "Parameters not allowed in Statement class".
  # ---------------------------------------------------
  
  dados <- dbGetQuery(
    con,
    sql,
    trimws(matricula)
  )
  
  if(nrow(dados) == 0){
    
    stop(
      paste(
        "Nenhum registro encontrado para a matrícula",
        matricula
      )
    )
    
  }
  
  # garante apenas o primeiro registro, já como lista
  # nomeada (compatível com params do RMarkdown)
  
  registro <- as.list(dados[1, ])
  
  list(
    nome                  = as.character(registro$nome),
    matricula             = as.character(registro$matricula),
    cpf                   = as.character(registro$cpf),
    cargo                 = as.character(registro$cargo),
    lotacao               = as.character(registro$lotacao),
    data_ingresso         = as.character(registro$data_ingresso),
    tempo_vinculo         = as.character(registro$tempo_vinculo),
    tempo_servico_publico = as.character(registro$tempo_servico_publico),
    tempo_contribuicao    = as.character(registro$tempo_contribuicao),
    regime_previdenciario = as.character(registro$regime_previdenciario),
    valor_contribuicao    = as.character(registro$valor_contribuicao)
  )
  
}

# =====================================================
# UI
# =====================================================

mod_declaracao_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    # ---------------------------------------------
    # Script para restringir o campo de matrícula
    # a apenas dígitos numéricos (bloqueia teclas
    # não numéricas e remove qualquer caractere não
    # numérico que venha de colar/arrastar texto)
    # ---------------------------------------------
    
    tags$head(
      tags$script(
        HTML(
          sprintf(
            "
            $(document).on('keypress', '#%s', function(e){
              var charCode = e.which ? e.which : e.keyCode;
              if(charCode < 48 || charCode > 57){
                e.preventDefault();
              }
            });

            $(document).on('input', '#%s', function(e){
              var valorLimpo = $(this).val().replace(/[^0-9]/g, '');
              if($(this).val() !== valorLimpo){
                $(this).val(valorLimpo);
              }
            });
            ",
            ns("matricula"),
            ns("matricula")
          )
        )
      )
    ),
    
    fluidRow(
      
      column(
        12,
        
        h3("Declaração de Vínculo Funcional"),
        
        p(
          "Informe a matrícula para gerar a declaração de vínculo",
          "funcional, tempo de serviço e contribuição previdenciária."
        ),
        
        textInput(
          ns("matricula"),
          "Matrícula:",
          value     = "",
          placeholder = "Somente números"
        ),
        
        actionButton(
          ns("gerar"),
          "Gerar Declaração",
          class = "btn btn-primary"
        ),
        
        br(),
        br(),
        
        uiOutput(
          ns("resumo")
        ),
        
        uiOutput(
          ns("download_ui")
        )
        
      )
      
    )
    
  )
  
}

# =====================================================
# SERVER
# =====================================================
#
# A matrícula utilizada na consulta agora vem do
# text box (input$matricula) preenchido pelo usuário
# na tela, e não mais do usuário autenticado no login.
# =====================================================

mod_declaracao_server <- function(id){
  
  moduleServer(
    
    id,
    
    function(
    input,
    output,
    session
    ){
      
      dados_declaracao <- reactiveVal(NULL)
      
      # ---------------------------------------------
      # GERAR (executa a consulta SQL a partir da
      # matrícula digitada no text box)
      # ---------------------------------------------
      
      observeEvent(
        input$gerar,
        {
          
          matricula_informada <- trimws(input$matricula)
          
          # -----------------------------------------
          # Validação: campo vazio ou com caracteres
          # não numéricos (segurança extra, além do
          # bloqueio via JavaScript no campo)
          # -----------------------------------------
          
          if(
            matricula_informada == "" ||
            !grepl(
              "^[0-9]+$",
              matricula_informada
            )
          ){
            
            dados_declaracao(NULL)
            
            showModal(
              modalDialog(
                title = "Matrícula inválida",
                "Informe uma matrícula válida, contendo apenas números.",
                easyClose = TRUE,
                footer = modalButton("Fechar")
              )
            )
            
            return()
            
          }
          
          tryCatch({
            
            dados <- obter_dados_declaracao(
              matricula_informada
            )
            
            dados_declaracao(dados)
            
            showNotification(
              "Declaração gerada com sucesso.",
              type = "message"
            )
            
          },
          
          error = function(e){
            
            dados_declaracao(NULL)
            
            showModal(
              modalDialog(
                title = "Não foi possível gerar a declaração",
                paste(
                  "Não foi possível localizar os dados para a matrícula informada.",
                  "Verifique o número digitado e tente novamente."
                ),
                easyClose = TRUE,
                footer = modalButton("Fechar")
              )
            )
            
          })
          
        }
        
      )
      
      # ---------------------------------------------
      # RESUMO NA TELA
      # ---------------------------------------------
      
      output$resumo <- renderUI({
        
        req(dados_declaracao())
        
        dados <- dados_declaracao()
        
        tagList(
          tags$p(strong("Nome: "), dados$nome),
          tags$p(strong("Matrícula: "), dados$matricula),
          tags$p(strong("Cargo: "), dados$cargo),
          tags$p(strong("Lotação: "), dados$lotacao)
        )
        
      })
      
      # ---------------------------------------------
      # BOTÃO DE DOWNLOAD (só aparece após gerar)
      # ---------------------------------------------
      
      output$download_ui <- renderUI({
        
        req(dados_declaracao())
        
        downloadButton(
          session$ns("baixar"),
          "Baixar Declaração",
          class = "btn btn-success"
        )
        
      })
      
      # ---------------------------------------------
      # RENDERIZAÇÃO DO RMARKDOWN COM OS PARAMS
      # VINDOS DA TIBBLE (nome, matricula, cpf, cargo,
      # lotacao, data_ingresso)
      # ---------------------------------------------
      
      output$baixar <- downloadHandler(
        
        filename = function(){
          
          req(dados_declaracao())
          
          paste0(
            "declaracao_",
            dados_declaracao()$matricula,
            ".html"
          )
          
        },
        
        content = function(file){
          
          dados <- dados_declaracao()
          
          rmarkdown::render(
            input = "templates/declaracao.Rmd",
            output_file = file,
            params = list(
              nome                  = dados$nome,
              matricula             = dados$matricula,
              cpf                   = dados$cpf,
              cargo                 = dados$cargo,
              lotacao               = dados$lotacao,
              data_ingresso         = dados$data_ingresso,
              tempo_vinculo         = dados$tempo_vinculo,
              tempo_servico_publico = dados$tempo_servico_publico,
              tempo_contribuicao    = dados$tempo_contribuicao,
              regime_previdenciario = dados$regime_previdenciario,
              valor_contribuicao    = dados$valor_contribuicao
            ),
            envir = new.env(
              parent = globalenv()
            )
          )
          
        }
        
      )
      
    }
    
  )
  
}

# # =====================================================
# # modules/mod_declaracao.R
# # =====================================================
# 
# library(shiny)
# library(DBI)
# library(rmarkdown)
# 
# # =====================================================
# # CONSULTA DOS DADOS DO SERVIDOR (TIBBLE) PARA A DECLARAÇÃO
# # =====================================================
# #
# # Retorna uma linha (as.list) com os campos:
# #   nome, matricula, cpf, cargo, lotacao, data_ingresso
# #
# # O filtro utiliza SEMPRE "Matricula =" com o valor do
# # usuário autenticado (input$usuario da tela de login).
# # =====================================================
# 
# obter_dados_declaracao <- function(matricula){
#   
#   if(
#     is.null(matricula) ||
#     trimws(matricula) == ""
#   ){
#     
#     stop("Matrícula do usuário não informada.")
#     
#   }
#   
#   con <- conectar_banco()
#   
#   on.exit(
#     dbDisconnect(con),
#     add = TRUE
#   )
#   
#   sql <- "
#     SELECT
#       Nome AS nome,
#       Matricula AS matricula,
#       Pessoa->CPFFormatado AS cpf,
#       Funcional->CargoEfetivo->Descricao AS cargo,
#       Funcional->LotacaoContracheque->Descricao AS lotacao,
#       Funcional->DataIngOrgaoFormatada AS data_ingresso,
#      'Tempo_vinculo' AS tempo_vinculo,
#      'Tempo_servico_publico' AS tempo_servico_publico,
#      'Tempo_total_contribuicao' AS tempo_contribuicao,
#       Financeiro->RegimePrev->Descricao AS regime_previdenciario,
#      'R$_Valor_contribuicao' AS valor_contribuicao
#     FROM
#       RHCadServidor
#     WHERE
#       Matricula = ?
#   "
#   
#   # ---------------------------------------------------
#   # Consulta parametrizada (evita injeção de SQL e
#   # garante que o WHERE use exatamente o valor de
#   # input$usuario recebido no login)
#   #
#   # IMPORTANTE: com o driver JDBC do IRIS/Caché (via
#   # RJDBC), o bind SÓ funciona como PreparedStatement
#   # quando o parâmetro é passado diretamente na chamada
#   # de dbGetQuery/dbSendQuery. Usar dbSendQuery() +
#   # dbBind() separadamente faz o driver cair para um
#   # Statement comum (sem suporte a "?"), gerando o erro:
#   # "Parameters not allowed in Statement class".
#   # ---------------------------------------------------
#   
#   dados <- dbGetQuery(
#     con,
#     sql,
#     trimws(matricula)
#   )
#   
#   if(nrow(dados) == 0){
#     
#     stop(
#       paste(
#         "Nenhum registro encontrado para a matrícula",
#         matricula
#       )
#     )
#     
#   }
#   
#   # garante apenas o primeiro registro, já como lista
#   # nomeada (compatível com params do RMarkdown)
#   
#   registro <- as.list(dados[1, ])
#   
#   list(
#     nome                  = as.character(registro$nome),
#     matricula             = as.character(registro$matricula),
#     cpf                   = as.character(registro$cpf),
#     cargo                 = as.character(registro$cargo),
#     lotacao               = as.character(registro$lotacao),
#     data_ingresso         = as.character(registro$data_ingresso),
#     tempo_vinculo         = as.character(registro$tempo_vinculo),
#     tempo_servico_publico = as.character(registro$tempo_servico_publico),
#     tempo_contribuicao    = as.character(registro$tempo_contribuicao),
#     regime_previdenciario = as.character(registro$regime_previdenciario),
#     valor_contribuicao    = as.character(registro$valor_contribuicao)
#   )
#   
# }
# 
# # =====================================================
# # UI
# # =====================================================
# 
# mod_declaracao_ui <- function(id){
#   
#   ns <- NS(id)
#   
#   fluidPage(
#     
#     fluidRow(
#       
#       column(
#         12,
#         
#         h3("Declaração de Vínculo Funcional"),
#         
#         p(
#           "Gera a declaração de vínculo funcional, tempo de serviço",
#           "e contribuição previdenciária do usuário autenticado."
#         ),
#         
#         actionButton(
#           ns("gerar"),
#           "Gerar Declaração",
#           class = "btn btn-primary"
#         ),
#         
#         br(),
#         br(),
#         
#         uiOutput(
#           ns("resumo")
#         ),
#         
#         uiOutput(
#           ns("download_ui")
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
# #
# # matricula_usuario: reactive que retorna a matrícula do
# # usuário autenticado. Em app.R, corresponde ao valor
# # digitado em input$usuario no login (usuarioLogado()).
# # =====================================================
# 
# mod_declaracao_server <- function(
#     id,
#     matricula_usuario
# ){
#   
#   moduleServer(
#     
#     id,
#     
#     function(
#     input,
#     output,
#     session
#     ){
#       
#       dados_declaracao <- reactiveVal(NULL)
#       
#       # ---------------------------------------------
#       # GERAR (executa a consulta SQL)
#       # ---------------------------------------------
#       
#       observeEvent(
#         input$gerar,
#         {
#           
#           tryCatch({
#             
#             dados <- obter_dados_declaracao(
#               matricula_usuario()
#             )
#             
#             dados_declaracao(dados)
#             
#             showNotification(
#               "Declaração gerada com sucesso.",
#               type = "message"
#             )
#             
#           },
#           
#           error = function(e){
#             
#             dados_declaracao(NULL)
#             
#             showNotification(
#               e$message,
#               type = "error",
#               duration = NULL
#             )
#             
#           })
#           
#         }
#         
#       )
#       
#       # ---------------------------------------------
#       # RESUMO NA TELA
#       # ---------------------------------------------
#       
#       output$resumo <- renderUI({
#         
#         req(dados_declaracao())
#         
#         dados <- dados_declaracao()
#         
#         tagList(
#           tags$p(strong("Nome: "), dados$nome),
#           tags$p(strong("Matrícula: "), dados$matricula),
#           tags$p(strong("Cargo: "), dados$cargo),
#           tags$p(strong("Lotação: "), dados$lotacao)
#         )
#         
#       })
#       
#       # ---------------------------------------------
#       # BOTÃO DE DOWNLOAD (só aparece após gerar)
#       # ---------------------------------------------
#       
#       output$download_ui <- renderUI({
#         
#         req(dados_declaracao())
#         
#         downloadButton(
#           session$ns("baixar"),
#           "Baixar Declaração",
#           class = "btn btn-success"
#         )
#         
#       })
#       
#       # ---------------------------------------------
#       # RENDERIZAÇÃO DO RMARKDOWN COM OS PARAMS
#       # VINDOS DA TIBBLE (nome, matricula, cpf, cargo,
#       # lotacao, data_ingresso)
#       # ---------------------------------------------
#       
#       output$baixar <- downloadHandler(
#         
#         filename = function(){
#           
#           req(dados_declaracao())
#           
#           paste0(
#             "declaracao_",
#             dados_declaracao()$matricula,
#             ".html"
#           )
#           
#         },
#         
#         content = function(file){
#           
#           dados <- dados_declaracao()
#           
#           rmarkdown::render(
#             input = "templates/declaracao.Rmd",
#             output_file = file,
#             params = list(
#               nome                  = dados$nome,
#               matricula             = dados$matricula,
#               cpf                   = dados$cpf,
#               cargo                 = dados$cargo,
#               lotacao               = dados$lotacao,
#               data_ingresso         = dados$data_ingresso,
#               tempo_vinculo         = dados$tempo_vinculo,
#               tempo_servico_publico = dados$tempo_servico_publico,
#               tempo_contribuicao    = dados$tempo_contribuicao,
#               regime_previdenciario = dados$regime_previdenciario,
#               valor_contribuicao    = dados$valor_contribuicao
#             ),
#             envir = new.env(
#               parent = globalenv()
#             )
#           )
#           
#         }
#         
#       )
#       
#     }
#     
#   )
#   
# }