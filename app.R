# =========================================================================
# app.R — Radar Social (Formato HTML)
# -------------------------------------------------------------------------
# Login corporativo (Active Directory) ou código Authenticator (TOTP),
# seguindo o mesmo layout/fluxo de autenticação usado em outros sistemas
# internos, adaptado para a aplicação Declaraserv.
#
# Corrige o diretório de trabalho caso o projeto não
# tenha sido aberto pelo .Rproj
# =========================================================================

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)), silent = TRUE)
}

library(here)
here::i_am("app.R")

readRenviron(here::here(".Renviron"))

library(shiny)
library(bslib)
library(DT)
library(jsonlite)
library(digest)

library(DBI)
library(RSQLite)

library(reticulate)

python_path <- Sys.getenv("RETICULATE_PYTHON", unset = Sys.which("python"))
if (!nzchar(python_path) || !file.exists(python_path)) {
    stop(
        "Python não encontrado em '", python_path, "'. ",
        "Verifique a instalação do Python ou defina RETICULATE_PYTHON no .Renviron ",
        "apontando para o python.exe correto."
    )
}
use_python(python_path, required = TRUE)

# =====================================================
# CARREGA CONEXAO DB PARA LOGIN COM AUTH
# =====================================================
con <- dbConnect(
    SQLite(),
    "data/radarsocial.db"
)

# =========================================================================
# MÓDULOS DE AUTENTICAÇÃO, BANCO E UTILITÁRIOS
# =========================================================================
source(here::here("R", "utils.R"))
source(here::here("R", "auth.R"))
source(here::here("R", "auth_totp.R"))
source(here::here("R", "database.R"))
source(here::here("modules", "mod_totp_admin.R"))
source(here::here("modules", "mod_usuario.R"))

source(here::here("modules", "mod_rejeitados.R"))
source(here::here("modules", "mod_inconsistencias.R"))
source(here::here("modules", "mod_totalizadores.R"))

# =========================================================================
# MIGRAÇÃO DE ESQUEMA (coluna "distro" em usuarios_totp)
# -------------------------------------------------------------------------
# Precisa rodar logo após abrir a conexão e depois de source("auth_totp.R"),
# de onde vem garantir_schema_totp().
# =========================================================================
garantir_schema_totp(con)

# =========================================================================
# EMPRESAS (MULTI-DISTRO)
# -------------------------------------------------------------------------
# Lista de empresas/unidades configuradas em DISTRO_1, DISTRO_2... no
# .Renviron (ver listar_distros() em R/utils.R). Cada uma tem sua própria
# configuração de LDAP ({DISTRO}_LDAP_*). Adicionar uma nova empresa não
# exige alterar este arquivo nem R/auth.R — só DISTRO_N + as variáveis
# correspondentes no .Renviron.
# =========================================================================
DISTROS_DISPONIVEIS <- listar_distros()

if (length(DISTROS_DISPONIVEIS) == 0) {
    warning(
        "Nenhuma empresa configurada em DISTRO_1, DISTRO_2... no ",
        ".Renviron. O login corporativo (AD) ficará sem opções até isso ",
        "ser configurado."
    )
}

# Avisa (sem interromper) sobre configuração LDAP incompleta de cada
# empresa — o app ainda pode ser usado normalmente para as empresas que
# estiverem corretamente configuradas.
for (distro_check in DISTROS_DISPONIVEIS) {
    
    variaveis_esperadas <- c(
        distro_env(distro_check, "LDAP_SERVER"),
        distro_env(distro_check, "LDAP_PORT"),
        distro_env(distro_check, "LDAP_DOMAIN"),
        distro_env(distro_check, "LDAP_SEARCH_BASE")
    )
    
    faltando <- variaveis_esperadas[
        Sys.getenv(variaveis_esperadas, unset = "") == ""
    ]
    
    if (length(faltando) > 0) {
        warning(
            "Configuração LDAP incompleta para a empresa '", distro_check,
            "' no .Renviron: ", paste(faltando, collapse = ", "),
            ". O login corporativo (AD) dessa empresa falhará até isso ",
            "ser definido."
        )
    }
}
rm(distro_check)

# =====================================================
# RECURSOS ESTÁTICOS
# =====================================================

addResourcePath("img", "img")

# =========================================================================
# README.md — exibido em janela modal a partir da tela principal
# =========================================================================
README_PATH <- here::here("README.md")

if (!file.exists(README_PATH)) {
    warning(
        "Arquivo README.md não encontrado em: ", README_PATH,
        ". O botão de ajuda exibirá uma mensagem informando que o ",
        "arquivo não está disponível."
    )
}

# =====================================================
# UI
# =====================================================

ui <- fluidPage(
    
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly"
    ),
    
    tags$head(
        
        tags$style(HTML("

      body {
        background: #f4f6f9;
      }

      /* =============================================
         LOGIN - CARD ENTERPRISE
         ============================================= */

      .login-wrapper {
        min-height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 40px 20px;
      }

      .login-card {
        width: 100%;
        max-width: 460px;
        background: #fff;
        border: 1px solid rgba(0,0,0,.05);
        border-radius: 1rem;
      }

      .login-icon-badge {
        width: 60px;
        height: 60px;
        border-radius: 50%;
        margin: 0 auto;
        display: flex;
        align-items: center;
        justify-content: center;
        background: linear-gradient(135deg, #003366, #0d6efd);
        color: #fff;
        font-size: 1.4rem;
        box-shadow: 0 .4rem 1rem rgba(13,110,253,.25);
      }

      .login-title {
        text-align: center;
        letter-spacing: -.01em;
      }

      .login-subtitle {
        text-align: center;
        font-size: .9rem;
      }

      /* Cartões de método de acesso (radioButtons estilizado) —
         mesma largura do botão .btn-acesso (Continuar); a altura
         permanece livre, de acordo com o conteúdo (ícone, título e
         descrição). */

      .metodo-opcoes .radio {
        margin-bottom: .85rem;
      }

      .metodo-opcoes .radio label {
        display: flex;
        align-items: flex-start;
        gap: .85rem;
        width: 100%;
        margin: 0;
        border: 1.5px solid #e2e6ea;
        border-radius: .85rem;
        padding: 1rem 1.1rem;
        cursor: pointer;
        box-sizing: border-box;
        transition: border-color .15s ease,
                    background-color .15s ease,
                    box-shadow .15s ease,
                    transform .1s ease;
      }

      .metodo-opcoes .radio label:hover {
        border-color: #8fb8ff;
        background: #f5f9ff;
        box-shadow: 0 .25rem .75rem rgba(13,110,253,.08);
        transform: translateY(-1px);
      }

      .metodo-opcoes .radio input[type=radio] {
        margin-top: .3rem;
        accent-color: #0d6efd;
        width: 1.05rem;
        height: 1.05rem;
        flex-shrink: 0;
      }

      .metodo-opcoes .radio:has(input:checked) label {
        border-color: #0d6efd;
        background: #eef4ff;
        box-shadow: 0 .3rem .9rem rgba(13,110,253,.15);
      }

      .metodo-opcao-icone {
        color: #0d6efd;
        font-size: 1.15rem;
        margin-top: .1rem;
      }

      .metodo-opcao-titulo {
        font-weight: 600;
        color: #212529;
      }

      .metodo-opcao-desc {
        font-weight: 400;
        font-size: .8rem;
        color: #6c757d;
      }

      .btn-acesso {
        width: 100%;
        box-sizing: border-box;
        height: 48px;
        font-weight: 600;
        font-size: 1rem;
        border-radius: .6rem;
      }

      .voltar-link {
        font-size: .85rem;
        color: #6c757d !important;
      }

      .voltar-link:hover {
        color: #0d6efd !important;
      }

      .logo {
        text-align: center;
        font-size: 30px;
        font-weight: bold;
        color: #003366;
        margin-bottom: 25px;
      }

      .logo-login {
        display: block;
        width: 100%;
        max-width: 320px;
        height: auto;
        margin: 0 auto 30px auto;
        filter: drop-shadow(0 3px 8px rgba(0,0,0,.10));
      }

      .foto {
        width: 180px;
        border-radius: 50%;
        border: 4px solid #ddd;
      }

      .shiny-notification {
        position: fixed !important;
        top: 20px !important;
        right: 20px !important;
        left: auto !important;
        bottom: auto !important;
        transform: none !important;
      }

      #capslock_warning {
        display: none;
        margin-top: 8px;
        padding: 6px 10px;
        font-size: 13px;
        color: #842029;
        background: #f8d7da;
        border: 1px solid #f5c2c7;
        border-radius: 6px;
      }

      .icon-bar {
        position: fixed;
        top: 0;
        left: 0;
        width: 52px;
        height: 100vh;
        background: #003366;
        display: flex;
        flex-direction: column;
        align-items: center;
        padding-top: 14px;
        gap: 12px;
        z-index: 1050;
      }

      .icon-bar .icon-btn {
        width: 36px;
        height: 36px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: rgba(255,255,255,.75);
        font-size: 16px;
        border-radius: 8px;
        cursor: pointer;
        text-decoration: none !important;
        transition: background .15s ease,
                    color .15s ease;
      }

      .icon-bar .icon-btn:hover {
        background: rgba(255,255,255,.12);
        color: #fff;
      }

      .icon-bar .icon-btn.sair {
        margin-top: auto;
        margin-bottom: 14px;
        color: #ff9d9d;
      }

      .icon-bar .icon-btn.sair:hover {
        background: rgba(220,53,69,.25);
        color: #fff;
      }

      #app-content {
        margin-left: 52px;
        padding: 20px 25px;
      }

      .header-container {
        overflow: hidden;
        max-height: 220px;
        opacity: 1;
        transition: max-height .28s ease,
                    opacity .2s ease,
                    margin .28s ease;
        margin-bottom: 15px;
      }

      .header-container.collapsed {
        max-height: 0;
        opacity: 0;
        margin-bottom: 0;
      }

    ")),
        
        # =================================================
        # AVISO DE CAPS LOCK
        # =================================================
        
        tags$script(HTML("

      $(document).on(
        'keydown keyup focus',
        '#senha',
        function(event) {

          var aviso =
            document.getElementById(
              'capslock_warning'
            );

          if (!aviso) return;

          if (
            event.originalEvent &&
            typeof event.originalEvent
              .getModifierState === 'function'
          ) {

            if (
              event.originalEvent
                .getModifierState('CapsLock')
            ) {

              $(aviso).show();

            } else {

              $(aviso).hide();

            }

          }

        }
      );

      $(document).on(
        'blur',
        '#senha',
        function() {

          $('#capslock_warning').hide();

        }
      );

    ")),
        
        # =================================================
        # TOGGLE DO CABEÇALHO (client-side, preserva estado dos módulos)
        # =================================================
        
        tags$script(HTML("

      Shiny.addCustomMessageHandler(

        'toggle-header',

        function(message) {

          var header =
            document.querySelector(
              '.header-container'
            );

          if (!header) return;

          if (message.oculto) {

            header.classList.add(
              'collapsed'
            );

          } else {

            header.classList.remove(
              'collapsed'
            );

          }

        }

      );

    "))
        
    ),
    
    # ===================================================
    # LOGIN
    # ===================================================
    
    uiOutput("tela_login"),
    
    # ===================================================
    # SISTEMA PRINCIPAL
    # ===================================================
    
    uiOutput("tela_principal")
    
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {
    
    # ===================================================
    # ESTADO DA SESSÃO
    # ===================================================
    
    autenticado <- reactiveVal(FALSE)
    usuarioLogado <- reactiveVal(NULL)
    dadosUsuario <- reactiveVal(NULL)
    fotoUsuario <- reactiveVal(NULL)
    
    # Método escolhido na tela de seleção ("ad" | "totp" | NULL = seletor)
    metodoAcesso <- reactiveVal(NULL)
    
    # Método efetivamente usado no login bem-sucedido ("AD" | "TOTP")
    metodoAutenticado <- reactiveVal(NULL)
    
    # Empresa (distro) do usuário autenticado — escolhida manualmente no
    # login AD, ou herdada do cadastro TOTP (ver R/auth_totp.R).
    distroSelecionado <- reactiveVal(NULL)
    
    # ===================================================
    # ESTADO DO CABEÇALHO
    # ===================================================
    
    header_oculto <- reactiveVal(FALSE)
    
    # ===================================================
    # MÓDULO SELECIONADO
    # ===================================================
    
    menuSelecionado <- reactiveVal("Usuário")
    
    # Incrementado a cada logout — sinaliza para mod_totp_admin_server()
    # limpar seu estado interno (chave recém-gerada, campos do formulário),
    # já que o módulo é iniciado uma única vez por sessão do navegador e,
    # sem isso, essas informações ficariam visíveis para quem fizer login
    # em seguida na mesma aba/sessão.
    resetarAdminTotp <- reactiveVal(0)
    
    # ===================================================
    # SELEÇÃO DO MÉTODO DE ACESSO
    # ===================================================
    
    observeEvent(
        
        input$continuar,
        
        {
            
            req(input$metodo_acesso)
            
            metodoAcesso(input$metodo_acesso)
            
        },
        
        ignoreInit = TRUE
        
    )
    
    observeEvent(
        
        input$voltar_metodo,
        
        {
            
            metodoAcesso(NULL)
            
        },
        
        ignoreInit = TRUE
        
    )
    
    # ===================================================
    # LOGIN - AD
    # ===================================================
    
    observeEvent(
        
        input$entrar,
        
        {
            
            req(
                input$usuario,
                input$senha
            )
            
            distro_escolhida <- input$distro_ad
            
            if (is.null(distro_escolhida) || trimws(distro_escolhida) == "") {
                
                showNotification(
                    "Selecione uma empresa válida antes de continuar.",
                    type = "warning"
                )
                
                return(invisible(NULL))
                
            }
            
            # Defesa extra: o <select> já restringe as opções no navegador,
            # mas nada impede uma requisição manipulada tentando mandar um
            # valor fora da lista.
            if (!(distro_escolhida %in% DISTROS_DISPONIVEIS)) {
                
                showNotification(
                    "Empresa selecionada é inválida.",
                    type = "error"
                )
                
                return(invisible(NULL))
                
            }
            
            dados <- tryCatch(
                authenticate_ad(
                    input$usuario,
                    input$senha,
                    distro_escolhida
                ),
                error = function(e) {
                    
                    showNotification(
                        paste("Erro ao consultar o Active Directory:", conditionMessage(e)),
                        type = "error"
                    )
                    
                    NULL
                    
                }
            )
            
            registrar_auditoria(
                con,
                input$usuario,
                paste0("AD:", distro_escolhida),
                !is.null(dados)
            )
            
            if (!is.null(dados)) {
                
                autenticado(TRUE)
                
                usuarioLogado(input$usuario)
                
                dadosUsuario(dados)
                
                fotoUsuario(
                    obter_foto_usuario(dados)
                )
                
                metodoAutenticado("AD")
                
                distroSelecionado(distro_escolhida)
                
                menuSelecionado("Usuário")
                
                # Garante que a aba volte para "Usuário" sem recriar a UI toda
                updateTabsetPanel(
                    session,
                    "menu",
                    selected = "Usuário"
                )
                
                showNotification(
                    paste(
                        "Bem-vindo",
                        obter_campo(dados, "displayName")
                    ),
                    type = "message"
                )
                
            } else {
                
                showNotification(
                    "Usuário ou senha inválidos",
                    type = "error"
                )
                
            }
            
        },
        
        ignoreInit = TRUE
        
    )
    
    # ===================================================
    # LOGIN - TOTP (Authenticator)
    # ===================================================
    
    observeEvent(
        
        input$entrar_totp,
        
        {
            
            req(
                input$usuario_totp,
                input$codigo_totp
            )
            
            dados <- autenticar_totp(
                con,
                input$usuario_totp,
                input$codigo_totp
            )
            
            if (
                !is.null(dados) &&
                (is.null(dados$distro) || is.na(dados$distro) || trimws(dados$distro) == "")
            ) {
                
                # Usuário TOTP cadastrado antes do suporte multi-empresa (ou sem
                # empresa definida): não dá pra saber a qual empresa pertence.
                showNotification(
                    paste0(
                        "O usuário '", dados$login, "' não tem uma empresa associada. ",
                        "Peça para um administrador recadastrar o acesso TOTP em ",
                        "Administração TOTP, selecionando a empresa."
                    ),
                    type = "error",
                    duration = 10
                )
                
                dados <- NULL
                
            }
            
            if (!is.null(dados)) {
                
                autenticado(TRUE)
                
                usuarioLogado(dados$login)
                
                dadosUsuario(dados)
                
                fotoUsuario(NULL)
                
                metodoAutenticado("TOTP")
                
                distroSelecionado(dados$distro)
                
                menuSelecionado("Usuário")
                
                updateTabsetPanel(
                    session,
                    "menu",
                    selected = "Usuário"
                )
                
                showNotification(
                    paste(
                        "Bem-vindo",
                        dados$displayName
                    ),
                    type = "message"
                )
                
            } else {
                
                showNotification(
                    "Usuário ou código inválido",
                    type = "error"
                )
                
            }
            
        },
        
        ignoreInit = TRUE
        
    )
    
    # ===================================================
    # LOGOUT
    # ===================================================
    
    observeEvent(
        
        input$sair,
        
        {
            
            autenticado(FALSE)
            usuarioLogado(NULL)
            dadosUsuario(NULL)
            fotoUsuario(NULL)
            metodoAutenticado(NULL)
            metodoAcesso(NULL)
            distroSelecionado(NULL)
            menuSelecionado("Usuário")
            
            # Ver comentário na definição de resetarAdminTotp acima.
            resetarAdminTotp(resetarAdminTotp() + 1)
            
        },
        
        ignoreInit = TRUE
        
    )
    
    # ===================================================
    # ALTERNÂNCIA DO CABEÇALHO (client-side, não recria a UI)
    # ===================================================
    
    observeEvent(
        
        input$toggle_header,
        
        {
            
            header_oculto(!header_oculto())
            
            session$sendCustomMessage(
                "toggle-header",
                list(oculto = header_oculto())
            )
            
        },
        
        ignoreInit = TRUE
        
    )
    
    # ===================================================
    # README (janela modal, renderizado a partir de README.md)
    # ===================================================
    
    observeEvent(
        
        input$mostrar_readme,
        
        {
            
            conteudo_modal <- if (file.exists(README_PATH)) {
                
                texto_readme <- paste(
                    readLines(README_PATH, warn = FALSE, encoding = "UTF-8"),
                    collapse = "\n"
                )
                
                shiny::markdown(texto_readme)
                
            } else {
                
                div(
                    style = "color:#842029;",
                    "Arquivo README.md não encontrado em: ", README_PATH
                )
                
            }
            
            showModal(
                modalDialog(
                    title = "README",
                    conteudo_modal,
                    easyClose = TRUE,
                    size = "l",
                    footer = modalButton("Fechar")
                )
            )
            
        },
        
        ignoreInit = TRUE
        
    )
    
    # ===================================================
    # CAPTURA DA ABA SELECIONADA
    # ===================================================
    
    observeEvent(
        
        input$menu,
        
        {
            
            req(input$menu)
            
            menuSelecionado(input$menu)
            
        },
        
        ignoreInit = TRUE
        
    )
    
    # ===================================================
    # TELA DE LOGIN
    # ===================================================
    
    output$tela_login <- renderUI({
        
        if (autenticado()) {
            return(NULL)
        }
        
        div(
            class = "login-wrapper",
            
            div(
                class = "login-card shadow-sm p-4",
                
                div(
                    class = "logo-container mb-4",
                    
                    tags$img(
                        src = "img/radarSocial_logo_horizontal_fundo_claro.png",
                        class = "logo-login",
                        alt = "RadarSocial"
                    )
                ),
                
                tags$h4(
                    "Acesso ao sistema",
                    class = "login-title fw-bold mb-1"
                ),
                
                tags$p(
                    "Escolha como deseja entrar no RadarSocial",
                    class = "login-subtitle text-muted mb-4"
                ),
                
                if (is.null(metodoAcesso())) {
                    
                    # =============================================
                    # PASSO 1 - SELETOR DE MÉTODO
                    # =============================================
                    
                    tagList(
                        
                        div(
                            class = "metodo-opcoes",
                            
                            radioButtons(
                                "metodo_acesso",
                                NULL,
                                choiceNames = list(
                                    
                                    tagList(
                                        icon("building-shield", class = "metodo-opcao-icone"),
                                        div(
                                            div("Login Corporativo (AD)", class = "metodo-opcao-titulo"),
                                            div("Entrar com seu usuário e senha de domínio", class = "metodo-opcao-desc")
                                        )
                                    ),
                                    
                                    tagList(
                                        icon("mobile-screen-button", class = "metodo-opcao-icone"),
                                        div(
                                            div("Código Authenticator", class = "metodo-opcao-titulo"),
                                            div("Entrar com um código gerado no seu celular", class = "metodo-opcao-desc")
                                        )
                                    )
                                    
                                ),
                                choiceValues = list("ad", "totp"),
                                selected = character(0)
                            )
                            
                        ),
                        
                        actionButton(
                            "continuar",
                            tagList("Continuar", icon("arrow-right", class = "ms-2")),
                            class = "btn btn-primary w-100 btn-acesso mt-2"
                        )
                        
                    )
                    
                } else if (metodoAcesso() == "ad") {
                    
                    # =============================================
                    # PASSO 2A - LOGIN CORPORATIVO (AD)
                    # =============================================
                    
                    tagList(
                        
                        actionLink(
                            "voltar_metodo",
                            tagList(icon("arrow-left"), " Voltar"),
                            class = "voltar-link mb-4 d-inline-block"
                        ),
                        
                        div(
                            class = "mb-3",
                            selectInput(
                                "distro_ad",
                                "Empresa / Unidade",
                                choices = c(
                                    "Selecione uma empresa" = "",
                                    DISTROS_DISPONIVEIS
                                ),
                                selected = "",
                                width = "100%"
                            )
                        ),
                        
                        div(
                            class = "mb-3",
                            textInput("usuario", "Usuário", width = "100%")
                        ),
                        
                        div(
                            class = "mb-2",
                            passwordInput("senha", "Senha", width = "100%")
                        ),
                        
                        div(
                            id = "capslock_warning",
                            icon("triangle-exclamation"),
                            " Caps Lock está ativado"
                        ),
                        
                        actionButton(
                            "entrar",
                            tagList(icon("right-to-bracket", class = "me-2"), "Entrar"),
                            class = "btn btn-primary w-100 btn-acesso mt-4"
                        )
                        
                    )
                    
                } else if (metodoAcesso() == "totp") {
                    
                    # =============================================
                    # PASSO 2B - CÓDIGO AUTHENTICATOR (TOTP)
                    # =============================================
                    
                    tagList(
                        
                        actionLink(
                            "voltar_metodo",
                            tagList(icon("arrow-left"), " Voltar"),
                            class = "voltar-link mb-4 d-inline-block"
                        ),
                        
                        div(
                            class = "mb-3",
                            textInput("usuario_totp", "Usuário", width = "100%")
                        ),
                        
                        div(
                            class = "mb-2",
                            textInput(
                                "codigo_totp",
                                "Código do Authenticator",
                                placeholder = "000000",
                                width = "100%"
                            )
                        ),
                        
                        actionButton(
                            "entrar_totp",
                            tagList(icon("key", class = "me-2"), "Entrar"),
                            class = "btn btn-primary w-100 btn-acesso mt-4"
                        )
                        
                    )
                    
                }
                
            )
            
        )
    })
    
    # ===================================================
    # TELA PRINCIPAL
    # ===================================================
    
    output$tela_principal <- renderUI({
        
        req(autenticado())
        
        tagList(
            
            # =================================================
            # BARRA DE ÍCONES
            # =================================================
            
            div(
                
                class = "icon-bar",
                
                actionLink(
                    "toggle_header",
                    icon("id-badge"),
                    class = "icon-btn",
                    title = "Mostrar/ocultar informações do usuário"
                ),
                
                actionLink(
                    "mostrar_readme",
                    icon("circle-info"),
                    class = "icon-btn",
                    title = "Ver instruções (README)"
                ),
                
                actionLink(
                    "sair",
                    icon("power-off"),
                    class = "icon-btn sair",
                    title = "Sair"
                )
                
            ),
            
            # =================================================
            # CONTEÚDO
            # =================================================
            
            div(
                
                id = "app-content",
                
                # ===============================================
                # CABEÇALHO
                # ===============================================
                
                div(
                    
                    class = "header-container",
                    
                    h2("Radar Social"),
                    
                    if (identical(metodoAutenticado(), "AD")) {
                        
                        tags$div(
                            
                            style = "color:#555;",
                            
                            tags$b("Usuário: "),
                            obter_campo(dadosUsuario(), "displayName"),
                            br(),
                            
                            tags$b("Departamento: "),
                            obter_campo(dadosUsuario(), "department"),
                            br(),
                            
                            tags$b("Criado em: "),
                            formatar_whenCreated(
                                obter_campo(dadosUsuario(), "whenCreated")
                            ),
                            br(),
                            
                            tags$b("Último acesso: "),
                            formatar_lastLogon(
                                obter_campo(dadosUsuario(), "lastLogonTimestamp")
                            ),
                            br(),
                            
                            tags$b("Gestor: "),
                            extrair_manager(dadosUsuario()$manager),
                            br(),
                            
                            tags$b("Empresa: "),
                            distroSelecionado(),
                            br(),
                            
                            tags$b("Método de acesso: "),
                            "Login Corporativo (AD)"
                            
                        )
                        
                    } else {
                        
                        tags$div(
                            
                            style = "color:#555;",
                            
                            tags$b("Usuário: "),
                            dadosUsuario()$displayName,
                            br(),
                            
                            tags$b("Login: "),
                            dadosUsuario()$login,
                            br(),
                            
                            tags$b("Empresa: "),
                            distroSelecionado(),
                            br(),
                            
                            tags$b("Método de acesso: "),
                            "Código Authenticator (TOTP)"
                            
                        )
                        
                    }
                    
                ),
                
                hr(),
                
                # ===============================================
                # ABAS
                # ===============================================
                
                do.call(
                    navset_tab,
                    c(
                        list(
                            id = "menu",
                            selected = isolate(menuSelecionado())
                        ),
                        list(
                            nav_panel("Inconsistências", mod_inconsistencias_ui("inconsistencias")),
                            nav_panel("Rejeitados", mod_rejeitados_ui("rejeitados")),
                            nav_panel("Totalizadores", mod_totalizadores_ui("totalizadores"))
                        ),
                        if (identical(metodoAutenticado(), "AD")) {
                            list(
                                nav_panel("Administração TOTP", mod_totp_admin_ui("totp_admin"))
                            )
                        }
                    )
                )
                
            )
            
        )
        
    })
    
    # ===================================================
    # SERVIDORES DOS MÓDULOS
    # ===================================================
    
    mod_usuario_server(
        "usuario",
        dados_usuario = dadosUsuario,
        foto_usuario = fotoUsuario
    )
    
    mod_rejeitados_server(
        "rejeitados",
        ativo = reactive(menuSelecionado() == "Rejeitados")
    )
    
    mod_inconsistencias_server(
        "inconsistencias",
        ativo = reactive(menuSelecionado() == "Inconsistências")
    )
    
    mod_totalizadores_server(
        "totalizadores",
        ativo = reactive(menuSelecionado() == "Totalizadores")
    )
    
    # Cadastro TOTP fica disponível apenas para quem entrou via AD
    # (a UI da aba só é renderizada nesse caso, mas o módulo em si
    # não depende disso para funcionar caso a regra mude no futuro).
    mod_totp_admin_server(
        "totp_admin",
        con = con,
        ativo = reactive(menuSelecionado() == "Administração TOTP"),
        resetar = resetarAdminTotp
    )
    
}

# =====================================================
# EXECUÇÃO
# =====================================================

shinyApp(
    ui = ui,
    server = server
)