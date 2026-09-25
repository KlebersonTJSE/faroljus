# =========================================================================
# app.R — FarolJus
# -------------------------------------------------------------------------
# Login corporativo (Active Directory) ou código Authenticator (TOTP),
# seguindo o mesmo layout/fluxo de autenticação usado em outros sistemas
# internos.
#
#   - Barra lateral de ícones com um ícone "Menu" no topo, que expande/
#     recolhe a barra mostrando a descrição de cada ícone.
#   - "Administração" (janela modal com o cadastro TOTP e a auditoria de
#     login) e "Trocar empresa": disponíveis só para quem entrou via Login
#     Corporativo (AD). Um usuário TOTP continua sempre fixo na empresa
#     do próprio cadastro. (Regra centralizada em ehAdmin() /
#     podeTrocarEmpresa(), no server.)
#   - O módulo Alertas recebe a empresa da sessão (distroSelecionado) e a
#     conexão SQLite (con), usada para traduzir os códigos de Situação
#     Profissional Atual e Cargo.
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

library(bslib)
library(DT)
library(jsonlite)
library(digest)

library(DBI)
library(RSQLite)

# shiny é carregado depois de jsonlite de propósito: jsonlite também
# exporta validate(), e o pacote carregado por último fica na frente na
# busca de funções. Assim, um validate() sem "shiny::" continua
# resolvendo para shiny::validate().
library(shiny)

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
# -----------------------------------------------------
# Se a pasta data/ não existir (ex.: projeto recém-clonado), o SQLite
# não consegue criar o arquivo — a pasta é criada antes. Um arquivo
# novo/vazio é aceito: garantir_schema_totp(), mais abaixo, cria as
# tabelas que faltarem.
# =====================================================
dir.create(here::here("data"), recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(
    SQLite(),
    here::here("data", "radarsocial.db")
)

# Fecha a conexão SQLite quando a aplicação for encerrada.
onStop(function() {
    try(DBI::dbDisconnect(con), silent = TRUE)
})

# =========================================================================
# MÓDULOS DE AUTENTICAÇÃO, BANCO E UTILITÁRIOS
# =========================================================================
source(here::here("R", "utils.R"))
source(here::here("R", "auth.R"))
source(here::here("R", "auth_totp.R"))
# Integração com o IRIS (RJDBC/rJava): conectar_banco(),
# consultar_iris() e testar_iris(). Requer Java instalado e JAVA_HOME,
# IRIS_DRIVER_CLASS, IRIS_JAR_PATH e {DISTRO}_IRIS_* no .Renviron — ver
# comentários em R/database.R.
source(here::here("R", "database.R"))
source(here::here("modules", "mod_totp_admin.R"))

source(here::here("modules", "mod_alertas.R"))

# =========================================================================
# ESQUEMA DO BANCO (usuarios_totp / login_auditoria + coluna "distro")
# -------------------------------------------------------------------------
# Precisa rodar logo após abrir a conexão e depois de source("auth_totp.R"),
# de onde vem garantir_schema_totp(). Cria as tabelas num banco novo e
# faz a migração da coluna "distro" num banco antigo. Idempotente.
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

# =========================================================================
# ITEM DAS INFORMAÇÕES DO USUÁRIO (cabeçalho da tela principal)
# -------------------------------------------------------------------------
# Um par "Rótulo: valor" da grade .info-usuario. O title repete o texto
# para ele aparecer inteiro ao passar o mouse, caso o valor seja cortado
# (valores muito longos, como departamento/gestor, ficam em uma linha
# com reticências — ver CSS .info-item).
# =========================================================================
info_item <- function(rotulo, valor) {
    
    texto_title <- if (is.character(valor) && length(valor) == 1) valor else NULL
    
    div(
        class = "info-item",
        title = texto_title,
        tags$b(paste0(rotulo, ": ")),
        valor
    )
    
}

# =========================================================================
# TÍTULO DE JANELA MODAL COM "X" NO CANTO SUPERIOR DIREITO
# -------------------------------------------------------------------------
# Usado em Administração, Trocar empresa e Ver instruções (README).
#
#   - Sem `input_fechar`: o X fecha a janela direto pelo Bootstrap
#     (data-dismiss / data-bs-dismiss, os dois — funciona no Bootstrap 4
#     e no 5), igual ao botão modalButton() do rodapé.
#   - Com `input_fechar`: o X dispara esse input do Shiny (em vez de
#     fechar direto), para a janela que precisa fazer algo ao fechar —
#     caso de Administração (ver observeEvent(input$fechar_admin)).
#
# O X vai para o canto graças à regra CSS .modal-header .modal-title
# { flex: 1 1 auto; } — ver o bloco de estilos da UI.
# =========================================================================
titulo_modal <- function(icone, texto, input_fechar = NULL) {
    
    botao_x <- if (is.null(input_fechar)) {
        tags$button(
            type = "button",
            class = "btn-close",
            style = "position:absolute; top:50%; right:0; transform:translateY(-50%);",
            `aria-label` = "Fechar",
            title = "Fechar",
            `data-dismiss` = "modal",
            `data-bs-dismiss` = "modal"
        )
    } else {
        tags$button(
            type = "button",
            class = "btn-close",
            style = "position:absolute; top:50%; right:0; transform:translateY(-50%);",
            `aria-label` = "Fechar",
            title = "Fechar",
            onclick = sprintf(
                "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
                input_fechar
            )
        )
    }
    
    div(
        style = "position:relative; padding-right:28px;",
        icon(icone, class = "me-2"),
        texto,
        botao_x
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
        transition: width .2s ease;
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

      /* =============================================
         ÍCONE MENU (expandir/recolher a barra lateral)
         — maior que os demais e sempre no topo.
         ============================================= */

      .icon-bar .icon-btn-menu {
        width: 44px;
        height: 44px;
        font-size: 20px;
        margin-bottom: 4px;
        border-bottom: 1px solid rgba(255,255,255,.15);
        padding-bottom: 10px;
      }

      /* Descrição ao lado de cada ícone — oculta por padrão, aparece
         quando a barra está expandida (.icon-bar.expandida). */

      .icon-bar .icon-label {
        display: none;
        margin-left: 10px;
        font-size: 13px;
        white-space: nowrap;
      }

      .icon-bar.expandida {
        width: 210px;
        align-items: stretch;
        padding-left: 8px;
        padding-right: 8px;
      }

      .icon-bar.expandida .icon-btn {
        width: 100%;
        justify-content: flex-start;
        padding: 0 8px;
      }

      .icon-bar.expandida .icon-btn-menu {
        justify-content: flex-start;
        padding-left: 8px;
      }

      .icon-bar.expandida .icon-label {
        display: inline;
      }

      .icon-bar.expandida ~ #app-content {
        margin-left: 210px;
      }

      #app-content {
        margin-left: 52px;
        padding: 20px 25px;
        transition: margin-left .2s ease;
      }

      .header-container {
        overflow: hidden;
        max-height: 400px; /* folga para a grade de informações quebrar em várias linhas em telas estreitas */
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

      /* =============================================
         CABEÇALHO: TÍTULO (logo + nome) E INFORMAÇÕES
         ============================================= */

      .header-titulo {
        display: flex;
        align-items: center;
        gap: .75rem;
        margin-bottom: .75rem;
      }

      .header-titulo h2 {
        margin: 0;
      }

      .header-logo {
        height: 44px;
        width: auto;
      }

      /* Grade: cada coluna tem no mínimo 260px e quantas couberem
         dividem a largura disponível — em tela larga ficam 3 ou 4
         informações por linha; em tela estreita, menos. */
      .info-usuario {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
        gap: .3rem 1.75rem;
        color: #555;
        font-size: .92rem;
      }

      .info-item {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      /* =============================================
         README (JANELA MODAL - Ver instruções)
         ---------------------------------------------
         Escala tipográfica própria: sem isto, os títulos do
         Markdown usavam os tamanhos padrão do tema (h2 com
         ~2rem etc.), grandes demais para uma janela modal e
         desproporcionais ao texto.
         ============================================= */

      .readme-conteudo {
        font-size: .92rem;
        line-height: 1.6;
        color: #333;
      }

      .readme-conteudo h1 {
        font-size: 1.5rem;
        font-weight: 700;
        margin: .25rem 0 .5rem 0;
      }

      .readme-conteudo h2 {
        font-size: 1.2rem;
        font-weight: 700;
        color: #003366;
        margin: 1.6rem 0 .6rem 0;
        padding-bottom: .3rem;
        border-bottom: 1px solid #e3e7ec;
      }

      .readme-conteudo h3 {
        font-size: 1.02rem;
        font-weight: 600;
        margin: 1.1rem 0 .4rem 0;
      }

      .readme-conteudo h4 {
        font-size: .95rem;
        font-weight: 600;
        margin: 1rem 0 .35rem 0;
      }

      .readme-conteudo p,
      .readme-conteudo li {
        text-align: justify;
      }

      .readme-conteudo ul,
      .readme-conteudo ol {
        padding-left: 1.3rem;
        margin-bottom: .75rem;
      }

      .readme-conteudo li {
        margin-bottom: .2rem;
      }

      .readme-conteudo hr {
        display: none;
      }

      .readme-conteudo pre {
        background: #f6f8fa;
        border: 1px solid #e3e7ec;
        border-radius: .5rem;
        padding: .75rem 1rem;
        font-size: .82rem;
      }

      .readme-conteudo code {
        font-size: .85em;
      }

      .readme-conteudo table {
        width: 100%;
        border-collapse: collapse;
        margin-bottom: 1rem;
        font-size: .85rem;
      }

      .readme-conteudo th,
      .readme-conteudo td {
        border: 1px solid #e3e7ec;
        padding: .4rem .6rem;
        vertical-align: top;
      }

      .readme-conteudo th {
        background: #f4f6f9;
        font-weight: 600;
      }

      .readme-conteudo blockquote {
        border-left: 4px solid #0d6efd;
        background: #f5f9ff;
        padding: .6rem 1rem;
        margin: 1rem 0;
        color: #444;
      }

      .readme-conteudo blockquote p {
        margin: 0;
      }

      .readme-conteudo img {
        max-width: 100%;
        height: auto;
      }

      /* =============================================
         TÍTULO DOS MODAIS OCUPANDO A LARGURA TODA
         ---------------------------------------------
         No Bootstrap 5, o .modal-header é flex e o
         .modal-title encolhe até o tamanho do texto — por
         isso o X posicionado com 'right:0' dentro do título
         ficava colado ao texto, e não no canto. Fazendo o
         título crescer, o X vai para o canto superior
         direito (vale para Administração e também para
         Gerenciar Arquivos, que usa o mesmo padrão).
         ============================================= */

      .modal-header .modal-title {
        flex: 1 1 auto;
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

      Shiny.addCustomMessageHandler(

        'toggle-icon-bar',

        function(message) {

          var barra =
            document.querySelector(
              '.icon-bar'
            );

          if (!barra) return;

          if (message.expandida) {

            barra.classList.add(
              'expandida'
            );

          } else {

            barra.classList.remove(
              'expandida'
            );

          }

        }

      );

      // Enviado por mod_alertas.R depois de fechar os modais de
      // 'Gerenciar Arquivos'. Remove um backdrop do Bootstrap que tenha
      // ficado 'grudado' na tela (bloqueando cliques) quando um modal é
      // trocado por outro rapidamente. Sem este handler registrado, o
      // navegador acusava erro de mensagem sem tratador.
      Shiny.addCustomMessageHandler(

        'limpar-modal-backdrop',

        function(message) {

          setTimeout(function() {

            if (document.querySelector('.modal.show')) return;

            document
              .querySelectorAll('.modal-backdrop')
              .forEach(function(el) { el.remove(); });

            document.body.classList.remove('modal-open');
            document.body.style.removeProperty('overflow');
            document.body.style.removeProperty('padding-right');

          }, 400);

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
    
    # Barra lateral (ícones) expandida ou não — ver ícone "toggle_icon_bar"
    # e o handler JS "toggle-icon-bar".
    iconBarExpandida <- reactiveVal(FALSE)
    
    # ===================================================
    # MÓDULO SELECIONADO
    # ---------------------------------------------------
    # Começa em "Alertas" (antes era "Usuário", aba que não existe — o
    # módulo Alertas ficava com ativo() = FALSE até o usuário clicar em
    # uma aba).
    # ===================================================
    
    menuSelecionado <- reactiveVal("Alertas")
    
    # Incrementado a cada logout — sinaliza para mod_totp_admin_server()
    # limpar seu estado interno (chave recém-gerada, campos do formulário),
    # já que o módulo é iniciado uma única vez por sessão do navegador e,
    # sem isso, essas informações ficariam visíveis para quem fizer login
    # em seguida na mesma aba/sessão.
    resetarAdminTotp <- reactiveVal(0)
    
    # ===================================================
    # TROCA DE EMPRESA
    # ---------------------------------------------------
    # Quem pode trocar: quem entrou via Login Corporativo (AD) — mesmo
    # perfil do ícone "Administração". Usuário TOTP fica sempre na
    # empresa do próprio cadastro. Para mudar a regra, altere só
    # podeTrocarEmpresa().
    # ===================================================
    
    ehAdmin <- reactive({
        isTRUE(autenticado()) && identical(metodoAutenticado(), "AD")
    })
    
    podeTrocarEmpresa <- reactive({
        ehAdmin()
    })
    
    mostrar_modal_empresa <- function() {
        
        escolha_atual <- distroSelecionado()
        
        selecionada <- if (!is.null(escolha_atual) && escolha_atual %in% DISTROS_DISPONIVEIS) {
            escolha_atual
        } else if (length(DISTROS_DISPONIVEIS) > 0) {
            DISTROS_DISPONIVEIS[1]
        } else {
            character(0)
        }
        
        showModal(
            modalDialog(
                title = titulo_modal("building", "Trocar empresa"),
                
                if (length(DISTROS_DISPONIVEIS) == 0) {
                    
                    div(
                        class = "alert alert-warning mb-0",
                        "Nenhuma empresa está configurada em DISTRO_1, DISTRO_2... ",
                        "no .Renviron."
                    )
                    
                } else {
                    
                    tagList(
                        p(
                            class = "text-muted",
                            "Selecione a empresa com a qual deseja trabalhar. Os ",
                            "dados de Alertas exibidos passarão a seguir a empresa ",
                            "selecionada abaixo."
                        ),
                        selectInput(
                            "empresa_troca",
                            "Empresa",
                            choices = DISTROS_DISPONIVEIS,
                            selected = selecionada,
                            width = "100%"
                        )
                    )
                    
                },
                
                easyClose = TRUE,
                
                footer = tagList(
                    modalButton("Cancelar"),
                    if (length(DISTROS_DISPONIVEIS) > 0) {
                        actionButton(
                            "confirmar_troca_empresa",
                            "Usar esta empresa",
                            class = "btn-primary"
                        )
                    }
                )
            )
        )
        
    }
    
    observeEvent(
        input$trocar_empresa,
        {
            req(podeTrocarEmpresa())
            mostrar_modal_empresa()
        },
        ignoreInit = TRUE
    )
    
    observeEvent(
        input$confirmar_troca_empresa,
        {
            req(podeTrocarEmpresa(), input$empresa_troca)
            
            nova_empresa <- input$empresa_troca
            
            # Defesa extra contra valor fora da lista (requisição manipulada).
            if (!(nova_empresa %in% DISTROS_DISPONIVEIS)) {
                showNotification("Empresa selecionada é inválida.", type = "error")
                return(invisible(NULL))
            }
            
            removeModal()
            
            if (identical(nova_empresa, distroSelecionado())) {
                return(invisible(NULL))
            }
            
            # Não é gravado em login_auditoria de propósito: essa tabela
            # alimenta os gráficos de acessos de "Administração"
            # (mod_totp_admin.R), e uma troca de empresa contaria ali como
            # um login a mais.
            distroSelecionado(nova_empresa)
            
            showNotification(
                paste("Usando o sistema como a empresa", nova_empresa),
                type = "message"
            )
        },
        ignoreInit = TRUE
    )
    
    # ===================================================
    # ADMINISTRAÇÃO (janela modal)
    # ---------------------------------------------------
    # Antes era a aba "Administração TOTP". Agora abre em uma janela
    # modal pelo ícone "Administração" da barra lateral (só para AD).
    #
    # adminAberto() substitui o antigo menuSelecionado() == "Administração
    # TOTP" como `ativo` do módulo: as consultas ao banco (usuários TOTP,
    # auditoria) só rodam com a janela aberta. Por isso a janela não fecha
    # com clique fora/Esc (easyClose = FALSE) — só pelo "Fechar" ou pelo X,
    # que passam por observeEvent(input$fechar_admin) e mantêm
    # adminAberto() coerente.
    #
    # Ao fechar, resetarAdminTotp() é incrementado: limpa a chave
    # recém-gerada (secret em texto puro) e os campos do formulário, para
    # não reaparecerem na próxima abertura.
    # ===================================================
    
    adminAberto <- reactiveVal(FALSE)
    
    observeEvent(
        input$abrir_admin,
        {
            req(ehAdmin())
            
            adminAberto(TRUE)
            
            showModal(
                modalDialog(
                    title = titulo_modal(
                        "user-shield",
                        "Administração",
                        input_fechar = "fechar_admin"
                    ),
                    mod_totp_admin_ui("totp_admin"),
                    size = "xl",
                    easyClose = FALSE,
                    footer = actionButton("fechar_admin", "Fechar")
                )
            )
        },
        ignoreInit = TRUE
    )
    
    observeEvent(
        input$fechar_admin,
        {
            adminAberto(FALSE)
            resetarAdminTotp(resetarAdminTotp() + 1)
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
        },
        ignoreInit = TRUE
    )
    
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
                
                metodoAutenticado("AD")
                distroSelecionado(distro_escolhida)
                menuSelecionado("Alertas")
                
                # Garante que a aba volte para "Alertas" sem recriar a UI toda
                updateTabsetPanel(
                    session,
                    "menu",
                    selected = "Alertas"
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
                metodoAutenticado("TOTP")
                distroSelecionado(dados$distro)
                menuSelecionado("Alertas")
                
                updateTabsetPanel(
                    session,
                    "menu",
                    selected = "Alertas"
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
            metodoAutenticado(NULL)
            metodoAcesso(NULL)
            distroSelecionado(NULL)
            menuSelecionado("Alertas")
            
            # A tela principal é recriada no próximo login com o cabeçalho
            # visível e a barra recolhida — o estado precisa acompanhar,
            # senão o primeiro clique nesses ícones "não faz nada".
            header_oculto(FALSE)
            iconBarExpandida(FALSE)
            
            # Ver comentário na definição de resetarAdminTotp acima.
            adminAberto(FALSE)
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
    # ALTERNÂNCIA DA BARRA LATERAL (client-side, não recria a UI)
    # ===================================================
    
    observeEvent(
        
        input$toggle_icon_bar,
        
        {
            
            iconBarExpandida(!iconBarExpandida())
            
            session$sendCustomMessage(
                "toggle-icon-bar",
                list(expandida = iconBarExpandida())
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
                
                # div.readme-conteudo: escopo do CSS de tipografia do
                # README (tamanhos de título, texto, tabelas, código) —
                # ver bloco "README (JANELA MODAL)" nos estilos da UI.
                div(
                    class = "readme-conteudo",
                    shiny::markdown(texto_readme)
                )
                
            } else {
                
                div(
                    style = "color:#842029;",
                    "Arquivo README.md não encontrado em: ", README_PATH
                )
                
            }
            
            showModal(
                modalDialog(
                    title = titulo_modal("circle-info", "Instruções"),
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
                        src = "img/faroljus_logo_principal.png",
                        class = "logo-login",
                        alt = "FarolJus"
                    )
                ),
                
                tags$h4(
                    "Acesso ao sistema",
                    class = "login-title fw-bold mb-1"
                ),
                
                tags$p(
                    "Escolha como deseja entrar no FarolJus",
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
                
                class = paste(
                    "icon-bar",
                    if (isolate(iconBarExpandida())) "expandida"
                ),
                
                # Maior que os demais e sempre no topo: expande/recolhe a
                # barra, revelando a descrição de cada ícone (ver CSS
                # .icon-btn-menu / .icon-bar.expandida e o handler JS
                # "toggle-icon-bar").
                actionLink(
                    "toggle_icon_bar",
                    tagList(
                        icon("bars"),
                        tags$span(class = "icon-label", "Menu")
                    ),
                    class = "icon-btn icon-btn-menu",
                    title = "Expandir/recolher menu"
                ),
                
                actionLink(
                    "toggle_header",
                    tagList(
                        icon("id-badge"),
                        tags$span(class = "icon-label", "Mostrar/ocultar")
                    ),
                    class = "icon-btn",
                    title = "Mostrar/ocultar informações do usuário"
                ),
                
                if (ehAdmin()) {
                    actionLink(
                        "abrir_admin",
                        tagList(
                            icon("user-shield"),
                            tags$span(class = "icon-label", "Administração")
                        ),
                        class = "icon-btn",
                        title = "Administração"
                    )
                },
                
                if (podeTrocarEmpresa()) {
                    actionLink(
                        "trocar_empresa",
                        tagList(
                            icon("building"),
                            tags$span(class = "icon-label", "Trocar empresa")
                        ),
                        class = "icon-btn",
                        title = "Trocar empresa"
                    )
                },
                
                actionLink(
                    "mostrar_readme",
                    tagList(
                        icon("circle-info"),
                        tags$span(class = "icon-label", "Ver instruções")
                    ),
                    class = "icon-btn",
                    title = "Ver instruções (README)"
                ),
                
                actionLink(
                    "sair",
                    tagList(
                        icon("power-off"),
                        tags$span(class = "icon-label", "Sair")
                    ),
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
                    
                    class = paste(
                        "header-container",
                        if (isolate(header_oculto())) "collapsed"
                    ),
                    
                    # Título: logo + "Farol Jus".
                    div(
                        class = "header-titulo",
                        tags$img(
                            src = "img/faroljus_logo_principal.png",
                            class = "header-logo",
                            alt = "Logo Farol Jus"
                        ),
                        h2("")
                    ),
                    
                    # Informações do usuário em grade (várias por linha,
                    # quantas couberem na largura da tela — ver CSS
                    # .info-usuario). A ordem abaixo é a ordem de leitura,
                    # da esquerda para a direita.
                    #
                    # "Empresa" usa um textOutput próprio (em vez de ler
                    # distroSelecionado() aqui): trocar de empresa só
                    # atualiza esse item, sem recriar toda a tela_principal
                    # (barra de ícones e abas).
                    if (identical(metodoAutenticado(), "AD")) {
                        
                        div(
                            class = "info-usuario",
                            
                            info_item("Usuário", obter_campo(dadosUsuario(), "displayName")),
                            info_item("Departamento", obter_campo(dadosUsuario(), "department")),
                            info_item("Gestor", extrair_manager(dadosUsuario()$manager)),
                            info_item("Empresa", textOutput("empresa_atual_display", inline = TRUE)),
                            info_item(
                                "Criado em",
                                formatar_whenCreated(obter_campo(dadosUsuario(), "whenCreated"))
                            ),
                            info_item(
                                "Último acesso",
                                formatar_lastLogon(obter_campo(dadosUsuario(), "lastLogonTimestamp"))
                            ),
                            info_item("Método de acesso", "Login Corporativo (AD)")
                        )
                        
                    } else {
                        
                        div(
                            class = "info-usuario",
                            
                            info_item("Usuário", dadosUsuario()$displayName),
                            info_item("Login", dadosUsuario()$login),
                            info_item("Empresa", textOutput("empresa_atual_display", inline = TRUE)),
                            info_item("Método de acesso", "Código Authenticator (TOTP)")
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
                        # "Usuário" foi removida e "Administração TOTP" virou
                        # janela modal (ícone "Administração", ver
                        # observeEvent(input$abrir_admin)).
                        list(
                            nav_panel("Alertas", mod_alertas_ui("alertas"))
                        )
                    )
                )
                
            )
            
        )
        
    })
    
    output$empresa_atual_display <- renderText({
        req(autenticado())
        distroSelecionado()
    })
    
    # ===================================================
    # SERVIDORES DOS MÓDULOS
    # ===================================================
    
    # Antes, empresa e con não eram repassados: empresa() ficava sempre
    # NULL (o req(empresa()) do botão "Gerenciar Arquivos" abortava em
    # silêncio e nenhum dado era carregado) e, sem con, as colunas
    # Situação Profissional Atual / Cargo continuavam em código.
    mod_alertas_server(
        "alertas",
        ativo = reactive(menuSelecionado() == "Alertas"),
        empresa = distroSelecionado,
        con = con
    )
    
    # Cadastro TOTP / auditoria: só roda com a janela "Administração"
    # aberta E para login AD — a checagem de ehAdmin() aqui protege o
    # módulo mesmo que alguém dispare input$abrir_admin manualmente.
    mod_totp_admin_server(
        "totp_admin",
        con = con,
        ativo = reactive(adminAberto() && ehAdmin()),
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