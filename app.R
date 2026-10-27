# =========================================================================
# app.R — FarolJus
# -------------------------------------------------------------------------
# Login corporativo (Active Directory) ou código Authenticator (TOTP),
# seguindo o mesmo layout/fluxo de autenticação usado em outros sistemas
# internos.
#
#   - Barra lateral de ícones com um ícone "Menu" no topo, que expande/
#     recolhe a barra mostrando a descrição de cada ícone.
#   - A navegação entre os módulos é feita pela barra lateral (não há
#     mais abas):
#       "Magistrados"        -> mod_magistrados.R
#                               (<PASTA_ALERTAS>/<EMPRESA>/magistrados)
#       "Pessoal e Auxiliar" -> mod_pessoal_auxiliar.R
#                               (<PASTA_ALERTAS>/<EMPRESA>/pessoal)
#       "Serventias"         -> mod_serventias.R
#                               (<PASTA_ALERTAS>/<EMPRESA>/serventias)
#     (mesma ordem, ícones e aparência do FarolJus Lite; a barra começa
#     expandida).
#     Todos recebem a empresa da sessão (distroSelecionado); os de
#     Magistrados e de Pessoal também recebem a conexão SQLite (con),
#     usada para traduzir os códigos de Situação Profissional Atual e
#     Cargo (cada um com as próprias tabelas auxiliares).
#   - Botão "Configurações": abre/fecha, DENTRO da barra lateral, o grupo
#     com "Trocar empresa", "Ver instruções" e "Administração" (janela
#     modal com o cadastro TOTP e a auditoria de login). Administração e Trocar empresa só aparecem para
#     quem entrou via Login Corporativo (AD); um usuário TOTP continua
#     fixo na empresa do próprio cadastro. (Regra centralizada em
#     ehAdmin() / podeTrocarEmpresa(), no server.)
#   - Banco SQLite da aplicação: data/faroljus.db (antes radarsocial.db).
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

# O banco se chamava radarsocial.db (herança do RadarSocial). Migração
# automática: se só existir o arquivo antigo, ele é COPIADO para o novo
# nome na primeira execução — cadastros TOTP, auditoria de login e
# tabelas auxiliares continuam valendo. O arquivo antigo não é apagado;
# pode ser removido manualmente depois de conferir que está tudo certo.
DB_PATH <- here::here("data", "faroljus.db")
DB_PATH_ANTIGO <- here::here("data", "radarsocial.db")

if (!file.exists(DB_PATH) && file.exists(DB_PATH_ANTIGO)) {
    
    if (file.copy(DB_PATH_ANTIGO, DB_PATH)) {
        message("Banco migrado: ", DB_PATH_ANTIGO, " -> ", DB_PATH)
    } else {
        warning(
            "Não foi possível copiar ", DB_PATH_ANTIGO, " para ", DB_PATH,
            ". Um banco novo e vazio será criado em ", DB_PATH, "."
        )
    }
    
}

con <- dbConnect(
    SQLite(),
    DB_PATH
)

# Desempenho/concorrência do SQLite:
#   - WAL: leituras (telas, auditoria) não ficam bloqueadas por uma
#     gravação (login, cadastro) e cada gravação custa menos E/S;
#   - synchronous = NORMAL: seguro com WAL e bem mais rápido que FULL;
#   - busy_timeout: com mais de um processo R (Shiny Server/Connect com
#     vários workers), espera até 5 s pelo lock em vez de falhar na hora
#     com "database is locked".
# Falha em algum PRAGMA não impede o app de subir.
#
# ATENÇÃO: o modo WAL não funciona em pasta de rede (SMB/NFS). Se data/
# estiver num compartilhamento de rede, defina SQLITE_WAL=FALSE no
# .Renviron (o banco continua no modo padrão, só sem o ganho do WAL).
SQLITE_WAL <- !identical(toupper(Sys.getenv("SQLITE_WAL", unset = "TRUE")), "FALSE")

for (pragma in c(
    if (SQLITE_WAL) c("PRAGMA journal_mode = WAL", "PRAGMA synchronous = NORMAL"),
    "PRAGMA busy_timeout = 5000"
)) {
    try(DBI::dbExecute(con, pragma), silent = TRUE)
}
rm(pragma)

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

# Módulos de dados, Inconsistências e Painel — os mesmos do FarolJus
# Lite (com o cache de leitura e a carga sob demanda do FarolJus). A
# ordem importa: Serventias, Magistrados e Painel reaproveitam funções de
# mod_pessoal_auxiliar.R e mod_inconsistencias.R; o Painel usa os três.
source(here::here("modules", "mod_pessoal_auxiliar.R"))
source(here::here("modules", "mod_inconsistencias.R"))
source(here::here("modules", "mod_serventias.R"))
source(here::here("modules", "mod_magistrados.R"))
source(here::here("modules", "mod_painel.R"))

# Tabelas auxiliares do MPM (cargos, situações, sexo, raça, deficiência,
# cotas, competências, municípios, tipo/classificação de unidade...) no
# SQLite, com descrições no lugar de códigos nas telas — ver
# R/tabelas_referencia.R, a pasta referencias/ e o módulo Tabelas
# Auxiliares (Configurações). Depois dos módulos, que definem os padrões.
source(here::here("R", "tabelas_referencia.R"))
source(here::here("modules", "mod_tabelas_auxiliares.R"))

# =========================================================================
# MÓDULOS (valores internos do navset "menu")
# -------------------------------------------------------------------------
# Cada valor identifica um painel do navset_hidden "menu" e o botão da
# barra lateral que o abre (atributo data-menu). ABA_INICIAL é o módulo
# aberto após o login/logout.
# =========================================================================
ABA_SERVENTIAS <- "Serventias"
ABA_MAGISTRADOS <- "Magistrados"
ABA_PESSOAL_AUXILIAR <- "PessoalAuxiliar"
# Ordem, ícones e rótulos dos módulos na barra lateral — iguais aos do
# FarolJus Lite (Magistrados, Pessoal e Auxiliar, Serventias). O primeiro
# é o módulo aberto após o login.
MENU_MODULOS <- c(ABA_MAGISTRADOS, ABA_PESSOAL_AUXILIAR, ABA_SERVENTIAS)
MENU_IDS <- setNames(c("ir_magistrados", "ir_pessoal", "ir_serventias"), MENU_MODULOS)
MENU_ICONES <- setNames(c("gavel", "users", "building-columns"), MENU_MODULOS)
MENU_ROTULOS <- setNames(c("Magistrados", "Pessoal e Auxiliar", "Serventias"), MENU_MODULOS)

# Painel (indicadores de Magistrados e Servidores): item próprio do menu,
# logo acima de Configurações, como no FarolJus Lite.
ABA_PAINEL <- "Painel"

# Tabelas Auxiliares (Configurações): consulta das tabelas do MPM
# gravadas no SQLite; Gerenciar Arquivos só para administradores.
ABA_TABELAS_AUXILIARES <- "TabelasAuxiliares"

ABA_INICIAL <- MENU_MODULOS[1]

# =========================================================================
# ESQUEMA DO BANCO (usuarios_totp / login_auditoria + coluna "distro")
# -------------------------------------------------------------------------
# Precisa rodar logo após abrir a conexão e depois de source("auth_totp.R"),
# de onde vem garantir_schema_totp(). Cria as tabelas num banco novo e
# faz a migração da coluna "distro" num banco antigo. Idempotente.
# =========================================================================
garantir_schema_totp(con)

# Tabelas auxiliares do MPM: (re)carregadas no SQLite quando o arquivo
# de origem muda e lidas para os vetores usados pelos módulos — ver
# R/tabelas_referencia.R.
carregar_tabelas_referencia(con)

# =========================================================================
# MIGRAÇÃO DAS PASTAS DE PESSOAL E AUXILIAR (layout antigo -> Lite)
# -------------------------------------------------------------------------
# O módulo Pessoal e Auxiliar agora segue o layout do FarolJus Lite:
#   <PASTA_ALERTAS>/<EMPRESA>/pessoal_auxiliar/pessoal_auxiliar.xlsx (base)
#   <PASTA_ALERTAS>/<EMPRESA>/alertas_csv/<arquivos de alertas>
# Antes, tudo ficava em <EMPRESA>/pessoal/ (base como
# quadro_pessoal_auxiliar.*). Na inicialização, se a pasta antiga existir,
# os arquivos são COPIADOS para o lugar novo (sem sobrescrever nada que
# já esteja lá) e a pasta antiga é renomeada para
# pessoal_migrado_<data> — nada é apagado, e a cópia não se repete.
# =========================================================================
migrar_pastas_pessoal <- function(raiz = PASTA_ALERTAS_BASE) {
    
    if (!dir.exists(raiz)) {
        return(invisible(NULL))
    }
    
    for (pasta_antiga in file.path(list.dirs(raiz, recursive = FALSE), "pessoal")) {
        
        if (!dir.exists(pasta_antiga)) {
            next
        }
        
        empresa_dir <- dirname(pasta_antiga)
        
        tryCatch({
            
            arquivos <- list.files(pasta_antiga, full.names = TRUE)
            arquivos <- arquivos[extensao_arquivo(arquivos) %in% EXTENSOES_PLANILHA]
            arquivos <- arquivos[!grepl("^(~\\$|\\.~lock)", basename(arquivos))]
            
            eh_base <- normalizar_nome_arquivo(arquivos) %in%
                c("quadro_pessoal_auxiliar", "quadro_pessoal_e_auxiliar")
            
            pasta_base <- file.path(empresa_dir, SUBPASTA_PESSOAL)
            pasta_alertas <- file.path(empresa_dir, SUBPASTA_ALERTAS)
            dir.create(pasta_base, recursive = TRUE, showWarnings = FALSE)
            dir.create(pasta_alertas, recursive = TRUE, showWarnings = FALSE)
            
            for (arq in arquivos[eh_base][1]) {
                if (is.na(arq)) next
                destino <- file.path(pasta_base, paste0(RADICAL_BASE_PESSOAL, ".", extensao_arquivo(arq)))
                if (length(arquivos_base_pessoal(pasta_base)) == 0) {
                    file.copy(arq, destino, copy.date = TRUE)
                }
            }
            
            for (arq in arquivos[!eh_base]) {
                destino <- file.path(pasta_alertas, basename(arq))
                if (!file.exists(destino)) {
                    file.copy(arq, destino, copy.date = TRUE)
                }
            }
            
            renomeada <- file.path(empresa_dir, paste0("pessoal_migrado_", format(Sys.Date(), "%Y%m%d")))
            if (file.rename(pasta_antiga, renomeada)) {
                message("Pessoal e Auxiliar: arquivos de ", pasta_antiga,
                        " copiados para o novo layout; pasta antiga renomeada para ", renomeada, ".")
            }
            
        }, error = function(e) {
            warning("Não foi possível migrar ", pasta_antiga, ": ", conditionMessage(e))
        })
        
    }
    
    invisible(NULL)
    
}

migrar_pastas_pessoal()

# Quem é administrador (Administração TOTP e Trocar empresa). Sem
# ADMINS_AD no .Renviron, vale a regra antiga: QUALQUER login AD
# bem-sucedido é admin — ou seja, todo servidor com conta no AD pode
# criar acessos TOTP para qualquer empresa. Com ADMINS_AD (logins AD
# separados por vírgula, sem diferenciar maiúsculas, como no AD), só eles.
ADMINS_AD <- tolower(trimws(strsplit(Sys.getenv("ADMINS_AD", unset = ""), ",")[[1]]))
ADMINS_AD <- ADMINS_AD[nzchar(ADMINS_AD)]

if (length(ADMINS_AD) == 0) {
    message(
        "ADMINS_AD não definida: todo login AD tem acesso à Administração. ",
        "Defina ADMINS_AD no .Renviron para restringir."
    )
}

usuario_eh_admin_ad <- function(login) {
    if (length(ADMINS_AD) == 0) {
        return(TRUE)
    }
    !is.null(login) && tolower(trimws(login)) %in% ADMINS_AD
}

# Limite de tentativas inválidas por sessão do navegador (complementa o
# bloqueio por login de login_bloqueado(), em R/auth_totp.R, que impede
# testar códigos/senhas de um mesmo login; este impede usar uma única
# sessão para varrer muitos logins diferentes). Igual ao FarolJus Lite.
LOGIN_MAX_FALHAS_SESSAO <- suppressWarnings(as.integer(Sys.getenv("LOGIN_MAX_FALHAS_SESSAO", unset = "10")))
if (is.na(LOGIN_MAX_FALHAS_SESSAO) || LOGIN_MAX_FALHAS_SESSAO < 1) LOGIN_MAX_FALHAS_SESSAO <- 10L

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

      /* LOGIN CORPORATIVO e CÓDIGO AUTHENTICATOR LADO A LADO quando há
         espaço: o cartão do passo 1 fica mais largo e as duas opções
         dividem a linha (cada uma com no mínimo 220px). Em tela estreita
         (celular), não cabem as duas e cada uma ocupa a linha inteira,
         uma embaixo da outra, como antes. */

      .login-card.login-card-larga {
        max-width: 640px;
      }

      .metodo-opcoes .shiny-options-group {
        display: flex;
        flex-wrap: wrap;
        gap: .85rem;
        margin-bottom: .85rem;
      }

      .metodo-opcoes .shiny-options-group .radio {
        flex: 1 1 220px;
        margin: 0;
        display: flex;
      }

      .metodo-opcoes .shiny-options-group .radio label {
        height: 100%;
      }

      .metodo-opcoes .shiny-input-container {
        width: 100%;
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

      /* Botão do módulo que está aberto (Serventias / Magistrados /
         Pessoal e Auxiliar) — ver handler JS marcar-menu-ativo. */
      .icon-bar .icon-btn.ativo {
        background: rgba(255,255,255,.18);
        color: #fff;
        box-shadow: inset 3px 0 0 #6ea8fe;
      }

      /* =============================================
         GRUPO 'CONFIGURAÇÕES' — abre/fecha no próprio navegador
         (sem ida ao servidor), mostrando Trocar empresa, Ver
         instruções e Administração logo abaixo, DENTRO da barra
         lateral (mesmo funcionamento do FarolJus Lite).
         ============================================= */

      .icon-bar .icon-grupo {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 8px;
      }

      .icon-bar .icon-submenu {
        display: none;
        flex-direction: column;
        align-items: center;
        gap: 8px;
        padding: 6px 0;
        border-radius: 8px;
        background: rgba(255,255,255,.06);
      }

      .icon-bar .icon-grupo.aberto .icon-submenu {
        display: flex;
      }

      .icon-bar .icon-grupo.aberto > .icon-btn {
        background: rgba(255,255,255,.12);
        color: #fff;
      }

      .icon-bar .icon-submenu .icon-btn {
        width: 32px;
        height: 32px;
        font-size: 14px;
      }

      .icon-bar .icon-seta {
        display: none;
        margin-left: auto;
        font-size: 11px;
        transition: transform .15s ease;
      }

      .icon-bar .icon-grupo.aberto .icon-seta {
        transform: rotate(180deg);
      }

      .icon-bar.expandida .icon-grupo,
      .icon-bar.expandida .icon-submenu {
        align-items: stretch;
      }

      .icon-bar.expandida .icon-submenu {
        padding: 6px 0 6px 12px;
      }

      .icon-bar.expandida .icon-submenu .icon-btn {
        width: 100%;
        height: 32px;
      }

      .icon-bar.expandida .icon-seta {
        display: inline;
      }

      /* =============================================
         BARRA DE ROLAGEM EM CIMA DAS TABELAS
         — uma faixa só com a barra horizontal, logo acima do
         cabeçalho, espelhando a barra nativa de baixo (ver o script
         'rolagem dupla' em tags$head). Some quando a tabela cabe na
         largura.
         ============================================= */

      .dt-rolagem-topo {
        overflow-x: auto;
        overflow-y: hidden;
        height: 14px;
        margin-bottom: 4px;
      }

      .dt-rolagem-topo > div {
        height: 1px;
      }

      .dt-rolagem-topo.sem-rolagem {
        display: none;
      }

      /* As duas barras (em cima e embaixo) com o mesmo visual e sempre
         visíveis — sem isso, alguns navegadores só mostram a barra
         enquanto se rola. */
      .dt-rolagem-topo,
      .dataTables_scrollBody {
        scrollbar-width: thin;
        scrollbar-color: #9aa5b1 #eef1f4;
      }

      .dt-rolagem-topo::-webkit-scrollbar,
      .dataTables_scrollBody::-webkit-scrollbar {
        height: 10px;
      }

      .dt-rolagem-topo::-webkit-scrollbar-track,
      .dataTables_scrollBody::-webkit-scrollbar-track {
        background: #eef1f4;
        border-radius: 5px;
      }

      .dt-rolagem-topo::-webkit-scrollbar-thumb,
      .dataTables_scrollBody::-webkit-scrollbar-thumb {
        background: #9aa5b1;
        border-radius: 5px;
      }

      .dt-rolagem-topo::-webkit-scrollbar-thumb:hover,
      .dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
        background: #6c7a89;
      }

      /* =============================================
         JANELA 'PROCESSANDO O PAINEL' — aberta no navegador ao clicar
         em Painel e fechada quando os indicadores estão prontos (ver
         farolPainelProcessando(), no script abaixo).
         ============================================= */

      .painel-processando {
        position: fixed;
        inset: 0;
        z-index: 2000;
        display: none;
        align-items: center;
        justify-content: center;
        background: rgba(15, 23, 42, .45);
      }

      .painel-processando.aberta {
        display: flex;
      }

      .painel-processando-caixa {
        background: #fff;
        border-radius: 1rem;
        box-shadow: 0 1rem 2.5rem rgba(0, 0, 0, .25);
        padding: 1.75rem 2rem;
        max-width: 440px;
        width: calc(100% - 32px);
        text-align: center;
      }

      .painel-processando-icone {
        width: 56px;
        height: 56px;
        border-radius: 50%;
        margin: 0 auto 1rem;
        display: flex;
        align-items: center;
        justify-content: center;
        background: linear-gradient(135deg, #003366, #0d6efd);
        color: #fff;
        font-size: 1.4rem;
      }

      .painel-processando-titulo {
        font-weight: 700;
        font-size: 1.1rem;
        color: #003366;
        margin-bottom: .35rem;
      }

      .painel-processando-texto {
        font-size: .88rem;
        color: #6c757d;
        margin-bottom: 1.1rem;
      }

      .painel-processando-barra {
        height: 6px;
        border-radius: 3px;
        background: #e7eef7;
        overflow: hidden;
        position: relative;
      }

      .painel-processando-barra::after {
        content: '';
        position: absolute;
        top: 0;
        bottom: 0;
        width: 35%;
        border-radius: 3px;
        background: #0d6efd;
        animation: painel-processando-anda 1.1s ease-in-out infinite;
      }

      @keyframes painel-processando-anda {
        from { left: -35%; }
        to   { left: 100%; }
      }

      /* =============================================
         FILTROS BLOQUEADOS (aba sem arquivo) e JANELA DE AVISO
         'nenhum arquivo para exibir' — ver o handler JS
         'bloquear-filtros', abaixo, e mostrar_alerta_sem_arquivo(),
         em mod_pessoal_auxiliar.R.
         ============================================= */

      .al-filtros-aviso {
        display: none;
        align-items: center;
        font-size: .82rem;
        color: #664d03;
        background: #fff8e1;
        border: 1px solid #ffe69c;
        border-radius: .6rem;
        padding: .45rem .75rem;
        margin-bottom: .9rem;
      }

      .filtros-bloqueados .al-filtros-aviso {
        display: flex;
      }

      .filtros-bloqueados .form-control,
      .filtros-bloqueados .form-select,
      .filtros-bloqueados .selectize-input,
      .filtros-bloqueados .btn,
      .filtros-bloqueados .control-label {
        opacity: .55;
        cursor: not-allowed !important;
      }

      .filtros-bloqueados .selectize-input,
      .filtros-bloqueados .form-control {
        background: #f1f3f5 !important;
      }

      .modal-dialog:has(.al-sem-arquivo) {
        display: flex;
        align-items: center;
        min-height: calc(100% - 3.5rem);
      }

      .modal-content:has(.al-sem-arquivo) {
        border-radius: 1rem;
        border: 0;
        box-shadow: 0 1rem 2.5rem rgba(0, 0, 0, .25);
      }

      .modal-content:has(.al-sem-arquivo) .modal-footer {
        justify-content: center;
        border-top: 0;
        padding-top: 0;
        padding-bottom: 1.5rem;
      }

      .al-sem-arquivo {
        text-align: center;
        padding: 1rem .5rem 0;
      }

      .al-sem-arquivo-icone {
        width: 56px;
        height: 56px;
        border-radius: 50%;
        margin: 0 auto 1rem;
        display: flex;
        align-items: center;
        justify-content: center;
        background: linear-gradient(135deg, #b8860b, #f0ad4e);
        color: #fff;
        font-size: 1.4rem;
      }

      .al-sem-arquivo-titulo {
        font-weight: 700;
        font-size: 1.1rem;
        color: #003366;
        margin-bottom: .5rem;
      }

      .al-sem-arquivo-texto {
        font-size: .9rem;
        color: #495057;
        margin-bottom: .75rem;
      }

      .al-sem-arquivo-nota {
        font-size: .8rem;
        color: #6c757d;
      }

      @media (prefers-reduced-motion: reduce) {
        .painel-processando-barra::after {
          animation: none;
          left: 0;
          width: 100%;
          opacity: .5;
        }
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

      // Destaca, na barra lateral, o botão do módulo aberto.
      Shiny.addCustomMessageHandler(

        'marcar-menu-ativo',

        function(message) {

          document
            .querySelectorAll('.icon-bar .icon-btn[data-menu]')
            .forEach(function(el) {
              el.classList.toggle(
                'ativo',
                el.getAttribute('data-menu') === message.menu
              );
            });

        }

      );

      // Enviado pelos módulos de alertas (mod_pessoal_auxiliar.R, mod_magistrados.R e mod_serventias.R) depois de fechar os modais de
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

      // =================================================
      // JANELA 'PROCESSANDO O PAINEL'
      // -------------------------------------------------
      // Aberta no próprio navegador, no clique do item Painel do menu:
      // o servidor só manda mensagens ao fim do processamento, então
      // uma janela aberta por ele só apareceria com tudo pronto. Fecha
      // quando o servidor terminou (html sem .shiny-busy) e o conteúdo
      // do painel está desenhado (sem .recalculating), depois de um
      // tempo mínimo para não 'piscar'. Fecha de qualquer jeito depois
      // de PAINEL_ESPERA_MAX_MS.
      // =================================================
      var PAINEL_MIN_MS = 600;
      var PAINEL_ESPERA_MAX_MS = 120000;
      var painelVerificacao = null;

      // =================================================
      // FILTROS BLOQUEADOS (aba sem arquivo)
      // -------------------------------------------------
      // Enviado pelos módulos com {ids, bloqueado}: marca cada bloco
      // (.filtros-bloqueados) e desabilita os campos, os botões e as
      // listas (selectize). O estado é guardado e reaplicado quando um
      // campo é atualizado ou criado, ou uma aba aparece — senão um
      // updateSelectInput() ou a criação tardia da lista o desfaria.
      // =================================================
      (function() {

        var estado = {};

        function aplicar(id) {
          var el = document.getElementById(id);
          if (!el) return;
          var b = !!estado[id];
          el.classList.toggle('filtros-bloqueados', b);
          el.setAttribute('aria-disabled', b ? 'true' : 'false');
          el.querySelectorAll('input, button, textarea, select').forEach(function(c) {
            if (b) c.setAttribute('disabled', 'disabled'); else c.removeAttribute('disabled');
          });
          el.querySelectorAll('select').forEach(function(s) {
            if (s.selectize) { if (b) s.selectize.disable(); else s.selectize.enable(); }
          });
        }

        function aplicarTodos() {
          Object.keys(estado).forEach(aplicar);
        }

        Shiny.addCustomMessageHandler('bloquear-filtros', function(m) {
          [].concat(m.ids).forEach(function(id) {
            estado[id] = m.bloqueado;
            aplicar(id);
          });
          setTimeout(aplicarTodos, 300);
          setTimeout(aplicarTodos, 1200);
        });

        $(document).on('shiny:updateinput shiny:bound shown.bs.tab', function() {
          setTimeout(aplicarTodos, 0);
        });

      })();

      window.farolPainelProcessando = function() {

        var janela = document.getElementById('painel-processando');
        if (!janela) return;

        janela.classList.add('aberta');
        janela.setAttribute('aria-hidden', 'false');

        var inicio = Date.now();
        clearInterval(painelVerificacao);

        painelVerificacao = setInterval(function() {

          var decorrido = Date.now() - inicio;
          var saida = document.getElementById('painel-conteudo');
          var pronto = saida &&
            saida.children.length > 0 &&
            !saida.classList.contains('recalculating') &&
            !document.documentElement.classList.contains('shiny-busy');

          if ((pronto && decorrido >= PAINEL_MIN_MS) || decorrido >= PAINEL_ESPERA_MAX_MS) {
            clearInterval(painelVerificacao);
            janela.classList.remove('aberta');
            janela.setAttribute('aria-hidden', 'true');
          }

        }, 150);

      };

      // =================================================
      // ROLAGEM DUPLA NAS TABELAS (barra em cima e embaixo)
      // -------------------------------------------------
      // Vale para TODAS as tabelas DT com rolagem horizontal
      // (scrollX = TRUE), dos módulos e da administração, sem mudar o
      // código de cada uma. Quando uma tabela é desenhada, uma faixa
      // .dt-rolagem-topo é inserida acima do cabeçalho, com a mesma
      // largura de conteúdo da tabela; rolar uma das barras move a
      // outra. A largura é recalculada sempre que a tabela muda de
      // tamanho (filtro, página, coluna, aba que fica visível,
      // janela redimensionada) — tabelas em abas ocultas têm largura
      // 0 até serem exibidas.
      // =================================================
      (function() {

        // =================================================
        // CABEÇALHO ALINHADO AOS DADOS
        // -------------------------------------------------
        // Com scrollX, o DataTables desenha o cabeçalho numa tabela à
        // parte e só calcula a largura das colunas ao criar a tabela.
        // Quando a largura da área muda depois (menu recolhido/expandido,
        // janela redimensionada, aba ou tela que fica visível), o título
        // de cada coluna saía de cima dos seus dados. columns.adjust()
        // recalcula as larguras das tabelas visíveis; o ResizeObserver
        // (abaixo, em preparar()) chama isto sempre que a largura de uma
        // tabela muda, com uma pequena espera para agrupar os eventos.
        // =================================================
        var esperaAjuste = null;
        function ajustarColunas() {
          clearTimeout(esperaAjuste);
          esperaAjuste = setTimeout(function() {
            if (!$.fn.dataTable) return;
            $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust();
          }, 120);
        }
        window.farolAjustarColunas = ajustarColunas;

        function ajustar(topo, corpo) {
          var interno = topo.firstChild;
          interno.style.width = corpo.scrollWidth + 'px';
          var sobra = corpo.scrollWidth - corpo.clientWidth;
          topo.classList.toggle('sem-rolagem', sobra <= 1);
          if (topo.scrollLeft !== corpo.scrollLeft) {
            topo.scrollLeft = corpo.scrollLeft;
          }
        }

        function preparar(wrapper) {

          var rolagem = wrapper.querySelector('.dataTables_scroll');
          var corpo = wrapper.querySelector('.dataTables_scrollBody');
          if (!rolagem || !corpo) return;

          // A faixa fica logo antes da área de rolagem. No tema Bootstrap
          // 5, essa área está dentro de uma div de coluna (não é filha
          // direta do wrapper), então a faixa é procurada como irmã dela.
          var topo = rolagem.previousElementSibling;
          if (!topo || !topo.classList.contains('dt-rolagem-topo')) topo = null;

          if (!topo) {
            topo = document.createElement('div');
            topo.className = 'dt-rolagem-topo';
            topo.setAttribute('aria-hidden', 'true');
            topo.appendChild(document.createElement('div'));
            rolagem.parentNode.insertBefore(topo, rolagem);

            // Uma barra segue a outra. Só atribui quando as posições
            // diferem, então o eco de uma na outra para sozinho.
            topo.addEventListener('scroll', function() {
              if (corpo.scrollLeft !== topo.scrollLeft) corpo.scrollLeft = topo.scrollLeft;
            });
            corpo.addEventListener('scroll', function() {
              if (topo.scrollLeft !== corpo.scrollLeft) topo.scrollLeft = corpo.scrollLeft;
            });

            if (window.ResizeObserver) {
              var obs = new ResizeObserver(function() { ajustar(topo, corpo); });
              obs.observe(corpo);
              var tabela = corpo.querySelector('table');
              if (tabela) obs.observe(tabela);

              // Largura da área da tabela mudou: realinha cabeçalho e
              // dados (só quando muda a LARGURA, para não entrar em
              // laço com o próprio ajuste, que mexe na altura).
              var larguraAnterior = wrapper.clientWidth;
              var obsLargura = new ResizeObserver(function() {
                var w = wrapper.clientWidth;
                if (w > 0 && Math.abs(w - larguraAnterior) > 1) {
                  larguraAnterior = w;
                  ajustarColunas();
                }
              });
              obsLargura.observe(wrapper);
            }
          }

          ajustar(topo, corpo);
        }

        // init.dt / draw.dt disparam a cada tabela criada ou redesenhada
        // (inclusive quando o Shiny recria a tabela com dados novos).
        $(document).on('init.dt draw.dt column-sizing.dt', function(e, settings) {
          if (!settings || !settings.nTableWrapper) return;
          var wrapper = settings.nTableWrapper;
          setTimeout(function() { preparar(wrapper); }, 0);
        });

        $(window).on('resize', function() {
          document.querySelectorAll('.dataTables_wrapper').forEach(preparar);
          ajustarColunas();
        });

        // Uma tabela que estava numa aba oculta só ganha largura quando a
        // aba aparece.
        $(document).on('shown.bs.tab', function() {
          setTimeout(function() {
            document.querySelectorAll('.dataTables_wrapper').forEach(preparar);
            ajustarColunas();
          }, 50);
        });

        // Menu lateral expandido/recolhido (a área de conteúdo muda de
        // largura com transição de .2s).
        $(document).on('transitionend', '#app-content', ajustarColunas);

      })();

    "))
        
    ),
    
    # ===================================================
    # LOGIN
    # ===================================================
    
    uiOutput("tela_login"),
    
    # ===================================================
    # SISTEMA PRINCIPAL
    # ===================================================
    
    uiOutput("tela_principal"),

    # Janela "processando" do Painel (fica oculta; ver
    # farolPainelProcessando()).
    div(
        id = "painel-processando",
        class = "painel-processando",
        role = "status",
        `aria-live` = "polite",
        `aria-hidden` = "true",
        div(
            class = "painel-processando-caixa",
            div(class = "painel-processando-icone", icon("chart-pie")),
            div(class = "painel-processando-titulo", "Processando os dados"),
            div(
                class = "painel-processando-texto",
                "Lendo as bases de Magistrados e de Pessoal e Auxiliar e calculando ",
                "os indicadores dos painéis de BI. Aguarde um instante."
            ),
            div(class = "painel-processando-barra")
        )
    )

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
    
    # Tentativas de login inválidas nesta sessão do navegador (ver
    # LOGIN_MAX_FALHAS_SESSAO).
    falhasSessao <- reactiveVal(0L)
    
    # Verificações comuns aos dois métodos de login, ANTES de consultar o
    # AD ou o banco: limite por sessão e bloqueio por login (força bruta).
    # Devolve TRUE se a tentativa pode seguir.
    tentativa_permitida <- function(login_informado) {
        
        if (falhasSessao() >= LOGIN_MAX_FALHAS_SESSAO) {
            showNotification(
                "Muitas tentativas inválidas nesta sessão. Recarregue a página e tente mais tarde.",
                type = "error",
                duration = 10
            )
            return(FALSE)
        }
        
        if (login_bloqueado(con, login_informado)) {
            registrar_auditoria(con, login_informado, "BLOQUEADO", FALSE)
            falhasSessao(falhasSessao() + 1L)
            showNotification(
                paste0(
                    "Muitas tentativas inválidas para este usuário. ",
                    "Aguarde ", LOGIN_JANELA_MIN, " minutos e tente novamente."
                ),
                type = "error",
                duration = 10
            )
            return(FALSE)
        }
        
        TRUE
        
    }
    
    # ===================================================
    # ESTADO DO CABEÇALHO
    # ===================================================
    
    header_oculto <- reactiveVal(FALSE)
    
    # Barra lateral (ícones) expandida ou não — ver ícone "toggle_icon_bar"
    # e o handler JS "toggle-icon-bar". Começa EXPANDIDA (com as descrições
    # ao lado dos ícones), como no FarolJus Lite.
    iconBarExpandida <- reactiveVal(TRUE)
    
    # ===================================================
    # MÓDULO SELECIONADO
    # ---------------------------------------------------
    # Começa na aba inicial (ABA_INICIAL = Magistrados, o primeiro item
    # da barra lateral). Precisa ser uma aba que existe, senão o
    # módulo correspondente fica com ativo() = FALSE até o usuário clicar
    # em uma aba.
    # ===================================================
    
    menuSelecionado <- reactiveVal(ABA_INICIAL)
    
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
        isTRUE(autenticado()) &&
            identical(metodoAutenticado(), "AD") &&
            usuario_eh_admin_ad(usuarioLogado())
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
            
            login_informado <- trimws(input$usuario)
            
            if (!tentativa_permitida(login_informado)) {
                return(invisible(NULL))
            }
            
            # O detalhe técnico do erro (servidor, porta, mensagem do
            # ldap3) vai só para o log do servidor — na tela, mensagem
            # genérica, para não expor a infraestrutura a quem não entrou.
            dados <- tryCatch(
                authenticate_ad(
                    login_informado,
                    input$senha,
                    distro_escolhida
                ),
                error = function(e) {
                    
                    warning("Erro ao consultar o Active Directory (", distro_escolhida, "): ", conditionMessage(e))
                    
                    showNotification(
                        "Não foi possível consultar o Active Directory agora. Tente novamente em instantes.",
                        type = "error"
                    )
                    
                    NULL
                    
                }
            )
            
            registrar_auditoria(
                con,
                login_informado,
                paste0("AD:", distro_escolhida),
                !is.null(dados)
            )
            
            if (!is.null(dados)) {
                
                autenticado(TRUE)
                usuarioLogado(login_informado)
                dadosUsuario(dados)
                
                metodoAutenticado("AD")
                distroSelecionado(distro_escolhida)
                menuSelecionado(ABA_INICIAL)
                
                # Garante que a aba volte para a aba inicial sem recriar a UI toda
                updateTabsetPanel(
                    session,
                    "menu",
                    selected = ABA_INICIAL
                )
                
                showNotification(
                    paste(
                        "Bem-vindo",
                        obter_campo(dados, "displayName")
                    ),
                    type = "message"
                )
                
            } else {
                
                falhasSessao(falhasSessao() + 1L)
                
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
            
            login_informado <- trimws(input$usuario_totp)
            
            if (!tentativa_permitida(login_informado)) {
                return(invisible(NULL))
            }
            
            dados <- autenticar_totp(
                con,
                login_informado,
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
                menuSelecionado(ABA_INICIAL)
                
                updateTabsetPanel(
                    session,
                    "menu",
                    selected = ABA_INICIAL
                )
                
                showNotification(
                    paste(
                        "Bem-vindo",
                        dados$displayName
                    ),
                    type = "message"
                )
                
            } else {
                
                falhasSessao(falhasSessao() + 1L)
                
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
            menuSelecionado(ABA_INICIAL)
            
            # A tela principal é recriada no próximo login com o cabeçalho
            # visível e a barra expandida — o estado precisa acompanhar,
            # senão o primeiro clique nesses ícones "não faz nada".
            header_oculto(FALSE)
            iconBarExpandida(TRUE)
            
            # Ver comentário na definição de resetarAdminTotp acima.
            adminAberto(FALSE)
            resetarAdminTotp(resetarAdminTotp() + 1)
            
            # Recarrega a página: descarta TODO o estado da sessão (dados
            # e filtros dos módulos — CPF/nome digitados —, chave
            # recém-gerada no admin etc.), para que nada do usuário anterior
            # fique na memória da sessão ou visível para quem fizer login
            # em seguida na mesma aba. (Igual ao FarolJus Lite.)
            session$reload()
            
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
    
    # ===================================================
    # NAVEGAÇÃO PELOS BOTÕES DA BARRA LATERAL
    # ---------------------------------------------------
    # Troca o painel do navset_hidden "menu", atualiza menuSelecionado()
    # (que controla o `ativo` de cada módulo) e destaca o botão.
    # ===================================================
    
    selecionar_menu <- function(aba) {
        menuSelecionado(aba)
        nav_select("menu", selected = aba, session = session)
        session$sendCustomMessage("marcar-menu-ativo", list(menu = aba))
    }
    
    observeEvent(input$ir_serventias, {
        selecionar_menu(ABA_SERVENTIAS)
    }, ignoreInit = TRUE)
    
    observeEvent(input$ir_magistrados, {
        selecionar_menu(ABA_MAGISTRADOS)
    }, ignoreInit = TRUE)
    
    observeEvent(input$ir_pessoal, {
        selecionar_menu(ABA_PESSOAL_AUXILIAR)
    }, ignoreInit = TRUE)
    
    observeEvent(input$ir_painel, {
        selecionar_menu(ABA_PAINEL)
    }, ignoreInit = TRUE)
    
    observeEvent(input$ir_tabelas_auxiliares, {
        selecionar_menu(ABA_TABELAS_AUXILIARES)
    }, ignoreInit = TRUE)
    
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
                # No passo de escolha do método, o cartão fica mais largo
                # para os dois métodos caberem lado a lado (ver CSS
                # .login-card-larga / .metodo-opcoes).
                class = paste(
                    "login-card shadow-sm p-4",
                    if (is.null(metodoAcesso())) "login-card-larga"
                ),
                
                div(
                    class = "logo-container mb-4",
                    
                    tags$img(
                        src = "img/FarolJus_logo_principal.png",
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
                
                # -----------------------------------------------
                # MÓDULOS — substituem as antigas abas. data-menu liga
                # o botão ao painel do navset "menu" e serve para
                # destacar o módulo aberto (handler "marcar-menu-ativo").
                # -----------------------------------------------
                
                # -----------------------------------------------
                # MÓDULOS — mesma ordem e ícones do FarolJus Lite. data-menu
                # liga o botão ao painel do navset "menu" e serve para
                # destacar o módulo aberto (handler "marcar-menu-ativo").
                # -----------------------------------------------
                
                lapply(MENU_MODULOS, function(aba) {
                    actionLink(
                        MENU_IDS[[aba]],
                        tagList(
                            icon(MENU_ICONES[[aba]]),
                            tags$span(class = "icon-label", MENU_ROTULOS[[aba]])
                        ),
                        class = paste(
                            "icon-btn",
                            if (identical(isolate(menuSelecionado()), aba)) "ativo"
                        ),
                        title = MENU_ROTULOS[[aba]],
                        `data-menu` = aba
                    )
                }),
                
                # Painel (indicadores de Magistrados e Servidores), logo
                # acima de Configurações, como no FarolJus Lite. Abre a
                # janela "Processando os dados" na hora do clique (ver
                # farolPainelProcessando()).
                actionLink(
                    "ir_painel",
                    tagList(
                        icon("chart-pie"),
                        tags$span(class = "icon-label", "Painel")
                    ),
                    class = paste(
                        "icon-btn",
                        if (identical(isolate(menuSelecionado()), ABA_PAINEL)) "ativo"
                    ),
                    title = "Painel",
                    `data-menu` = ABA_PAINEL,
                    onclick = "window.farolPainelProcessando && window.farolPainelProcessando();"
                ),
                
                # -----------------------------------------------
                # CONFIGURAÇÕES — agrupa Trocar empresa, Ver instruções e
                # Administração (as duas de admin só para quem pode). O
                # clique em "Configurações" só abre/fecha o grupo no
                # navegador (classe .aberto, ver CSS .icon-grupo), e as
                # opções aparecem logo abaixo, dentro da própria barra
                # lateral. Cada opção continua sendo um actionLink com o
                # mesmo id de antes, tratado no servidor.
                # -----------------------------------------------
                
                div(
                    class = "icon-grupo",
                    
                    tags$a(
                        href = "#",
                        class = "icon-btn",
                        title = "Configurações",
                        role = "button",
                        `aria-expanded` = "false",
                        onclick = paste0(
                            "var g = this.parentElement; g.classList.toggle('aberto');",
                            "this.setAttribute('aria-expanded', g.classList.contains('aberto'));",
                            "return false;"
                        ),
                        icon("gear"),
                        tags$span(class = "icon-label", "Configurações"),
                        tags$span(class = "icon-seta", icon("chevron-down"))
                    ),
                    
                    div(
                        class = "icon-submenu",
                        
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
                        
                        # Tela das tabelas auxiliares do MPM (SQLite) —
                        # consulta para todos; Gerenciar Arquivos só para
                        # administradores (ver mod_tabelas_auxiliares.R).
                        actionLink(
                            "ir_tabelas_auxiliares",
                            tagList(
                                icon("table-list"),
                                tags$span(class = "icon-label", "Tabelas Auxiliares")
                            ),
                            class = paste(
                                "icon-btn",
                                if (identical(isolate(menuSelecionado()), ABA_TABELAS_AUXILIARES)) "ativo"
                            ),
                            title = "Tabelas Auxiliares",
                            `data-menu` = ABA_TABELAS_AUXILIARES
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
                        }
                    )
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
                            src = "img/FarolJus_logo_principal.png",
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
                # MÓDULOS (painéis sem abas)
                # -----------------------------------------------
                # A troca de painel é feita pelos botões "Magistrados",
                # "Pessoal e Auxiliar" e "Serventias" da barra lateral (ver
                # selecionar_menu() no server).
                # ===============================================
                
                navset_hidden(
                    id = "menu",
                    selected = isolate(menuSelecionado()),
                    nav_panel_hidden(
                        value = ABA_MAGISTRADOS,
                        mod_magistrados_ui("magistrados")
                    ),
                    nav_panel_hidden(
                        value = ABA_PESSOAL_AUXILIAR,
                        mod_pessoal_auxiliar_ui("pessoal_auxiliar")
                    ),
                    nav_panel_hidden(
                        value = ABA_SERVENTIAS,
                        mod_serventias_ui("serventias")
                    ),
                    nav_panel_hidden(
                        value = ABA_PAINEL,
                        mod_painel_ui("painel")
                    ),
                    nav_panel_hidden(
                        value = ABA_TABELAS_AUXILIARES,
                        mod_tabelas_auxiliares_ui("tabelas_auxiliares")
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
    
    # Os módulos não recebem mais `con`: os códigos do MPM vêm das
    # constantes carregadas do SQLite na inicialização (ver
    # R/tabelas_referencia.R).
    #
    # `ativo` e `empresa` exigem sessão AUTENTICADA: os módulos são
    # iniciados para toda sessão (inclusive na tela de login), e é a
    # empresa que libera leitura, envio e exclusão de arquivos. Antes,
    # `empresa` era o próprio distroSelecionado e `ativo` valia TRUE para
    # Serventias já na tela de login.
    empresa_sessao <- reactive({
        if (isTRUE(autenticado())) distroSelecionado() else NULL
    })
    
    modulo_ativo <- function(aba) {
        reactive(isTRUE(autenticado()) && identical(menuSelecionado(), aba))
    }
    
    mod_serventias_server(
        "serventias",
        ativo = modulo_ativo(ABA_SERVENTIAS),
        empresa = empresa_sessao
    )
    
    mod_magistrados_server(
        "magistrados",
        ativo = modulo_ativo(ABA_MAGISTRADOS),
        empresa = empresa_sessao
    )
    
    mod_pessoal_auxiliar_server(
        "pessoal_auxiliar",
        ativo = modulo_ativo(ABA_PESSOAL_AUXILIAR),
        empresa = empresa_sessao
    )
    
    # Painel: lê as bases só quando é aberto (e quando algum arquivo
    # mudou), com o mesmo cache de leitura dos módulos.
    # Tabelas Auxiliares: consulta para quem está logado; Gerenciar
    # Arquivos (troca as tabelas de todas as empresas) só para admin.
    mod_tabelas_auxiliares_server(
        "tabelas_auxiliares",
        con = con,
        ativo = modulo_ativo(ABA_TABELAS_AUXILIARES),
        pode_gerenciar = reactive(ehAdmin())
    )
    
    mod_painel_server(
        "painel",
        ativo = modulo_ativo(ABA_PAINEL),
        empresa = empresa_sessao
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
