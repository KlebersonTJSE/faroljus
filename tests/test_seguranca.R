# =====================================================
# tests/test_seguranca.R
# -----------------------------------------------------
# Testes de segurança e desempenho do FarolJus (mesma estrutura dos
# testes do FarolJus Lite). Rode a partir da raiz do projeto:
#
#   Rscript tests/test_seguranca.R
#
# Não usa o banco real, o Active Directory nem o IRIS: copia o app para
# uma pasta temporária e roda tudo lá, com um SQLite novo. O ldap3
# (Python) e o driver JDBC são substituídos por versões falsas. Depende
# de shiny, DBI, RSQLite, digest, openssl, DT, bslib, jsonlite, here,
# readr, readxl, dplyr, purrr, stringr; ggplot2/echarts4r/ggiraph,
# reticulate e RJDBC são opcionais aqui (os gráficos não são exercitados).
# =====================================================

if (!isTRUE(l10n_info()$`UTF-8`)) {
    for (loc in c("C.UTF-8", "en_US.UTF-8", "pt_BR.UTF-8")) {
        if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
    }
}

suppressPackageStartupMessages({
    library(shiny)
    library(DBI)
    library(RSQLite)
})

raiz_projeto <- normalizePath(".")
stopifnot(file.exists(file.path(raiz_projeto, "app.R")))

# ---- mini framework ---------------------------------------------------

falhas <- 0L
total  <- 0L

checar <- function(descricao, condicao) {
    total <<- total + 1L
    ok <- isTRUE(tryCatch(condicao, error = function(e) {
        message("    erro: ", conditionMessage(e)); FALSE
    }))
    if (!ok) falhas <<- falhas + 1L
    cat(if (ok) "  ok    " else "  FALHA ", descricao, "\n", sep = "")
}

# ---- cópia isolada do app --------------------------------------------

dir_teste <- file.path(tempdir(), "faroljus_teste")
unlink(dir_teste, recursive = TRUE)
dir.create(file.path(dir_teste, "data"), recursive = TRUE)
for (item in c("app.R", "README.md", "R", "modules", "img", "referencias")) {
    file.copy(file.path(raiz_projeto, item), dir_teste, recursive = TRUE)
}

Sys.setenv(
    DISTRO_1 = "TJSE",
    DISTRO_2 = "MPRO",
    TJSE_LDAP_SERVER = "ldap.teste", TJSE_LDAP_PORT = "636",
    TJSE_LDAP_DOMAIN = "teste.local", TJSE_LDAP_SEARCH_BASE = "DC=teste,DC=local",
    MPRO_LDAP_SERVER = "ldap.teste", MPRO_LDAP_PORT = "636",
    MPRO_LDAP_DOMAIN = "teste.local", MPRO_LDAP_SEARCH_BASE = "DC=teste,DC=local",
    PASTA_ALERTAS = file.path(dir_teste, "data"),
    RETICULATE_PYTHON = file.path(R.home("bin"), "R"),
    ADMINS_AD = "",
    LOGIN_MAX_FALHAS = "5",
    LOGIN_JANELA_MIN = "15"
)

# ---- substitutos (só nos testes) --------------------------------------

# Pula pacotes ausentes que não são exercitados aqui.
library <- function(package, ...) {
    nome <- as.character(substitute(package))
    opcionais <- c("echarts4r", "ggiraph", "ggplot2", "reticulate", "RJDBC")
    if (nome %in% opcionais && !requireNamespace(nome, quietly = TRUE)) {
        return(invisible(NULL))
    }
    base::library(nome, character.only = TRUE, ...)
}

if (!requireNamespace("echarts4r", quietly = TRUE)) {
    renderEcharts4r <- function(expr, ...) shiny::renderUI(NULL)
    echarts4rOutput <- function(...) shiny::div()
}
if (!requireNamespace("ggiraph", quietly = TRUE)) {
    renderGirafe <- function(expr, ...) shiny::renderUI(NULL)
    girafeOutput <- function(...) shiny::div()
}

# ldap3 falso: aceita só "ana" / "senha certa" e registra cada bind e
# cada filtro de busca, para os testes conferirem o que foi enviado.
ldap_log <- new.env()
ldap_log$binds <- character(0)
ldap_log$filtros <- character(0)
ldap_log$atributos <- list()

ldap3_falso <- list(
    NONE = "NONE",
    ALL_ATTRIBUTES = "*",
    Server = function(host, ...) list(host = host),
    Connection = function(servidor, user, password, ...) {
        estado <- new.env()
        estado$entries <- list()
        estado$bind <- function() {
            ldap_log$binds <- c(ldap_log$binds, user)
            identical(user, "ana@teste.local") && identical(password, "senha certa")
        }
        estado$search <- function(search_base, search_filter, attributes, ...) {
            ldap_log$filtros <- c(ldap_log$filtros, search_filter)
            ldap_log$atributos[[length(ldap_log$atributos) + 1]] <- attributes
            estado$entries <- list(list(entry_attributes_as_dict = list(
                sAMAccountName = list("ana"), displayName = list("Ana Teste"),
                department = list("TI"), manager = list("CN=Chefe,OU=x")
            )))
            TRUE
        }
        estado$unbind <- function() TRUE
        estado
    }
)
use_python <- function(...) invisible(NULL)
import <- function(modulo, ...) ldap3_falso
py_to_r <- function(x) x
JDBC <- function(...) stop("JDBC não disponível nos testes")

# Carrega o app sem a chamada final shinyApp(...)
carregar_app <- function() {
    linhas <- readLines(file.path(dir_teste, "app.R"), encoding = "UTF-8", warn = FALSE)
    inicio_shinyapp <- max(grep("^shinyApp\\(", linhas))
    old <- setwd(dir_teste)
    on.exit(setwd(old))
    suppressWarnings(suppressMessages(
        eval(parse(text = linhas[seq_len(inicio_shinyapp - 1)], encoding = "UTF-8"), envir = globalenv())
    ))
}

carregar_app()

info_maria <- cadastrar_usuario_totp(con, "maria", "Maria", "TJSE")

chave <- function(login) {
    dbGetQuery(con, "SELECT secret_key FROM usuarios_totp WHERE login = ?", params = list(login))$secret_key
}
ativo_de <- function(login) {
    dbGetQuery(con, "SELECT ativo FROM usuarios_totp WHERE login = ?", params = list(login))$ativo
}
limpar_auditoria <- function() invisible(dbExecute(con, "DELETE FROM login_auditoria"))

# =====================================================
cat("\n[1] Administração TOTP sem permissão (ativo = FALSE)\n")
# =====================================================

chave_maria_antes <- chave("maria")

testServer(mod_totp_admin_server, args = list(con = con, ativo = reactive(FALSE)), {
    session$setInputs(login = "maria", nome = "x", distro = "TJSE")
    session$setInputs(cadastrar = 1)
    checar("não recadastra a chave de um usuário", identical(chave("maria"), chave_maria_antes))
    checar(
        "não expõe segredo em resultado_cadastro",
        inherits(tryCatch(output$resultado_cadastro, error = function(e) e), "error")
    )
    session$setInputs(login = "invasor", nome = "Invasor", distro = "TJSE", cadastrar = 2)
    checar("não cria usuário novo", length(chave("invasor")) == 0)
    session$setInputs(login_desativar = "maria", desativar = 1)
    checar("não desativa usuário", ativo_de("maria") == 1)
})

cat("\n[2] Administração TOTP com permissão (ativo = TRUE)\n")

testServer(mod_totp_admin_server, args = list(con = con, ativo = reactive(TRUE)), {
    session$setInputs(login = "joao", nome = "João", distro = "TJSE", cadastrar = 1)
    checar("admin cadastra usuário", length(chave("joao")) == 1)
    checar("admin vê o segredo gerado",
           grepl(chave("joao"), as.character(output$resultado_cadastro$html), fixed = TRUE))
    session$setInputs(login_desativar = "joao", desativar = 1)
    checar("admin desativa usuário", ativo_de("joao") == 0)
})

# =====================================================
cat("\n[3] TOTP\n")
# =====================================================

segredo_rfc <- base32_encode(charToRaw("12345678901234567890"))
checar("base32 bate com a RFC 4648", segredo_rfc == "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ")
checar("TOTP bate com a RFC 6238", totp_gerar(segredo_rfc, tempo = 59, digitos = 8) == "94287082")
checar("decode(encode(x)) == x", identical(base32_decode(base32_encode(as.raw(0:255))), as.raw(0:255)))

segredos <- replicate(50, gerar_totp_secret())
checar("segredo tem 32 caracteres base32", all(grepl("^[A-Z2-7]{32}$", segredos)))
checar("segredos não se repetem", length(unique(segredos)) == 50)
set.seed(1); s1 <- gerar_totp_secret(); set.seed(1); s2 <- gerar_totp_secret()
checar("segredo não depende de set.seed()", s1 != s2)

limpar_auditoria()
invisible(dbExecute(con, "UPDATE usuarios_totp SET ultimo_contador = NULL"))
codigo <- totp_gerar(chave("maria"))
checar("código válido autentica", !is.null(autenticar_totp(con, "maria", codigo)))
checar("mesmo código não pode ser reutilizado", is.null(autenticar_totp(con, "maria", codigo)))
checar("código de 30s atrás (já usado) é recusado",
       is.null(autenticar_totp(con, "maria", totp_gerar(chave("maria"), Sys.time() - 30))))
checar("recadastro zera o anti-replay", {
    cadastrar_usuario_totp(con, "maria", "Maria", "TJSE")
    !is.null(autenticar_totp(con, "maria", totp_gerar(chave("maria"))))
})

# =====================================================
cat("\n[4] Bloqueio por tentativas\n")
# =====================================================

limpar_auditoria()
for (i in 1:4) invisible(autenticar_totp(con, "maria", "000000"))
checar("4 falhas ainda não bloqueiam", !login_bloqueado(con, "maria"))
invisible(autenticar_totp(con, "maria", "000000"))
checar("5 falhas bloqueiam", login_bloqueado(con, "maria"))
checar("bloqueio é por login (outro login livre)", !login_bloqueado(con, "joao"))

limpar_auditoria()
invisible(dbExecute(
    con,
    "INSERT INTO login_auditoria (login, metodo, sucesso, datahora) VALUES ('maria','TOTP',0,?)",
    params = list(format(Sys.time() - 20 * 60, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
))
for (i in 1:4) invisible(autenticar_totp(con, "maria", "000000"))
checar("falhas fora da janela não contam", !login_bloqueado(con, "maria"))

checar("índices da auditoria existem", all(c("idx_login_auditoria_login", "idx_login_auditoria_datahora") %in%
    dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'index'")$name))

# =====================================================
cat("\n[5] Login AD (ldap3 falso)\n")
# =====================================================

ldap_log$binds <- character(0)
checar("usuário com caractere de filtro LDAP é recusado sem bind",
       is.null(authenticate_ad("a*)(cn=*", "x", "TJSE")) && length(ldap_log$binds) == 0)
checar("usuário com @ (UPN de outro domínio) é recusado sem bind",
       is.null(authenticate_ad("ana@outro.dominio", "senha certa", "TJSE")) && length(ldap_log$binds) == 0)
checar("senha só com espaços é recusada sem bind",
       is.null(authenticate_ad("ana", "   ", "TJSE")) && length(ldap_log$binds) == 0)
checar("escape de filtro LDAP (RFC 4515)", escapar_filtro_ldap("a*(b)\\") == "a\\2a\\28b\\29\\5c")
dados_ana <- authenticate_ad("ana", "senha certa", "TJSE")
checar("login AD válido autentica", identical(obter_campo(dados_ana, "displayName"), "Ana Teste"))
checar("busca pede só os atributos usados (não ALL_ATTRIBUTES)",
       !identical(ldap_log$atributos[[length(ldap_log$atributos)]], "*") &&
           "displayName" %in% unlist(ldap_log$atributos[[length(ldap_log$atributos)]]))
checar("config LDAP não fica em variável global", !exists("LDAP_SERVER", envir = globalenv()))

# =====================================================
cat("\n[6] Fluxo de login do app (server completo)\n")
# =====================================================

limpar_auditoria()
testServer(server, {
    session$setInputs(entrar = 0, entrar_totp = 0)
    checar("módulos inativos antes do login", !isTRUE(autenticado()))

    session$setInputs(usuario = "ana", senha = "errada", distro_ad = "TJSE", entrar = 1)
    checar("senha AD errada não autentica", !autenticado())

    session$setInputs(usuario = "ana", senha = "senha certa", distro_ad = "OUTRA", entrar = 2)
    checar("empresa fora da lista é recusada", !autenticado())

    session$setInputs(usuario = "ana", senha = "senha certa", distro_ad = "TJSE", entrar = 3)
    checar("login AD válido autentica e vira admin (sem ADMINS_AD)", autenticado() && ehAdmin())
})

limpar_auditoria()
ADMINS_AD <- "outra.pessoa"
testServer(server, {
    session$setInputs(entrar = 0)
    session$setInputs(usuario = "ana", senha = "senha certa", distro_ad = "TJSE", entrar = 1)
    checar("com ADMINS_AD, login AD fora da lista não é admin", autenticado() && !ehAdmin())
})
ADMINS_AD <- character(0)

limpar_auditoria()
testServer(server, {
    session$setInputs(entrar = 0)
    for (i in 1:5) {
        session$setInputs(usuario = "ana", senha = "errada", distro_ad = "TJSE", entrar = i)
    }
    session$setInputs(usuario = "ana", senha = "senha certa", distro_ad = "TJSE", entrar = 6)
    checar("AD: login bloqueado recusa até a senha certa", !autenticado())
    checar("AD: tentativa durante bloqueio é auditada",
           dbGetQuery(con, "SELECT COUNT(*) n FROM login_auditoria WHERE metodo='BLOQUEADO'")$n == 1)
})

limpar_auditoria()
testServer(server, {
    session$setInputs(entrar_totp = 0)
    for (i in 1:5) {
        session$setInputs(usuario_totp = "maria", codigo_totp = "111111", entrar_totp = i)
    }
    invisible(dbExecute(con, "UPDATE usuarios_totp SET ultimo_contador = NULL"))
    session$setInputs(usuario_totp = "maria", codigo_totp = totp_gerar(chave("maria")), entrar_totp = 6)
    checar("TOTP: login bloqueado recusa até o código certo", !autenticado())
})

limpar_auditoria()
testServer(server, {
    session$setInputs(entrar_totp = 0)
    for (i in 1:10) {
        session$setInputs(usuario_totp = paste0("u", i), codigo_totp = "111111", entrar_totp = i)
    }
    n_antes <- dbGetQuery(con, "SELECT COUNT(*) n FROM login_auditoria")$n
    session$setInputs(usuario_totp = "u11", codigo_totp = "111111", entrar_totp = 11)
    checar("após 10 falhas, a sessão para de tentar",
           dbGetQuery(con, "SELECT COUNT(*) n FROM login_auditoria")$n == n_antes)
})

limpar_auditoria()
invisible(dbExecute(con, "UPDATE usuarios_totp SET ultimo_contador = NULL"))
testServer(server, {
    recarregou <- FALSE
    session$reload <- function() recarregou <<- TRUE
    session$setInputs(entrar_totp = 0, sair = 0)
    session$setInputs(usuario_totp = "maria", codigo_totp = totp_gerar(chave("maria")), entrar_totp = 1)
    checar("usuário TOTP entra", autenticado() && !ehAdmin())
    session$setInputs(sair = 1)
    checar("logout limpa a sessão e recarrega a página", !autenticado() && recarregou)
})

# =====================================================
cat("\n[7] Auditoria\n")
# =====================================================

h <- function(x) format(converter_datahora_auditoria(x), "%H:%M")
checar("registro novo em UTC aparece na hora local", h("2026-10-15T13:00:00Z") == "10:00")
checar("registro antigo (hora local) é mantido", h("2026-10-15 10:00:00") == "10:00")
invisible(registrar_auditoria(con, "x", "TOTP", FALSE))
gravado <- dbGetQuery(con, "SELECT datahora FROM login_auditoria ORDER BY id DESC LIMIT 1")$datahora
checar("auditoria é gravada em UTC (ISO 8601 com Z)", grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", gravado))
invisible(registrar_auditoria(con, strrep("a", 5000), "TOTP", FALSE))
checar("login gigante é truncado na auditoria",
       dbGetQuery(con, "SELECT MAX(length(login)) n FROM login_auditoria")$n <= 100)

# =====================================================
cat("\n[8] Upload: nome de arquivo\n")
# =====================================================

checar("nome normal é mantido",
       nome_arquivo_seguro("Alertas quadro pessoal e auxiliar.csv") == "Alertas quadro pessoal e auxiliar.csv")
checar("acentos são mantidos", identical(nome_arquivo_seguro("Órgãos ação.csv"), "Órgãos ação.csv"))
checar("../ é removido", nome_arquivo_seguro("../../MPRO/pessoal/x.csv") == "x.csv")
checar("\\ do Windows é removido", nome_arquivo_seguro("..\\..\\app.csv") == "app.csv")
checar("extensão fora de .csv/.xlsx é recusada", is.na(nome_arquivo_seguro("../../app.R")))
checar(".csv sem nome é recusado", is.na(nome_arquivo_seguro(".csv")))

# =====================================================
cat("\n[9] Desempenho: cache de arquivos e consolidação\n")
# =====================================================

arq <- file.path(tempdir(), "cache_teste.csv")
writeLines(c("CPF;Nome", "1;A", "2;B"), arq)
leituras <- 0L
ler <- function() { leituras <<- leituras + 1L; readr::read_csv2(arq, show_col_types = FALSE) }
a1 <- com_cache_arquivo(arq, "t", ler)
a2 <- com_cache_arquivo(arq, "t", ler)
checar("segunda leitura vem do cache", leituras == 1L && identical(a1, a2))
Sys.sleep(1.1)
writeLines(c("CPF;Nome", "1;A", "2;B", "3;C"), arq)
a3 <- com_cache_arquivo(arq, "t", ler)
checar("arquivo alterado é relido", leituras == 2L && nrow(a3) == 3)
limpar_cache_arquivos(tempdir())
invisible(com_cache_arquivo(arq, "t", ler))
checar("limpar_cache_arquivos(pasta) força releitura", leituras == 3L)
checar("resultado vazio não fica em cache", {
    n <- 0L
    com_cache_arquivo(arq, "vazio", function() { n <<- n + 1L; data.frame() })
    com_cache_arquivo(arq, "vazio", function() { n <<- n + 1L; data.frame() })
    n == 2L
})

# Implementação antiga (apply por linha), para comparar o resultado.
consolidar_antigo <- function(df, colunas) {
    if (length(colunas) == 0) return(rep(NA_character_, nrow(df)))
    matriz <- as.matrix(df[colunas])
    apply(matriz, 1, function(linha) {
        preenchidos <- !is.na(linha) & stringr::str_trim(linha) != ""
        if (!any(preenchidos)) return(NA_character_)
        sufixos <- stringr::str_trim(stringr::str_remove(colunas[preenchidos], "^[^:]*:"))
        valores <- stringr::str_trim(linha[preenchidos])
        paste0(sufixos, ": ", valores, collapse = " | ")
    })
}
set.seed(42)
n <- 3000
df_alertas <- data.frame(
    CPF = as.character(seq_len(n)),
    `Alerta: Nome` = sample(c(NA, "", " x ", "Nome divergente"), n, TRUE),
    `Alerta: Data Nascimento` = sample(c(NA, "", "Data errada"), n, TRUE),
    `Alerta: Email` = sample(c(NA, "inválido", "  "), n, TRUE),
    check.names = FALSE, stringsAsFactors = FALSE
)
cols <- grep("^Alerta", names(df_alertas), value = TRUE)
checar("consolidação vetorizada = antiga",
       identical(consolidar_colunas_por_linha(df_alertas, cols), unname(consolidar_antigo(df_alertas, cols))))

# Tabelas auxiliares: semeadas no SQLite e lidas para as constantes.
checar("tabelas auxiliares semeadas no SQLite",
       dbGetQuery(con, "SELECT COUNT(*) n FROM tab_cargo_servidor")$n == length(CARGO_SERVIDOR))
invisible(dbExecute(con, "UPDATE tab_cargo_servidor SET descricao = 'Estagiário(a) TESTE' WHERE codigo = '5'"))
carregar_tabelas_referencia(con)
checar("ajuste feito no banco vale para a tradução", unname(CARGO_SERVIDOR["5"]) == "Estagiário(a) TESTE")
invisible(dbExecute(con, "UPDATE tab_cargo_servidor SET descricao = 'Estagiário(a)' WHERE codigo = '5'"))
carregar_tabelas_referencia(con)

# =====================================================
cat("\n[10] Módulos: carga sob demanda\n")
# =====================================================

pasta_serv <- file.path(Sys.getenv("PASTA_ALERTAS"), "TJSE", "serventias")
dir.create(pasta_serv, recursive = TRUE, showWarnings = FALSE)
writeLines(c("Código Serventia;Nome Serventia;CEP;Status", "1;Vara A;49000-000;Ativo"),
           file.path(pasta_serv, "serventias.csv"))

testServer(mod_serventias_server,
           args = list(ativo = reactive(FALSE), empresa = reactive("TJSE")), {
    session$flushReact()
    checar("módulo fechado não lê arquivos", nrow(dados_base()) == 0)
})

ativo_rv <- reactiveVal(FALSE)
empresa_rv <- reactiveVal("TJSE")
testServer(mod_serventias_server,
           args = list(ativo = ativo_rv, empresa = empresa_rv), {
    ativo_rv(TRUE)
    session$flushReact()
    checar("módulo aberto carrega a base", nrow(dados_base()) == 1)
    empresa_rv(NULL)
    session$flushReact()
    checar("sem empresa (logout), dados são descartados", nrow(dados_base()) == 0)
})

testServer(mod_serventias_server,
           args = list(ativo = reactive(TRUE), empresa = reactive(NULL)), {
    session$flushReact()
    checar("sem empresa, nada é carregado", nrow(dados_base()) == 0)
})

# =====================================================
cat("\n[11] Menu lateral: módulos e Configurações\n")
# =====================================================

pos_em <- function(h, x) regexpr(x, h, fixed = TRUE)[1]
limpar_auditoria()
testServer(server, {
    session$setInputs(entrar = 0)
    session$setInputs(usuario = "ana", senha = "senha certa", distro_ad = "TJSE", entrar = 1)
    h <- as.character(output$tela_principal$html)
    p_mag <- pos_em(h, 'data-menu="Magistrados"')
    p_pes <- pos_em(h, 'data-menu="PessoalAuxiliar"')
    p_serv <- pos_em(h, 'data-menu="Serventias"')
    p_cfg <- pos_em(h, 'class="icon-grupo"')
    p_sair <- pos_em(h, 'id="sair"')
    p_painel <- pos_em(h, 'data-menu="Painel"')
    checar("ordem: Magistrados, Pessoal e Auxiliar, Serventias, Painel, Configurações, Sair",
           all(c(p_mag, p_pes, p_serv, p_painel, p_cfg, p_sair) > 0) &&
               p_mag < p_pes && p_pes < p_serv && p_serv < p_painel && p_painel < p_cfg && p_cfg < p_sair)
    checar("Painel abre a janela 'Processando os dados'",
           grepl("farolPainelProcessando", h, fixed = TRUE))
    checar("barra começa expandida", grepl('class="icon-bar expandida"', h, fixed = TRUE))
    checar("módulo inicial é Magistrados", identical(menuSelecionado(), ABA_MAGISTRADOS))
    session$setInputs(ir_painel = 1)
    checar("clicar em Painel seleciona o Painel", identical(menuSelecionado(), ABA_PAINEL))
    sub <- regmatches(h, regexpr('class="icon-submenu".*?id="sair"', h))
    checar("opções de Configurações ficam dentro da barra (Trocar empresa, Ver instruções, Administração)",
           length(sub) == 1 && pos_em(sub, 'id="trocar_empresa"') > 0 &&
               pos_em(sub, 'id="trocar_empresa"') < pos_em(sub, 'id="mostrar_readme"') &&
               pos_em(sub, 'id="mostrar_readme"') < pos_em(sub, 'id="abrir_admin"'))
    checar("submenu flutuante antigo removido", !grepl("submenu-config", h, fixed = TRUE))
})

testServer(server, {
    session$setInputs(continuar = 0, voltar_metodo = 0)
    h <- as.character(output$tela_login$html)
    checar("tela inicial: cartão largo com os dois métodos (lado a lado se couber)",
           grepl("login-card-larga", h, fixed = TRUE) &&
               grepl("Login Corporativo", h, fixed = TRUE) && grepl("Código Authenticator", h, fixed = TRUE))
    session$setInputs(metodo_acesso = "ad", continuar = 1)
    h <- as.character(output$tela_login$html)
    checar("depois de escolher o método, cartão volta à largura normal", !grepl("login-card-larga", h, fixed = TRUE))
})

# Migração do layout antigo de Pessoal e Auxiliar (<EMPRESA>/pessoal).
raiz_mig <- file.path(tempdir(), "migracao")
unlink(raiz_mig, recursive = TRUE)
dir.create(file.path(raiz_mig, "TJSE", "pessoal"), recursive = TRUE)
writeLines("CPF;Nome", file.path(raiz_mig, "TJSE", "pessoal", "quadro_pessoal_auxiliar.csv"))
writeLines("CPF;Alerta: X", file.path(raiz_mig, "TJSE", "pessoal", "alertas mpm.csv"))
migrar_pastas_pessoal(raiz_mig)
checar("migração: base vai para pessoal_auxiliar/pessoal_auxiliar.csv",
       file.exists(file.path(raiz_mig, "TJSE", "pessoal_auxiliar", "pessoal_auxiliar.csv")))
checar("migração: alertas vão para alertas_csv/",
       file.exists(file.path(raiz_mig, "TJSE", "alertas_csv", "alertas mpm.csv")))
checar("migração: pasta antiga é renomeada (nada apagado)",
       !dir.exists(file.path(raiz_mig, "TJSE", "pessoal")) &&
           length(list.files(file.path(raiz_mig, "TJSE"), pattern = "^pessoal_migrado_")) == 1)

limpar_auditoria()
invisible(dbExecute(con, "UPDATE usuarios_totp SET ultimo_contador = NULL"))
testServer(server, {
    session$setInputs(entrar_totp = 0)
    session$setInputs(usuario_totp = "maria", codigo_totp = totp_gerar(chave("maria")), entrar_totp = 1)
    h <- as.character(output$tela_principal$html)
    checar("usuário TOTP vê só Ver instruções em Configurações",
           grepl('id="mostrar_readme"', h, fixed = TRUE) &&
               !grepl('id="trocar_empresa"', h, fixed = TRUE) && !grepl('id="abrir_admin"', h, fixed = TRUE))
})

# =====================================================
# TESTES DOS MÓDULOS VINDOS DO FAROLJUS LITE
# -----------------------------------------------------
# Seções [8], [9], [10], [12], [13], [14] e [15] de
# tests/test_seguranca.R do FarolJus Lite, renumeradas como [L8]...
# =====================================================

limpar_cache_arquivos()
codigo_app <- paste(readLines(file.path(dir_teste, "app.R"), encoding = "UTF-8", warn = FALSE), collapse = "\n")

# =====================================================
cat("\n[L8] Pessoal e Auxiliar: aba Alertas (filtros e mensagens)\n")
# =====================================================

escrever_csv <- function(linhas, arquivo) {
    dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
    writeLines(enc2utf8(linhas), arquivo, useBytes = TRUE)
    invisible(arquivo)
}

CAB_ORGAO <- "Órgão de lotação do(a) Servidor(a) ou Auxiliar"
CAB_SIT <- "Situação profissional atual"
CAB_INICIO <- "Data de início da situação"
CAB_SAIDA <- "Data de saída da situação"

escrever_csv(c(
    paste("CPF;Nome", CAB_SIT, "Cargo", CAB_INICIO, CAB_ORGAO, "Alerta: Email;Conflito: Duplicidade no próprio Tribunal", sep = ";"),
    "00011122233;FULANA (TESTE) DA SILVA;3;1;01/01/2026;VARA A;E-mail não informado;",
    "44455566677;BELTRANO SOUZA;1;5;01/02/2026;VARA B;;2 registros ativos"
), file.path(caminho_alertas("TJSE"), "teste.csv"))

testServer(mod_pessoal_auxiliar_server, args = list(empresa = reactive("TJSE"), ativo = reactive(TRUE)), {
    session$setInputs(alerta = "Todos", conflito = "Todos", cpf = "", nome = "")
    checar("carrega o CSV de alertas da empresa", nrow(dados()) == 2)

    session$setInputs(nome = "(TESTE")
    checar("parêntese no filtro de nome não quebra", nrow(dados_filtrados()) == 1)

    session$setInputs(nome = "", cpf = "000.111.222-33")
    checar("CPF com pontuação encontra o registro", nrow(dados_filtrados()) == 1)

    session$setInputs(cpf = "[")
    checar("colchete no filtro de CPF não quebra", nrow(dados_filtrados()) == 0)
    checar("sem resultado mostra a mensagem de filtro", grepl("filtros", mensagem_sem_linhas()))

    session$setInputs(cpf = "")
    checar("tabela traduz o código do cargo",
           any(grepl("^Estagi", tabela_exibicao()$Cargo)) && !any(tabela_exibicao()$Cargo %in% c("1", "5")))
})

testServer(mod_pessoal_auxiliar_server, args = list(empresa = reactive("MPRO"), ativo = reactive(TRUE)), {
    checar("empresa sem arquivos: tabela de alertas fica vazia (aviso vai para a janela)", isFALSE(mensagem_sem_linhas()))
})

# =====================================================
cat("\n[L9] Regras de inconsistência corrigidas\n")
# =====================================================

hoje_teste <- as.Date("2026-10-15")
serv <- function(cpf, sit, inicio, orgao = "VARA X", cargo = "1", saida = NA) {
    df <- data.frame(cpf, "P", orgao, sit, cargo, inicio, saida, stringsAsFactors = FALSE)
    names(df) <- c("CPF", "Nome", CAB_ORGAO, CAB_SIT, "Cargo", CAB_INICIO, CAB_SAIDA)
    df
}
verif <- function(df, ...) verificar_inconsistencias_servidores(df, hoje = hoje_teste, ...)
tem <- function(r, v) any(r$`Verificação` == v)

r <- verif(rbind(serv("1", "7", "01/01/2020"), serv("1", "8", "01/06/2024")))
checar("servidor aposentado que depois falece NÃO é 'retorno'", !tem(r, VERIFICACAO_SERV_DEFINITIVA))

r <- verif(rbind(serv("2", "7", "01/01/2021"), serv("2", "3", "01/03/2022")))
checar("aposentado que volta a situação ativa é apontado (Alta)",
       tem(r, VERIFICACAO_SERV_DEFINITIVA) && r$Gravidade[r$`Verificação` == VERIFICACAO_SERV_DEFINITIVA][1] == "Alta")

r <- verif(rbind(serv("3", "8", "01/01/2023"), serv("3", "9", "01/01/2024")))
checar("depois de falecimento, qualquer registro continua sendo apontado", tem(r, VERIFICACAO_SERV_DEFINITIVA))

lot <- lotacoes_ativas_pessoas(
    rbind(serv("4", "3", "10/08/2026", orgao = "VARA NOVA"), serv("4", "3", NA, orgao = "VARA ANTIGA")),
    REGEX_COL_ORGAO_SERVIDOR, SITUACOES_SERV_ENCERRADAS
)
checar("lotação atual é o registro datado mais recente (não o sem data)", identical(lot$orgao, "VARA NOVA"))

lot <- lotacoes_ativas_pessoas(rbind(serv("5", "3", NA, orgao = "A"), serv("5", "3", NA, orgao = "B")),
                               REGEX_COL_ORGAO_SERVIDOR, SITUACOES_SERV_ENCERRADAS)
checar("sem nenhuma data, vale o último do arquivo", identical(lot$orgao, "B"))

excl <- serv("6", "3", "01/01/2030")
excl[["Exclusão de registro por erro"]] <- "S"
checar("registro marcado como exclusão por erro não é verificado", nrow(verif(excl)) == 0)
excl[["Exclusão de registro por erro"]] <- "N"
checar("o mesmo registro sem a marca é apontado (data no futuro)", tem(verif(excl), VERIFICACAO_SERV_DATAS))

mag <- function(cpf, sit, inicio) {
    df <- data.frame(cpf, "M", "100", sit, "1", inicio, stringsAsFactors = FALSE)
    names(df) <- c("CPF", "Nome", "Órgão de lotação do magistrado(a)", CAB_SIT, "Cargo", CAB_INICIO)
    df
}
r <- verificar_inconsistencias_magistrados(rbind(mag("7", "12", "01/01/2019"), mag("7", "13", "10/03/2025")))
checar("magistrado aposentado que depois falece NÃO é 'retorno'", nrow(r) == 0)
r <- verificar_inconsistencias_magistrados(rbind(mag("8", "12", "01/01/2019"), mag("8", "8", "10/03/2025")))
checar("magistrado aposentado que volta a atuar é apontado", any(r$`Verificação` == VERIFICACAO_MAG_APOSENTADORIA))

# =====================================================
cat("\n[L10] Pessoal e Auxiliar: base de dados e upload\n")
# =====================================================

dir_up <- file.path(tempdir(), "uploads_teste")
dir.create(dir_up, showWarnings = FALSE)

base_csv <- escrever_csv(c(
    paste("CPF;Nome", CAB_ORGAO, CAB_SIT, "Cargo", CAB_INICIO, CAB_SAIDA, "Status", sep = ";"),
    "11111111111;ANA;10;3;1;10/08/2026;;Ativo",
    "11111111111;ANA;20;3;1;;;Inativo",
    "22222222222;BRUNO;20;7;1;01/01/2020;;Inativo",
    "22222222222;BRUNO;20;8;1;01/06/2024;;Inativo",
    "33333333333;CARLA;20;1;4;01/01/2022;;Ativo"
), file.path(dir_up, "base.csv"))

alertas_csv <- escrever_csv(c(
    paste("CPF;Nome", CAB_SIT, "Cargo", CAB_INICIO, CAB_ORGAO, "Alerta: Email", sep = ";"),
    "99999999999;DANI;3;1;01/01/2026;20;E-mail não informado"
), file.path(dir_up, "alertas.csv"))

invalido_csv <- escrever_csv(c("Coluna A;Coluna B", "1;2"), file.path(dir_up, "notas.csv"))

escrever_csv(c(
    "Código Serventia;Nome da Serventia;Status;CEP",
    "10;VARA DEZ;Inativo;49000000",
    "20;VARA VINTE;Ativo;49000001"
), file.path(pasta_serventias("TJSE"), "serventias.csv"))

checar("planilha da base é reconhecida como base",
       identificar_tipo_arquivo_pessoal(ler_arquivo_dados(base_csv, "csv")) == "base")
checar("arquivo de alertas é reconhecido como alertas",
       identificar_tipo_arquivo_pessoal(ler_arquivo_dados(alertas_csv, "csv")) == "alertas")
checar("arquivo sem CPF/órgão não é reconhecido",
       is.na(identificar_tipo_arquivo_pessoal(ler_arquivo_dados(invalido_csv, "csv"))))

unlink(caminho_alertas("TJSE"), recursive = TRUE)

testServer(mod_pessoal_auxiliar_server, args = list(empresa = reactive("TJSE"), ativo = reactive(TRUE)), {
    session$setInputs(gerenciar_arquivos = 1)
    session$setInputs(upload_arquivos_1 = data.frame(
        name = c("../../x/qualquer_nome.csv", "../../MPRO/alertas_csv/evil.csv", "notas.csv"),
        size = file.size(c(base_csv, alertas_csv, invalido_csv)),
        type = "text/csv",
        datapath = c(base_csv, alertas_csv, invalido_csv),
        stringsAsFactors = FALSE
    ))
    session$setInputs(enviar_arquivos = 1)

    checar("base gravada com o nome fixo na pasta própria",
           identical(list.files(pasta_pessoal("TJSE")), "pessoal_auxiliar.csv"))
    checar("alertas gravados na pasta da empresa com nome saneado",
           identical(list.files(caminho_alertas("TJSE")), "evil.csv"))
    checar("nada foi gravado fora da pasta da empresa", length(list.files(file.path(dir_teste, "data", "MPRO"), recursive = TRUE)) == 0)
    checar("arquivo não reconhecido é ignorado", !any(grepl("notas", list.files(file.path(dir_teste, "data"), recursive = TRUE))))
    checar("base carregada (5 registros)", nrow(dados_base()) == 5)

    session$setInputs(base_status = "Ativo", base_cpf = "", base_nome = "", base_detalhe = "",
                      base_cargo = "Todos", base_situacao = "Todos", base_orgao = "Todos")
    session$elapse(500)
    checar("filtro Status = Ativo", nrow(dados_base_filtrados()) == 2)

    session$setInputs(base_status = "Todos", base_cpf = "111.111.111-11")
    session$elapse(500)
    checar("filtro CPF da base aceita pontuação", nrow(dados_base_filtrados()) == 2)

    checar("órgão aparece como 'código - nome da serventia'",
           any(base_exibicao()[[CAB_ORGAO]] == "10 - VARA DEZ"))
    checar("cargo da base por extenso", any(grepl("^Servidor", base_exibicao()$Cargo)))

    checar("inconsistências usam a base quando ela existe", usa_base())
    r <- inconsistencias()
    checar("lotação atual em serventia inativa é apontada (ANA, VARA DEZ)",
           any(r$`Verificação` == VERIFICACAO_SERV_LOTACAO & r$CPF == "11111111111"))
    checar("aposentado que faleceu não é apontado como retorno (BRUNO)",
           !any(r$`Verificação` == VERIFICACAO_SERV_DEFINITIVA & r$CPF == "22222222222"))

    # Nova base com uma já existente: pede confirmação e substitui.
    session$setInputs(gerenciar_arquivos = 2)
    session$setInputs(upload_arquivos_2 = data.frame(
        name = "base2.csv", size = file.size(base_csv), type = "text/csv", datapath = base_csv
    ))
    session$setInputs(enviar_arquivos = 2)
    checar("base nova aguarda confirmação quando já existe uma", !is.null(upload_pendente()))
    session$setInputs(manter_existentes = 1)
    checar("base substituída continua única", identical(list.files(pasta_pessoal("TJSE")), "pessoal_auxiliar.csv"))
})

# Cópia antes de apagar: se a cópia falha, o arquivo anterior fica.
pasta_g <- file.path(tempdir(), "gravacao")
dir.create(pasta_g, showWarnings = FALSE)
antigo <- file.path(pasta_g, "serventias.xlsx")
writeLines("antigo", antigo)
ok <- suppressWarnings(gravar_arquivo_substituindo(file.path(pasta_g, "nao_existe.csv"), file.path(pasta_g, "serventias.csv"), antigo))
checar("falha na cópia mantém o arquivo anterior", !ok && file.exists(antigo))
novo <- file.path(pasta_g, "novo.csv"); writeLines("novo", novo)
ok <- gravar_arquivo_substituindo(novo, file.path(pasta_g, "serventias.csv"), antigo)
checar("cópia bem-sucedida substitui o anterior",
       ok && !file.exists(antigo) && identical(readLines(file.path(pasta_g, "serventias.csv")), "novo") &&
           length(list.files(pasta_g, all.files = TRUE, no.. = TRUE)) == 2)

# Upload de Serventias: um .csv novo substitui um .xlsx antigo do mesmo tipo.
pasta_srv <- pasta_serventias("TJSE")
writeLines("antigo", file.path(pasta_srv, "serventias.xlsx"))
srv_csv <- file.path(dir_up, "minha_base.csv")
file.copy(file.path(pasta_srv, "serventias.csv"), srv_csv, overwrite = TRUE)
unlink(file.path(pasta_srv, "serventias.csv"))
testServer(mod_serventias_server, args = list(empresa = reactive("TJSE"), ativo = reactive(TRUE)), {
    session$setInputs(gerenciar_arquivos = 1)
    session$setInputs(upload_serventias_1 = data.frame(
        name = "../minha_base.csv", size = file.size(srv_csv), type = "text/csv", datapath = srv_csv
    ))
    session$setInputs(enviar_arquivos = 1)
    session$setInputs(confirmar_substituir = 1)
    checar("Serventias: .csv novo substitui o .xlsx antigo",
           identical(list.files(pasta_srv), "serventias.csv"))
    checar("Serventias: base nova carregada", nrow(dados_base()) == 2)
})

# Planilha de exemplo (opcional, fora do repositório):
#   FAROLJUS_XLSX_EXEMPLO=/caminho/Quadro_pessoal_e_auxiliar_exemplo.xlsx Rscript tests/test_seguranca.R
xlsx_exemplo <- Sys.getenv("FAROLJUS_XLSX_EXEMPLO")
if (nzchar(xlsx_exemplo) && file.exists(xlsx_exemplo)) {
    df_ex <- ler_arquivo_dados(xlsx_exemplo, "xlsx")
    checar("planilha de exemplo (.xlsx) é reconhecida como base", identificar_tipo_arquivo_pessoal(df_ex) == "base")
    checar("planilha de exemplo carrega todas as linhas", nrow(limpar_dados_lidos(df_ex)) == nrow(df_ex))
    checar("verificações rodam sobre a planilha de exemplo",
           is.data.frame(verificar_inconsistencias_servidores(limpar_dados_lidos(df_ex))))
}

# =====================================================
cat("\n[L12] Painel: indicadores de Magistrados e Servidores\n")
# =====================================================

checar("códigos do MPM traduzidos (raça, gênero, sexo)",
       identical(traduzir_dominio_painel(c("4", "05", "Branco(a)", "", "7"), RACA_COR_PAINEL),
                 c("Negro(a)-Pardo(a)", "Negro(a)-Preto(a)", "Branco(a)", PAINEL_EM_BRANCO, PAINEL_NAO_DECLARADO)) &&
           identical(traduzir_dominio_painel("1", IDENTIDADE_GENERO_PAINEL), "Cisgênero") &&
           identical(traduzir_sexo_painel(c("F", "m", "Feminino", "N", NA)),
                     c("Feminino", "Masculino", "Feminino", PAINEL_NAO_INFORMADO, PAINEL_EM_BRANCO)))
checar("deficiência com vários códigos conta a pessoa uma vez e cada tipo",
       identical(classificar_deficiencia_painel(tipos_deficiencia_painel(c("1, 3", "8", "10", "", "Não"))),
                 c(PAINEL_COM_DEFICIENCIA, PAINEL_SEM_DEFICIENCIA, PAINEL_NAO_DECLARADO, PAINEL_EM_BRANCO, PAINEL_SEM_DEFICIENCIA)))
checar("idade completa só depois do aniversário e faixas de 10 anos",
       identical(idade_em_anos(as.Date(c("1980-10-16", "1980-10-17", "2020-01-01")), as.Date("2026-10-16")),
                 c(46L, 45L, NA)) &&
           identical(faixa_etaria_painel(c(29L, 30L, 75L)), c("Até 29 anos", "30 a 39 anos", "70 anos ou mais")))

base_srv_painel <- data.frame(
    CPF = c("11111111111", "11111111111", "22222222222", "3333333333", "44444444444"),
    `Situação profissional atual` = c("3", "3", "7", "1", "3"),
    Cargo = c("1", "1", "1", "4", "5"),
    `Data de início da situação` = c("01/02/2020", "10/08/2026", "01/01/2020", "01/01/2022", "01/03/2023"),
    `Data Nascimento` = c("15/06/1970", "", "01/01/1950", "20/12/1990", "05/05/2003"),
    Sexo = c("F", "", "M", "M", "F"),
    `Identidade de gênero` = c("1", "1", "1", "2", "9"),
    `Raça / Cor` = c("4", "4", "2", "5", "6"),
    `Deficiência` = c("8", "1,3", "8", "8", ""),
    Status = c("Inativo", "Ativo", "Inativo", "Ativo", "Ativo"),
    check.names = FALSE
)
ps <- pessoas_painel(base_srv_painel, PAINEL_SERVIDORES, SITUACOES_SERV_ENCERRADAS, CARGO_SERVIDOR, as.Date("2026-10-16"))$pessoas
checar("painel conta cada CPF uma vez e tira aposentados",
       nrow(ps) == 3 && !("22222222222" %in% ps$CPF) && "03333333333" %in% ps$CPF)
ana <- ps[ps$CPF == "11111111111", ]
checar("campo vazio no registro atual usa o último preenchido (sexo, nascimento)",
       ana$sexo == "Feminino" && ana$idade == 56L && ana$deficiencia == PAINEL_COM_DEFICIENCIA &&
           identical(ana$tipos_deficiencia[[1]], c("Física/motora", "Visual")))
checar("cargo do servidor aparece por extenso", "Servidor(a) comissionado(a) sem vínculo" %in% ps$cargo)
r_raca <- resumo_painel(ps$raca)
checar("resumo põe \"sem informação\" no fim e soma 100%",
       tail(r_raca$categoria, 1) == PAINEL_NAO_INFORMADO && abs(sum(r_raca$pct) - 100) < 1e-9)
checar("tipos de deficiência são contados entre as pessoas com deficiência",
       identical(resumo_tipos_deficiencia(ps)$n, c(1L, 1L)))
sem_cols <- pessoas_painel(base_srv_painel[, c("CPF", "Cargo", "Situação profissional atual")],
                           PAINEL_SERVIDORES, SITUACOES_SERV_ENCERRADAS, CARGO_SERVIDOR)
checar("sem as colunas, os indicadores ficam vazios (não \"Em branco\")",
       !sem_cols$colunas[["raca"]] && all(is.na(sem_cols$pessoas$raca)) && nrow(sem_cols$pessoas) == 3)
exp_csv <- tabela_exportacao_painel(ps)
checar("CSV do painel traz todos os indicadores",
       all(c("Faixa etária", "Sexo", "Identidade de gênero", "Raça/cor", "Deficiência", "Cargo") %in% exp_csv$Indicador))

# Bases da empresa TJSE para o servidor do painel.
dir.create(pasta_pessoal("TJSE"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv2(base_srv_painel, file.path(pasta_pessoal("TJSE"), "pessoal_auxiliar.csv"))
dir.create(pasta_magistrados("TJSE"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv2(data.frame(
    CPF = c("55555555555", "66666666666"),
    `Órgão de lotação do magistrado(a)` = c("10", "20"),
    `Situação profissional atual` = c("8", "12"),
    Cargo = c("3", "1"),
    `Data de início da situação` = c("01/01/2015", "01/01/2010"),
    `Data Nascimento` = c("01/01/1965", "01/01/1950"),
    Sexo = c("F", "M"), `Identidade de gênero` = c("1", "1"),
    `Raça / Cor` = c("2", "2"), `Deficiência` = c("8", "8"),
    check.names = FALSE
), file.path(pasta_magistrados("TJSE"), "magistrados.csv"))

testServer(mod_painel_server, args = list(ativo = reactive(TRUE), empresa = reactive("TJSE"),
                                          hoje = function() as.Date("2026-10-16")), {
    session$flushReact()
    p <- dados()$pessoas
    checar("painel do servidor lê magistrados e servidores da empresa",
           sum(p$publico == PAINEL_MAGISTRADOS) == 1 && sum(p$publico == PAINEL_SERVIDORES) == 3)
    h <- paste(as.character(output$conteudo$html), collapse = "")
    checar("painel mostra os seis indicadores",
           all(vapply(c("Faixa etária", "Sexo", "Identidade de gênero", "Raça/cor", "Deficiência", "Cargo"),
                      function(x) grepl(x, h, fixed = TRUE), logical(1))))
    checar("com Todos, há comparativo Magistrados × Servidores", grepl("Comparativo Magistrados", h, fixed = TRUE))
    session$setInputs(publico = PAINEL_MAGISTRADOS, cargo = "Todos")
    checar("filtro Magistrados mostra só magistrados", nrow(pessoas_filtradas()) == 1)
    session$setInputs(publico = PAINEL_SERVIDORES, cargo = "Estagiário(a)")
    checar("filtro de cargo restringe as pessoas", nrow(pessoas_filtradas()) == 1)
})

limpar_auditoria()
invisible(dbExecute(con, "UPDATE usuarios_totp SET ultimo_contador = NULL"))
testServer(server, {
    session$setInputs(entrar_totp = 0)
    session$setInputs(usuario_totp = "maria", codigo_totp = totp_gerar(chave("maria")), entrar_totp = 1)
    h <- paste(as.character(output$tela_principal$html), collapse = "")
    p_serv <- pos_em(h, 'id="ir_serventias"')
    p_painel <- pos_em(h, 'id="ir_painel"')
    p_config <- pos_em(h, 'title="Configurações"')
    checar("item Painel fica logo acima de Configurações",
           p_painel > 0 && p_serv < p_painel && p_painel < p_config)
    checar("tela do Painel existe para usuário comum", grepl('data-value="Painel"', h, fixed = TRUE))
    session$setInputs(ir_painel = 1)
    checar("menu Painel abre o painel", menuSelecionado() == ABA_PAINEL)
})
checar("lotações ativas sem nenhuma linha têm chave texto (Serventias junta as duas)",
       is.character(lotacoes_ativas_pessoas(base_srv_painel, REGEX_COL_ORGAO_SERVIDOR, SITUACOES_SERV_ENCERRADAS)$chave))
ui_app <- paste(as.character(ui), collapse = "")
checar("janela \"Processando os dados\" do Painel existe e começa oculta",
       grepl('id="painel-processando"', ui_app, fixed = TRUE) && grepl('aria-hidden="true"', ui_app, fixed = TRUE) &&
           grepl("window.farolPainelProcessando = function", codigo_app, fixed = TRUE))
limpar_auditoria()
invisible(dbExecute(con, "UPDATE usuarios_totp SET ultimo_contador = NULL"))
testServer(server, {
    session$setInputs(entrar_totp = 0)
    session$setInputs(usuario_totp = "maria", codigo_totp = totp_gerar(chave("maria")), entrar_totp = 1)
    h <- paste(as.character(output$tela_principal$html), collapse = "")
    checar("clique em Painel abre a janela de processamento",
           grepl('id="ir_painel"[^>]*onclick="window.farolPainelProcessando', h) ||
               grepl('onclick="window.farolPainelProcessando[^"]*"[^>]*id="ir_painel"', h))
})

# =====================================================
cat("\n[L13] Pessoal e Auxiliar: aba Envio e Gerenciar Arquivos\n")
# =====================================================

# Mesmas colunas da base (o arquivo de envio do MPM não tem Status).
envio_csv <- escrever_csv(c(
    paste("CPF;Nome", CAB_ORGAO, CAB_SIT, "Cargo", CAB_INICIO, CAB_SAIDA, sep = ";"),
    "44444444444;DIEGO;20;3;1;01/09/2026;",
    "55555555555;ELIS;10;2;5;10/09/2026;30/09/2026",
    "66666666666;FABIO;20;3;1;15/09/2026;"
), file.path(dir_up, "quadro_pessoal_e_auxiliar_envio.csv"))

df_envio <- ler_arquivo_dados(envio_csv, "csv")
checar("arquivo com \"envio\" no nome é reconhecido como envio",
       identificar_tipo_arquivo_pessoal(df_envio, "quadro_pessoal_e_auxiliar_envio.csv") == "envio" &&
           identificar_tipo_arquivo_pessoal(df_envio, "Pessoal Auxiliar - ENVIO.xlsx") == "envio")
checar("mesmo arquivo sem \"envio\" no nome continua sendo base",
       identificar_tipo_arquivo_pessoal(df_envio, "pessoal_auxiliar.csv") == "base" &&
           identificar_tipo_arquivo_pessoal(df_envio, "reenviado.csv") == "base")
checar("alertas com \"envio\" no nome continuam sendo alertas",
       identificar_tipo_arquivo_pessoal(ler_arquivo_dados(alertas_csv, "csv"), "alertas_envio.csv") == "alertas")

testServer(mod_pessoal_auxiliar_server, args = list(empresa = reactive("TJSE"), ativo = reactive(TRUE)), {
    session$flushReact()
    n_base <- nrow(dados_base())
    session$setInputs(gerenciar_arquivos = 1)
    session$setInputs(upload_arquivos_1 = data.frame(
        name = "quadro_pessoal_e_auxiliar_envio.csv", size = file.size(envio_csv),
        type = "text/csv", datapath = envio_csv, stringsAsFactors = FALSE
    ))
    session$setInputs(enviar_arquivos = 1)
    checar("envio gravado com o nome fixo, ao lado da base",
           setequal(list.files(pasta_pessoal("TJSE")), c("pessoal_auxiliar.csv", "pessoal_auxiliar_envio.csv")))
    checar("envio carregado sem mexer na base", nrow(dados_envio()) == 3 && n_base > 0 && nrow(dados_base()) == n_base)

    session$setInputs(envio_status = "Ativo", envio_cpf = "", envio_nome = "", envio_detalhe = "",
                      envio_cargo = "Todos", envio_situacao = "Todos", envio_orgao = "Todos")
    session$elapse(500)
    checar("envio sem coluna Status: filtro Ativo não esconde registros", nrow(dados_envio_filtrados()) == 3)
    session$setInputs(envio_cargo = "Estagiário(a)")
    session$elapse(500)
    checar("filtro de cargo do envio (por extenso)", nrow(dados_envio_filtrados()) == 1)
    session$setInputs(envio_cargo = "Todos", envio_nome = "fáb")
    session$elapse(500)
    checar("filtro de nome do envio ignora acento", nrow(dados_envio_filtrados()) == 1)
    session$setInputs(envio_nome = "", envio_orgao = "10 - VARA DEZ")
    session$elapse(500)
    checar("filtro de órgão do envio usa o nome da serventia", nrow(dados_envio_filtrados()) == 1)
    session$setInputs(envio_limpar_filtros = 1)
    checar("filtros da base não são afetados pelo envio", nrow(dados_base_filtrados()) > 0)

    h <- paste(as.character(tagList(
        linha_cadastro(arquivos_base_pessoal(pasta_pessoal("TJSE")), RADICAL_BASE_PESSOAL, "Base de Dados - Pessoal e Auxiliar", "apagar_base"),
        linha_cadastro(arquivos_envio_pessoal(pasta_pessoal("TJSE")), RADICAL_ENVIO_PESSOAL, "Envio - Pessoal e Auxiliar", "apagar_envio")
    )), collapse = "")
    checar("Gerenciar Arquivos mostra base e envio com data de atualização",
           grepl("pessoal_auxiliar.csv", h, fixed = TRUE) && grepl("pessoal_auxiliar_envio.csv", h, fixed = TRUE) &&
               length(gregexpr("atualizado em \\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}", h)[[1]]) == 2)
    h_vazio <- as.character(linha_cadastro(character(0), RADICAL_ENVIO_PESSOAL, "Envio - Pessoal e Auxiliar", "apagar_envio"))
    checar("sem envio, a linha diz \"ainda não enviado\" e não tem Apagar",
           grepl("ainda não enviado", h_vazio, fixed = TRUE) && !grepl("apagar_envio", h_vazio, fixed = TRUE))
    checar("data dos alertas é a do arquivo mais recente",
           data_atualizacao(arquivos_alertas_existentes()) ==
               format(max(file.info(arquivos_alertas_existentes())$mtime), "%d/%m/%Y %H:%M"))

    # Novo envio com um já existente: pede confirmação e substitui.
    session$setInputs(gerenciar_arquivos = 2)
    session$setInputs(upload_arquivos_2 = data.frame(
        name = "Envio MPM.csv", size = file.size(envio_csv), type = "text/csv", datapath = envio_csv
    ))
    session$setInputs(enviar_arquivos = 2)
    checar("envio novo aguarda confirmação quando já existe um", !is.null(upload_pendente()))
    session$setInputs(manter_existentes = 2)
    checar("envio substituído continua único",
           sum(list.files(pasta_pessoal("TJSE")) == "pessoal_auxiliar_envio.csv") == 1)

    session$setInputs(apagar_envio = 1)
    session$setInputs(confirmar_apagar_pasta = 1)
    checar("Apagar envio remove só o envio",
           identical(list.files(pasta_pessoal("TJSE")), "pessoal_auxiliar.csv") && nrow(dados_envio()) == 0)
})

# Arquivo de envio real (opcional, fora do repositório):
#   FAROLJUS_XLSX_ENVIO=/caminho/quadro_pessoal_e_auxiliar_envio.xlsx Rscript tests/test_seguranca.R
xlsx_envio <- Sys.getenv("FAROLJUS_XLSX_ENVIO")
if (nzchar(xlsx_envio) && file.exists(xlsx_envio)) {
    df_env <- ler_arquivo_dados(xlsx_envio, "xlsx")
    checar("planilha de envio (.xlsx) é reconhecida como envio",
           identificar_tipo_arquivo_pessoal(df_env, basename(xlsx_envio)) == "envio")
    checar("planilha de envio carrega todas as linhas", nrow(limpar_dados_lidos(df_env)) == nrow(df_env))
}

# =====================================================
cat("\n[L14] Envio: Gerar Novo Envio (Duplicidade no próprio Tribunal) e tabelas\n")
# =====================================================

CAB_NASC <- "Data Nascimento"
env_dup <- data.frame(
    CPF = c("1001715594", "22222222222", "77777777777", "99999999999", "12345678901"),
    Nome = c("JULIA", "BRUNO", "SEM BASE", "APOSENTADO", "CHEFE"),
    `Órgão de lotação do(a) Servidor(a) ou Auxiliar` = c("8582", "20", "30", "30", "40"),
    `Situação profissional atual` = c("2", "3", "3", "3", "1"),
    Cargo = c("1", "1", "5", "1", "1"),
    `Data de início da situação` = c("01/09/2026", "10/09/2026", "01/09/2026", "01/09/2026", "05/09/2026"),
    `Data Nascimento` = c("16/03/1989", "01/01/1980", "01/01/2000", "01/01/1960", "01/01/1975"),
    Email = c("julia@tj.jus.br", "", "x@tj.jus.br", "", ""),
    `Data de saída da situação` = c("", "", "", "", ""),
    `Exclusão de registro por erro` = c("N", "N", "N", "N", "N"),
    check.names = FALSE
)
base_dup <- data.frame(
    CPF = c("01001715594", "01001715594", "01001715594", "22222222222", "99999999999", "99999999999", "12345678901"),
    Nome = c("JULIA", "JULIA", "JULIA", "BRUNO", "APOSENTADO", "APOSENTADO", "CHEFE"),
    `Órgão de lotação do(a) Servidor(a) ou Auxiliar` = c("32", "40", "50", "20", "30", "30", "40"),
    `Situação profissional atual` = c("3", "1", "2", "3", "3", "7", "3"),
    Cargo = c("1", "1", "1", "1", "1", "1", "1"),
    `Data de início da situação` = c("01/01/2015", "01/03/2020", "01/09/2026", "15/09/2026", "01/01/2010", "01/01/2024", "01/02/2022"),
    `Data de saída da situação` = c("28/02/2020", "", "", "", "31/12/2023", "", ""),
    Status = c("Inativo", "Ativo", "Ativo", "Ativo", "Inativo", "Inativo", "Ativo"),
    `Data Nascimento` = c("16/03/1989", "45000", "16/03/1989", "01/01/1980", "01/01/1960", "01/01/1960", "01/01/1975"),
    check.names = FALSE
)
alertas_dup <- data.frame(
    CPF = c("1001715594", "22222222222", "77777777777", "88888888888", "33333333333", "99999999999", "12345678901"),
    `Conflito: Duplicidade no próprio Tribunal` = c("Registro duplicado", "Registro duplicado", "x", "x", NA, "x", "x"),
    `Alerta: Email` = c(NA, "E-mail não informado", NA, NA, "E-mail não informado", NA, NA),
    check.names = FALSE
)
r_dup <- gerar_novo_envio_duplicidade(env_dup, base_dup, alertas_dup)
d_dup <- r_dup$dados
checar("novo envio tem as colunas do envio, na mesma ordem", identical(names(d_dup), names(env_dup)))
checar("CPFs com o conflito são encontrados (com e sem zero à esquerda)",
       setequal(r_dup$cpfs_conflito, c("01001715594", "22222222222", "77777777777", "88888888888", "99999999999", "12345678901")))
checar("duas linhas por CPF com envio e base anterior em situação 1, 2 ou 3 (JULIA e CHEFE)",
       nrow(d_dup) == 4 && identical(d_dup$Nome, c("JULIA", "JULIA", "CHEFE", "CHEFE")))
checar("base em situação fora de 1, 2 ou 3 não gera linhas (APOSENTADO, situação 7)",
       identical(r_dup$fora_situacao, "99999999999") && !("APOSENTADO" %in% d_dup$Nome))
chefe <- d_dup[d_dup$Nome == "CHEFE", ][1, ]
base_chefe <- base_dup[base_dup$Nome == "CHEFE", ]
checar("linha da base muda só a Data de saída (situação 3, demais campos iguais à base)",
       chefe$`Data de saída da situação` == "05/09/2026" &&
           all(vapply(setdiff(intersect(names(env_dup), names(base_dup)), "Data de saída da situação"),
                      function(c) identical(as.character(chefe[[c]]), as.character(base_chefe[[c]])), logical(1))))
checar("1ª linha: registro da base mais recente com início ANTES do envio",
       d_dup$`Órgão de lotação do(a) Servidor(a) ou Auxiliar`[1] == "40" &&
           d_dup$`Data de início da situação`[1] == "01/03/2020" && d_dup$CPF[1] == "01001715594")
checar("1ª linha: Data de saída = Data de início do envio",
       d_dup$`Data de saída da situação`[1] == "01/09/2026")
checar("2ª linha: registro do envio exatamente igual",
       identical(unname(unlist(d_dup[2, ])), unname(unlist(env_dup[1, ]))))
checar("códigos de Cargo e Situação saem como no arquivo (não por extenso)",
       all(d_dup$Cargo == "1") && identical(d_dup$`Situação profissional atual`, c("1", "2", "3", "1")))
checar("coluna do envio que a base não tem sai vazia (Email da base)", d_dup$Email[1] == "")
checar("data serial do Excel sai em dd/mm/aaaa", d_dup$`Data Nascimento`[1] == format(as.Date(45000, origin = "1899-12-30"), "%d/%m/%Y"))
checar("CPF fora do envio e CPF sem base anterior são informados",
       setequal(r_dup$sem_envio, "88888888888") && setequal(r_dup$sem_base, c("22222222222", "77777777777")))
checar("sem conflito de duplicidade, nada é gerado",
       nrow(gerar_novo_envio_duplicidade(env_dup, base_dup, alertas_dup[, c(1, 3)])$dados) == 0)

arq_novo <- tempfile(fileext = ".csv")
readr::write_excel_csv2(d_dup, arq_novo, na = "")
releitura <- ler_arquivo_dados(arq_novo, "csv")
checar("CSV do novo envio relido igual (separador ;, cabeçalho do envio)",
       identical(names(releitura), names(env_dup)) && identical(as.character(releitura$CPF), c("01001715594", "1001715594", "12345678901", "12345678901")))

# Pelo módulo: botão e download.
unlink(caminho_alertas("TJSE"), recursive = TRUE)
dir.create(caminho_alertas("TJSE"), recursive = TRUE, showWarnings = FALSE)
readr::write_excel_csv2(alertas_dup, file.path(caminho_alertas("TJSE"), "alertas_dup.csv"), na = "")
readr::write_excel_csv2(base_dup, file.path(pasta_pessoal("TJSE"), "pessoal_auxiliar.csv"), na = "")
readr::write_excel_csv2(env_dup, file.path(pasta_pessoal("TJSE"), "pessoal_auxiliar_envio.csv"), na = "")
testServer(mod_pessoal_auxiliar_server, args = list(empresa = reactive("TJSE"), ativo = reactive(TRUE)), {
    session$flushReact()
    checar("módulo monta o novo envio a partir dos arquivos da empresa", nrow(novo_envio()$dados) == 4)
    h <- paste(as.character(output$envio_csv_ui$html), collapse = "")
    checar("aba Gerar arquivo CSV do Envio tem o botão Gerar Novo Envio",
           grepl("novo_envio_ui", h, fixed = TRUE) &&
               grepl("Gerar Novo Envio", paste(as.character(output$novo_envio_ui$html), collapse = ""), fixed = TRUE))
    h2 <- paste(as.character(output$novo_envio_ui$html), collapse = "")
    checar("botão Gerar Novo Envio fica ativo quando há linhas", grepl('id="proxy1-novo_envio_download"', h2, fixed = TRUE))
    arq <- output$novo_envio_download
    d <- ler_arquivo_dados(arq, "csv")
    checar("download do novo envio traz as linhas dos dois CPFs", nrow(d) == 4 && d$`Data de saída da situação`[1] == "01/09/2026")
    h_base <- paste(as.character(output$base_csv_ui$html), collapse = "")
    checar("Base de Dados não tem o botão Gerar Novo Envio", !grepl("Gerar Novo Envio", h_base, fixed = TRUE))
})

ui_pessoal <- paste(as.character(mod_pessoal_auxiliar_ui("pessoal_auxiliar")), collapse = "")
checar("abas do Pessoal e Auxiliar sem \"- Pessoal e Auxiliar\" no nome",
       all(vapply(c("Base de Dados", "Envio", "Alertas", "Inconsistências"), function(x) grepl(x, ui_pessoal, fixed = TRUE), logical(1))) &&
           !grepl("Base de Dados - Pessoal e Auxiliar</a>", ui_pessoal, fixed = TRUE) &&
           !grepl("> Alertas - Pessoal e Auxiliar", ui_pessoal, fixed = TRUE))
checar("tabelas não forçam mais 100% de largura (desalinhava o cabeçalho)",
       !any(grepl("table\\.dataTable \\{\\s*width: 100%", unlist(lapply(list.files(file.path(dir_teste, "modules"), full.names = TRUE),
                                                                     function(f) paste(readLines(f, warn = FALSE), collapse = " "))))))
checar("app realinha as colunas quando a largura muda",
       grepl("columns.adjust()", codigo_app, fixed = TRUE) && grepl("transitionend", codigo_app, fixed = TRUE))

# =====================================================
cat("\n[L15] Pessoal e Auxiliar: aba sem arquivo (janela de aviso e filtros bloqueados)\n")
# =====================================================

erro_saida <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))

unlink(file.path(dir_teste, "data", "MPRO"), recursive = TRUE)
testServer(mod_pessoal_auxiliar_server, args = list(empresa = reactive("MPRO"), ativo = reactive(TRUE)), {
    session$setInputs(secao = "base")
    checar("Base de Dados sem arquivo abre a janela de aviso", identical(alerta_sem_arquivo_aberto(), "base"))
    session$setInputs(secao = "envio")
    checar("Envio sem arquivo abre a janela de aviso", identical(alerta_sem_arquivo_aberto(), "envio"))
    session$setInputs(secao = "alertas")
    checar("Alertas sem arquivo abre a janela de aviso", identical(alerta_sem_arquivo_aberto(), "alertas"))
    session$setInputs(secao = "inconsistencias")
    checar("Inconsistências não abre a janela de aviso", identical(alerta_sem_arquivo_aberto(), "alertas"))
    checar("filtros de Base, Envio e Alertas bloqueados sem arquivo", all(filtros_bloqueados()))
    checar("mensagem do Envio vai para a janela, não para a tabela",
           grepl("pessoal_auxiliar_envio", ABAS_COM_ARQUIVO$envio$mensagem(), fixed = TRUE) &&
               identical(erro_saida(output$envio_tabela), ""))
    checar("tabela da Base fica vazia, sem mensagem", identical(erro_saida(output$base_tabela), ""))
    n_up <- contador_upload()
    session$setInputs(sem_arquivo_gerenciar = 1)
    checar("botão Gerenciar Arquivos da janela abre o Gerenciar Arquivos", contador_upload() == n_up + 1)
})

dir.create(pasta_pessoal("MPRO"), recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(base_csv, file.path(pasta_pessoal("MPRO"), "pessoal_auxiliar.csv")))
testServer(mod_pessoal_auxiliar_server, args = list(empresa = reactive("MPRO"), ativo = reactive(TRUE)), {
    session$setInputs(secao = "base")
    checar("Base de Dados com arquivo não abre a janela", is.null(alerta_sem_arquivo_aberto()))
    b <- filtros_bloqueados()
    checar("com a base enviada, só os filtros da Base ficam liberados",
           !b[["base"]] && b[["envio"]] && b[["alertas"]])
})

ativo_teste <- reactiveVal(FALSE)
unlink(pasta_pessoal("MPRO"), recursive = TRUE)
testServer(mod_pessoal_auxiliar_server, args = list(empresa = reactive("MPRO"), ativo = ativo_teste), {
    session$setInputs(secao = "envio")
    checar("módulo fechado não abre a janela", is.null(alerta_sem_arquivo_aberto()))
    ativo_teste(TRUE)
    session$flushReact()
    checar("abrir o módulo numa aba sem arquivo abre a janela", identical(alerta_sem_arquivo_aberto(), "envio"))
})

ui_p <- paste(as.character(mod_pessoal_auxiliar_ui("pessoal_auxiliar")), collapse = "")
checar("blocos de filtros de Base, Envio e Alertas têm id (alvo do bloqueio)",
       all(vapply(c("pessoal_auxiliar-base_filtros", "pessoal_auxiliar-envio_filtros", "pessoal_auxiliar-alertas_filtros",
                    "pessoal_auxiliar-base_grafico_controles", "pessoal_auxiliar-envio_grafico_controles"),
                  function(x) grepl(sprintf('id="%s"', x), ui_p, fixed = TRUE), logical(1))))
checar("app tem o handler que bloqueia os filtros",
       grepl("'bloquear-filtros'", codigo_app, fixed = TRUE) && grepl("selectize.disable()", codigo_app, fixed = TRUE))

# =====================================================
cat("\n[12] Magistrados e Serventias: abas Base, Envio, Alertas e Inconsistências; janela sem arquivo\n")
# =====================================================

pos_abas <- function(h, valores) vapply(valores, function(v) regexpr(sprintf('data-value="%s"', v), h, fixed = TRUE)[1], numeric(1))

for (mod in list(
    list(nome = "Magistrados", ui = mod_magistrados_ui, server = mod_magistrados_server,
         pasta = pasta_magistrados, id = "mag"),
    list(nome = "Serventias", ui = mod_serventias_ui, server = mod_serventias_server,
         pasta = pasta_serventias, id = "srv")
)) {

    h <- paste(as.character(mod$ui(mod$id)), collapse = "")
    pa <- pos_abas(h, c("base", "envio", "alertas", "inconsistencias"))
    checar(sprintf("%s: abas Base de Dados, Envio, Alertas e Inconsistências, nessa ordem", mod$nome),
           all(pa > 0) && !is.unsorted(pa))
    checar(sprintf("%s: blocos de filtros com id (alvo do bloqueio)", mod$nome),
           all(vapply(paste0(mod$id, "-", c("base_filtros", "envio_filtros", "alertas_filtros",
                                             "base_grafico_controles", "envio_grafico_controles")),
                      function(x) grepl(sprintf('id="%s"', x), h, fixed = TRUE), logical(1))))

    unlink(file.path(dir_teste, "data", "MPRO"), recursive = TRUE)
    testServer(mod$server, args = list(empresa = reactive("MPRO"), ativo = reactive(TRUE)), {
        session$setInputs(secao = "base")
        checar(sprintf("%s: Base sem arquivo abre a janela de aviso", mod$nome), identical(alerta_sem_arquivo_aberto(), "base"))
        session$setInputs(secao = "envio")
        checar(sprintf("%s: Envio sem arquivo abre a janela de aviso", mod$nome), identical(alerta_sem_arquivo_aberto(), "envio"))
        session$setInputs(secao = "alertas")
        checar(sprintf("%s: Alertas sem arquivo abre a janela de aviso", mod$nome), identical(alerta_sem_arquivo_aberto(), "alertas"))
        session$setInputs(secao = "inconsistencias")
        checar(sprintf("%s: Inconsistências não abre a janela", mod$nome), identical(alerta_sem_arquivo_aberto(), "alertas"))
        checar(sprintf("%s: filtros de Base, Envio e Alertas bloqueados", mod$nome), all(filtros_bloqueados()))
        checar(sprintf("%s: tabelas ficam vazias, sem mensagem (aviso vai para a janela)", mod$nome),
               identical(erro_saida(output$base_tabela), "") && identical(erro_saida(output$envio_tabela), "") &&
                   identical(erro_saida(output$tabela), ""))
        n_up <- contador_upload()
        session$setInputs(sem_arquivo_gerenciar = 1)
        checar(sprintf("%s: botão da janela abre o Gerenciar Arquivos", mod$nome), contador_upload() == n_up + 1)
    })

    ativo_teste <- reactiveVal(FALSE)
    testServer(mod$server, args = list(empresa = reactive("MPRO"), ativo = ativo_teste), {
        session$setInputs(secao = "envio")
        checar(sprintf("%s: módulo fechado não abre a janela", mod$nome), is.null(alerta_sem_arquivo_aberto()))
        ativo_teste(TRUE)
        session$flushReact()
        checar(sprintf("%s: abrir o módulo numa aba sem arquivo abre a janela", mod$nome),
               identical(alerta_sem_arquivo_aberto(), "envio"))
    })
}

# Arquivo de envio: identificação no upload, leitura e filtros.
mag_envio <- data.frame(
    CPF = c("11122233344", "55566677788"), Nome = c("ANA JUIZA", "BRUNO JUIZ"),
    `Órgão de lotação do magistrado(a)` = c("1", "2"), Cargo = c("1", "2"),
    `Situação profissional atual` = c("7", "7"), Status = c("Ativo", "Inativo"),
    check.names = FALSE
)
checar("Magistrados: arquivo com \"envio\" no nome é identificado como envio",
       identical(identificar_tipo_arquivo_magistrados(mag_envio, "Magistrados envio.xlsx"), "envio") &&
           identical(identificar_tipo_arquivo_magistrados(mag_envio, "magistrados.xlsx"), "base"))
srv_envio <- data.frame(`Código Serventia` = c("10", "20"), `Nome Serventia` = c("VARA A", "VARA B"),
                        CEP = c("49000000", "49000001"), Status = c("Ativo", "Ativo"), check.names = FALSE)
checar("Serventias: arquivo com \"envio\" no nome é identificado como envio",
       identical(identificar_tipo_arquivo_serventias(srv_envio, "serventias_envio.csv"), "envio") &&
           identical(identificar_tipo_arquivo_serventias(srv_envio, "serventias.csv"), "base"))

dir.create(pasta_magistrados("MPRO"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv2(mag_envio, file.path(pasta_magistrados("MPRO"), "magistrados_envio.csv"))
dir.create(pasta_serventias("MPRO"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv2(srv_envio, file.path(pasta_serventias("MPRO"), "serventias_envio.csv"))

testServer(mod_magistrados_server, args = list(empresa = reactive("MPRO"), ativo = reactive(TRUE)), {
    session$setInputs(secao = "envio", envio_status = "Todos", envio_cargo = "Todos", envio_situacao = "Todos",
                      envio_orgao = "Todos", envio_cpf = "", envio_nome = "", envio_detalhe = "")
    checar("Magistrados: Envio com arquivo não abre a janela", is.null(alerta_sem_arquivo_aberto()))
    checar("Magistrados: Envio carrega o arquivo de envio", nrow(dados_envio()) == 2 && nrow(dados_base()) == 0)
    checar("Magistrados: só os filtros do Envio ficam liberados",
           filtros_bloqueados()[["base"]] && !filtros_bloqueados()[["envio"]])
    session$setInputs(envio_status = "Ativo")
    checar("Magistrados: filtro do Envio funciona", nrow(dados_envio_filtrados()) == 1)
    checar("Magistrados: Envio traduz o código do cargo", any(grepl("Titular", envio_exibicao()$Cargo)))
})

testServer(mod_serventias_server, args = list(empresa = reactive("MPRO"), ativo = reactive(TRUE)), {
    session$setInputs(secao = "envio", envio_status = "Todos", envio_unidade = "Todos",
                      envio_entrancia = "Todos", envio_codigo = "", envio_detalhe = "")
    checar("Serventias: Envio com arquivo não abre a janela", is.null(alerta_sem_arquivo_aberto()))
    checar("Serventias: Envio carrega o arquivo de envio", nrow(dados_envio()) == 2 && nrow(dados_base()) == 0)
    checar("Serventias: tabela do Envio é exibida", identical(erro_saida(output$envio_tabela), NA_character_))
})

# =====================================================
cat("\n[13] Magistrados e Serventias: Gerar Novo Envio e tabelas do envio\n")
# =====================================================

# Magistrados: alerta "cargos distintos ativos" (como vem do MPM).
mag_base_ne <- data.frame(
    CPF = c("00979548535", "00979548535", "00979548535", "11111111111", "22222222222", "33333333333"),
    Nome = c("JAIR", "JAIR", "JAIR", "ANA", "BRUNO", "CARLA"),
    `Órgão de lotação do magistrado(a)` = c("32", "32", "8673", "10", "20", "30"),
    Cargo = c("2", "2", "1", "2", "2", "2"),
    `Situação profissional atual` = c("8", "12", "8", "8", "12", "8"),
    `Data de início da situação` = c("17/10/2018", "18/01/2025", "14/09/2026", "01/01/2020", "01/01/2020", "01/01/2020"),
    `Data de saída da situação` = c("", "", "", "", "", "01/06/2026"),
    Status = c("Ativo", "Inativo", "Ativo", "Ativo", "Inativo", "Inativo"),
    `Exclusão de registro por erro` = "N",
    check.names = FALSE
)
mag_envio_ne <- data.frame(
    CPF = c("979548535", "11111111111", "22222222222", "33333333333", "44444444444"),
    Nome = c("JAIR", "ANA", "BRUNO", "CARLA", "DIEGO"),
    `Órgão de lotação do magistrado(a)` = c("8404", "10", "20", "30", "40"),
    Cargo = c("1", "2", "1", "1", "1"),
    `Situação profissional atual` = c("8", "16", "8", "8", "8"),
    `Data de início da situação` = c("46279", "01/09/2026", "01/09/2026", "01/09/2026", "01/09/2026"),
    `Data de saída da situação` = "",
    `Exclusão de registro por erro` = "N",
    check.names = FALSE
)
mag_alertas_ne <- data.frame(
    CPF = c("00979548535", "11111111111", "22222222222", "33333333333", "55555555555", "66666666666"),
    Nome = "x",
    `Alerta: Inconsistência` = c(rep("Há cargos distintos ativos para o mesmo CPF informado. Favor verificar...", 5), NA),
    `Alerta: Email` = c(NA, NA, NA, NA, NA, "E-mail inválido"),
    check.names = FALSE
)
rm_ <- gerar_novo_envio_magistrados(mag_envio_ne, mag_base_ne, mag_alertas_ne)
dm <- rm_$dados
checar("Magistrados: CPFs com o alerta \"cargos distintos ativos\" (com e sem zero à esquerda)",
       setequal(rm_$cpfs_conflito, c("00979548535", "11111111111", "22222222222", "33333333333", "55555555555")))
checar("Magistrados: alerta de outro tipo não entra", !("66666666666" %in% rm_$cpfs_conflito))
checar("Magistrados: duas linhas só para quem tem registro em aberto de outro cargo (JAIR)",
       nrow(dm) == 2 && identical(dm$Nome, c("JAIR", "JAIR")))
checar("Magistrados: 1ª linha é o registro em aberto de outro cargo (cargo 2, 2018), não o encerrado (situação 12)",
       dm$Cargo[1] == "2" && dm$`Data de início da situação`[1] == "17/10/2018" && dm$`Situação profissional atual`[1] == "8")
checar("Magistrados: 1ª linha recebe Data de saída = início do envio (serial do Excel em dd/mm/aaaa)",
       dm$`Data de saída da situação`[1] == format(as.Date(46279, origin = "1899-12-30"), "%d/%m/%Y"))
checar("Magistrados: 2ª linha é o envio sem alteração (datas em dd/mm/aaaa)",
       dm$CPF[2] == "979548535" && dm$Cargo[2] == "1" && dm$`Órgão de lotação do magistrado(a)`[2] == "8404")
checar("Magistrados: novo envio tem as colunas do envio (sem Status da base)", identical(names(dm), names(mag_envio_ne)))
checar("Magistrados: mesmo cargo, registro encerrado ou já com saída não geram linhas (ANA, BRUNO, CARLA)",
       setequal(rm_$sem_base, c("11111111111", "22222222222", "33333333333")))
checar("Magistrados: CPF com alerta fora do envio é informado", identical(rm_$sem_envio, "55555555555"))
checar("Magistrados: alerta de conflito de duplicidade também conta",
       identical(cpfs_com_duplicidade_magistrados(data.frame(CPF = "1", `Conflito: Duplicidade no próprio Tribunal` = "x",
                                                            check.names = FALSE)), "00000000001"))

checar("Magistrados: arquivo sem Status (gerado pelo órgão) é reconhecido como envio",
       identical(identificar_tipo_arquivo_magistrados(mag_envio_ne, "Magistrados_i.xlsx"), "envio") &&
           identical(identificar_tipo_arquivo_magistrados(mag_base_ne, "Magistrados_i.xlsx"), "base"))

# Serventias: linhas do envio das serventias com alerta.
srv_envio_ne <- data.frame(`Código Serventia` = c("18160", "18169", "18130"), CEP = c("49580970", "49650000", "49690000"),
                           `Data Adesão` = c("44533", "03/12/2021", ""), `Entrância` = c("2", "1", "2"), check.names = FALSE)
srv_alertas_ne <- data.frame(`Código Serventia` = c("18160", "018130", "99999", "18169"),
                             `Alerta: Geolocalização` = c("Fora do município", "x", "y", NA), check.names = FALSE)
rs_ <- gerar_novo_envio_serventias(srv_envio_ne, srv_alertas_ne)
checar("Serventias: novo envio só com as serventias que têm alerta (código com/sem zero à esquerda)",
       identical(rs_$dados$`Código Serventia`, c("18160", "18130")))
checar("Serventias: colunas do envio, data serial em dd/mm/aaaa",
       identical(names(rs_$dados), names(srv_envio_ne)) && rs_$dados$`Data Adesão`[1] == "03/12/2021")
checar("Serventias: código com alerta fora do envio é informado", identical(rs_$sem_envio, "99999"))
checar("Serventias: arquivo sem Status (gerado pelo órgão) é reconhecido como envio",
       identical(identificar_tipo_arquivo_serventias(srv_envio_ne, "serventias_i.xlsx"), "envio"))

# Pelo módulo: tabelas do envio e botão.
unlink(file.path(dir_teste, "data", "TJSE", "magistrados"), recursive = TRUE)
unlink(file.path(dir_teste, "data", "TJSE", "serventias"), recursive = TRUE)
dir.create(pasta_magistrados("TJSE"), recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_serventias("TJSE"), recursive = TRUE, showWarnings = FALSE)
readr::write_excel_csv2(mag_base_ne, file.path(pasta_magistrados("TJSE"), "magistrados.csv"), na = "")
readr::write_excel_csv2(mag_envio_ne, file.path(pasta_magistrados("TJSE"), "magistrados_envio.csv"), na = "")
readr::write_excel_csv2(mag_alertas_ne, file.path(pasta_magistrados("TJSE"), "alertas_magistrados.csv"), na = "")
readr::write_excel_csv2(
    data.frame(`Código Serventia` = c("8404", "32"), `Nome Serventia` = c("1ª VARA CÍVEL", "TRIBUNAL"), CEP = "49000000",
               Status = "Ativo", check.names = FALSE),
    file.path(pasta_serventias("TJSE"), "serventias.csv"), na = "")
readr::write_excel_csv2(srv_envio_ne, file.path(pasta_serventias("TJSE"), "serventias_envio.csv"), na = "")
readr::write_excel_csv2(srv_alertas_ne, file.path(pasta_serventias("TJSE"), "alertas_serventias.csv"), na = "")
limpar_cache_arquivos()

testServer(mod_magistrados_server, args = list(empresa = reactive("TJSE"), ativo = reactive(TRUE)), {
    session$setInputs(secao = "envio", envio_status = "Todos", envio_cargo = "Todos", envio_situacao = "Todos",
                      envio_orgao = "Todos", envio_cpf = "", envio_nome = "", envio_detalhe = "")
    checar("Magistrados (módulo): novo envio montado dos arquivos da empresa", nrow(novo_envio()$dados) == 2)
    h <- paste(as.character(output$novo_envio_ui$html), collapse = "")
    checar("Magistrados (módulo): botão Gerar Novo Envio ativo", grepl("novo_envio_download", h, fixed = TRUE))
    d <- ler_arquivo_dados(output$novo_envio_download, "csv")
    checar("Magistrados (módulo): download traz as duas linhas", nrow(d) == 2 && d$Cargo[1] == "2")
    checar("Magistrados (módulo): tabela do Envio mostra o órgão com o nome da serventia e o cargo por extenso",
           any(envio_exibicao()$`Órgão de lotação do magistrado(a)` == "8404 - 1ª VARA CÍVEL") &&
               any(grepl("Titular", envio_exibicao()$Cargo)))
    h_csv <- paste(as.character(output$envio_csv_ui$html), collapse = "")
    checar("Magistrados (módulo): aba CSV do Envio tem o Gerar Novo Envio, a da Base não",
           grepl("novo_envio_ui", h_csv, fixed = TRUE) &&
               !grepl("novo_envio_ui", paste(as.character(output$base_csv_ui$html), collapse = ""), fixed = TRUE))
})

testServer(mod_serventias_server, args = list(empresa = reactive("TJSE"), ativo = reactive(TRUE)), {
    session$setInputs(secao = "envio", envio_status = "Todos", envio_unidade = "Todos",
                      envio_entrancia = "Todos", envio_codigo = "", envio_detalhe = "")
    checar("Serventias (módulo): novo envio com as serventias com alerta", nrow(novo_envio()$dados) == 2)
    d <- ler_arquivo_dados(output$novo_envio_download, "csv")
    checar("Serventias (módulo): download traz as linhas do envio", nrow(d) == 2)
    checar("Serventias (módulo): aba CSV do Envio tem o Gerar Novo Envio",
           grepl("novo_envio_ui", paste(as.character(output$envio_csv_ui$html), collapse = ""), fixed = TRUE))
})

# =====================================================
cat("\n[14] Tabelas de referência (municípios, competências, classificação de unidades)\n")
# =====================================================

checar("módulos com os nomes novos (mod_pessoal_auxiliar, mod_magistrados, mod_serventias)",
       all(file.exists(file.path(dir_teste, "modules", c("mod_pessoal_auxiliar.R", "mod_magistrados.R", "mod_serventias.R")))) &&
           !any(file.exists(file.path(dir_teste, "modules", c("mod_alertas.R", "mod_alertas_magistrados.R", "mod_alertas_serventias.R")))) &&
           !grepl("mod_alertas", codigo_app, fixed = TRUE))
n_tab <- function(t) dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", t))$n
checar("tabelas de referência criadas e preenchidas a partir dos CSVs",
       n_tab("tab_municipio") == 75 && n_tab("tab_competencia") == 39 &&
           n_tab("tab_classificacao_unidade") == 46 && n_tab("tab_tipo_unidade") == 4)
checar("vetores de referência carregados para a aplicação",
       unname(REF_MUNICIPIOS["366"]) == "AREIA BRANCA" && unname(REF_COMPETENCIAS["1"]) == "Juízo Único" &&
           unname(REF_CLASSIFICACOES_UNIDADE["109"]) == "PA - Postos Avançados")
checar("vários códigos na mesma célula, zero à esquerda e código desconhecido",
       identical(codigos_para_nomes(c("366, 1546", "0291", "9999", NA, ""), REF_MUNICIPIOS),
                 c("AREIA BRANCA; DIVINA PASTORA", "ARACAJU", "9999", NA, "")))
invisible(dbExecute(con, "UPDATE tab_competencia SET descricao = 'Vara Única' WHERE codigo = '1'"))
carregar_tabelas_referencia(con)
checar("ajuste no banco é preservado (o CSV não sobrescreve)", unname(REF_COMPETENCIAS["1"]) == "Vara Única")
invisible(dbExecute(con, "UPDATE tab_competencia SET descricao = 'Juízo Único' WHERE codigo = '1'"))
carregar_tabelas_referencia(con)

srv_ref <- data.frame(
    `Código Serventia` = c("18160", "18169"), CEP = c("49580970", "49650000"),
    `Juizo 100% Digital` = c("S", "N"), `Tipo de Unidade Judiciária` = c("UJ1", "uj2"),
    `Classificação da Unidade Judiciária` = c("109", "201"), `Municípios Abrangidos` = c("366", "1546;291"),
    `Competências` = c("1", "2, 3"), `Entrância` = c("2", "1"), `Balcão Virtual` = c("S", ""),
    check.names = FALSE
)
t_ref <- traduzir_referencias_serventias(srv_ref)
checar("Serventias: município, competência, tipo e classificação por nome; S/N vira Sim/Não",
       identical(t_ref$`Municípios Abrangidos`, c("AREIA BRANCA", "DIVINA PASTORA; ARACAJU")) &&
           identical(t_ref$`Competências`, c("Juízo Único", "Cível; Criminal")) &&
           identical(t_ref$`Tipo de Unidade Judiciária`, c("Unidades judiciárias de primeiro grau",
                                                           "Unidades judiciárias de segundo grau ou de tribunais superiores")) &&
           grepl("^Gab2", t_ref$`Classificação da Unidade Judiciária`[2]) &&
           identical(t_ref$`Juizo 100% Digital`, c("Sim", "Não")) && identical(t_ref$`Balcão Virtual`, c("Sim", "")))
checar("Serventias: CEP, código e entrância continuam como no arquivo",
       identical(t_ref$CEP, srv_ref$CEP) && identical(t_ref$`Código Serventia`, srv_ref$`Código Serventia`) &&
           identical(t_ref$`Entrância`, srv_ref$`Entrância`))

unlink(file.path(dir_teste, "data", "MPRO"), recursive = TRUE)
dir.create(pasta_serventias("MPRO"), recursive = TRUE, showWarnings = FALSE)
readr::write_excel_csv2(srv_ref, file.path(pasta_serventias("MPRO"), "serventias_envio.csv"), na = "")
readr::write_excel_csv2(data.frame(`Código Serventia` = "18169", `Alerta: Competência` = "x", check.names = FALSE),
                        file.path(pasta_serventias("MPRO"), "alertas_serventias.csv"), na = "")
limpar_cache_arquivos()
testServer(mod_serventias_server, args = list(empresa = reactive("MPRO"), ativo = reactive(TRUE)), {
    session$setInputs(secao = "envio", envio_status = "Todos", envio_unidade = "Todos",
                      envio_entrancia = "Todos", envio_codigo = "", envio_detalhe = "")
    checar("Serventias (módulo): tabela do Envio com nomes",
           any(envio_exibicao()$`Municípios Abrangidos` == "AREIA BRANCA"))
    session$setInputs(envio_detalhe = "divina")
    session$elapse(500)
    checar("Serventias (módulo): Detalhe encontra pelo nome do município", nrow(dados_envio_filtrados()) == 1)
    session$setInputs(envio_detalhe = "", envio_grafico_agrupar = "Município abrangido")
    session$elapse(500)
    r <- resumo_agrupamento_serventias(envio_exibicao(), "Município abrangido")
    checar("Serventias (módulo): gráfico por Município abrangido (um grupo por município)",
           setequal(r$Grupo, c("AREIA BRANCA", "DIVINA PASTORA", "ARACAJU")) && all(r$Quantidade == 1))
    checar("Serventias (módulo): gráfico por Competência e por CEP",
           setequal(resumo_agrupamento_serventias(envio_exibicao(), "Competência")$Grupo, c("Juízo Único", "Cível", "Criminal")) &&
               setequal(resumo_agrupamento_serventias(envio_exibicao(), "CEP")$Grupo, c("49580-970", "49650-000")))
    d <- ler_arquivo_dados(output$envio_download_csv, "csv")
    checar("Serventias (módulo): CSV da tabela sai com os nomes", "AREIA BRANCA" %in% d$`Municípios Abrangidos`)
    n <- novo_envio()$dados
    checar("Serventias (módulo): Gerar Novo Envio continua com os códigos",
           nrow(n) == 1 && n$`Municípios Abrangidos` == "1546;291" && n$`Competências` == "2, 3")
})

# =====================================================
cat("\n[15] Tabelas Auxiliares do MPM: banco, telas e Gerenciar Arquivos\n")
# =====================================================

esperado <- c(tab_cargo_magistrado = 11, tab_situacao_profissional_magistrado = 19, tab_forma_promocao_magistrado = 9,
              tab_cargo_servidor = 14, tab_situacao_profissional_servidor = 12, tab_area_atuacao = 7, tab_sexo = 5,
              tab_identidade_genero = 9, tab_raca_cor = 7, tab_deficiencia = 10, tab_regime_cotas = 8,
              tab_competencia = 39, tab_tipo_unidade = 4, tab_classificacao_unidade = 46, tab_entrancia = 5,
              tab_municipio = 75)
contagens <- vapply(names(esperado), n_tab, numeric(1))
checar("as 16 tabelas auxiliares foram gravadas no SQLite com todos os registros", all(contagens == esperado))
checar("numeração oficial do MPM para magistrados (cargo 4 = Desembargador(a), situação 12 = Aposentado(a))",
       unname(CARGO_MAGISTRADO["4"]) == "Desembargador(a)" && unname(CARGO_MAGISTRADO["3"]) == "Juiz(a) Substituto(a) de 2º grau" &&
           unname(SITUACAO_PROFISSIONAL_MAGISTRADO["12"]) == "Aposentado(a)" &&
           unname(SITUACAO_PROFISSIONAL_MAGISTRADO["10"]) == "Licenças ou Concessões previstas em lei")
checar("servidor: situação 4 (Cargo de Assessoramento) e cargo 14 vêm da tabela do MPM",
       unname(SITUACAO_PROFISSIONAL_SERVIDOR["4"]) == "Cargo de Assessoramento" && !is.na(CARGO_SERVIDOR["14"]))
checar("situações encerradas de magistrados seguem a tabela do MPM",
       setequal(SITUACOES_MAG_ENCERRADAS, c("12", "13", "14", "15", "17")))
origem <- dbGetQuery(con, "SELECT tabela, arquivo, local FROM tab_carga_referencia")
checar("origem registrada: competências do CSV específico, cargos da planilha do MPM",
       origem$arquivo[origem$tabela == "tab_competencia"] == "competencias.csv" &&
           origem$arquivo[origem$tabela == "tab_cargo_magistrado"] == "tabelas_auxiliares_mpm.xlsx" &&
           all(origem$local == "padrao"))
checar("tipo de unidade responde pela sigla e classificação sem \";\" no fim",
       unname(REF_TIPOS_UNIDADE["UJ1"]) == "Unidades judiciárias de primeiro grau" &&
           !any(grepl(";$", REF_CLASSIFICACOES_UNIDADE)))

# Ajuste no banco é preservado enquanto o arquivo não muda; forçar recarrega.
invisible(dbExecute(con, "UPDATE tab_sexo SET descricao = 'Fem.' WHERE codigo = 'F'"))
carregar_tabelas_referencia(con)
checar("ajuste no banco preservado na recarga normal", unname(REF_SEXOS["F"]) == "Fem.")
carregar_tabelas_referencia(con, forcar = TRUE)
checar("recarga forçada volta ao arquivo", unname(REF_SEXOS["F"]) == "Feminino")

# Banco de versão anterior (tab_cargo_servidor com "nomenclatura").
invisible(dbExecute(con, "DROP TABLE tab_cargo_servidor"))
invisible(dbExecute(con, "CREATE TABLE tab_cargo_servidor (codigo INTEGER PRIMARY KEY, nomenclatura TEXT NOT NULL)"))
invisible(dbExecute(con, "INSERT INTO tab_cargo_servidor VALUES (1, 'antigo')"))
carregar_tabelas_referencia(con)
checar("tabela no formato antigo é recriada e recarregada do arquivo",
       identical(dbListFields(con, "tab_cargo_servidor"), c("codigo", "descricao")) && n_tab("tab_cargo_servidor") == 14)

checar("identificação dos arquivos de referência",
       identical(identificar_arquivo_referencia(file.path(PASTA_REFERENCIAS, "tabelas_auxiliares_mpm.xlsx")), "mpm") &&
           identical(identificar_arquivo_referencia(file.path(PASTA_REFERENCIAS, "municipios.csv")), "municipios") &&
           identical(identificar_arquivo_referencia(file.path(PASTA_REFERENCIAS, "competencias.csv")), "competencias") &&
           identical(identificar_arquivo_referencia(file.path(PASTA_REFERENCIAS, "tipos_classificacao_unidade.csv")), "classificacao"))
arq_ruim <- tempfile(fileext = ".csv"); writeLines(c("a;b", "1;2"), arq_ruim)
checar("CSV qualquer não é reconhecido", is.na(identificar_arquivo_referencia(arq_ruim)))

# Descrições nas telas (cadastro de pessoas).
pessoas_ref <- data.frame(
    Sexo = c("M", "f", "NDR"), `Identidade de gênero` = c("1", "9", ""), `Raça / Cor` = c("2", "4", "99"),
    `Deficiência` = c("8", "1, 3", NA), `Foi aprovado(a) em Regime de Cotas` = c("1", "2", "6"),
    `Forma de promoção Desembargador(a)/Juiz(a) Titular` = c("6", "", "3"), `Área de atuação` = c("4", "2", "6"),
    `Alerta: Sexo` = c("M", "", ""), check.names = FALSE
)
tp <- traduzir_descricoes_mpm(pessoas_ref)
checar("sexo, identidade, raça/cor, deficiência, cotas, promoção e área de atuação por descrição",
       identical(tp$Sexo, c("Masculino", "Feminino", "Não Declarado pelo Respondente")) &&
           identical(tp$`Identidade de gênero`, c("Cisgênero", "Não Declarado pelo Respondente", "")) &&
           identical(tp$`Raça / Cor`, c("Branco(a)", "Negro(a)-Pardo(a)", "99")) &&
           identical(tp$`Deficiência`, c("Não possui", "Física/motora; Visual", NA)) &&
           identical(tp$`Foi aprovado(a) em Regime de Cotas`, c("Não", "Cota Étnico-Racial", "Não informado")) &&
           identical(tp$`Forma de promoção Desembargador(a)/Juiz(a) Titular`[1], "Juiz (a) Titular por Antiguidade") &&
           identical(tp$`Área de atuação`, c("Área Judiciária do 1º Grau", "TI", "Não se aplica")))
checar("coluna de alerta (\"Alerta: Sexo\") não é tocada", identical(tp$`Alerta: Sexo`, pessoas_ref$`Alerta: Sexo`))
checar("Magistrados e Pessoal usam as descrições nas tabelas",
       traduzir_codigos_magistrados(pessoas_ref)$Sexo[1] == "Masculino" &&
           traduzir_codigos_tabela(pessoas_ref)$`Área de atuação`[2] == "TI")

# Regras de magistrados com a numeração oficial.
mag2 <- function(cpf, cargo, sit) {
    df <- data.frame(cpf, "M", "100", sit, cargo, "01/01/2020", stringsAsFactors = FALSE)
    names(df) <- c("CPF", "Nome", "Órgão de lotação do magistrado(a)", CAB_SIT, "Cargo", CAB_INICIO)
    df
}
r_cargo <- verificar_inconsistencias_magistrados(rbind(mag2("1", "3", "1"), mag2("2", "4", "1")))
checar("Cargo 3 (substituto de 2º grau) como Presidente é apontado; Desembargador(a) (cargo 4) não",
       identical(unique(r_cargo$CPF[r_cargo$`Verificação` == VERIFICACAO_MAG_CARGO]), "1"))

# Arquivo ENVIADO tem prioridade sobre o padrão.
dir.create(PASTA_REFERENCIAS_ENVIADAS, recursive = TRUE, showWarnings = FALSE)
writeLines(c("id;descricao;", "1;Juízo Único (enviado);", "2;Cível;"), file.path(PASTA_REFERENCIAS_ENVIADAS, "competencias.csv"))
carregar_tabelas_referencia(con)
checar("CSV enviado passa a valer", unname(REF_COMPETENCIAS["1"]) == "Juízo Único (enviado)" &&
           dbGetQuery(con, "SELECT local FROM tab_carga_referencia WHERE tabela = 'tab_competencia'")$local == "enviado")
invisible(file.remove(file.path(PASTA_REFERENCIAS_ENVIADAS, "competencias.csv")))
carregar_tabelas_referencia(con)
checar("sem o enviado, volta o padrão", unname(REF_COMPETENCIAS["1"]) == "Juízo Único" && n_tab("tab_competencia") == 39)

# Módulo: consulta para todos, Gerenciar Arquivos só para admin.
testServer(mod_tabelas_auxiliares_server,
           args = list(con = con, ativo = reactive(TRUE), pode_gerenciar = reactive(FALSE)), {
    session$setInputs(tabela = "tab_cargo_magistrado")
    checar("tela lista as 16 tabelas", nrow(resumo()) == 16)
    checar("tela mostra o conteúdo da tabela escolhida",
           nrow(exibicao()) == 11 && all(c("Código", "Descrição") %in% names(exibicao())))
    checar("usuário comum não vê o botão Gerenciar Arquivos", is.null(output$botao_gerenciar$html) ||
               !grepl("gerenciar_arquivos", as.character(output$botao_gerenciar$html), fixed = TRUE))
    session$setInputs(gerenciar_arquivos = 1)
    checar("usuário comum não abre o Gerenciar Arquivos (servidor confere)", contador_upload() == 0)
})

testServer(mod_tabelas_auxiliares_server,
           args = list(con = con, ativo = reactive(TRUE), pode_gerenciar = reactive(TRUE)), {
    session$setInputs(tabela = "tab_municipio")
    checar("municípios com UF e código IBGE", all(c("UF", "Código IBGE") %in% names(exibicao())))
    checar("admin vê o botão Gerenciar Arquivos", grepl("gerenciar_arquivos", as.character(output$botao_gerenciar$html), fixed = TRUE))
    session$setInputs(gerenciar_arquivos = 1)
    checar("admin abre o Gerenciar Arquivos", contador_upload() == 1)
    novo <- tempfile(fileext = ".csv")
    writeLines(c("UF;seq_municipio;nome;cod_ibge;", "SE;366;AREIA BRANCA (ENVIADO);2800506;"), novo)
    session$setInputs(upload_referencias_1 = data.frame(name = "../../municipios novos.csv", size = 1, type = "text/csv",
                                                        datapath = novo, stringsAsFactors = FALSE))
    session$setInputs(enviar_arquivos = 1)
    checar("envio grava com o nome padrão e recarrega a tabela",
           file.exists(file.path(PASTA_REFERENCIAS_ENVIADAS, "municipios.csv")) &&
               unname(REF_MUNICIPIOS["366"]) == "AREIA BRANCA (ENVIADO)" && n_tab("tab_municipio") == 1)
    session$setInputs(apagar_municipios = 1)
    session$setInputs(confirmar_apagar = 1)
    checar("apagar o enviado volta ao padrão",
           !file.exists(file.path(PASTA_REFERENCIAS_ENVIADAS, "municipios.csv")) && n_tab("tab_municipio") == 75)
})

limpar_auditoria()
invisible(dbExecute(con, "UPDATE usuarios_totp SET ultimo_contador = NULL"))
testServer(server, {
    session$setInputs(entrar_totp = 0)
    session$setInputs(usuario_totp = "maria", codigo_totp = totp_gerar(chave("maria")), entrar_totp = 1)
    h <- as.character(output$tela_principal$html)
    sub_cfg <- regmatches(h, regexpr('class="icon-submenu".*?id="sair"', h))
    checar("Configurações tem Tabelas Auxiliares (dentro do grupo, depois de Ver instruções)",
           length(sub_cfg) == 1 && pos_em(sub_cfg, 'id="ir_tabelas_auxiliares"') > pos_em(sub_cfg, 'id="mostrar_readme"'))
    session$setInputs(ir_tabelas_auxiliares = 1)
    checar("clicar em Tabelas Auxiliares abre a tela", identical(menuSelecionado(), ABA_TABELAS_AUXILIARES))
})

# O RStudio (runApp) carrega os arquivos de R/ sozinho, antes do app.R e
# dos módulos: eles não podem depender, ao serem carregados, de objetos
# definidos nos módulos (ex.: PASTA_ALERTAS_BASE).
env_autoload <- new.env(parent = baseenv())
r_autoload <- try(sys.source(here::here("R", "tabelas_referencia.R"), envir = env_autoload), silent = TRUE)
checar("R/tabelas_referencia.R carrega sozinho (autoload do Shiny, antes dos módulos)",
       !inherits(r_autoload, "try-error"))

# =====================================================
cat("\n[16] Magistrados: clique na barra de Cargo abre os registros; paleta dos gráficos\n")
# =====================================================

mag_exib <- traduzir_codigos_magistrados(mag_base_ne)
cargo_2 <- CARGO_MAGISTRADO[["2"]]
det <- detalhe_cargo_magistrados(mag_exib, cargo_2)
checar("Detalhe do cargo: colunas CPF, Nome, Cargo, Situação, Início e Saída, nessa ordem",
       identical(names(det), c("CPF", "Nome", "Cargo", "Situação Profissional",
                               "Data de Início da Situação", "Data de Saída da Situação")))
checar("Detalhe do cargo: só os registros do cargo clicado (5 do cargo 2), ordenados por nome",
       nrow(det) == 5 && all(det$Cargo == cargo_2) && !is.unsorted(det$Nome))
checar("Detalhe do cargo: situação por extenso e datas em dd/mm/aaaa",
       any(det$`Situação Profissional` == SITUACAO_PROFISSIONAL_MAGISTRADO[["12"]]) &&
           all(grepl("^\\d{2}/\\d{2}/\\d{4}$", det$`Data de Início da Situação`)) &&
           "01/06/2026" %in% det$`Data de Saída da Situação`)
checar("Detalhe do cargo: data serial do Excel vira dd/mm/aaaa",
       identical(detalhe_cargo_magistrados(data.frame(Cargo = "X", `Data de início da situação` = "46279", check.names = FALSE), "X")$`Data de Início da Situação`,
                 format(as.Date(46279, origin = "1899-12-30"), "%d/%m/%Y")))
checar("Detalhe do cargo: cargo vazio casa com o rótulo da barra (vazio)",
       nrow(detalhe_cargo_magistrados(data.frame(Cargo = c("", "1"), Nome = c("A", "B")), ROTULO_VALOR_VAZIO)) == 1)

readr::write_csv2(mag_base_ne, file.path(pasta_magistrados("MPRO"), "magistrados.csv"))
limpar_cache_arquivos()
testServer(mod_magistrados_server, args = list(empresa = reactive("MPRO"), ativo = reactive(TRUE)), {
    session$setInputs(secao = "base", base_status = "Todos", base_cargo = "Todos", base_situacao = "Todos",
                      base_orgao = "Todos", base_cpf = "", base_nome = "", base_detalhe = "",
                      base_grafico_agrupar = "Cargo", base_grafico_qtd = "20")
    session$setInputs(base_grafico_selected = cargo_2)
    d <- aba_base$detalhe_cargo()
    checar("Base > Gráfico (Cargo): clicar na barra abre a janela com os registros do cargo",
           !is.null(d) && identical(d$cargo, cargo_2) && nrow(d$tabela) == 5)
    session$setInputs(base_status = "Ativo")
    session$setInputs(base_grafico_selected = character(0))
    session$setInputs(base_grafico_selected = cargo_2)
    checar("Base > Gráfico (Cargo): a janela respeita os filtros da aba (Status Ativo)",
           nrow(aba_base$detalhe_cargo()$tabela) == 2)
    aba_base$detalhe_cargo(NULL)
    session$setInputs(base_grafico_agrupar = "Sexo", base_grafico_selected = "Masculino")
    checar("Base > Gráfico: com outro agrupamento o clique não abre a janela", is.null(aba_base$detalhe_cargo()))
    session$setInputs(secao = "envio", envio_grafico_agrupar = "Cargo", envio_grafico_selected = cargo_2)
    checar("Envio > Gráfico: o clique não abre a janela (só na Base de Dados)", is.null(aba_envio$detalhe_cargo()))
})
invisible(file.remove(file.path(pasta_magistrados("MPRO"), "magistrados.csv")))

checar("Paleta: as 5 cores definidas, na ordem", identical(unname(PALETA_GRAFICOS),
       c("#00B8D9", "#005B96", "#FF8C42", "#4A4A4A", "#4CAF50")))
checar("Paleta: barras e destaque usam a paleta", COR_GRAFICO %in% PALETA_GRAFICOS && COR_GRAFICO_DESTAQUE %in% PALETA_GRAFICOS)
checar("Paleta: mais de 5 categorias recebem tons das mesmas cores, sem repetir até 15",
       length(unique(paleta_graficos(15))) == 15 && identical(paleta_graficos(5), unname(PALETA_GRAFICOS)))
checar("Paleta: Painel usa a paleta (público, sexo, raça)",
       all(c(PAINEL_COR_PUBLICO, PAINEL_CORES_SEXO, PAINEL_CORES_RACA) %in% PALETA_GRAFICOS) &&
           identical(PAINEL_PALETA[1:5], unname(PALETA_GRAFICOS)))
cores_fixas_antigas <- c("#2C7FB8", "#F28E2B", "#59A14F", "#E15759", "#C2456E", "#2C9AA8", "#B07AA1", "#EDC948", "#0d6efd;cursor")
codigo_modulos <- paste(unlist(lapply(list.files(here::here("modules"), full.names = TRUE), readLines, warn = FALSE)), collapse = "\n")
checar("Paleta: nenhuma cor antiga de gráfico sobrou nos módulos",
       !any(vapply(cores_fixas_antigas, function(cor) grepl(cor, codigo_modulos, fixed = TRUE), logical(1))))

# =====================================================

cat(sprintf("\n%d de %d verificações passaram.\n", total - falhas, total))
if (falhas > 0) quit(status = 1)
