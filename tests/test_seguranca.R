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
for (item in c("app.R", "README.md", "R", "modules", "img")) {
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

# Tabelas auxiliares: lidas uma vez e reaproveitadas (TTL).
consultas <- 0L
trace_id <- trace("dbGetQuery", quote(consultas <<- consultas + 1L), where = asNamespace("DBI"), print = FALSE)
rm(list = ls(.tabelas_auxiliares, pattern = "^tabela:"), envir = .tabelas_auxiliares)
for (i in 1:5) invisible(ler_tabela_auxiliar(con, TAB_CARGO))
untrace("dbGetQuery", where = asNamespace("DBI"))
checar("tabela auxiliar consultada uma vez em 5 leituras", consultas == 1L)
checar("tabelas auxiliares semeadas", length(ler_tabela_auxiliar(con, TAB_CARGO)) == length(SEMENTE_CARGO_SERVIDOR))

# =====================================================
cat("\n[10] Módulos: carga sob demanda\n")
# =====================================================

pasta_serv <- file.path(Sys.getenv("PASTA_ALERTAS"), "TJSE", "serventias")
dir.create(pasta_serv, recursive = TRUE, showWarnings = FALSE)
writeLines(c("Código Serventia;Nome Serventia;CEP;Status", "1;Vara A;49000-000;Ativo"),
           file.path(pasta_serv, "serventias.csv"))

testServer(mod_alertas_serventias_server,
           args = list(ativo = reactive(FALSE), empresa = reactive("TJSE")), {
    session$flushReact()
    checar("módulo fechado não lê arquivos", nrow(dados_base()) == 0)
})

ativo_rv <- reactiveVal(FALSE)
empresa_rv <- reactiveVal("TJSE")
testServer(mod_alertas_serventias_server,
           args = list(ativo = ativo_rv, empresa = empresa_rv), {
    ativo_rv(TRUE)
    session$flushReact()
    checar("módulo aberto carrega a base", nrow(dados_base()) == 1)
    empresa_rv(NULL)
    session$flushReact()
    checar("sem empresa (logout), dados são descartados", nrow(dados_base()) == 0)
})

testServer(mod_alertas_serventias_server,
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
    p_mag <- pos_em(h, 'data-menu="AlertasMagistrados"')
    p_pes <- pos_em(h, 'data-menu="Alertas"')
    p_serv <- pos_em(h, 'data-menu="AlertasServentias"')
    p_cfg <- pos_em(h, 'class="icon-grupo"')
    p_sair <- pos_em(h, 'id="sair"')
    checar("ordem: Magistrados, Pessoal e Auxiliar, Serventias, Configurações, Sair",
           all(c(p_mag, p_pes, p_serv, p_cfg, p_sair) > 0) &&
               p_mag < p_pes && p_pes < p_serv && p_serv < p_cfg && p_cfg < p_sair)
    checar("barra começa expandida", grepl('class="icon-bar expandida"', h, fixed = TRUE))
    checar("módulo inicial é Magistrados", identical(menuSelecionado(), ABA_ALERTAS_MAGISTRADOS))
    sub <- regmatches(h, regexpr('class="icon-submenu".*?id="sair"', h))
    checar("opções de Configurações ficam dentro da barra (Trocar empresa, Ver instruções, Administração)",
           length(sub) == 1 && pos_em(sub, 'id="trocar_empresa"') > 0 &&
               pos_em(sub, 'id="trocar_empresa"') < pos_em(sub, 'id="mostrar_readme"') &&
               pos_em(sub, 'id="mostrar_readme"') < pos_em(sub, 'id="abrir_admin"'))
    checar("submenu flutuante antigo removido", !grepl("submenu-config", h, fixed = TRUE))
})

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

cat(sprintf("\n%d de %d verificações passaram.\n", total - falhas, total))
if (falhas > 0) quit(status = 1)
