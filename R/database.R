# =====================================================
# R/database.R
# -----------------------------------------------------
# Conexão com o InterSystems IRIS via JDBC (RJDBC/rJava), por empresa.
#
# Variáveis de ambiente (.Renviron):
#   - Compartilhadas por todas as empresas (o driver não muda):
#       IRIS_DRIVER_CLASS  classe do driver JDBC
#                          (ex.: com.intersystems.jdbc.IRISDriver)
#       IRIS_JAR_PATH      caminho do .jar do driver
#     Os nomes antigos IRIS_DRIVER / IRIS_JAR ainda são aceitos como
#     alternativa — antes, este arquivo lia só esses nomes, enquanto o
#     .Renviron define IRIS_DRIVER_CLASS / IRIS_JAR_PATH, então o driver
#     chegava vazio ao JDBC().
#   - Por empresa (ver distro_env() e listar_distros() em R/utils.R):
#       {DISTRO}_IRIS_URL, {DISTRO}_IRIS_USER, {DISTRO}_IRIS_PASSWORD
#   - JAVA_HOME: precisa estar definido ANTES de o rJava ser carregado.
#     O app.R lê o .Renviron (readRenviron) antes de dar source() neste
#     arquivo, então basta a variável estar no .Renviron.
# =====================================================

library(DBI)

# -----------------------------------------------------
# JAVA_HOME — checagem antes de carregar o rJava
# -----------------------------------------------------
# Se JAVA_HOME apontar para uma pasta inexistente, o rJava falha com uma
# mensagem pouco clara (ou derruba o R no Windows). Aqui o problema é
# avisado de forma explícita. Sem JAVA_HOME definido, o rJava tenta
# localizar o Java sozinho (comportamento padrão).
local({
    
    java_home <- Sys.getenv("JAVA_HOME", unset = "")
    
    if (nzchar(java_home) && !dir.exists(java_home)) {
        stop(
            "JAVA_HOME aponta para uma pasta inexistente: '", java_home, "'. ",
            "Corrija JAVA_HOME no .Renviron (pasta do JDK/JRE, sem \\bin no final)."
        )
    }
    
})

library(RJDBC)

# -----------------------------------------------------
# LEITURA DE VARIÁVEL COM NOME ALTERNATIVO
# -----------------------------------------------------
# Devolve o valor da primeira variável definida (não vazia) entre as
# informadas, ou "" se nenhuma estiver definida.
iris_env <- function(...) {
    
    for (nome in c(...)) {
        valor <- trimws(Sys.getenv(nome, unset = ""))
        if (nzchar(valor)) {
            return(valor)
        }
    }
    
    ""
    
}

# -----------------------------------------------------
# DRIVER JDBC (carregado uma única vez por processo R)
# -----------------------------------------------------
# JDBC() carrega a classe do driver na JVM; não há motivo para repetir
# isso a cada conexão. O objeto fica guardado em .iris_cache e é
# reaproveitado por todas as sessões do app.
.iris_cache <- new.env(parent = emptyenv())

obter_driver_iris <- function() {
    
    if (!is.null(.iris_cache$drv)) {
        return(.iris_cache$drv)
    }
    
    driver_class <- iris_env("IRIS_DRIVER_CLASS", "IRIS_DRIVER")
    jar_path     <- iris_env("IRIS_JAR_PATH", "IRIS_JAR")
    
    if (!nzchar(driver_class)) {
        stop(
            "Classe do driver JDBC do IRIS não configurada. ",
            "Defina IRIS_DRIVER_CLASS no .Renviron ",
            "(ex.: com.intersystems.jdbc.IRISDriver)."
        )
    }
    
    if (!nzchar(jar_path)) {
        stop(
            "Caminho do .jar do driver JDBC do IRIS não configurado. ",
            "Defina IRIS_JAR_PATH no .Renviron."
        )
    }
    
    if (!file.exists(jar_path)) {
        stop(
            "Arquivo do driver JDBC do IRIS não encontrado em: '", jar_path, "'. ",
            "Verifique IRIS_JAR_PATH no .Renviron."
        )
    }
    
    .iris_cache$drv <- JDBC(
        driverClass = driver_class,
        classPath = jar_path
    )
    
    .iris_cache$drv
    
}

# -----------------------------------------------------
# CONEXÃO POR EMPRESA
# -----------------------------------------------------
# Abre uma conexão com o IRIS da empresa (distro) informada. Quem chama
# é responsável por fechar (DBI::dbDisconnect) — ou use
# consultar_iris(), abaixo, que já faz isso.
conectar_banco <- function(distro){
    
    if(
        is.null(distro) ||
        is.na(distro) ||
        trimws(distro) == ""
    ){
        
        stop(
            "Empresa (distro) não informada para conexão com o banco IRIS."
        )
        
    }
    
    url      <- Sys.getenv(distro_env(distro, "IRIS_URL"))
    usuario  <- Sys.getenv(distro_env(distro, "IRIS_USER"))
    senha    <- Sys.getenv(distro_env(distro, "IRIS_PASSWORD"))
    
    if(url == "" || usuario == ""){
        
        stop(
            "Configuração de banco IRIS não encontrada para a empresa '",
            distro, "'. Defina ", distro_env(distro, "IRIS_URL"), " e ",
            distro_env(distro, "IRIS_USER"), " no .Renviron."
        )
        
    }
    
    dbConnect(
        obter_driver_iris(),
        url,
        user = usuario,
        password = senha
    )
    
}

# -----------------------------------------------------
# CONSULTA COM ABERTURA/FECHAMENTO AUTOMÁTICOS
# -----------------------------------------------------
# Abre a conexão da empresa, executa a consulta e fecha a conexão ao
# final — inclusive em caso de erro (on.exit). Evita conexões JDBC
# esquecidas abertas no IRIS a cada consulta feita pelos módulos.
#
#   consultar_iris("TJSE", "SELECT TOP 10 * FROM Tabela WHERE Ano = ?",
#                  params = list(2026))
consultar_iris <- function(distro, sql, params = NULL) {
    
    con_iris <- conectar_banco(distro)
    on.exit(try(DBI::dbDisconnect(con_iris), silent = TRUE), add = TRUE)
    
    if (is.null(params) || length(params) == 0) {
        DBI::dbGetQuery(con_iris, sql)
    } else {
        do.call(DBI::dbGetQuery, c(list(con_iris, sql), params))
    }
    
}

# -----------------------------------------------------
# TESTE DE CONEXÃO
# -----------------------------------------------------
# Útil no console para validar a configuração de uma empresa antes de
# usar a integração em um módulo. Retorna TRUE/FALSE e imprime o erro.
testar_iris <- function(distro) {
    
    tryCatch({
        
        consultar_iris(distro, "SELECT 1 AS ok")
        message("Conexão com o IRIS da empresa '", distro, "' OK.")
        TRUE
        
    }, error = function(e) {
        
        message("Falha na conexão com o IRIS da empresa '", distro, "': ",
                conditionMessage(e))
        FALSE
        
    })
    
}