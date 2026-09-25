# =====================================================
# R/database.R
# =====================================================

library(DBI)
library(RJDBC)

# O driver JDBC (classe + .jar) é compartilhado por todas as empresas —
# não muda de uma empresa para outra, então continua lido de
# IRIS_DRIVER/IRIS_JAR (sem prefixo de empresa). Já a URL, usuário e
# senha de conexão são específicos de cada empresa (distro), lidos de
# {DISTRO}_IRIS_URL, {DISTRO}_IRIS_USER e {DISTRO}_IRIS_PASSWORD (ver
# distro_env() em R/utils.R e listar_distros() para a lista de empresas
# configuradas em DISTRO_1, DISTRO_2... no .Renviron).
conectar_banco <- function(distro){
    
    if(
        is.null(distro) ||
        trimws(distro) == ""
    ){
        
        stop(
            "Empresa (distro) não informada para conexão com o banco IRIS."
        )
        
    }
    
    drv <- JDBC(
        driverClass = Sys.getenv("IRIS_DRIVER"),
        classPath = Sys.getenv("IRIS_JAR")
    )
    
    url      <- Sys.getenv(distro_env(distro, "IRIS_URL"))
    usuario  <- Sys.getenv(distro_env(distro, "IRIS_USER"))
    senha    <- Sys.getenv(distro_env(distro, "IRIS_PASSWORD"))
    
    if(url == "" || usuario == ""){
        
        stop(
            "Configuração de banco IRIS não encontrada para a empresa '",
            distro, "'."
        )
        
    }
    
    dbConnect(
        drv,
        url,
        user = usuario,
        password = senha
    )
    
}