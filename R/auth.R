# =====================================================
# R/auth.R
# =====================================================

library(reticulate)
library(jsonlite)

# =====================================================
# PYTHON
# =====================================================

python_path <- Sys.getenv("RETICULATE_PYTHON", unset = Sys.which("python"))
if (!nzchar(python_path) || !file.exists(python_path)) {
    stop(
        "Python não encontrado em '", python_path, "'. ",
        "Verifique a instalação do Python ou defina RETICULATE_PYTHON no .Renviron ",
        "apontando para o python.exe correto."
    )
}
use_python(python_path, required = TRUE)

ldap3 <- import("ldap3")

# =====================================================
# CONFIGURAÇÕES LDAP E VALIDAÇÃO DINÂMICA (POR EMPRESA)
# =====================================================

# Cada empresa (distro) configurada em DISTRO_1, DISTRO_2... no
# .Renviron (ver listar_distros() em R/utils.R) tem sua própria
# configuração de LDAP, nas variáveis {DISTRO}_LDAP_SERVER,
# {DISTRO}_LDAP_PORT, {DISTRO}_LDAP_DOMAIN e {DISTRO}_LDAP_SEARCH_BASE.
# Adicionar uma nova empresa não exige alterar este arquivo — só
# DISTRO_N + as variáveis correspondentes no .Renviron.
validar_config_ldap <- function(distro){
    
    LDAP_SERVER <<- Sys.getenv(distro_env(distro, "LDAP_SERVER"))
    LDAP_PORT   <<- as.integer(Sys.getenv(distro_env(distro, "LDAP_PORT")))
    LDAP_DOMAIN <<- Sys.getenv(distro_env(distro, "LDAP_DOMAIN"))
    SEARCH_BASE <<- Sys.getenv(distro_env(distro, "LDAP_SEARCH_BASE"))
    
    campos <- c(LDAP_SERVER, LDAP_PORT, LDAP_DOMAIN, SEARCH_BASE)
    
    all(!is.na(campos) & campos != "")
}

# =====================================================
# AUTENTICAÇÃO ACTIVE DIRECTORY
# =====================================================

authenticate_ad <- function(
        usuario,
        senha,
        distro
){
    
    if(
        is.null(distro) ||
        trimws(distro) == ""
    ){
        
        stop(
            "Empresa (distro) não informada para autenticação AD."
        )
        
    }
    
    if(!validar_config_ldap(distro)){
        
        stop(
            "Configuração LDAP não encontrada para a empresa '", distro, "'."
        )
        
    }
    
    if(
        is.null(usuario) ||
        is.null(senha)
    ){
        
        return(NULL)
        
    }
    
    usuario <- trimws(usuario)
    senha   <- trimws(senha)
    
    if(
        usuario == "" ||
        senha == ""
    ){
        
        return(NULL)
        
    }
    
    servidor <- ldap3$Server(
        
        LDAP_SERVER,
        
        port = LDAP_PORT,
        
        use_ssl = TRUE,
        
        get_info = ldap3$NONE
        
    )
    
    usuario_ad <- paste0(
        usuario,
        "@",
        LDAP_DOMAIN
    )
    
    conexao <- ldap3$Connection(
        
        servidor,
        
        user = usuario_ad,
        
        password = senha,
        
        auto_bind = FALSE
        
    )
    
    autenticado <- tryCatch({
        
        conexao$bind()
        
    },
    
    error = function(e){
        
        FALSE
        
    })
    
    if(!autenticado){
        
        return(NULL)
        
    }
    
    resultado <- tryCatch({
        
        filtro <- paste0(
            "(sAMAccountName=",
            usuario,
            ")"
        )
        
        conexao$search(
            
            search_base = SEARCH_BASE,
            
            search_filter = filtro,
            
            attributes = ldap3$ALL_ATTRIBUTES
            
        )
        
        if(length(conexao$entries) == 0){
            
            conexao$unbind()
            
            return(NULL)
            
        }
        
        entry <- conexao$entries[[1]]
        
        atributos <- py_to_r(
            entry$entry_attributes_as_dict
        )
        
        conexao$unbind()
        
        atributos
        
    },
    
    error = function(e){
        
        try(
            conexao$unbind(),
            silent = TRUE
        )
        
        NULL
        
    })
    
    resultado
    
}

# =====================================================
# OBTÉM FOTO DO USUÁRIO
# =====================================================

obter_foto_usuario <- function(
        dados_usuario
){
    
    if(is.null(dados_usuario)){
        
        return(NULL)
        
    }
    
    if(
        is.null(
            dados_usuario$thumbnailPhoto
        )
    ){
        
        return(NULL)
        
    }
    
    tryCatch({
        
        bytes <- as.raw(
            
            unlist(
                dados_usuario$thumbnailPhoto
            )
            
        )
        
        paste0(
            
            "data:image/jpeg;base64,",
            
            jsonlite::base64_enc(
                bytes
            )
            
        )
        
    },
    
    error = function(e){
        
        NULL
        
    })
    
}

# =====================================================
# TESTE DE CONECTIVIDADE LDAP
# =====================================================

testar_ldap <- function(distro){
    
    if(!validar_config_ldap(distro)){
        return(FALSE)
    }
    
    tryCatch({
        
        servidor <- ldap3$Server(
            
            LDAP_SERVER,
            
            port = LDAP_PORT,
            
            use_ssl = TRUE,
            
            get_info = ldap3$NONE
            
        )
        
        TRUE
        
    },
    
    error = function(e){
        
        FALSE
        
    })
    
}

# =====================================================
# OBTÉM NOME COMPLETO
# =====================================================

obter_nome_usuario <- function(
        dados_usuario
){
    
    if(is.null(dados_usuario))
        return("")
    
    if(is.null(dados_usuario$displayName))
        return("")
    
    as.character(
        dados_usuario$displayName[[1]]
    )
    
}

# =====================================================
# OBTÉM EMAIL
# =====================================================

obter_email_usuario <- function(
        dados_usuario
){
    
    if(is.null(dados_usuario))
        return("")
    
    if(is.null(dados_usuario$mail))
        return("")
    
    as.character(
        dados_usuario$mail[[1]]
    )
    
}

# =====================================================
# OBTÉM LOGIN
# =====================================================

obter_login_usuario <- function(
        dados_usuario
){
    
    if(is.null(dados_usuario))
        return("")
    
    if(
        is.null(
            dados_usuario$sAMAccountName
        )
    )
        return("")
    
    as.character(
        dados_usuario$sAMAccountName[[1]]
    )
    
}

# =====================================================
# OBTÉM DEPARTAMENTO
# =====================================================

obter_departamento_usuario <- function(
        dados_usuario
){
    
    if(is.null(dados_usuario))
        return("")
    
    if(
        is.null(
            dados_usuario$department
        )
    )
        return("")
    
    as.character(
        dados_usuario$department[[1]]
    )
    
}