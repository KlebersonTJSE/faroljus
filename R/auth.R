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
# Antes, esta função gravava a configuração em variáveis GLOBAIS
# (LDAP_SERVER <<- ...). Como todas as sessões do Shiny rodam no mesmo
# processo R, uma sessão podia ler a configuração gravada por outra
# (outra empresa). Agora a configuração é devolvida como lista, local a
# cada chamada; NULL quando incompleta.
obter_config_ldap <- function(distro){
    
    cfg <- list(
        servidor    = Sys.getenv(distro_env(distro, "LDAP_SERVER")),
        porta       = suppressWarnings(as.integer(Sys.getenv(distro_env(distro, "LDAP_PORT")))),
        dominio     = Sys.getenv(distro_env(distro, "LDAP_DOMAIN")),
        search_base = Sys.getenv(distro_env(distro, "LDAP_SEARCH_BASE"))
    )
    
    campos <- unlist(cfg)
    
    if (all(!is.na(campos) & campos != "")) cfg else NULL
}

validar_config_ldap <- function(distro){
    !is.null(obter_config_ldap(distro))
}

# Usuário AD aceito no login: letras, números, ponto, hífen e
# sublinhado (formato de sAMAccountName). Recusa qualquer outro
# caractere antes de falar com o servidor — inclusive "@" (que permitiria
# autenticar com um UPN de outro domínio) e os caracteres especiais de
# filtro LDAP (*, (, ), \).
USUARIO_AD_VALIDO <- "^[A-Za-z0-9._-]{1,64}$"

# Escapa um valor para uso em filtro LDAP (RFC 4515). Defesa extra além
# da validação acima.
escapar_filtro_ldap <- function(x) {
    x <- gsub("\\\\", "\\\\5c", x)
    x <- gsub("\\*", "\\\\2a", x)
    x <- gsub("\\(", "\\\\28", x)
    gsub("\\)", "\\\\29", x)
}

# Somente os atributos usados pela aplicação (cabeçalho da tela
# principal). Antes era ALL_ATTRIBUTES — trazia o objeto inteiro do AD
# (grupos, foto, histórico...) a cada login: mais lento e guardava na
# sessão dados que o app não usa.
ATRIBUTOS_AD <- c(
    "sAMAccountName", "displayName", "mail", "department",
    "manager", "whenCreated", "lastLogonTimestamp"
)

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
    
    cfg <- obter_config_ldap(distro)
    
    if(is.null(cfg)){
        
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
    
    # A senha NÃO é aparada: espaços nas pontas fazem parte dela. Senha
    # vazia é recusada antes do bind — um bind com senha vazia vira
    # "unauthenticated bind" e pode ser aceito pelo servidor.
    if(
        !grepl(USUARIO_AD_VALIDO, usuario) ||
        !nzchar(senha) ||
        !nzchar(trimws(senha))
    ){
        
        return(NULL)
        
    }
    
    servidor <- ldap3$Server(
        
        cfg$servidor,
        
        port = cfg$porta,
        
        use_ssl = TRUE,
        
        get_info = ldap3$NONE,
        
        connect_timeout = 10L
        
    )
    
    usuario_ad <- paste0(
        usuario,
        "@",
        cfg$dominio
    )
    
    conexao <- ldap3$Connection(
        
        servidor,
        
        user = usuario_ad,
        
        password = senha,
        
        auto_bind = FALSE,
        
        receive_timeout = 15L
        
    )
    
    autenticado <- tryCatch({
        
        isTRUE(conexao$bind())
        
    },
    
    error = function(e){
        
        FALSE
        
    })
    
    if(!autenticado){
        
        try(conexao$unbind(), silent = TRUE)
        
        return(NULL)
        
    }
    
    resultado <- tryCatch({
        
        filtro <- paste0(
            "(sAMAccountName=",
            escapar_filtro_ldap(usuario),
            ")"
        )
        
        conexao$search(
            
            search_base = cfg$search_base,
            
            search_filter = filtro,
            
            attributes = as.list(ATRIBUTOS_AD),
            
            size_limit = 1L
            
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
    
    cfg <- obter_config_ldap(distro)
    
    if(is.null(cfg)){
        return(FALSE)
    }
    
    tryCatch({
        
        servidor <- ldap3$Server(
            
            cfg$servidor,
            
            port = cfg$porta,
            
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