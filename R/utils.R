# R/utils.R

# Operador "valor ou padrão" (base R só o tem a partir da 4.4). Definido
# aqui porque utils.R é o primeiro arquivo carregado pelo app.R.
if (!exists("%||%", mode = "function")) {
    `%||%` <- function(a, b) if (is.null(a)) b else a
}

# =====================================================
# EMPRESAS (MULTI-DISTRO)
# =====================================================

# Nome da variável de ambiente específica de uma empresa (distro), ex.:
# distro_env("MPRO", "LDAP_SERVER") -> "MPRO_LDAP_SERVER"
# Sempre normaliza para MAIÚSCULAS, pelo mesmo motivo documentado em
# authenticate_ad() (R/auth.R) e conectar_banco() (R/database.R): o
# cadastro de usuário TOTP grava a empresa sempre em maiúsculas, então
# login AD e login TOTP da mesma empresa precisam resolver para o
# mesmo conjunto de variáveis de ambiente.
distro_env <- function(distro, sufixo){
    
    paste0(
        toupper(trimws(distro)),
        "_",
        sufixo
    )
    
}

# Lista as empresas/unidades configuradas em DISTRO_1, DISTRO_2... no
# .Renviron. Percorre sequencialmente a partir de 1 e para no primeiro
# número não definido (ou vazio) — não é preciso numeração contínua além
# de uma lacuna, então não pule números.
listar_distros <- function(){
    
    distros <- character(0)
    i <- 1
    
    repeat {
        
        valor <- Sys.getenv(
            paste0("DISTRO_", i),
            unset = ""
        )
        
        if(valor == ""){
            break
        }
        
        distros <- c(distros, trimws(valor))
        i <- i + 1
        
    }
    
    distros
    
}

obter_campo <- function(lista, nome){
    
    valor <- lista[[nome]]
    
    if(is.null(valor))
        return("")
    
    if(length(valor) == 0)
        return("")
    
    paste(as.character(valor), collapse = "; ")
    
}

extrair_manager <- function(manager){
    
    if(is.null(manager))
        return("")
    
    if(length(manager) == 0)
        return("")
    
    manager <- as.character(manager)[1]
    
    sub("^CN=([^,]+),.*$", "\\1", manager)
    
}

formatar_whenCreated <- function(x) {
    if (is.null(x) || is.na(x) || x == "") return("")
    
    format(
        as.POSIXct(
            gsub("\\.0Z$", "", x),
            format = "%Y%m%d%H%M%S",
            tz = "UTC"
        ),
        "%d/%m/%Y %H:%M:%S"
    )
}

formatar_lastLogon <- function(x) {
    if (is.null(x) || is.na(x) || x == "" || x == "0") {
        return("Nunca acessou")
    }
    
    format(
        as.POSIXct(
            (as.numeric(x) / 10000000) - 11644473600,
            origin = "1970-01-01",
            tz = "UTC"
        ),
        "%d/%m/%Y %H:%M:%S"
    )
}

# =====================================================
# NOME DE ARQUIVO SEGURO (upload)
# -----------------------------------------------------
# O nome original de um arquivo enviado (input$<fileInput>$name) vem do
# navegador e pode ser manipulado — ex.: "../../MPRO/pessoal/x.csv"
# gravaria o arquivo na pasta de OUTRA empresa, ou fora de data/.
# Mantém só o último componente do caminho (tanto "/" quanto "\\"),
# troca caracteres fora de letras/números/espaço/._()- por "_", tira
# pontos/espaços do início e aceita só as extensões informadas.
# Devolve NA quando o nome não serve. (Mesma função do FarolJus Lite.)
# =====================================================

nome_arquivo_seguro <- function(nome, extensao = c("csv", "xlsx")) {
    
    nome <- as.character(nome)
    
    # Bytes UTF-8 válidos sem marcação de encoding (comum em locale "C")
    # são marcados como UTF-8, para os acentos serem reconhecidos como
    # letras abaixo.
    sem_marca <- !is.na(nome) & Encoding(nome) == "unknown" & validUTF8(nome)
    Encoding(nome[sem_marca]) <- "UTF-8"
    
    nome <- gsub("\\\\", "/", nome)
    nome <- sub("^.*/", "", nome)
    nome <- gsub("[^\\p{L}\\p{N} ._()-]", "_", enc2utf8(nome), perl = TRUE)
    nome <- sub("^[. ]+", "", nome)
    nome <- trimws(nome)
    
    ext <- tolower(tools::file_ext(nome))
    base <- tools::file_path_sans_ext(nome)
    
    valido <- !is.na(nome) &
        ext %in% tolower(extensao) &
        nchar(trimws(base)) > 0 &
        nchar(nome) <= 200
    
    ifelse(valido, nome, NA_character_)
    
}

# =====================================================
# GRAVAÇÃO SEGURA DE UM ARQUIVO ENVIADO
# -----------------------------------------------------
# Copia `origem` para `destino` e só DEPOIS apaga os `anteriores` (ex.:
# o serventias.xlsx antigo quando chega um serventias.csv). Antes, os
# módulos apagavam o anterior primeiro: se a cópia falhasse, a base da
# empresa sumia. A cópia vai para um nome temporário oculto na mesma
# pasta e é renomeada no fim, então quem ler a pasta no meio do processo
# nunca vê um arquivo pela metade.
# Devolve TRUE se o arquivo novo ficou gravado. (Mesma função do
# FarolJus Lite, usada pelos módulos de dados.)
# =====================================================

gravar_arquivo_substituindo <- function(origem, destino, anteriores = character(0)) {
    
    pasta <- dirname(destino)
    temporario <- file.path(pasta, paste0(".enviando_", basename(destino)))
    
    if (!isTRUE(file.copy(origem, temporario, overwrite = TRUE))) {
        unlink(temporario)
        return(FALSE)
    }
    
    a_remover <- setdiff(normalizePath(anteriores, mustWork = FALSE),
                         normalizePath(destino, mustWork = FALSE))
    a_remover <- a_remover[file.exists(a_remover)]
    if (length(a_remover) > 0) {
        file.remove(a_remover)
    }
    
    if (file.exists(destino)) {
        file.remove(destino)
    }
    
    ok <- isTRUE(file.rename(temporario, destino))
    
    if (!ok) {
        # file.rename pode falhar entre dispositivos; tenta copiar.
        ok <- isTRUE(file.copy(temporario, destino, overwrite = TRUE))
        unlink(temporario)
    }
    
    ok
    
}

# =====================================================
# CACHE DE ARQUIVOS LIDOS (compartilhado entre as sessões)
# -----------------------------------------------------
# Ler e limpar as planilhas do MPM/CNJ é a parte mais lenta do app (o
# Quadro de Pessoal tem ~17 mil linhas em .xlsx). Antes, CADA sessão (cada
# login, cada "Atualizar Dados", cada troca de empresa) relia todos os
# arquivos do disco, inclusive dos módulos que nem estavam abertos.
#
# Todas as sessões do Shiny rodam no mesmo processo R, então o resultado
# da leitura fica guardado aqui, por arquivo, e é reaproveitado enquanto
# o arquivo não mudar. A "assinatura" do arquivo é caminho + data de
# modificação + tamanho: enviar, substituir ou apagar o arquivo pelo
# "Gerenciar Arquivos" (ou direto na pasta) muda a assinatura e a próxima
# leitura vai ao disco. Não precisa invalidar nada manualmente.
#
# Resultados vazios/NULL (arquivo ilegível, sem linhas) não são guardados
# — assim o aviso de erro continua aparecendo para quem tentar de novo.
#
# Limite: CACHE_ARQUIVOS_MAX entradas (padrão 30); passando disso, sai a
# menos usada recentemente. CACHE_ARQUIVOS_MAX=0 no .Renviron desliga o
# cache.
# =====================================================

CACHE_ARQUIVOS_MAX <- suppressWarnings(as.integer(Sys.getenv("CACHE_ARQUIVOS_MAX", unset = "30")))
if (is.na(CACHE_ARQUIVOS_MAX) || CACHE_ARQUIVOS_MAX < 0) CACHE_ARQUIVOS_MAX <- 30L

.cache_arquivos <- new.env(parent = emptyenv())

assinatura_arquivo <- function(caminho) {
    
    if (is.null(caminho) || length(caminho) != 1 || is.na(caminho)) {
        return(NULL)
    }
    
    info <- suppressWarnings(file.info(caminho, extra_cols = FALSE))
    
    if (is.na(info$size) || isTRUE(info$isdir)) {
        return(NULL)
    }
    
    paste(
        normalizePath(caminho, winslash = "/", mustWork = FALSE),
        format(as.numeric(info$mtime), nsmall = 3),
        info$size,
        sep = "|"
    )
    
}

com_cache_arquivo <- function(caminho, variante, ler) {
    
    assinatura <- assinatura_arquivo(caminho)
    
    if (CACHE_ARQUIVOS_MAX == 0 || is.null(assinatura)) {
        return(ler())
    }
    
    chave <- paste(
        normalizePath(caminho, winslash = "/", mustWork = FALSE),
        variante,
        sep = "#"
    )
    
    item <- .cache_arquivos[[chave]]
    
    if (!is.null(item) && identical(item$assinatura, assinatura)) {
        item$usado_em <- Sys.time()
        assign(chave, item, envir = .cache_arquivos)
        return(item$valor)
    }
    
    valor <- ler()
    
    vazio <- is.null(valor) || (is.data.frame(valor) && nrow(valor) == 0)
    
    if (vazio) {
        if (!is.null(item)) rm(list = chave, envir = .cache_arquivos)
        return(valor)
    }
    
    assign(
        chave,
        list(assinatura = assinatura, valor = valor, usado_em = Sys.time()),
        envir = .cache_arquivos
    )
    
    # Mantém no máximo CACHE_ARQUIVOS_MAX entradas (sai a menos usada).
    chaves <- ls(.cache_arquivos, all.names = TRUE)
    
    if (length(chaves) > CACHE_ARQUIVOS_MAX) {
        usados <- vapply(chaves, function(k) as.numeric(.cache_arquivos[[k]]$usado_em), numeric(1))
        remover <- names(sort(usados))[seq_len(length(chaves) - CACHE_ARQUIVOS_MAX)]
        rm(list = remover, envir = .cache_arquivos)
    }
    
    valor
    
}

# Descarta o cache (todo, ou só o dos arquivos dentro de `pasta`). Usado
# pelo botão "Atualizar Dados", para garantir uma releitura do disco
# mesmo que a data de modificação do arquivo não tenha mudado (ex.: cópia
# que preserva a data original).
limpar_cache_arquivos <- function(pasta = NULL) {
    
    chaves <- ls(.cache_arquivos, all.names = TRUE)
    
    if (!is.null(pasta)) {
        prefixo <- paste0(normalizePath(pasta, winslash = "/", mustWork = FALSE), "/")
        chaves <- chaves[startsWith(chaves, prefixo)]
    }
    
    if (length(chaves) > 0) {
        rm(list = chaves, envir = .cache_arquivos)
    }
    
    invisible(TRUE)
    
}
