# =====================================================
# modules/mod_alertas.R
# -----------------------------------------------------
# Módulo "Pessoal e Auxiliar" (botão da barra lateral), com duas abas:
#
#   1) "Base de Dados - Quadro Pessoal e Auxiliar" — cadastro do Quadro
#      de Pessoal e Auxiliar do MPM/CNJ, lido DIRETAMENTE da planilha
#      quadro_pessoal_auxiliar.xlsx (ou .csv). A tabela tem as mesmas
#      colunas do arquivo, na mesma ordem (Situação profissional atual e
#      Cargo aparecem por extenso). Filtros: Status (padrão "Ativo"),
#      CPF, Nome, Cargo, Situação profissional atual e Detalhe (busca o
#      texto em todas as colunas). Gráfico com agrupamento selecionável.
#
#   2) "Alertas - Quadro Pessoal e Auxiliar" — arquivos de alertas do MPM
#      (um ou vários, .csv ou .xlsx), consolidados. Funcionamento de
#      sempre: filtros, alertas consolidados na tabela, gráficos, CSV.
#
# Todos os arquivos ficam na subpasta "pessoal" da empresa:
#   <PASTA_ALERTAS>/<EMPRESA>/pessoal/quadro_pessoal_auxiliar.xlsx
#   <PASTA_ALERTAS>/<EMPRESA>/pessoal/<arquivos de alertas>
#
# Este arquivo também define funções GENÉRICAS (leitura de .xlsx, busca
# "Detalhe", combos, botão de CSV) reaproveitadas por
# mod_alertas_serventias.R — por isso ele é carregado primeiro no app.R.
# =====================================================

library(shiny)
library(readr)
library(readxl)   # leitura direta de .xlsx
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(DT)
library(echarts4r)
library(ggiraph)

# =====================================================
# LIMITE DE TAMANHO DE UPLOAD
# -----------------------------------------------------
# Por padrão, o Shiny limita o tamanho TOTAL de um upload (soma de
# todos os arquivos selecionados de uma vez) a 5 MB. Aumentamos aqui
# para caber confortavelmente vários arquivos de uma vez.
#
# Essa opção é GLOBAL do processo R (não é possível limitar por
# fileInput ou por módulo). Fica definida aqui e vale também para
# mod_alertas_serventias.R (carregado depois deste arquivo no app.R).
#
# Configurável via MAX_UPLOAD_MB no .Renviron (ou nas Vars do
# shinyapps.io); sem essa variável, usa 50 MB como padrão. Isso não
# contorna um eventual limite de tamanho de requisição imposto pelo
# próprio shinyapps.io por cima do app — se o plano/instância tiver um
# teto menor que isso, ele ainda vale e não há como alterá-lo por
# código; nesse caso, ajuste MAX_UPLOAD_MB para um valor compatível.
# =====================================================

MAX_UPLOAD_MB <- suppressWarnings(
    as.numeric(Sys.getenv("MAX_UPLOAD_MB", unset = "50"))
)

if (is.na(MAX_UPLOAD_MB) || MAX_UPLOAD_MB <= 0) {
    MAX_UPLOAD_MB <- 50
}

options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)

# =====================================================
# CONFIGURAÇÃO (multi-empresa, uma subpasta por módulo)
# -----------------------------------------------------
# Cada empresa (distro) tem sua própria pasta, com uma subpasta para
# cada módulo de alertas:
#   <PASTA_ALERTAS>/<EMPRESA>/pessoal     -> mod_alertas.R
#                                            (Quadro de Pessoal e Auxiliar)
#   <PASTA_ALERTAS>/<EMPRESA>/serventias  -> mod_alertas_serventias.R
# Como cada módulo só enxerga a própria subpasta, "Apagar" e "Enviar"
# de um módulo nunca mexem nos arquivos do outro.
#
# PASTA_ALERTAS (env var, default "data") define só o diretório raiz;
# a empresa é resolvida em tempo de execução a partir do usuário
# autenticado (ver parâmetro `empresa` de mod_alertas_server(),
# abaixo) — por isso o caminho NÃO é uma constante fixa calculada uma
# única vez ao carregar o módulo (isso seria compartilhado entre TODAS
# as sessões do app, já que rodam no mesmo processo R).
# =====================================================

PASTA_ALERTAS_BASE <- Sys.getenv("PASTA_ALERTAS", unset = "data")

SUBPASTA_PESSOAL <- "pessoal"
SUBPASTA_SERVENTIAS <- "serventias"

# Monta (e garante que existe) a subpasta de alertas da empresa
# informada. Retorna NULL se `empresa` não estiver definida (ex.:
# sessão ainda não autenticada). O padrão é a subpasta deste módulo
# (pessoal); mod_alertas_serventias.R passa SUBPASTA_SERVENTIAS.
caminho_alertas <- function(empresa, subpasta = SUBPASTA_PESSOAL) {
    
    if (is.null(empresa) || is.na(empresa) || trimws(empresa) == "") {
        return(NULL)
    }
    
    caminho <- file.path(PASTA_ALERTAS_BASE, trimws(empresa), subpasta)
    
    if (!dir.exists(caminho)) {
        dir.create(caminho, recursive = TRUE, showWarnings = FALSE)
    }
    
    caminho
    
}

# =====================================================
# TEMPO DE PROCESSAMENTO
# =====================================================

tempo_decorrido <- function(inicio) {
    round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 2)
}

# =====================================================
# LEITURA DE UM ARQUIVO
# -----------------------------------------------------
# Tenta primeiro como CSV separado por ";" (padrão de exportação
# brasileiro/Excel); se resultar em uma única coluna (sinal de que o
# delimitador está errado), tenta de novo com detecção automática do
# delimitador.
#
# Fica dentro de um tryCatch: em vez de travar o carregamento inteiro
# por causa de um arquivo ilegível (corrompido, formato inesperado),
# avisa qual arquivo deu problema e pula só ele — os demais continuam
# sendo processados normalmente.
# =====================================================

ler_arquivo_alertas <- function(arquivo, n_max = Inf) {
    
    resultado <- tryCatch({
        
        df <- read_csv2(
            arquivo,
            show_col_types = FALSE,
            locale = locale(
                encoding = "UTF-8",
                decimal_mark = ",",
                grouping_mark = "."
            ),
            col_types = cols(.default = col_character()),
            n_max = n_max
        )
        
        if (ncol(df) <= 1) {
            
            df <- read_delim(
                arquivo,
                delim = NULL,
                show_col_types = FALSE,
                locale = locale(encoding = "UTF-8"),
                col_types = cols(.default = col_character()),
                n_max = n_max
            )
            
        }
        
        df
        
    }, error = function(e) {
        
        msg <- paste0(
            "Não foi possível ler o arquivo ", basename(arquivo), ": ",
            conditionMessage(e), ". Esse arquivo foi ignorado."
        )
        
        warning(msg)
        showNotification(msg, type = "warning", duration = 15)
        
        NULL
        
    })
    
    resultado
    
}

# =====================================================
# CARREGAMENTO DOS ALERTAS
# -----------------------------------------------------
# Consolida todos os arquivos de alertas (.csv ou .xlsx) da pasta
# "pessoal" da empresa — menos a base de dados (quadro_pessoal_auxiliar,
# ver listar_arquivos_alertas_pessoal()). Arquivos com colunas diferentes
# entre si são combinados sem problema — bind_rows() preenche com NA o
# que faltar em cada um.
#
# Um arquivo sem nenhuma coluna "Alerta..."/"Conflito..." (ex.: uma cópia
# da base colocada na pasta com outro nome) é ignorado com aviso, para
# não misturar milhares de linhas de cadastro com os alertas.
# =====================================================

carregar_alertas <- function(caminho, arquivos = listar_arquivos_alertas_pessoal(caminho)) {
    
    if (is.null(caminho)) {
        return(data.frame())
    }
    
    if (length(arquivos) == 0) {
        return(data.frame())
    }
    
    resultados <- map(arquivos, function(arquivo) {
        
        df <- ler_arquivo_dados(arquivo)
        
        if (is.null(df)) {
            return(NULL)
        }
        
        names(df) <- limpar_cabecalho(names(df))
        
        if (length(colunas_por_prefixo(df, "Alerta")) == 0 &&
            length(colunas_por_prefixo(df, "Conflito")) == 0) {
            
            msg <- paste0(
                "O arquivo ", basename(arquivo), " não tem colunas de Alerta/Conflito ",
                "e foi ignorado na aba de alertas."
            )
            warning(msg)
            showNotification(msg, type = "warning", duration = 12)
            return(NULL)
        }
        
        df
    })
    
    resultados <- Filter(Negate(is.null), resultados)
    
    if (length(resultados) == 0) {
        return(data.frame())
    }
    
    bind_rows(resultados) %>%
        distinct()
    
}


# =====================================================
# COLUNAS POR PREFIXO
# =====================================================

colunas_por_prefixo <- function(df, prefixo) {
    
    if (is.null(df) || nrow(df) == 0) {
        return(character(0))
    }
    
    names(df)[str_starts(names(df), prefixo)]
    
}

# =====================================================
# COLUNAS COM DADOS
# =====================================================

colunas_com_dados <- function(df, colunas) {
    
    if (length(colunas) == 0) {
        return(character(0))
    }
    
    colunas[
        vapply(
            colunas,
            function(col) {
                valores <- df[[col]]
                any(!is.na(valores) & str_trim(valores) != "")
            },
            logical(1)
        )
    ]
    
}

# =====================================================
# CÓDIGO -> NOMENCLATURA (Situação Profissional Atual e Cargo)
# -----------------------------------------------------
# No arquivo do Quadro de Pessoal e Auxiliar (servidores) do CNJ/MPM,
# as colunas "Situação Profissional Atual" e "Cargo" vêm como código
# numérico. A correspondência código -> nomenclatura fica em duas
# tabelas do banco SQLite da aplicação (o mesmo faroljus.db do
# controle de acesso, conexão `con` do app.R):
#
#   - tab_situacao_profissional_servidor (codigo, nomenclatura)
#   - tab_cargo_servidor                 (codigo, nomenclatura)
#
# Os valores seguem o Manual de Preenchimento do MPM (Quadro de Pessoal
# e Auxiliar). As constantes SEMENTE_* logo abaixo são só a carga
# inicial: garantir_tabelas_auxiliares() cria as tabelas se não
# existirem e insere os códigos que faltarem (INSERT OR IGNORE — nunca
# sobrescreve uma nomenclatura já existente, então ajustes feitos
# direto no banco são preservados). Na hora de exibir, a leitura é
# sempre feita no banco.
#
# É só para exibição: a tabela e o CSV gerado a partir dela mostram a
# nomenclatura, enquanto dados(), dados_filtrados() e o gráfico
# continuam com os valores originais.
#
# Obs.: na Situação Profissional Atual não existem os códigos 4 e 5
# (a numeração pula de 3 para 6) — é assim no manual.
# =====================================================

TAB_SITUACAO_PROFISSIONAL <- "tab_situacao_profissional_servidor"
TAB_CARGO <- "tab_cargo_servidor"

SEMENTE_SITUACAO_PROFISSIONAL_SERVIDOR <- c(
    "1"  = "Cargo de chefia",
    "2"  = "Outros cargos em comissão ou funções comissionadas",
    "3"  = "Não exerce cargo em comissão ou função comissionada",
    "6"  = "Afastado(a)",
    "7"  = "Aposentado(a)",
    "8"  = "Falecido(a)",
    "9"  = "Exoneração/Vacância",
    "10" = "Demitido(a)",
    "11" = "Saída por Remoção",
    "12" = "Saída por cessão/requisição",
    "13" = "Vigência de Contrato/Vínculo"
)

SEMENTE_CARGO_SERVIDOR <- c(
    "1"  = "Servidor(a) efetivo(a) ou removido(a) para o Tribunal",
    "2"  = "Servidor(a) cedido(a) ou requisitado(a) de outro tribunal",
    "3"  = "Servidor(a) cedido(a) ou requisitado(a) de órgãos de fora do judiciário",
    "4"  = "Servidor(a) Comissionado(a) Sem vínculo",
    "5"  = "Estagiário(a)",
    "6"  = "Terceirizado(a)",
    "7"  = "Servidor(a) de serventia privatizada",
    "8"  = "Juiz(a) leigo(a)",
    "9"  = "Conciliador(a)",
    "10" = "Aprendiz",
    "11" = "Voluntário(a)",
    "12" = "Residência Jurídica",
    "13" = "Outros"
)

# Cria (se preciso) as tabelas auxiliares no SQLite e insere os códigos
# que ainda não existirem. Idempotente: pode rodar a cada início da
# aplicação/sessão. Um banco novo/vazio (ex.: primeiro deploy no
# shinyapps.io) ganha as tabelas já preenchidas.
garantir_tabelas_auxiliares <- function(con) {
    
    criar_e_semear <- function(tabela, semente) {
        
        DBI::dbExecute(
            con,
            sprintf(
                paste0(
                    "CREATE TABLE IF NOT EXISTS %s (",
                    "codigo INTEGER PRIMARY KEY, ",
                    "nomenclatura TEXT NOT NULL)"
                ),
                tabela
            )
        )
        
        for (i in seq_along(semente)) {
            DBI::dbExecute(
                con,
                sprintf(
                    "INSERT OR IGNORE INTO %s (codigo, nomenclatura) VALUES (?, ?)",
                    tabela
                ),
                params = list(as.integer(names(semente)[i]), unname(semente[i]))
            )
        }
        
    }
    
    criar_e_semear(TAB_SITUACAO_PROFISSIONAL, SEMENTE_SITUACAO_PROFISSIONAL_SERVIDOR)
    criar_e_semear(TAB_CARGO, SEMENTE_CARGO_SERVIDOR)
    
    invisible(TRUE)
    
}

# Lê uma tabela auxiliar do SQLite e devolve um vetor nomeado
# (nome = código como texto, valor = nomenclatura), no formato esperado
# por codigo_para_nomenclatura(). Se a conexão não existir ou a leitura
# falhar, devolve um vetor vazio — a tabela do app continua funcionando,
# só mostrando os códigos como vieram no arquivo.
ler_tabela_auxiliar <- function(con, tabela) {
    
    if (is.null(con)) {
        return(character(0))
    }
    
    tryCatch({
        
        ref <- DBI::dbGetQuery(
            con,
            sprintf("SELECT codigo, nomenclatura FROM %s", tabela)
        )
        
        setNames(as.character(ref$nomenclatura), as.character(ref$codigo))
        
    }, error = function(e) {
        
        warning("Não foi possível ler a tabela ", tabela, ": ", conditionMessage(e))
        character(0)
        
    })
    
}

# Troca, em um vetor de valores, o código pela nomenclatura da tabela
# informada. Aceita o código "puro" com ou sem zero à esquerda e com
# espaços ("6", "06", " 6 ", "6.0"). O que não for um código conhecido
# (valor vazio, já por extenso ou código fora da tabela) é mantido como
# veio, para não esconder dado inesperado.
codigo_para_nomenclatura <- function(valores, tabela) {
    
    valores_chr <- as.character(valores)
    
    codigos <- str_match(
        valores_chr,
        "^\\s*0*(\\d+)(?:[.,]0+)?\\s*$"
    )[, 2]
    
    nomes <- unname(tabela[codigos])
    
    ifelse(is.na(nomes), valores_chr, nomes)
    
}

# Normaliza o nome de uma coluna para comparação (sem acento, minúsculo,
# sem espaços nas pontas) — tolera pequenas variações de grafia/acentuação
# no cabeçalho do CSV.
normalizar_nome_coluna <- function(x) {
    str_to_lower(str_trim(stringi::stri_trans_general(x, "Latin-ASCII")))
}

# Localiza a coluna "Cargo" (índice) no data.frame. Prioriza o nome
# exato "Cargo" (sem acento/maiúsculas) e, se não houver, aceita uma
# coluna que comece com a palavra "Cargo" (ex.: "Cargo do(a)
# Servidor(a)") — tolera variações de cabeçalho entre arquivos.
coluna_cargo <- function(df) {
    
    if (is.null(df) || ncol(df) == 0) {
        return(integer(0))
    }
    
    nomes_norm <- normalizar_nome_coluna(names(df))
    
    idx <- which(nomes_norm == "cargo")
    
    if (length(idx) == 0) {
        idx <- which(str_detect(nomes_norm, "^cargo\\b"))
    }
    
    if (length(idx) == 0) integer(0) else idx[1]
    
}

# Rótulo usado para Cargo vazio — no filtro e no gráfico por Cargo.
ROTULO_CARGO_VAZIO <- "(Não informado)"

# Devolve, linha a linha, o Cargo já por extenso (nomenclatura das
# tabelas auxiliares), com ROTULO_CARGO_VAZIO para os vazios. NULL se o
# data.frame não tiver coluna Cargo. Usado pelo filtro "Cargo" e pelo
# gráfico "Registros por Cargo".
valores_cargo_exibicao <- function(df, con = NULL) {
    
    idx <- coluna_cargo(df)
    
    if (length(idx) == 0 || nrow(df) == 0) {
        return(NULL)
    }
    
    valores <- df[[idx]]
    
    if (!is.null(con)) {
        valores <- codigo_para_nomenclatura(
            valores,
            ler_tabela_auxiliar(con, TAB_CARGO)
        )
    }
    
    valores <- str_trim(as.character(valores))
    
    ifelse(is.na(valores) | valores == "", ROTULO_CARGO_VAZIO, valores)
    
}

# Aplica a conversão código -> nomenclatura nas colunas "Situação
# Profissional Atual" e "Cargo" do data.frame (se existirem), lendo as
# tabelas auxiliares do SQLite (`con`). Só para exibição na tabela/CSV.
# Sem `con` (ou sem as tabelas), os códigos são mantidos como vieram.
traduzir_codigos_tabela <- function(df, con = NULL) {
    
    if (is.null(df) || nrow(df) == 0 || is.null(con)) {
        return(df)
    }
    
    situacoes <- ler_tabela_auxiliar(con, TAB_SITUACAO_PROFISSIONAL)
    cargos <- ler_tabela_auxiliar(con, TAB_CARGO)
    
    nomes_norm <- normalizar_nome_coluna(names(df))
    
    col_situacao <- which(str_starts(nomes_norm, "situacao profissional"))
    col_cargo <- coluna_cargo(df)
    
    for (i in col_situacao) {
        df[[i]] <- codigo_para_nomenclatura(df[[i]], situacoes)
    }
    
    for (i in col_cargo) {
        df[[i]] <- codigo_para_nomenclatura(df[[i]], cargos)
    }
    
    df
    
}

# =====================================================
# CONSOLIDA ALERTA(S)/CONFLITO(S) DETECTADO(S) — SÓ PARA A TABELA
# -----------------------------------------------------
# O arquivo original traz uma coluna "Alerta: <tipo>" ou
# "Conflito: <tipo>" para cada tipo possível (a maioria vazia na
# maior parte das linhas) — o que deixa a tabela com dezenas de
# colunas. Esta função mantém as colunas fixas até "Órgão de lotação
# do(a) Servidor(a) ou Auxiliar" e substitui:
#   - todas as colunas "Alerta: ..." por uma única "Alerta(s) Detectado";
#   - todas as colunas "Conflito: ..." por uma única "Conflito(s)
#     Detectado", logo em seguida.
# Para cada uma que tiver conteúdo naquela linha, junta o texto depois
# dos dois-pontos do nome da coluna com o conteúdo da célula (ex.:
# "Data Nascimento: Data de nascimento não corresponde..."); se a
# linha tiver mais de um alerta (ou mais de um conflito), cada um vira
# um trecho separado por " | " dentro da mesma célula.
#
# Usada só para o que é exibido na aba Tabela (e no CSV gerado a
# partir dela) — dados(), dados_filtrados() e o gráfico continuam
# trabalhando com as colunas originais, sem nenhuma alteração de
# regra.
# =====================================================

# Junta, linha a linha, o conteúdo das colunas informadas em um único
# texto por linha ("<sufixo depois de :>: <valor>", vários trechos
# separados por " | " quando mais de uma coluna tiver conteúdo na
# mesma linha). Usada por consolidar_alertas_tabela() para Alerta e
# Conflito separadamente.
consolidar_colunas_por_linha <- function(df, colunas) {
    
    if (length(colunas) == 0) {
        return(rep(NA_character_, nrow(df)))
    }
    
    matriz <- as.matrix(df[colunas])
    
    apply(matriz, 1, function(linha) {
        
        preenchidos <- !is.na(linha) & str_trim(linha) != ""
        
        if (!any(preenchidos)) {
            return(NA_character_)
        }
        
        sufixos <- str_trim(str_remove(colunas[preenchidos], "^[^:]*:"))
        valores <- str_trim(linha[preenchidos])
        
        paste0(sufixos, ": ", valores, collapse = " | ")
        
    })
    
}

consolidar_alertas_tabela <- function(df) {
    
    if (is.null(df) || nrow(df) == 0) {
        return(df)
    }
    
    coluna_orgao <- "Órgão de lotação do(a) Servidor(a) ou Auxiliar"
    idx_orgao <- which(names(df) == coluna_orgao)
    
    if (length(idx_orgao) == 0) {
        # Layout inesperado (coluna não encontrada) — não quebra a tela,
        # só não consolida nada e mostra os dados como vieram.
        return(df)
    }
    
    colunas_fixas <- names(df)[seq_len(idx_orgao[1])]
    
    colunas_alerta <- colunas_por_prefixo(df, "Alerta")
    colunas_conflito <- colunas_por_prefixo(df, "Conflito")
    
    df_exibicao <- df[colunas_fixas]
    df_exibicao[["Alerta(s) Detectado"]] <- consolidar_colunas_por_linha(df, colunas_alerta)
    df_exibicao[["Conflito(s) Detectado"]] <- consolidar_colunas_por_linha(df, colunas_conflito)
    
    df_exibicao
    
}


# =====================================================
# FUNÇÕES GENÉRICAS DE LEITURA (.xlsx e .csv)
# -----------------------------------------------------
# Usadas por este módulo e por mod_alertas_serventias.R.
# =====================================================

# Extensões aceitas. A ordem define a PRIORIDADE quando existem os dois
# formatos do mesmo arquivo na pasta (.xlsx antes de .csv).
EXTENSOES_PLANILHA <- c("xlsx", "csv")

# Rótulo para valor vazio em filtros e gráficos.
ROTULO_VALOR_VAZIO <- "(Não informado)"

extensao_arquivo <- function(nome) {
    tolower(tools::file_ext(nome))
}

limpar_cabecalho <- function(nomes) {
    str_trim(str_remove(nomes, "^\uFEFF"))
}

# Converte uma coluna lida com col_types = "list" em texto, célula a
# célula, preservando o que aparece na planilha:
#   - número -> sem notação científica e sem ".0" (8335, 49080901);
#   - data   -> dd/mm/aaaa (ou dd/mm/aaaa hh:mm, se tiver horário);
#   - texto  -> como está (códigos como "0001" mantêm os zeros).
celulas_para_texto <- function(celulas) {
    
    vapply(
        celulas,
        function(v) {
            
            if (is.null(v) || length(v) == 0 || is.na(v[1])) {
                return(NA_character_)
            }
            
            v <- v[1]
            
            if (inherits(v, c("POSIXt", "Date"))) {
                tem_hora <- inherits(v, "POSIXt") && format(v, "%H:%M:%S") != "00:00:00"
                return(format(v, if (tem_hora) "%d/%m/%Y %H:%M" else "%d/%m/%Y"))
            }
            
            if (is.numeric(v)) {
                return(format(v, scientific = FALSE, trim = TRUE, digits = 15))
            }
            
            as.character(v)
            
        },
        character(1),
        USE.NAMES = FALSE
    )
    
}

# Lê uma planilha .xlsx com TODAS as colunas como texto, na ordem da
# planilha. Usa a aba `aba_preferida` se existir (sem diferenciar
# maiúsculas); senão, a primeira aba.
#
# Desempenho: ler célula a célula (col_types = "list") é lento em
# planilhas grandes (o Quadro de Pessoal tem ~17 mil linhas). Por isso a
# leitura é feita como texto ("text", rápido) e só as colunas que têm
# células de DATA de verdade — detectadas numa amostra das primeiras
# 1.000 linhas — são lidas célula a célula, para não virarem o número
# serial do Excel (ex.: 44385 em vez de 08/07/2021).
#
# Erros viram aviso e o arquivo é ignorado (retorna NULL), como em
# ler_arquivo_alertas().
ler_planilha_xlsx <- function(caminho, aba_preferida = NULL, nome_exibicao = basename(caminho),
                              n_max = Inf) {
    
    tryCatch({
        
        abas <- readxl::excel_sheets(caminho)
        
        aba <- if (!is.null(aba_preferida) && tolower(aba_preferida) %in% tolower(abas)) {
            abas[tolower(abas) == tolower(aba_preferida)][1]
        } else {
            abas[1]
        }
        
        amostra <- suppressMessages(
            readxl::read_excel(
                caminho, sheet = aba, col_types = "list",
                n_max = min(1000, n_max), .name_repair = "unique"
            )
        )
        
        tem_data <- vapply(
            amostra,
            function(col) any(vapply(col, function(v) inherits(v, c("POSIXt", "Date")), logical(1))),
            logical(1)
        )
        
        brutos <- suppressMessages(
            readxl::read_excel(
                caminho, sheet = aba,
                col_types = ifelse(tem_data, "list", "text"),
                n_max = n_max,
                .name_repair = "unique"
            )
        )
        
        for (i in which(tem_data)) {
            brutos[[i]] <- celulas_para_texto(brutos[[i]])
        }
        
        tibble::as_tibble(brutos, .name_repair = "minimal")
        
    }, error = function(e) {
        
        msg <- paste0(
            "Não foi possível ler a planilha ", nome_exibicao, ": ",
            conditionMessage(e), ". Esse arquivo foi ignorado."
        )
        
        warning(msg)
        showNotification(msg, type = "warning", duration = 15)
        
        NULL
        
    })
    
}

# Lê .xlsx (ler_planilha_xlsx) ou .csv (ler_arquivo_alertas: ";" com
# fallback para detecção automática). `extensao` é informada no upload,
# em que o nome temporário do arquivo pode não refletir o original.
# `n_max` limita as linhas lidas da planilha — no upload, para
# identificar o tipo do arquivo, bastam o cabeçalho e poucas linhas.
# Extensão não suportada -> NULL.
ler_arquivo_dados <- function(caminho, extensao = extensao_arquivo(caminho),
                              nome_exibicao = basename(caminho), aba_preferida = NULL,
                              n_max = Inf) {
    
    if (identical(extensao, "xlsx")) {
        ler_planilha_xlsx(caminho, aba_preferida, nome_exibicao, n_max = n_max)
    } else if (identical(extensao, "csv")) {
        suppressMessages(ler_arquivo_alertas(caminho, n_max = n_max))
    } else {
        NULL
    }
    
}

# Ajustes comuns depois da leitura de uma BASE DE DADOS:
#   - remove a coluna sem nome gerada pelo ";" no fim de cada linha dos
#     CSVs do MPM (ou uma coluna sem cabeçalho na planilha) quando vazia;
#   - tira o BOM e espaços nas pontas dos cabeçalhos e dos valores;
#   - remove linhas duplicadas.
limpar_dados_lidos <- function(df) {
    
    if (is.null(df) || nrow(df) == 0) {
        return(data.frame())
    }
    
    names(df) <- limpar_cabecalho(names(df))
    
    sem_nome <- str_detect(names(df), "^\\.\\.\\.\\d+$") | names(df) == ""
    
    vazias <- vapply(
        df,
        function(v) all(is.na(v) | str_trim(v) == ""),
        logical(1)
    )
    
    df <- df[, !(sem_nome & vazias), drop = FALSE]
    
    df %>%
        mutate(across(everything(), ~ str_trim(as.character(.x)))) %>%
        distinct()
    
}

# =====================================================
# FUNÇÕES GENÉRICAS DE FILTRO / EXIBIÇÃO
# =====================================================

# Localiza uma coluna pelo nome normalizado (sem acento, minúsculo)
# contra uma expressão regular. Devolve o NOME da coluna, ou NULL.
coluna_por_regex <- function(df, regex) {
    
    if (is.null(df) || ncol(df) == 0) {
        return(NULL)
    }
    
    idx <- which(str_detect(normalizar_nome_coluna(names(df)), regex))
    
    if (length(idx) == 0) NULL else names(df)[idx[1]]
    
}

# Valores de uma coluna linha a linha, com ROTULO_VALOR_VAZIO para os
# vazios. NULL se a coluna não existir.
valores_coluna_regex <- function(df, regex) {
    
    col <- coluna_por_regex(df, regex)
    
    if (is.null(col) || nrow(df) == 0) {
        return(NULL)
    }
    
    valores <- str_trim(as.character(df[[col]]))
    
    ifelse(is.na(valores) | valores == "", ROTULO_VALOR_VAZIO, valores)
    
}

# Opções de um combo: ordem "natural" (0001 < 0002 < 0010; 1 < 2 < 10),
# com "(Não informado)" por último.
opcoes_combo <- function(valores) {
    
    valores <- unique(valores)
    
    c(
        str_sort(setdiff(valores, ROTULO_VALOR_VAZIO), numeric = TRUE),
        intersect(ROTULO_VALOR_VAZIO, valores)
    )
    
}

# Mantém a seleção atual se ela ainda existir nas opções; senão, volta
# para `padrao` (se existir) ou "Todos".
selecao_valida <- function(atual, opcoes, padrao = "Todos") {
    
    if (!is.null(atual) && atual %in% c("Todos", opcoes)) {
        atual
    } else if (padrao %in% c("Todos", opcoes)) {
        padrao
    } else {
        "Todos"
    }
    
}

# Normaliza texto para busca: minúsculo e sem acento ("Aracajú" acha
# "ARACAJU").
normalizar_texto_busca <- function(x) {
    str_to_lower(stringi::stri_trans_general(x, "Latin-ASCII"))
}

# Uma string por linha com o conteúdo de TODAS as colunas, já
# normalizada — calculada uma vez por carga do arquivo e usada pelo
# filtro Detalhe. O separador evita que o fim de uma coluna "emende" com
# o início da seguinte e gere um acerto falso.
texto_busca_linhas <- function(df) {
    
    if (is.null(df) || nrow(df) == 0) {
        return(character(0))
    }
    
    # Normaliza cada coluna pelos valores DISTINTOS (muitos se repetem —
    # cargos, situações, datas) e só depois junta: bem mais rápido do que
    # normalizar o texto inteiro de cada linha em bases grandes.
    colunas <- lapply(df, function(x) {
        x <- as.character(x)
        x[is.na(x)] <- ""
        distintos <- unique(x)
        normalizar_texto_busca(distintos)[match(x, distintos)]
    })
    
    do.call(paste, c(colunas, sep = " \u00a6 "))
    
}

# Conteúdo da aba "Gerar arquivo CSV".
ui_csv_dados <- function(ns, id_download, tem_dados) {
    
    div(
        class = "mt-4",
        style = "max-width: 420px;",
        
        p(
            class = "text-muted",
            "Gera um arquivo .csv com os dados exibidos na aba \"Tabela\" (respeitando os filtros aplicados e a busca da tabela)."
        ),
        
        if (tem_dados) {
            
            downloadButton(
                ns(id_download),
                "Gerar arquivo CSV",
                icon = icon("download"),
                class = "btn btn-primary btn-acao"
            )
            
        } else {
            
            tagList(
                tags$button(
                    type = "button",
                    class = "btn btn-primary btn-acao",
                    disabled = "disabled",
                    icon("download", class = "me-2"),
                    "Gerar arquivo CSV"
                ),
                div(
                    class = "text-muted mt-2",
                    style = "font-size: .82rem;",
                    "Não há dados na tabela para exportar."
                )
            )
            
        }
    )
    
}

# Gráfico de barras horizontais (ggiraph) com tooltip — usado nos
# gráficos das bases de dados. `resumo` precisa ter as colunas Grupo,
# Quantidade e dica (HTML do tooltip).
grafico_barras_girafe <- function(resumo, titulo, subtitulo = NULL, largura_rotulo = 45) {
    
    resumo <- resumo %>%
        arrange(Quantidade, desc(Grupo)) %>%   # com coord_flip, a maior fica no topo
        mutate(
            rotulo = str_wrap(Grupo, largura_rotulo),
            rotulo = factor(rotulo, levels = unique(rotulo))
        )
    
    p <- ggplot(resumo, aes(x = rotulo, y = Quantidade)) +
        geom_col_interactive(aes(tooltip = dica, data_id = Grupo), fill = "#2C7FB8") +
        geom_text(aes(label = format(Quantidade, big.mark = ".", decimal.mark = ",")), hjust = -0.2, size = 3.2) +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(title = titulo, subtitle = subtitulo, x = "", y = "Quantidade") +
        theme_minimal()
    
    linhas_rotulo <- sum(str_count(levels(resumo$rotulo), "\n") + 1)
    
    girafe(
        ggobj = p,
        width_svg = 9,
        height_svg = max(3, 0.24 * max(nrow(resumo), linhas_rotulo) + 1.6),
        options = list(
            opts_hover(css = "fill:#0d6efd;cursor:pointer;"),
            opts_tooltip(css = "background:#fff;color:#212529;padding:8px 10px;border:1px solid #dee2e6;border-radius:6px;font-size:12px;max-width:420px;"),
            opts_sizing(rescale = TRUE, width = 1)
        )
    )
    
}

# =====================================================
# BASE DE DADOS — QUADRO DE PESSOAL E AUXILIAR
# -----------------------------------------------------
# Como a base é reconhecida na pasta "pessoal" (nesta ordem):
#
#   1) PELO NOME, comparado de forma tolerante (normalizar_nome_arquivo):
#      sem diferenciar maiúsculas, acentos, espaços, hífens ou "_".
#      Assim "quadro_pessoal_auxiliar.xlsx", "Quadro_pessoal_e_auxiliar.xlsx"
#      e "Quadro pessoal e auxiliar.xlsx" são todos reconhecidos.
#
#   2) PELO CONTEÚDO, se nenhum arquivo tiver um desses nomes: o arquivo
#      .xlsx/.csv cujo cabeçalho tem CPF + Situação profissional/Status e
#      nenhuma coluna de Alerta/Conflito (identificar_tipo_arquivo_pessoal,
#      lendo só as primeiras linhas). Havendo mais de um, vale o mais
#      recente.
#
# Todo o resto (.xlsx/.csv) é tratado como arquivo de alertas. Arquivos
# enviados pelo "Gerenciar Arquivos" são sempre gravados como
# quadro_pessoal_auxiliar.<extensão original>.
# =====================================================

RADICAIS_BASE_PESSOAL <- c("quadro_pessoal_auxiliar", "quadro_pessoal_e_auxiliar")
ARQUIVO_BASE_PESSOAL <- "quadro_pessoal_auxiliar.xlsx"

# "Quadro pessoal e auxiliar.xlsx" -> "quadro_pessoal_e_auxiliar"
# (sem extensão, minúsculo, sem acento, separadores viram "_").
normalizar_nome_arquivo <- function(arquivos) {
    radical <- tools::file_path_sans_ext(basename(arquivos))
    radical <- tolower(stringi::stri_trans_general(radical, "Latin-ASCII"))
    radical <- gsub("[^a-z0-9]+", "_", radical)
    gsub("^_+|_+$", "", radical)
}

# O nome do arquivo corresponde ao da base? (critério 1, só pelo nome)
eh_arquivo_base_pessoal <- function(arquivos) {
    extensao_arquivo(arquivos) %in% EXTENSOES_PLANILHA &
        normalizar_nome_arquivo(arquivos) %in% RADICAIS_BASE_PESSOAL
}

# Separa os arquivos .xlsx/.csv da pasta em base (ordem de prioridade) e
# alertas (o resto). Ver os critérios no início desta seção.
arquivos_pessoal <- function(pasta) {
    
    vazio <- list(base = character(0), alertas = character(0))
    
    if (is.null(pasta) || !dir.exists(pasta)) {
        return(vazio)
    }
    
    arquivos <- list.files(pasta, full.names = TRUE)
    arquivos <- arquivos[extensao_arquivo(arquivos) %in% EXTENSOES_PLANILHA]
    
    if (length(arquivos) == 0) {
        return(vazio)
    }
    
    por_nome <- eh_arquivo_base_pessoal(arquivos)
    
    if (any(por_nome)) {
        
        base <- arquivos[por_nome]
        base <- base[order(
            match(normalizar_nome_arquivo(base), RADICAIS_BASE_PESSOAL),
            match(extensao_arquivo(base), EXTENSOES_PLANILHA)
        )]
        
    } else {
        
        tipos <- vapply(
            arquivos,
            function(f) identificar_tipo_arquivo_pessoal(
                suppressWarnings(ler_arquivo_dados(f, n_max = 5))
            ),
            character(1),
            USE.NAMES = FALSE
        )
        
        base <- arquivos[!is.na(tipos) & tipos == "base"]
        base <- base[order(file.info(base)$mtime, decreasing = TRUE)]
        
    }
    
    list(base = base, alertas = setdiff(arquivos, base))
    
}

# Arquivos da base existentes na pasta, na ordem de prioridade.
arquivos_base_pessoal <- function(pasta) {
    arquivos_pessoal(pasta)$base
}

arquivo_base_pessoal <- function(pasta) {
    achados <- arquivos_base_pessoal(pasta)
    if (length(achados) > 0) achados[1] else NULL
}

# Arquivos de ALERTAS da pasta: .csv/.xlsx que não sejam a base.
listar_arquivos_alertas_pessoal <- function(pasta) {
    arquivos_pessoal(pasta)$alertas
}

carregar_base_pessoal <- function(arquivo) {
    
    if (is.null(arquivo) || is.na(arquivo) || !file.exists(arquivo)) {
        return(data.frame())
    }
    
    limpar_dados_lidos(
        ler_arquivo_dados(arquivo, aba_preferida = "Quadro pessoal e auxiliar")
    )
    
}

REGEX_COL_CPF <- "^cpf$"
REGEX_COL_NOME <- "^nome$"
REGEX_COL_STATUS_PESSOAL <- "^status$"
REGEX_COL_SITUACAO <- "^situacao profissional"
REGEX_COL_CARGO <- "^cargo$"
REGEX_COL_ORGAO <- "^orgao de lotacao"
REGEX_COL_NATURALIDADE <- "^naturalidade$"
REGEX_COL_SEXO <- "^sexo$"

# Opções de agrupamento do gráfico da base (rótulo -> regex da coluna).
AGRUPAMENTOS_BASE_PESSOAL <- c(
    "Cargo"                        = REGEX_COL_CARGO,
    "Situação profissional atual"  = REGEX_COL_SITUACAO,
    "Órgão de lotação"             = REGEX_COL_ORGAO,
    "Naturalidade"                 = REGEX_COL_NATURALIDADE,
    "Sexo"                         = REGEX_COL_SEXO,
    "Status"                       = REGEX_COL_STATUS_PESSOAL
)

# Identifica o tipo de um arquivo enviado pelas colunas:
#   - tem CPF e colunas "Alerta..."/"Conflito..."          -> "alertas"
#   - tem CPF e Situação profissional (ou Status), sem
#     colunas de alerta                                     -> "base"
#   - qualquer outra coisa                                  -> NA
# Evita, por exemplo, gravar o arquivo de Serventias nesta pasta.
identificar_tipo_arquivo_pessoal <- function(df) {
    
    if (is.null(df) || ncol(df) == 0) {
        return(NA_character_)
    }
    
    names(df) <- limpar_cabecalho(names(df))
    
    if (is.null(coluna_por_regex(df, REGEX_COL_CPF))) {
        return(NA_character_)
    }
    
    tem_alertas <- any(str_starts(names(df), "Alerta") | str_starts(names(df), "Conflito"))
    
    if (tem_alertas) {
        "alertas"
    } else if (!is.null(coluna_por_regex(df, REGEX_COL_SITUACAO)) ||
               !is.null(coluna_por_regex(df, REGEX_COL_STATUS_PESSOAL))) {
        "base"
    } else {
        NA_character_
    }
    
}

# =====================================================
# UI
# =====================================================

mod_alertas_ui <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        
        # ===================================================
        # ESTILO ENTERPRISE (escopo deste módulo)
        # ===================================================
        
        tags$head(
            tags$style(HTML(sprintf("

        #%1$s .al-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 1rem;
          margin-bottom: 1.25rem;
        }

        #%1$s .al-titulo {
          display: flex;
          align-items: center;
          gap: .65rem;
        }

        #%1$s .al-titulo-icone {
          width: 44px;
          height: 44px;
          border-radius: 50%%;
          display: flex;
          align-items: center;
          justify-content: center;
          background: linear-gradient(135deg, #003366, #0d6efd);
          color: #fff;
          font-size: 1.1rem;
          flex-shrink: 0;
          box-shadow: 0 .3rem .8rem rgba(13,110,253,.2);
        }

        #%1$s .al-titulo h4 {
          margin: 0;
          font-weight: 700;
          letter-spacing: -.01em;
        }

        #%1$s .al-acoes {
          display: flex;
          gap: .6rem;
          flex-wrap: wrap;
        }

        #%1$s .btn-acao {
          font-weight: 600;
          border-radius: .6rem;
          padding: .55rem 1.1rem;
        }

        /* Abas principais (Base de Dados / Alertas) */
        #%1$s .srv-secoes > .nav-tabs {
          margin-bottom: 1.25rem;
        }

        #%1$s .srv-secoes > .nav-tabs .nav-link {
          font-size: 1rem;
          padding: .6rem 1.1rem;
        }

        #%1$s .al-card {
          background: #fff;
          border: 1px solid rgba(0,0,0,.06);
          border-radius: .9rem;
          box-shadow: 0 .2rem .6rem rgba(15,23,42,.05);
          padding: 1.25rem 1.25rem .5rem 1.25rem;
          margin-bottom: 1.5rem;
        }

        #%1$s .al-card-titulo {
          font-weight: 600;
          font-size: .8rem;
          text-transform: uppercase;
          letter-spacing: .04em;
          color: #6c757d;
          margin-bottom: .9rem;
        }

        #%1$s .al-conteudo {
          background: #fff;
          border: 1px solid rgba(0,0,0,.06);
          border-radius: .9rem;
          box-shadow: 0 .2rem .6rem rgba(15,23,42,.05);
          padding: 1.25rem;
        }

        #%1$s .nav-tabs .nav-link.active {
          font-weight: 600;
          color: #0d6efd;
        }

        .al-modal-secao-titulo {
          font-weight: 600;
          font-size: 1rem;
          display: flex;
          align-items: center;
          gap: .5rem;
          margin-bottom: .35rem;
        }

        .al-modal-secao-desc {
          font-size: .82rem;
          color: #6c757d;
          margin-bottom: .85rem;
        }

        .srv-arquivo-linha {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: .75rem;
          padding: .6rem .75rem;
          border: 1px solid #e9ecef;
          border-radius: .6rem;
          margin-bottom: .5rem;
        }

        .srv-arquivo-nome {
          font-weight: 600;
          font-size: .9rem;
        }

        .srv-arquivo-info {
          font-size: .78rem;
          color: #6c757d;
        }

        /* Garante que a tabela ocupe 100%% da largura disponível. */
        #%1$s .dataTables_wrapper,
        #%1$s table.dataTable {
          width: 100%% !important;
        }

      ", id)))
        ),
        
        div(
            
            id = id,
            
            # =================================================
            # CABEÇALHO - TÍTULO + AÇÕES (valem para as duas abas)
            # =================================================
            
            div(
                class = "al-header",
                
                div(
                    class = "al-titulo",
                    div(class = "al-titulo-icone", icon("users")),
                    tags$h4("Quadro de Pessoal e Auxiliar")
                ),
                
                div(
                    class = "al-acoes",
                    
                    actionButton(
                        ns("atualizar"),
                        tagList(icon("rotate", class = "me-2"), "Atualizar Dados"),
                        class = "btn btn-outline-primary btn-acao"
                    ),
                    
                    actionButton(
                        ns("gerenciar_arquivos"),
                        tagList(icon("folder-open", class = "me-2"), "Gerenciar Arquivos"),
                        class = "btn btn-primary btn-acao"
                    )
                )
                
            ),
            
            div(
                class = "srv-secoes",
                
                tabsetPanel(
                    id = ns("secao"),
                    
                    # =============================================
                    # ABA 1 — BASE DE DADOS
                    # =============================================
                    
                    tabPanel(
                        title = tagList(icon("database", class = "me-1"), "Base de Dados - Quadro Pessoal e Auxiliar"),
                        value = "base",
                        
                        div(
                            class = "al-card",
                            
                            div(class = "al-card-titulo", "Filtros"),
                            
                            fluidRow(
                                column(
                                    2,
                                    # Começa em "Ativo" (padrão); as demais
                                    # opções chegam quando o arquivo é lido.
                                    selectInput(
                                        ns("base_status"), "Status",
                                        choices = c("Todos", "Ativo"),
                                        selected = "Ativo",
                                        width = "100%"
                                    )
                                ),
                                column(3, textInput(ns("base_cpf"), "CPF", width = "100%")),
                                column(7, textInput(ns("base_nome"), "Nome", width = "100%"))
                            ),
                            
                            fluidRow(
                                column(
                                    6,
                                    selectInput(ns("base_cargo"), "Cargo", choices = c("Todos"), width = "100%")
                                ),
                                column(
                                    6,
                                    selectInput(
                                        ns("base_situacao"), "Situação profissional atual",
                                        choices = c("Todos"), width = "100%"
                                    )
                                )
                            ),
                            
                            fluidRow(
                                column(
                                    12,
                                    textInput(
                                        ns("base_detalhe"), "Detalhe",
                                        placeholder = "Procura o texto digitado em todas as colunas da tabela",
                                        width = "100%"
                                    )
                                )
                            ),
                            
                            fluidRow(
                                column(
                                    12,
                                    actionButton(
                                        ns("base_limpar_filtros"),
                                        tagList(icon("filter-circle-xmark", class = "me-2"), "Limpar Filtros"),
                                        class = "btn btn-outline-secondary btn-sm"
                                    )
                                )
                            )
                        ),
                        
                        div(
                            class = "al-conteudo",
                            
                            tabsetPanel(
                                tabPanel(
                                    tagList(icon("table", class = "me-1"), "Tabela"),
                                    DTOutput(ns("base_tabela"))
                                ),
                                tabPanel(
                                    tagList(icon("chart-column", class = "me-1"), "Gráfico"),
                                    
                                    fluidRow(
                                        class = "mt-3",
                                        column(
                                            4,
                                            selectInput(
                                                ns("base_grafico_agrupar"), "Agrupar por",
                                                choices = names(AGRUPAMENTOS_BASE_PESSOAL),
                                                selected = "Cargo",
                                                width = "100%"
                                            )
                                        ),
                                        column(
                                            4,
                                            selectInput(
                                                ns("base_grafico_qtd"), "Exibir",
                                                choices = c(
                                                    "Os 20 maiores grupos" = "20",
                                                    "Os 50 maiores grupos" = "50",
                                                    "Todos os grupos" = "0"
                                                ),
                                                selected = "20",
                                                width = "100%"
                                            )
                                        )
                                    ),
                                    
                                    girafeOutput(ns("base_grafico"))
                                ),
                                tabPanel(
                                    tagList(icon("file-csv", class = "me-1"), "Gerar arquivo CSV"),
                                    uiOutput(ns("base_csv_ui"))
                                )
                            )
                        )
                    ),
                    
                    # =============================================
                    # ABA 2 — ALERTAS
                    # -------------------------------------------------
                    # "Alerta" e "Conflito" se excluem mutuamente (ver
                    # observeEvent(input$alerta)/observeEvent(input$conflito)
                    # no server) — escolher um volta o outro para "Todos".
                    # =============================================
                    
                    tabPanel(
                        title = tagList(icon("triangle-exclamation", class = "me-1"), "Alertas - Quadro Pessoal e Auxiliar"),
                        value = "alertas",
                        
                        div(
                            class = "al-card",
                            
                            div(class = "al-card-titulo", "Filtros"),
                            
                            fluidRow(
                                column(4, selectInput(ns("alerta"), "Alerta", choices = c("Todos"), width = "100%")),
                                column(4, selectInput(ns("conflito"), "Conflito", choices = c("Todos"), width = "100%")),
                                column(4, selectInput(ns("cargo"), "Cargo", choices = c("Todos"), width = "100%"))
                            ),
                            
                            fluidRow(
                                column(4, textInput(ns("cpf"), "CPF", width = "100%")),
                                column(8, textInput(ns("nome"), "Nome", width = "100%"))
                            ),
                            
                            fluidRow(
                                column(
                                    12,
                                    actionButton(
                                        ns("limpar_filtros"),
                                        tagList(icon("filter-circle-xmark", class = "me-2"), "Limpar Filtros"),
                                        class = "btn btn-outline-secondary btn-sm"
                                    )
                                )
                            )
                        ),
                        
                        div(
                            class = "al-conteudo",
                            
                            tabsetPanel(
                                tabPanel(
                                    tagList(icon("table", class = "me-1"), "Tabela"),
                                    DTOutput(ns("tabela"))
                                ),
                                tabPanel(
                                    tagList(icon("chart-column", class = "me-1"), "Gráfico"),
                                    
                                    # 1) Tipo de Alerta/Conflito (echarts4r) — altura
                                    #    acompanha a quantidade de barras.
                                    uiOutput(ns("grafico_ui")),
                                    
                                    tags$hr(class = "my-4"),
                                    
                                    # 2) Registros por Cargo (ggiraph)
                                    girafeOutput(ns("grafico_cargo"))
                                ),
                                tabPanel(
                                    tagList(icon("file-csv", class = "me-1"), "Gerar arquivo CSV"),
                                    uiOutput(ns("csv_ui"))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    
}

# =====================================================
# SERVER
# =====================================================

mod_alertas_server <- function(id, ativo = reactive(TRUE), empresa = reactive(NULL), con = NULL) {
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        # Garante as tabelas auxiliares (Situação Profissional / Cargo) no
        # SQLite do app. Idempotente. Um erro aqui não pode derrubar o
        # módulo: sem as tabelas, a exibição só mostra os códigos originais.
        if (!is.null(con)) {
            tryCatch(
                garantir_tabelas_auxiliares(con),
                error = function(e) {
                    warning("Não foi possível preparar as tabelas auxiliares: ", conditionMessage(e))
                }
            )
        }
        
        # Começam vazios: a empresa só é conhecida depois do login.
        dados_base <- reactiveVal(data.frame())   # quadro_pessoal_auxiliar.xlsx
        dados <- reactiveVal(data.frame())        # arquivos de alertas
        
        empresa_definida <- function() {
            e <- empresa()
            !is.null(e) && !is.na(e) && trimws(e) != ""
        }
        
        recarregar <- function() {
            pasta <- caminho_alertas(empresa())
            arquivos <- arquivos_pessoal(pasta)
            dados_base(carregar_base_pessoal(arquivos$base[1]))
            dados(carregar_alertas(pasta, arquivos$alertas))
        }
        
        # ----------------------------------------
        # CONTADOR DO CAMPO DE UPLOAD
        # -----------------------------------------------------
        # O fileInput() do modal usa um ID novo a cada abertura. Sem isso,
        # o Shiny não garante que a seleção anterior fique vazia ao reabrir
        # o modal, e "Enviar para Pasta" poderia reenviar arquivos antigos.
        # ----------------------------------------
        contador_upload <- reactiveVal(0)
        
        id_upload_atual <- function() {
            paste0("upload_arquivos_", contador_upload())
        }
        
        entrada_upload_atual <- function() {
            input[[id_upload_atual()]]
        }
        
        # ----------------------------------------
        # CARREGA DADOS DA EMPRESA ATUAL (login / troca de empresa)
        # ----------------------------------------
        
        observeEvent(empresa(), {
            req(empresa())
            recarregar()
        }, ignoreInit = FALSE)
        
        # =================================================
        # BASE — EXIBIÇÃO (códigos -> nomenclatura)
        # -------------------------------------------------
        # Mesmas colunas e ordem do arquivo; só "Situação profissional
        # atual" e "Cargo" passam de código para nomenclatura (tabelas
        # auxiliares do SQLite), como na aba de alertas. Filtros, busca
        # Detalhe, gráfico e CSV usam esta versão — assim, digitar
        # "Aposentado" no Detalhe encontra os registros.
        # =================================================
        
        base_exibicao <- reactive({
            traduzir_codigos_tabela(dados_base(), con)
        })
        
        # =================================================
        # COMBOS
        # =================================================
        
        # Opções de Status da base na última atualização: quando o arquivo
        # passa de "sem dados" para "com dados" (primeiro envio, troca de
        # empresa), o Status volta ao padrão "Ativo".
        status_base_anterior <- character(0)
        
        atualizar_combos_base <- function() {
            
            df <- base_exibicao()
            
            status <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_STATUS_PESSOAL))
            cargos <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_CARGO))
            situacoes <- opcoes_combo(valores_coluna_regex(df, REGEX_COL_SITUACAO))
            
            status_atual <- if (length(status_base_anterior) == 0) "Ativo" else input$base_status
            status_base_anterior <<- status
            
            updateSelectInput(
                session, "base_status",
                choices = c("Todos", status),
                selected = selecao_valida(status_atual, status, padrao = "Ativo")
            )
            
            updateSelectInput(
                session, "base_cargo",
                choices = c("Todos", cargos),
                selected = selecao_valida(input$base_cargo, cargos)
            )
            
            updateSelectInput(
                session, "base_situacao",
                choices = c("Todos", situacoes),
                selected = selecao_valida(input$base_situacao, situacoes)
            )
            
        }
        
        atualizar_combos <- function() {
            
            df <- dados()
            
            colunas_alerta <- colunas_com_dados(df, colunas_por_prefixo(df, "Alerta"))
            colunas_conflito <- colunas_com_dados(df, colunas_por_prefixo(df, "Conflito"))
            
            updateSelectInput(
                session, "alerta",
                choices = c("Todos", colunas_alerta),
                selected = selecao_valida(input$alerta, colunas_alerta)
            )
            
            updateSelectInput(
                session, "conflito",
                choices = c("Todos", colunas_conflito),
                selected = selecao_valida(input$conflito, colunas_conflito)
            )
            
            # Cargo: opções por extenso (nomenclatura), em ordem alfabética,
            # com "(Não informado)" por último quando existir.
            cargos <- unique(valores_cargo_exibicao(df, con))
            cargos <- c(
                sort(setdiff(cargos, ROTULO_CARGO_VAZIO)),
                intersect(ROTULO_CARGO_VAZIO, cargos)
            )
            
            updateSelectInput(
                session, "cargo",
                choices = c("Todos", cargos),
                selected = selecao_valida(input$cargo, cargos)
            )
            
        }
        
        atualizar_todos_combos <- function() {
            atualizar_combos_base()
            atualizar_combos()
        }
        
        observeEvent(list(dados_base(), dados(), ativo()), {
            req(ativo())
            atualizar_todos_combos()
        }, ignoreInit = FALSE)
        
        observeEvent(input$atualizar, {
            req(empresa())
            inicio <- Sys.time()
            
            recarregar()
            atualizar_todos_combos()
            
            showNotification(
                sprintf("Dados atualizados em %.2fs.", tempo_decorrido(inicio)),
                type = "message"
            )
        })
        
        # ----------------------------------------
        # LIMPAR FILTROS
        # ----------------------------------------
        
        # Base: Status volta ao padrão "Ativo" (não para "Todos").
        observeEvent(input$base_limpar_filtros, {
            status <- opcoes_combo(valores_coluna_regex(base_exibicao(), REGEX_COL_STATUS_PESSOAL))
            updateSelectInput(session, "base_status", selected = selecao_valida("Ativo", status))
            updateTextInput(session, "base_cpf", value = "")
            updateTextInput(session, "base_nome", value = "")
            updateSelectInput(session, "base_cargo", selected = "Todos")
            updateSelectInput(session, "base_situacao", selected = "Todos")
            updateTextInput(session, "base_detalhe", value = "")
        })
        
        observeEvent(input$limpar_filtros, {
            updateSelectInput(session, "alerta", selected = "Todos")
            updateSelectInput(session, "conflito", selected = "Todos")
            updateSelectInput(session, "cargo", selected = "Todos")
            updateTextInput(session, "cpf", value = "")
            updateTextInput(session, "nome", value = "")
        })
        
        # ----------------------------------------
        # EXCLUSÃO MÚTUA (Alerta x Conflito)
        # ----------------------------------------
        
        observeEvent(input$alerta, {
            req(input$alerta)
            if (input$alerta != "Todos" && !is.null(input$conflito) && input$conflito != "Todos") {
                updateSelectInput(session, "conflito", selected = "Todos")
            }
        }, ignoreInit = TRUE)
        
        observeEvent(input$conflito, {
            req(input$conflito)
            if (input$conflito != "Todos" && !is.null(input$alerta) && input$alerta != "Todos") {
                updateSelectInput(session, "alerta", selected = "Todos")
            }
        }, ignoreInit = TRUE)
        
        # =================================================
        # GERENCIAR ARQUIVOS (comum às duas abas)
        # -------------------------------------------------
        # Mostra a base e os arquivos de alertas da pasta, com um botão
        # Apagar para cada grupo, e envia vários arquivos de uma vez. O
        # tipo de cada arquivo enviado é identificado pelas colunas
        # (identificar_tipo_arquivo_pessoal):
        #   - base    -> gravada como quadro_pessoal_auxiliar.<ext>,
        #                substituindo a anterior;
        #   - alertas -> gravados com o nome original (podem ser vários);
        #                se já houver alertas na pasta, pergunta se mantém
        #                ou apaga os existentes (como antes).
        # =================================================
        
        observeEvent(input$gerenciar_arquivos, {
            
            if (!empresa_definida()) {
                showNotification(
                    "Nenhuma empresa definida para esta sessão. Faça login novamente (ou use \"Trocar empresa\").",
                    type = "error",
                    duration = 8
                )
                return(invisible(NULL))
            }
            
            pasta <- caminho_alertas(empresa())
            arquivos <- arquivos_pessoal(pasta)
            bases <- arquivos$base
            alertas <- arquivos$alertas
            
            contador_upload(contador_upload() + 1)
            
            showModal(modalDialog(
                title = div(
                    style = "position:relative; padding-right:28px;",
                    icon("folder-open", class = "me-2"),
                    sprintf("Gerenciar Arquivos do Quadro de Pessoal — %s", empresa()),
                    
                    # X próprio no canto superior direito — ver comentário
                    # equivalente em titulo_modal() (app.R).
                    tags$button(
                        type = "button",
                        class = "btn-close",
                        style = "position:absolute; top:2px; right:0;",
                        `aria-label` = "Fechar",
                        onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
                            ns("fechar_modal_alertas")
                        )
                    )
                ),
                size = "m",
                easyClose = TRUE,
                
                div(
                    class = "mb-4",
                    
                    div(
                        class = "al-modal-secao-titulo",
                        icon("folder", class = "text-secondary"),
                        "Arquivos na pasta"
                    ),
                    
                    # Base de dados
                    div(
                        class = "srv-arquivo-linha",
                        div(
                            div(
                                class = "srv-arquivo-nome",
                                icon(if (length(bases) > 0 && extensao_arquivo(bases[1]) == "xlsx") "file-excel" else "file-csv", class = "me-1"),
                                if (length(bases) > 0) basename(bases[1]) else "quadro_pessoal_auxiliar.xlsx / .csv"
                            ),
                            div(
                                class = "srv-arquivo-info",
                                "Base de Dados — ",
                                if (length(bases) > 0) {
                                    sprintf("atualizado em %s", format(file.info(bases[1])$mtime, "%d/%m/%Y %H:%M"))
                                } else {
                                    "ainda não enviado"
                                },
                                if (length(bases) > 1) {
                                    tags$div(
                                        class = "text-warning",
                                        sprintf("também na pasta (ignorado): %s", paste(basename(bases[-1]), collapse = ", "))
                                    )
                                }
                            )
                        ),
                        if (length(bases) > 0) {
                            actionButton(
                                ns("apagar_base"),
                                tagList(icon("trash", class = "me-1"), "Apagar"),
                                class = "btn btn-outline-danger btn-sm"
                            )
                        }
                    ),
                    
                    # Alertas
                    div(
                        class = "srv-arquivo-linha",
                        div(
                            div(
                                class = "srv-arquivo-nome",
                                icon("file-csv", class = "me-1"),
                                sprintf("%d arquivo(s) de alertas", length(alertas))
                            ),
                            div(
                                class = "srv-arquivo-info",
                                if (length(alertas) > 0) {
                                    paste(
                                        c(head(basename(alertas), 5), if (length(alertas) > 5) sprintf("... e mais %d", length(alertas) - 5)),
                                        collapse = ", "
                                    )
                                } else {
                                    "nenhum arquivo de alertas na pasta"
                                }
                            )
                        ),
                        if (length(alertas) > 0) {
                            actionButton(
                                ns("apagar_alertas"),
                                tagList(icon("trash", class = "me-1"), "Apagar"),
                                class = "btn btn-outline-danger btn-sm"
                            )
                        }
                    )
                ),
                
                tags$hr(),
                
                div(
                    
                    div(
                        class = "al-modal-secao-titulo",
                        icon("upload", class = "text-primary"),
                        "Enviar arquivos para a pasta"
                    ),
                    
                    div(
                        class = "al-modal-secao-desc",
                        "Selecione a base do Quadro de Pessoal e Auxiliar e/ou arquivos de ",
                        "alertas gerados pelo MPM, em ", tags$b(".xlsx"), " ou ", tags$b(".csv"),
                        " (limite de ", MAX_UPLOAD_MB, " MB no total por envio). O tipo é ",
                        "identificado pelas colunas: com colunas \"Alerta: ...\" o arquivo ",
                        "entra nos alertas (com o nome original); sem alertas, é a base, ",
                        "gravada como ", tags$code("quadro_pessoal_auxiliar"),
                        " com a extensão original, substituindo a anterior."
                    ),
                    
                    div(
                        class = "text-muted mb-2",
                        style = "font-size: .78rem;",
                        icon("triangle-exclamation", class = "me-1"),
                        "Esses arquivos ficam no disco local da aplicação e podem ",
                        "ser perdidos em um reinício no shinyapps.io."
                    ),
                    
                    fileInput(
                        ns(id_upload_atual()),
                        NULL,
                        multiple = TRUE,
                        accept = c(
                            ".csv", ".xlsx", "text/csv",
                            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                        ),
                        width = "100%",
                        buttonLabel = "Procurar...",
                        placeholder = "Nenhum arquivo selecionado"
                    ),
                    
                    actionButton(
                        ns("enviar_arquivos"),
                        tagList(icon("upload", class = "me-2"), "Enviar para Pasta"),
                        class = "btn btn-primary w-100"
                    )
                ),
                
                footer = actionButton(ns("fechar_modal_alertas"), "Fechar")
            ))
            
        })
        
        observeEvent(input$fechar_modal_alertas, {
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
        })
        
        # ----------------------------------------
        # APAGAR (base ou todos os arquivos de alertas)
        # ----------------------------------------
        
        tipo_para_apagar <- reactiveVal(NULL)
        
        arquivos_do_grupo <- function(tipo) {
            pasta <- caminho_alertas(empresa())
            if (identical(tipo, "base")) arquivos_base_pessoal(pasta) else listar_arquivos_alertas_pessoal(pasta)
        }
        
        rotulo_do_grupo <- function(tipo) {
            if (identical(tipo, "base")) "Base de Dados - Quadro Pessoal e Auxiliar" else "Alertas - Quadro Pessoal e Auxiliar"
        }
        
        pedir_confirmacao_apagar <- function(tipo) {
            req(empresa())
            
            existentes <- arquivos_do_grupo(tipo)
            
            if (length(existentes) == 0) {
                showNotification(sprintf("Não há arquivos de %s na pasta.", rotulo_do_grupo(tipo)), type = "warning")
                return(invisible(NULL))
            }
            
            tipo_para_apagar(tipo)
            
            # Fecha o modal "Gerenciar Arquivos" ANTES de abrir a
            # confirmação — trocar um modal por outro na hora pode deixar o
            # backdrop do Bootstrap "grudado" na tela.
            removeModal()
            
            showModal(modalDialog(
                title = "Confirmar exclusão",
                sprintf(
                    "Tem certeza que deseja apagar %d arquivo(s) de %s da empresa %s? Esta ação não pode ser desfeita.",
                    length(existentes), rotulo_do_grupo(tipo), empresa()
                ),
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(ns("confirmar_apagar"), "Apagar", class = "btn-danger")
                )
            ))
        }
        
        observeEvent(input$apagar_base, pedir_confirmacao_apagar("base"))
        observeEvent(input$apagar_alertas, pedir_confirmacao_apagar("alertas"))
        
        observeEvent(input$confirmar_apagar, {
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
            
            req(empresa(), tipo_para_apagar())
            
            tipo <- tipo_para_apagar()
            tipo_para_apagar(NULL)
            
            inicio <- Sys.time()
            existentes <- arquivos_do_grupo(tipo)
            
            if (length(existentes) == 0) {
                showNotification(sprintf("Não há arquivos de %s na pasta.", rotulo_do_grupo(tipo)), type = "warning")
                return()
            }
            
            removidos <- tryCatch(
                file.remove(existentes),
                error = function(e) {
                    showNotification(sprintf("Erro ao apagar arquivos: %s", conditionMessage(e)), type = "error")
                    NULL
                }
            )
            
            if (is.null(removidos)) {
                return()
            }
            
            if (all(removidos)) {
                showNotification(
                    sprintf("%d arquivo(s) apagado(s) em %.2fs.", length(existentes), tempo_decorrido(inicio)),
                    type = "message"
                )
            } else {
                showNotification(
                    sprintf(
                        "%d de %d arquivo(s) não puderam ser apagados (verifique se estão abertos em outro programa).",
                        sum(!removidos), length(removidos)
                    ),
                    type = "error"
                )
            }
            
            tryCatch({
                recarregar()
                atualizar_todos_combos()
            }, error = function(e) {
                showNotification(
                    sprintf("Arquivos apagados, mas houve erro ao recarregar: %s", conditionMessage(e)),
                    type = "error"
                )
            })
        })
        
        # ----------------------------------------
        # UPLOAD
        # ----------------------------------------
        
        # Arquivos já validados, aguardando confirmação.
        upload_pendente <- reactiveVal(NULL)
        
        # Copia os arquivos para a pasta "pessoal" da empresa. Fecha o modal
        # logo depois de copiar (rápido) — ANTES de recarregar os dados,
        # que pode demorar com a base grande.
        processar_upload <- function(pendente, apagar_alertas_existentes = FALSE) {
            req(pendente, empresa())
            
            pasta <- caminho_alertas(empresa())
            req(pasta)
            
            inicio <- Sys.time()
            
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
            
            if (apagar_alertas_existentes && any(pendente$tipo == "alertas")) {
                anteriores <- listar_arquivos_alertas_pessoal(pasta)
                if (length(anteriores) > 0) {
                    file.remove(anteriores)
                }
            }
            
            gravados <- character(0)
            
            for (i in seq_len(nrow(pendente))) {
                
                if (pendente$tipo[i] == "base") {
                    
                    # Remove a base anterior em QUALQUER formato — senão um
                    # .xlsx antigo continuaria com prioridade sobre um .csv
                    # novo.
                    anteriores <- arquivos_base_pessoal(pasta)
                    if (length(anteriores) > 0) {
                        file.remove(anteriores)
                    }
                    
                    destino <- paste0(RADICAIS_BASE_PESSOAL[1], ".", pendente$extensao[i])
                    
                } else {
                    
                    # Alertas mantêm o nome original — exceto se o nome for
                    # o reservado para a base.
                    destino <- pendente$name[i]
                    if (eh_arquivo_base_pessoal(destino)) {
                        destino <- paste0("alertas_", destino)
                    }
                    
                }
                
                if (isTRUE(file.copy(pendente$datapath[i], file.path(pasta, destino), overwrite = TRUE))) {
                    gravados <- c(gravados, destino)
                } else {
                    showNotification(
                        sprintf("O arquivo %s não pôde ser copiado para a pasta.", pendente$name[i]),
                        type = "error"
                    )
                }
            }
            
            if (length(gravados) > 0) {
                showNotification(
                    sprintf(
                        "%d arquivo(s) enviado(s) em %.2fs: %s. Atualizando as tabelas...",
                        length(gravados), tempo_decorrido(inicio), paste(gravados, collapse = ", ")
                    ),
                    type = "message",
                    duration = 8
                )
            }
            
            upload_pendente(NULL)
            
            tryCatch({
                recarregar()
                atualizar_todos_combos()
            }, error = function(e) {
                showNotification(
                    sprintf("Arquivos enviados, mas houve erro ao recarregar as tabelas: %s", conditionMessage(e)),
                    type = "error",
                    duration = 15
                )
            })
        }
        
        observeEvent(input$enviar_arquivos, {
            up <- entrada_upload_atual()
            req(up, empresa())
            
            extensoes <- extensao_arquivo(up$name)
            
            tipos <- vapply(
                seq_len(nrow(up)),
                function(i) {
                    if (!(extensoes[i] %in% EXTENSOES_PLANILHA)) {
                        return(NA_character_)
                    }
                    identificar_tipo_arquivo_pessoal(
                        ler_arquivo_dados(up$datapath[i], extensoes[i], up$name[i], n_max = 5)
                    )
                },
                character(1)
            )
            
            invalidos <- up$name[is.na(tipos)]
            
            if (length(invalidos) > 0) {
                showNotification(
                    sprintf(
                        "Não reconhecido(s) como base ou alertas do Quadro de Pessoal em .xlsx ou .csv (ignorado[s]): %s.",
                        paste(invalidos, collapse = ", ")
                    ),
                    type = "error",
                    duration = 12
                )
            }
            
            validos <- !is.na(tipos)
            
            if (!any(validos)) {
                return(invisible(NULL))
            }
            
            if (sum(tipos[validos] == "base") > 1) {
                showNotification(
                    "Foram selecionadas duas bases do Quadro de Pessoal. Envie apenas uma por vez.",
                    type = "error",
                    duration = 12
                )
                return(invisible(NULL))
            }
            
            pendente <- data.frame(
                name = up$name[validos],
                datapath = up$datapath[validos],
                tipo = tipos[validos],
                extensao = extensoes[validos],
                stringsAsFactors = FALSE
            )
            
            upload_pendente(pendente)
            
            pasta <- caminho_alertas(empresa())
            
            substitui_base <- any(pendente$tipo == "base") && length(arquivos_base_pessoal(pasta)) > 0
            alertas_existentes <- if (any(pendente$tipo == "alertas")) listar_arquivos_alertas_pessoal(pasta) else character(0)
            
            if (!substitui_base && length(alertas_existentes) == 0) {
                processar_upload(pendente)
                return(invisible(NULL))
            }
            
            # Fecha o modal "Gerenciar Arquivos" ANTES de abrir a
            # confirmação (mesma correção do Apagar). O valor do fileInput
            # continua acessível depois disso.
            removeModal()
            
            showModal(modalDialog(
                title = "Arquivos existentes na pasta",
                
                if (substitui_base) {
                    p(sprintf(
                        "A base atual (%s) será substituída pela base enviada.",
                        basename(arquivos_base_pessoal(pasta)[1])
                    ))
                },
                
                if (length(alertas_existentes) > 0) {
                    p(sprintf(
                        "A pasta da empresa %s já contém %d arquivo(s) de alertas. Deseja apagar os existentes antes de enviar os novos, ou manter os dois conjuntos?",
                        empresa(), length(alertas_existentes)
                    ))
                },
                
                footer = tagList(
                    modalButton("Cancelar"),
                    if (length(alertas_existentes) > 0) {
                        tagList(
                            actionButton(ns("enviar_manter"), "Manter Alertas Existentes"),
                            actionButton(ns("enviar_apagar"), "Apagar Alertas e Enviar", class = "btn-danger")
                        )
                    } else {
                        actionButton(ns("enviar_manter"), "Substituir", class = "btn-danger")
                    }
                )
            ))
        })
        
        observeEvent(input$enviar_manter, {
            processar_upload(upload_pendente(), apagar_alertas_existentes = FALSE)
        })
        
        observeEvent(input$enviar_apagar, {
            processar_upload(upload_pendente(), apagar_alertas_existentes = TRUE)
        })
        
        # =================================================
        # ABA 1 — BASE DE DADOS
        # =================================================
        
        MSG_SEM_BASE <- paste0(
            "Não existe a base do Quadro de Pessoal e Auxiliar (", ARQUIVO_BASE_PESSOAL,
            " ou .csv) para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviá-la."
        )
        
        MSG_BASE_SEM_RESULTADO <- "Nenhum registro encontrado com os filtros aplicados."
        
        # Texto de busca por linha (filtro Detalhe) — recalculado só quando
        # a base muda, não a cada tecla digitada.
        busca_base <- reactive({
            texto_busca_linhas(base_exibicao())
        })
        
        # Espera o usuário parar de digitar (400 ms) antes de filtrar.
        base_cpf_d <- debounce(reactive(input$base_cpf), 400)
        base_nome_d <- debounce(reactive(input$base_nome), 400)
        base_detalhe_d <- debounce(reactive(input$base_detalhe), 400)
        
        # Filtros combinados como máscaras sobre a base completa.
        dados_base_filtrados <- reactive({
            df <- base_exibicao()
            
            if (nrow(df) == 0) {
                return(df)
            }
            
            manter <- rep(TRUE, nrow(df))
            
            filtrar_combo <- function(valor, regex) {
                if (!is.null(valor) && valor != "Todos") {
                    v <- valores_coluna_regex(df, regex)
                    if (!is.null(v)) manter <<- manter & v == valor
                }
            }
            
            filtrar_texto <- function(valor, regex) {
                valor <- str_trim(valor %||% "")
                col <- coluna_por_regex(df, regex)
                if (valor != "" && !is.null(col)) {
                    manter <<- manter & str_detect(
                        normalizar_texto_busca(coalesce(df[[col]], "")),
                        fixed(normalizar_texto_busca(valor))
                    )
                }
            }
            
            filtrar_combo(input$base_status, REGEX_COL_STATUS_PESSOAL)
            filtrar_combo(input$base_cargo, REGEX_COL_CARGO)
            filtrar_combo(input$base_situacao, REGEX_COL_SITUACAO)
            filtrar_texto(base_cpf_d(), REGEX_COL_CPF)
            filtrar_texto(base_nome_d(), REGEX_COL_NOME)
            
            # Detalhe: o texto digitado em QUALQUER coluna (sem diferenciar
            # maiúsculas/minúsculas nem acentos).
            detalhe <- str_trim(base_detalhe_d() %||% "")
            if (detalhe != "") {
                manter <- manter & str_detect(busca_base(), fixed(normalizar_texto_busca(detalhe)))
            }
            
            df[manter, , drop = FALSE]
        })
        
        # ---- Tabela (mesmas colunas do arquivo) ----
        
        output$base_tabela <- renderDT({
            shiny::validate(need(nrow(dados_base()) > 0, MSG_SEM_BASE))
            
            df <- dados_base_filtrados()
            
            shiny::validate(need(nrow(df) > 0, MSG_BASE_SEM_RESULTADO))
            
            datatable(
                df,
                filter = "top",
                rownames = FALSE,
                width = "100%",
                options = list(
                    pageLength = 20,
                    scrollX = TRUE,
                    autoWidth = TRUE,
                    width = "100%"
                )
            )
        })
        
        # ---- Gráfico (agrupamento selecionável) ----
        
        resumo_base <- reactive({
            if (nrow(dados_base()) == 0) {
                return(list(resumo = NULL, mensagem = MSG_SEM_BASE))
            }
            
            df <- dados_base_filtrados()
            
            if (nrow(df) == 0) {
                return(list(resumo = NULL, mensagem = MSG_BASE_SEM_RESULTADO))
            }
            
            agrupar <- input$base_grafico_agrupar %||% "Cargo"
            regex <- AGRUPAMENTOS_BASE_PESSOAL[[agrupar]]
            grupos <- valores_coluna_regex(df, regex)
            
            if (is.null(grupos)) {
                return(list(
                    resumo = NULL,
                    mensagem = sprintf("Coluna \"%s\" não encontrada na base carregada.", agrupar)
                ))
            }
            
            col_cpf <- coluna_por_regex(df, REGEX_COL_CPF)
            cpfs <- if (is.null(col_cpf)) rep(NA_character_, nrow(df)) else df[[col_cpf]]
            
            resumo <- tibble(Grupo = grupos, CPF = cpfs) %>%
                group_by(Grupo) %>%
                summarise(
                    Quantidade = n(),
                    Pessoas = n_distinct(CPF[!is.na(CPF) & CPF != ""]),
                    .groups = "drop"
                ) %>%
                arrange(desc(Quantidade), Grupo)
            
            list(resumo = resumo, mensagem = NULL, agrupar = agrupar)
        })
        
        output$base_grafico <- renderGirafe({
            r <- resumo_base()
            
            shiny::validate(need(is.null(r$mensagem), r$mensagem))
            
            total <- nrow(r$resumo)
            limite <- suppressWarnings(as.integer(input$base_grafico_qtd))
            
            resumo <- if (!is.na(limite) && limite > 0) head(r$resumo, limite) else r$resumo
            
            resumo <- resumo %>%
                mutate(
                    dica = sprintf(
                        "<b>%s</b><br/>%s registro(s)<br/>%s pessoa(s) (CPF distintos)",
                        htmltools::htmlEscape(Grupo),
                        format(Quantidade, big.mark = ".", decimal.mark = ","),
                        format(Pessoas, big.mark = ".", decimal.mark = ",")
                    )
                )
            
            grafico_barras_girafe(
                resumo,
                titulo = sprintf("Quantidade de Registros por %s", r$agrupar),
                subtitulo = sprintf("Exibindo %d de %d grupo(s)", nrow(resumo), total)
            )
        })
        
        # ---- Gerar arquivo CSV ----
        
        output$base_csv_ui <- renderUI({
            ui_csv_dados(ns, "base_download_csv", nrow(dados_base_filtrados()) > 0)
        })
        
        output$base_download_csv <- downloadHandler(
            
            filename = function() {
                paste0("base-quadro-pessoal-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
            },
            
            content = function(file) {
                inicio <- Sys.time()
                
                df <- dados_base_filtrados()
                
                # Respeita também a busca global e os filtros de coluna do DT.
                linhas_visiveis <- input$base_tabela_rows_all
                if (!is.null(linhas_visiveis)) {
                    df <- df[linhas_visiveis, , drop = FALSE]
                }
                
                readr::write_excel_csv2(df, file, na = "")
                
                showNotification(
                    sprintf("Arquivo CSV gerado em %.2fs.", tempo_decorrido(inicio)),
                    type = "message"
                )
            }
        )
        
        # =================================================
        # ABA 2 — ALERTAS
        # =================================================
        
        MSG_SEM_ARQUIVOS <- paste0(
            "Não existem arquivos de alertas para processamento. Utilize o botão ",
            "\"Gerenciar Arquivos\" para enviar os arquivos de alertas."
        )
        
        dados_filtrados <- reactive({
            req(dados())
            df <- dados()
            
            if (!is.null(input$cpf) && input$cpf != "" && "CPF" %in% names(df)) {
                df <- df %>%
                    filter(str_detect(
                        str_to_upper(as.character(CPF)),
                        fixed(str_to_upper(str_trim(input$cpf)))
                    ))
            }
            
            if (!is.null(input$nome) && input$nome != "" && "Nome" %in% names(df)) {
                df <- df %>%
                    filter(str_detect(str_to_upper(Nome), fixed(str_to_upper(str_trim(input$nome)))))
            }
            
            if (!is.null(input$alerta) && input$alerta != "Todos" && input$alerta %in% names(df)) {
                df <- df %>%
                    filter(!is.na(.data[[input$alerta]]) & str_trim(.data[[input$alerta]]) != "")
            }
            
            if (!is.null(input$conflito) && input$conflito != "Todos" && input$conflito %in% names(df)) {
                df <- df %>%
                    filter(!is.na(.data[[input$conflito]]) & str_trim(.data[[input$conflito]]) != "")
            }
            
            # Cargo: compara com a nomenclatura (mesma exibida na tabela e no
            # combo), não com o código original do arquivo.
            if (!is.null(input$cargo) && input$cargo != "Todos") {
                cargos <- valores_cargo_exibicao(df, con)
                if (!is.null(cargos)) {
                    df <- df[cargos == input$cargo, , drop = FALSE]
                }
            }
            
            df
        })
        
        # Colunas fixas + Alerta(s)/Conflito(s) Detectado consolidados, com
        # Situação Profissional Atual e Cargo por extenso. Só apresentação:
        # dados_filtrados() continua com as colunas originais.
        tabela_exibicao <- reactive({
            dados_filtrados() %>%
                consolidar_alertas_tabela() %>%
                traduzir_codigos_tabela(con)
        })
        
        output$tabela <- renderDT({
            df <- tabela_exibicao()
            
            shiny::validate(need(nrow(df) > 0, MSG_SEM_ARQUIVOS))
            
            datatable(
                df,
                filter = "top",
                rownames = FALSE,
                width = "100%",
                options = list(
                    pageLength = 20,
                    scrollX = TRUE,
                    autoWidth = TRUE,
                    width = "100%"
                )
            )
        })
        
        # ---- Gráficos: tipo de Alerta/Conflito (echarts4r) e Cargo (ggiraph) ----
        
        resumo_tipos <- reactive({
            df <- dados_filtrados()
            
            if (nrow(df) == 0) {
                return(list(resumo = NULL, mensagem = MSG_SEM_ARQUIVOS))
            }
            
            colunas_tipo <- c(
                colunas_por_prefixo(df, "Alerta"),
                colunas_por_prefixo(df, "Conflito")
            )
            
            if (length(colunas_tipo) == 0) {
                return(list(
                    resumo = NULL,
                    mensagem = "Nenhuma coluna de Alerta/Conflito encontrada nos arquivos carregados."
                ))
            }
            
            resumo <- map_dfr(colunas_tipo, function(col) {
                valores <- df[[col]]
                tibble(Tipo = col, Quantidade = sum(!is.na(valores) & str_trim(valores) != ""))
            }) %>%
                filter(Quantidade > 0) %>%
                arrange(Quantidade)   # crescente: com o eixo invertido, a maior fica no topo
            
            if (nrow(resumo) == 0) {
                return(list(resumo = NULL, mensagem = "Nenhum alerta/conflito encontrado nos dados atuais."))
            }
            
            list(resumo = resumo, mensagem = NULL)
        })
        
        resumo_cargos <- reactive({
            df <- dados_filtrados()
            
            if (nrow(df) == 0) {
                return(list(resumo = NULL, mensagem = MSG_SEM_ARQUIVOS))
            }
            
            valores <- valores_cargo_exibicao(df, con)
            
            if (is.null(valores)) {
                return(list(resumo = NULL, mensagem = "Coluna \"Cargo\" não encontrada nos arquivos carregados."))
            }
            
            resumo <- tibble(Cargo = valores) %>%
                count(Cargo, name = "Quantidade") %>%
                arrange(Quantidade)
            
            list(resumo = resumo, mensagem = NULL)
        })
        
        output$grafico_ui <- renderUI({
            r <- resumo_tipos()
            n <- if (is.null(r$resumo)) 0 else nrow(r$resumo)
            altura <- if (n == 0) 120 else max(320, 34 * n + 110)
            
            echarts4rOutput(ns("grafico"), height = paste0(altura, "px"))
        })
        
        output$grafico <- renderEcharts4r({
            r <- resumo_tipos()
            
            shiny::validate(need(is.null(r$mensagem), r$mensagem))
            
            r$resumo %>%
                e_charts(Tipo) %>%
                e_bar(Quantidade, name = "Registros") %>%
                e_flip_coords() %>%
                e_labels(position = "right") %>%
                e_color("#2C7FB8") %>%
                e_title("Quantidade de Registros por Tipo de Alerta/Conflito") %>%
                e_tooltip(trigger = "item") %>%
                e_legend(show = FALSE) %>%
                e_grid(containLabel = TRUE, left = "2%", right = "8%") %>%
                e_toolbox_feature(feature = "saveAsImage")
        })
        
        output$grafico_cargo <- renderGirafe({
            r <- resumo_cargos()
            
            shiny::validate(need(is.null(r$mensagem), r$mensagem))
            
            resumo <- r$resumo %>%
                mutate(
                    rotulo = str_wrap(Cargo, 45),
                    rotulo = factor(rotulo, levels = unique(rotulo)),
                    dica = sprintf(
                        "<b>%s</b><br/>%s registro(s)",
                        htmltools::htmlEscape(Cargo),
                        format(Quantidade, big.mark = ".", decimal.mark = ",")
                    )
                )
            
            p <- ggplot(resumo, aes(x = rotulo, y = Quantidade)) +
                geom_col_interactive(aes(tooltip = dica, data_id = Cargo), fill = "#2C7FB8") +
                geom_text(aes(label = Quantidade), hjust = -0.2) +
                coord_flip() +
                scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
                labs(title = "Quantidade de Registros por Cargo", x = "", y = "Quantidade") +
                theme_minimal()
            
            girafe(
                ggobj = p,
                width_svg = 9,
                height_svg = max(3, 0.45 * nrow(resumo) + 1.5),
                options = list(
                    opts_hover(css = "fill:#0d6efd;cursor:pointer;"),
                    opts_sizing(rescale = TRUE, width = 1)
                )
            )
        })
        
        # ---- Gerar arquivo CSV ----
        
        output$csv_ui <- renderUI({
            df <- tabela_exibicao()
            ui_csv_dados(ns, "download_csv", !is.null(df) && nrow(df) > 0)
        })
        
        output$download_csv <- downloadHandler(
            
            filename = function() {
                paste0("alertas-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
            },
            
            content = function(file) {
                inicio <- Sys.time()
                
                df <- tabela_exibicao()
                
                # Respeita também a busca global e os filtros de coluna do DT
                # (input$tabela_rows_all).
                linhas_visiveis <- input$tabela_rows_all
                if (!is.null(linhas_visiveis)) {
                    df <- df[linhas_visiveis, , drop = FALSE]
                }
                
                readr::write_excel_csv2(df, file, na = "")
                
                showNotification(
                    sprintf("Arquivo CSV gerado em %.2fs.", tempo_decorrido(inicio)),
                    type = "message"
                )
            }
        )
        
    })
}