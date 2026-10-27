# =====================================================
# modules/mod_alertas_serventias.R
# -----------------------------------------------------
# Módulo "Serventias" (aba Serventias do menu principal), com três abas:
#
#   1) "Base de Dados - Serventias" — cadastro de serventias do MPM/CNJ,
#      lido DIRETAMENTE da planilha serventias.xlsx (ou, se não houver
#      planilha, de serventias.csv). A tabela mostra EXATAMENTE as
#      colunas do arquivo, na mesma ordem. Filtros: Status
#      (padrão "Ativo"), Código Serventia, Código da Unidade de Origem,
#      Entrância e Detalhe (busca o texto em todas as colunas). Gráfico
#      por CEP.
#
#   2) "Alertas - Serventias" — arquivo de alertas de Serventias do MPM,
#      alertas_serventias.csv (ARQUIVO_ALERTAS_SERVENTIAS). Mesmo
#      funcionamento de mod_alertas.R (filtros, alertas consolidados na
#      tabela, gráficos, CSV).
#
#   3) "Inconsistências - Serventias" — verificações automáticas sobre a
#      base (ver verificar_inconsistencias_serventias()), no mesmo
#      formato da aba de inconsistências dos Magistrados: código
#      duplicado, CEP ausente/inválido, nome repetido entre ativas e
#      serventia inativa que ainda é lotação de magistrados/servidores.
#
# As abas 1 e 2 têm Tabela, Gráfico e Gerar arquivo CSV.
#
# Os dois arquivos ficam na subpasta de serventias da empresa:
#
#   <PASTA_ALERTAS>/<EMPRESA>/serventias/serventias.xlsx   (ou .csv)
#   <PASTA_ALERTAS>/<EMPRESA>/serventias/alertas_serventias.csv   (ou .xlsx)
#
# Os dois formatos (.xlsx e .csv) são aceitos para os dois arquivos; se
# existirem os dois para o mesmo tipo, o .xlsx tem prioridade.
#
# "Gerenciar Arquivos" (comum às duas abas) apaga cada arquivo
# separadamente e envia um ou dois arquivos de uma vez. O TIPO de cada
# arquivo enviado é identificado pelas colunas (ver
# identificar_tipo_arquivo_serventias()), e ele é gravado com o nome fixo
# correspondente + a extensão original (serventias.xlsx,
# alertas_serventias.csv...), qualquer que seja o nome original.
#
# Tudo roda só em R, sobre os arquivos enviados — sem banco de dados.
#
# DEPENDÊNCIA: reaproveita funções/constantes definidas em mod_alertas.R
# e mod_inconsistencias.R — precisa ser carregado DEPOIS deles no app.R.
# =====================================================

library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(DT)
library(echarts4r)
library(ggiraph)
library(readxl)   # leitura direta de .xlsx

local({
    
    dependencias <- c(
        "MAX_UPLOAD_MB",
        "SUBPASTA_SERVENTIAS",
        "caminho_alertas",
        "tempo_decorrido",
        "ler_arquivo_alertas",
        "colunas_por_prefixo",
        "colunas_com_dados",
        "consolidar_colunas_por_linha",
        "normalizar_nome_coluna",
        "EXTENSOES_PLANILHA",
        "ROTULO_VALOR_VAZIO",
        "extensao_arquivo",
        "ler_arquivo_dados",
        "limpar_dados_lidos",
        "limpar_cabecalho",
        "coluna_por_regex",
        "valores_coluna_regex",
        "opcoes_combo",
        "selecao_valida",
        "normalizar_texto_busca",
        "texto_busca_linhas",
        "ui_csv_dados",
        "normalizar_nome_arquivo",
        "codigo_limpo",
        "status_ativo",
        "chave_nome_orgao",
        "indice_serventias",
        "carregar_lotacoes_servidores_empresa",
        "gravar_arquivo_substituindo",
        "mod_inconsistencias_ui",
        "mod_inconsistencias_server"
    )
    
    faltando <- dependencias[!vapply(dependencias, exists, logical(1))]
    
    if (length(faltando) > 0) {
        stop(
            "mod_alertas_serventias.R precisa ser carregado depois de ",
            "mod_alertas.R e mod_inconsistencias.R. Não encontrado: ",
            paste(faltando, collapse = ", ")
        )
    }
    
})

# =====================================================
# ARQUIVOS DO MÓDULO
# -----------------------------------------------------
# Cada tipo tem um nome fixo (radical) e pode estar em .xlsx ou .csv.
# EXTENSOES_SERVENTIAS também define a PRIORIDADE de leitura: se houver
# serventias.xlsx e serventias.csv na pasta, vale o .xlsx. A comparação
# de nome não diferencia maiúsculas/minúsculas.
# =====================================================

EXTENSOES_SERVENTIAS <- EXTENSOES_PLANILHA   # definida em mod_alertas.R

TIPOS_ARQUIVO_SERVENTIAS <- list(
    base = list(
        radical = "serventias",
        rotulo = "Base de Dados - Serventias"
    ),
    alertas = list(
        radical = "alertas_serventias",
        rotulo = "Alertas - Serventias"
    )
)

# Nomes "principais" de cada tipo, usados nas mensagens.
ARQUIVO_BASE_SERVENTIAS <- "serventias.xlsx"
ARQUIVO_ALERTAS_SERVENTIAS <- "alertas_serventias.csv"

# Subpasta de serventias da empresa (<PASTA_ALERTAS>/<EMPRESA>/serventias).
pasta_serventias <- function(empresa) {
    caminho_alertas(empresa, SUBPASTA_SERVENTIAS)
}

# Nomes aceitos para o tipo: "serventias.xlsx", "serventias.csv"...
nomes_aceitos_tipo <- function(tipo) {
    paste0(TIPOS_ARQUIVO_SERVENTIAS[[tipo]]$radical, ".", EXTENSOES_SERVENTIAS)
}

# Arquivos do tipo existentes na pasta, na ordem de prioridade de
# leitura (.xlsx antes de .csv). O nome é comparado de forma tolerante
# (normalizar_nome_arquivo(), de mod_alertas.R): "Serventias.xlsx" ou
# "Alertas Serventias.csv" também são reconhecidos.
arquivos_do_tipo <- function(pasta, tipo) {
    
    if (is.null(pasta) || !dir.exists(pasta)) {
        return(character(0))
    }
    
    existentes <- list.files(pasta, full.names = TRUE)
    
    ok <- extensao_arquivo(existentes) %in% EXTENSOES_SERVENTIAS &
        normalizar_nome_arquivo(existentes) == TIPOS_ARQUIVO_SERVENTIAS[[tipo]]$radical
    
    achados <- existentes[ok]
    achados[order(match(extensao_arquivo(achados), EXTENSOES_SERVENTIAS))]
    
}

# Arquivo do tipo que será lido (o de maior prioridade), ou NULL.
arquivo_do_tipo <- function(pasta, tipo) {
    achados <- arquivos_do_tipo(pasta, tipo)
    if (length(achados) > 0) achados[1] else NULL
}

# =====================================================
# LEITURA E CARREGAMENTO (vale para os dois arquivos, .xlsx ou .csv)
# -----------------------------------------------------
# Usa as funções genéricas de mod_alertas.R: ler_arquivo_dados()
# (planilha pela aba "serventias", se existir; CSV com ";" e fallback) e
# limpar_dados_lidos() (colunas sem nome vazias, BOM, espaços,
# duplicadas). Todas as colunas ficam como texto, na ordem do arquivo.
# =====================================================

ler_arquivo_serventias <- function(caminho, extensao = extensao_arquivo(caminho),
                                   nome_exibicao = basename(caminho), n_max = Inf) {
    ler_arquivo_dados(caminho, extensao, nome_exibicao, aba_preferida = "serventias", n_max = n_max)
}

carregar_arquivo_serventias <- function(arquivo) {
    
    if (is.null(arquivo) || !file.exists(arquivo)) {
        return(data.frame())
    }
    
    # Lido e limpo uma vez por processo e reaproveitado por todas as sessões (e pelos outros módulos e pelo Painel) enquanto o arquivo não mudar — ver com_cache_arquivo() em R/utils.R.
    com_cache_arquivo(arquivo, "serventias", function() {
        limpar_dados_lidos(ler_arquivo_serventias(arquivo))
    })
    
}

# =====================================================
# COLUNAS (localização tolerante a variações de cabeçalho)
# -----------------------------------------------------
# Compara pelo nome normalizado (sem acento, minúsculo) contra uma
# expressão regular. Devolve o NOME da coluna, ou NULL se não houver.
# =====================================================

REGEX_COL_CODIGO_SERVENTIA <- "^codigo( da)? serventia$"
REGEX_COL_NOME_SERVENTIA <- "^nome( da)? serventia$"
REGEX_COL_STATUS <- "^status$"
REGEX_COL_CEP <- "^cep$"
REGEX_COL_UNIDADE_ORIGEM <- "^codigo da unidade de origem"
REGEX_COL_ENTRANCIA <- "^entrancia$"

# Funções genéricas de mod_alertas.R, com os nomes usados neste módulo.
coluna_serventia <- coluna_por_regex
valores_coluna_exibicao <- valores_coluna_regex
ROTULO_SERV_VAZIO <- ROTULO_VALOR_VAZIO

# =====================================================
# IDENTIFICAÇÃO DO TIPO DE ARQUIVO (upload)
# -----------------------------------------------------
#   - tem "Código Serventia" e colunas "Alerta..."/"Conflito..." -> alertas
#   - tem "Código Serventia" e "CEP", sem colunas de alerta     -> base
#   - qualquer outra coisa                                      -> NA
# Evita, por exemplo, gravar um arquivo do Quadro de Pessoal (ou a base
# no lugar dos alertas) por engano.
# =====================================================

identificar_tipo_arquivo_serventias <- function(df) {
    
    if (is.null(df) || ncol(df) == 0) {
        return(NA_character_)
    }
    
    names(df) <- limpar_cabecalho(names(df))
    
    if (is.null(coluna_serventia(df, REGEX_COL_CODIGO_SERVENTIA))) {
        return(NA_character_)
    }
    
    tem_alertas <- any(
        str_starts(names(df), "Alerta") | str_starts(names(df), "Conflito")
    )
    
    if (tem_alertas) {
        "alertas"
    } else if (!is.null(coluna_serventia(df, REGEX_COL_CEP))) {
        "base"
    } else {
        NA_character_
    }
    
}

# =====================================================
# BASE DE DADOS — BUSCA "DETALHE" E CEP
# =====================================================

# O filtro Detalhe usa normalizar_texto_busca() e texto_busca_linhas(),
# definidas em mod_alertas.R.


# CEP para exibição no gráfico: "49010080" -> "49010-080". O que não
# tiver 8 dígitos é mantido como veio; vazio vira "(Não informado)".
formatar_cep <- function(x) {
    
    x <- str_trim(as.character(x))
    digitos <- str_remove_all(x, "\\D")
    
    ifelse(
        is.na(x) | x == "",
        ROTULO_SERV_VAZIO,
        ifelse(
            nchar(digitos) == 8,
            paste0(substr(digitos, 1, 5), "-", substr(digitos, 6, 8)),
            x
        )
    )
    
}

# Lista de nomes para o tooltip do gráfico por CEP (no máximo `max`).
resumir_nomes <- function(nomes, max = 10) {
    
    nomes <- sort(unique(nomes[!is.na(nomes) & nomes != ""]))
    
    if (length(nomes) == 0) {
        return("")
    }
    
    texto <- paste(htmltools::htmlEscape(head(nomes, max)), collapse = "<br/>")
    
    if (length(nomes) > max) {
        texto <- paste0(texto, sprintf("<br/><i>... e mais %d</i>", length(nomes) - max))
    }
    
    texto
    
}

# =====================================================
# ALERTAS — CONSOLIDA ALERTA(S)/CONFLITO(S) DETECTADO(S) (só tabela)
# -----------------------------------------------------
# Mantém as colunas fixas (tudo que não começa com "Alerta"/"Conflito")
# e troca as colunas "Alerta: <tipo>" por "Alerta(s) Detectado".
# "Conflito(s) Detectado" só aparece se o arquivo tiver conflitos.
# =====================================================

consolidar_alertas_serventias_tabela <- function(df) {
    
    if (is.null(df) || nrow(df) == 0) {
        return(df)
    }
    
    colunas_alerta <- colunas_por_prefixo(df, "Alerta")
    colunas_conflito <- colunas_por_prefixo(df, "Conflito")
    
    colunas_fixas <- setdiff(names(df), c(colunas_alerta, colunas_conflito))
    
    df_exibicao <- df[colunas_fixas]
    df_exibicao[["Alerta(s) Detectado"]] <- consolidar_colunas_por_linha(df, colunas_alerta)
    
    if (length(colunas_conflito) > 0) {
        df_exibicao[["Conflito(s) Detectado"]] <- consolidar_colunas_por_linha(df, colunas_conflito)
    }
    
    df_exibicao
    
}

# =====================================================
# INCONSISTÊNCIAS — VERIFICAÇÕES AUTOMÁTICAS NA BASE
# -----------------------------------------------------
# Mesma "inteligência" da aba de inconsistências dos Magistrados,
# adaptada ao cadastro de Serventias. Rodam sobre a base (serventias.xlsx
# / .csv) e apontam padrões que contrariam o cadastro do MPM/CNJ. São
# indícios para conferência, não correções automáticas.
#
#   1) CÓDIGO DE SERVENTIA DUPLICADO
#      O mesmo "Código Serventia" em mais de um registro da base.
#      Alta se dois ou mais registros estão ativos; Média nos demais.
#
#   2) CEP AUSENTE OU INVÁLIDO (só serventias ativas)
#      Alta: CEP que não tem 8 dígitos ou é uma sequência repetida
#      ("00000000"); Média: CEP não informado.
#
#   3) NOME REPETIDO ENTRE SERVENTIAS ATIVAS
#      Duas ou mais serventias ATIVAS, com códigos diferentes, com o
#      mesmo nome (sem diferenciar acentos/maiúsculas). Média.
#
#   4) SERVENTIA INATIVA COM LOTAÇÃO ATIVA
#      Serventia com status diferente de "Ativo" que ainda é a lotação
#      ATUAL (último registro do CPF, em situação não encerrada) de
#      magistrados (base do módulo Magistrados) e/ou servidores
#      (arquivos de alertas do Quadro de Pessoal). Casada pelo código ou
#      pelo nome. Alta. Só roda se a base tiver a coluna Status e se ao
#      menos um desses arquivos tiver sido enviado.
# =====================================================

VERIFICACAO_SRV_CODIGO <- "Código de serventia duplicado"
VERIFICACAO_SRV_CEP <- "CEP ausente ou inválido"
VERIFICACAO_SRV_NOME <- "Nome repetido entre serventias ativas"
VERIFICACAO_SRV_LOTACAO <- "Serventia inativa com lotação ativa"

VERIFICACOES_SERVENTIAS <- c(
    VERIFICACAO_SRV_CODIGO,
    VERIFICACAO_SRV_CEP,
    VERIFICACAO_SRV_NOME,
    VERIFICACAO_SRV_LOTACAO
)

FILTROS_TEXTO_INC_SERVENTIAS <- c(
    "Código Serventia" = "Código Serventia",
    "Nome da Serventia" = "Nome da Serventia"
)

inconsistencias_serventias_vazio <- function() {
    tibble(
        `Verificação` = character(0),
        Gravidade = character(0),
        `Código Serventia` = character(0),
        `Nome da Serventia` = character(0),
        Status = character(0),
        CEP = character(0),
        Motivo = character(0)
    )
}

# Lista curta de nomes para o Motivo ("A, B, C e mais 4").
listar_nomes_motivo <- function(nomes, max = 5) {
    nomes <- sort(unique(nomes[!is.na(nomes) & nomes != ""]))
    if (length(nomes) == 0) {
        return("")
    }
    texto <- paste(head(nomes, max), collapse = ", ")
    if (length(nomes) > max) {
        texto <- paste0(texto, sprintf(" e mais %d", length(nomes) - max))
    }
    texto
}

# Lotações atuais de magistrados e servidores da empresa (para a
# verificação 4). Cada parte só entra se o respectivo módulo estiver
# carregado e o arquivo tiver sido enviado.
carregar_lotacoes_empresa <- function(empresa) {
    
    partes <- list(
        carregar_lotacoes_servidores_empresa(empresa) %>% mutate(tipo = "servidor")
    )
    
    if (exists("carregar_lotacoes_magistrados_empresa")) {
        partes <- c(
            partes,
            list(carregar_lotacoes_magistrados_empresa(empresa) %>% mutate(tipo = "magistrado"))
        )
    }
    
    bind_rows(partes)
    
}

# `base` = dados_base() (como veio no arquivo); `lotacoes` =
# carregar_lotacoes_empresa().
verificar_inconsistencias_serventias <- function(base, lotacoes = NULL) {
    
    vazio <- inconsistencias_serventias_vazio()
    
    if (is.null(base) || nrow(base) == 0) {
        return(vazio)
    }
    
    valor <- function(regex) {
        c <- coluna_serventia(base, regex)
        if (is.null(c)) rep(NA_character_, nrow(base)) else as.character(base[[c]])
    }
    
    tem_status <- !is.null(coluna_serventia(base, REGEX_COL_STATUS))
    
    d <- tibble(
        codigo = codigo_limpo(valor(REGEX_COL_CODIGO_SERVENTIA)),
        nome = valor(REGEX_COL_NOME_SERVENTIA),
        status = valor(REGEX_COL_STATUS),
        cep = valor(REGEX_COL_CEP)
    ) %>%
        mutate(
            .linha = row_number(),
            # Sem coluna Status (ou status vazio), conta como ativa.
            ativa = is.na(status) | str_trim(status) == "" | status_ativo(status),
            chave_nome = chave_nome_orgao(nome)
        )
    
    montar <- function(x, verificacao) {
        if (is.null(x) || nrow(x) == 0) {
            return(vazio)
        }
        tibble(
            `Verificação` = verificacao,
            Gravidade = x$Gravidade,
            `Código Serventia` = x$codigo,
            `Nome da Serventia` = x$nome,
            Status = x$status,
            CEP = formatar_cep(x$cep),
            Motivo = x$Motivo
        )
    }
    
    # ---- 1) Código duplicado ----
    
    r1 <- d %>%
        filter(!is.na(codigo) & codigo != "") %>%
        group_by(codigo) %>%
        filter(n() > 1) %>%
        mutate(
            Gravidade = ifelse(sum(ativa) >= 2, "Alta", "Média"),
            Motivo = sprintf(
                "O código %s aparece em %d registros da base (status: %s).",
                codigo, n(), paste(unique(coalesce(status, "não informado")), collapse = ", ")
            )
        ) %>%
        ungroup() %>%
        arrange(codigo, .linha)
    
    # ---- 2) CEP ausente ou inválido (ativas) ----
    
    r2 <- if (is.null(coluna_serventia(base, REGEX_COL_CEP))) {
        NULL
    } else {
        d %>%
            filter(ativa) %>%
            mutate(
                digitos = str_remove_all(coalesce(cep, ""), "\\D"),
                ausente = is.na(cep) | str_trim(cep) == "",
                invalido = !ausente & (nchar(digitos) != 8 | str_detect(digitos, "^(\\d)\\1{7}$"))
            ) %>%
            filter(ausente | invalido) %>%
            mutate(
                Gravidade = ifelse(invalido, "Alta", "Média"),
                Motivo = ifelse(
                    ausente,
                    "Serventia ativa sem CEP informado.",
                    sprintf("Serventia ativa com CEP \"%s\" inválido (precisa ter 8 dígitos e não ser uma sequência repetida).", cep)
                )
            ) %>%
            arrange(nome)
    }
    
    # ---- 3) Nome repetido entre serventias ativas ----
    
    r3 <- d %>%
        filter(ativa, !is.na(chave_nome) & chave_nome != "") %>%
        group_by(chave_nome) %>%
        filter(n_distinct(codigo) > 1) %>%
        mutate(
            Gravidade = "Média",
            Motivo = sprintf(
                "Mesmo nome de outra(s) serventia(s) ativa(s) — códigos %s.",
                paste(sort(unique(codigo)), collapse = ", ")
            )
        ) %>%
        ungroup() %>%
        arrange(chave_nome, codigo)
    
    # ---- 4) Serventia inativa com lotação ativa ----
    
    r4 <- NULL
    
    if (tem_status && !is.null(lotacoes) && nrow(lotacoes) > 0) {
        
        # Só chaves sem nenhuma serventia ativa (um nome repetido entre
        # uma ativa e uma inativa não é apontado).
        indice <- indice_serventias(base)
        chaves_inativas <- indice$chave[!indice$alguma_ativa]
        
        chaves_serv <- bind_rows(
            d %>% filter(!ativa, !is.na(codigo) & codigo != "") %>%
                transmute(.linha, chave = paste0("cod:", codigo)),
            d %>% filter(!ativa, !is.na(chave_nome) & chave_nome != "") %>%
                transmute(.linha, chave = paste0("nome:", chave_nome))
        ) %>%
            filter(chave %in% chaves_inativas) %>%
            distinct()
        
        lotados <- chaves_serv %>%
            inner_join(lotacoes, by = "chave", relationship = "many-to-many") %>%
            distinct(.linha, tipo, CPF, Nome) %>%
            group_by(.linha) %>%
            summarise(
                n_mag = n_distinct(CPF[tipo == "magistrado"]),
                n_serv = n_distinct(CPF[tipo == "servidor"]),
                nomes_mag = listar_nomes_motivo(Nome[tipo == "magistrado"]),
                nomes_serv = listar_nomes_motivo(Nome[tipo == "servidor"]),
                .groups = "drop"
            )
        
        r4 <- d %>%
            inner_join(lotados, by = ".linha") %>%
            mutate(
                Gravidade = "Alta",
                Motivo = paste0(
                    "Status \"", coalesce(status, "não informado"), "\" na base, mas ainda é a lotação atual de ",
                    ifelse(n_mag > 0, sprintf("%d magistrado(s) (%s)", n_mag, nomes_mag), ""),
                    ifelse(n_mag > 0 & n_serv > 0, " e de ", ""),
                    ifelse(n_serv > 0, sprintf("%d servidor(es) (%s)", n_serv, nomes_serv), ""),
                    ". Conferir o status da serventia ou a lotação."
                )
            ) %>%
            arrange(nome)
    }
    
    bind_rows(
        montar(r1, VERIFICACAO_SRV_CODIGO),
        montar(r2, VERIFICACAO_SRV_CEP),
        montar(r3, VERIFICACAO_SRV_NOME),
        montar(r4, VERIFICACAO_SRV_LOTACAO)
    )
    
}

# =====================================================
# UI
# =====================================================

# Conteúdo "Gerar arquivo CSV" — ui_csv_dados() de mod_alertas.R.
ui_csv_serventias <- ui_csv_dados


mod_alertas_serventias_ui <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        
        # ===================================================
        # ESTILO (escopo deste módulo) — mesmo visual de mod_alertas.R
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

        #%1$s .dataTables_wrapper {
          width: 100%% !important;
        }

        /* Tabelas que cabem na tela ocupam a largura toda. Sem
           !important: com rolagem horizontal, o DataTables precisa dar
           a mesma largura (maior que a tela) ao cabeçalho e aos dados —
           forçar 100%% na tabela de dados desalinhava os títulos das
           colunas ao redimensionar a janela. */
        #%1$s table.dataTable {
          min-width: 100%%;
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
                    div(class = "al-titulo-icone", icon("building-columns")),
                    tags$h4("Serventias")
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
                    # ABA 1 — BASE DE DADOS - SERVENTIAS
                    # =============================================
                    
                    tabPanel(
                        title = tagList(icon("database", class = "me-1"), "Base de Dados - Serventias"),
                        value = "base",
                        
                        div(
                            class = "al-card",
                            
                            div(class = "al-card-titulo", "Filtros"),
                            
                            fluidRow(
                                column(
                                    3,
                                    # Começa em "Ativo" (padrão pedido); as
                                    # demais opções chegam quando o arquivo
                                    # é carregado.
                                    selectInput(
                                        ns("base_status"), "Status",
                                        choices = c("Todos", "Ativo"),
                                        selected = "Ativo",
                                        width = "100%"
                                    )
                                ),
                                column(3, textInput(ns("base_codigo"), "Código Serventia", width = "100%")),
                                column(
                                    3,
                                    selectInput(
                                        ns("base_unidade"), "Código da Unidade de Origem",
                                        choices = c("Todos"), width = "100%"
                                    )
                                ),
                                column(
                                    3,
                                    selectInput(
                                        ns("base_entrancia"), "Entrância",
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
                                    
                                    div(
                                        class = "mt-3",
                                        style = "max-width: 320px;",
                                        selectInput(
                                            ns("base_grafico_qtd"),
                                            "Exibir",
                                            choices = c(
                                                "Os 20 CEPs com mais serventias" = "20",
                                                "Os 50 CEPs com mais serventias" = "50",
                                                "Todos os CEPs" = "0"
                                            ),
                                            selected = "20",
                                            width = "100%"
                                        )
                                    ),
                                    
                                    girafeOutput(ns("base_grafico_cep"))
                                ),
                                tabPanel(
                                    tagList(icon("file-csv", class = "me-1"), "Gerar arquivo CSV"),
                                    uiOutput(ns("base_csv_ui"))
                                )
                            )
                        )
                    ),
                    
                    # =============================================
                    # ABA 2 — ALERTAS - SERVENTIAS
                    # -------------------------------------------------
                    # "Alerta" e "Conflito" se excluem mutuamente, como
                    # em mod_alertas.R.
                    # =============================================
                    
                    tabPanel(
                        title = tagList(icon("triangle-exclamation", class = "me-1"), "Alertas - Serventias"),
                        value = "alertas",
                        
                        div(
                            class = "al-card",
                            
                            div(class = "al-card-titulo", "Filtros"),
                            
                            fluidRow(
                                column(4, selectInput(ns("alerta"), "Alerta", choices = c("Todos"), width = "100%")),
                                column(4, selectInput(ns("conflito"), "Conflito", choices = c("Todos"), width = "100%")),
                                column(4, selectInput(ns("status"), "Status", choices = c("Todos"), width = "100%"))
                            ),
                            
                            fluidRow(
                                column(4, textInput(ns("codigo"), "Código Serventia", width = "100%")),
                                column(8, textInput(ns("nome"), "Nome da Serventia", width = "100%"))
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
                                    uiOutput(ns("grafico_ui")),
                                    tags$hr(class = "my-4"),
                                    girafeOutput(ns("grafico_status"))
                                ),
                                tabPanel(
                                    tagList(icon("file-csv", class = "me-1"), "Gerar arquivo CSV"),
                                    uiOutput(ns("csv_ui"))
                                )
                            )
                        )
                    ),
                    
                    # =============================================
                    # ABA 3 — INCONSISTÊNCIAS - SERVENTIAS
                    # -------------------------------------------------
                    # Verificações automáticas sobre a base (ver
                    # verificar_inconsistencias_serventias());
                    # apresentação pelo submódulo mod_inconsistencias.R.
                    # =============================================
                    
                    tabPanel(
                        title = tagList(
                            icon("clipboard-check", class = "me-1"),
                            "Inconsistências - Serventias",
                            mod_inconsistencias_badge_ui(ns("inc"))
                        ),
                        value = "inconsistencias",
                        
                        mod_inconsistencias_ui(
                            ns("inc"),
                            verificacoes = VERIFICACOES_SERVENTIAS,
                            filtros_texto = FILTROS_TEXTO_INC_SERVENTIAS
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

mod_alertas_serventias_server <- function(id, ativo = reactive(TRUE), empresa = reactive(NULL)) {
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        # Começam vazios: a empresa só é conhecida depois do login.
        dados_base <- reactiveVal(data.frame())   # serventias.csv
        dados <- reactiveVal(data.frame())        # alertas_serventias.csv
        
        # Lotações atuais de magistrados/servidores (outros módulos) — só
        # para a verificação "Serventia inativa com lotação ativa".
        # Mudanças nesses arquivos entram aqui no "Atualizar Dados" ou na
        # troca de empresa.
        lotacoes <- reactiveVal(NULL)
        
        empresa_definida <- function() {
            e <- empresa()
            !is.null(e) && !is.na(e) && trimws(e) != ""
        }
        
        recarregar <- function() {
            pasta <- pasta_serventias(empresa())
            dados_base(carregar_arquivo_serventias(arquivo_do_tipo(pasta, "base")))
            dados(carregar_arquivo_serventias(arquivo_do_tipo(pasta, "alertas")))
            lotacoes(carregar_lotacoes_empresa(empresa()))
        }
        
        # ----------------------------------------
        # CONTADOR DO CAMPO DE UPLOAD — ID novo do fileInput a cada
        # abertura do modal (evita reenviar a seleção de uma abertura
        # anterior; mesmo motivo de mod_alertas.R).
        # ----------------------------------------
        contador_upload <- reactiveVal(0)
        
        id_upload_atual <- function() {
            paste0("upload_serventias_", contador_upload())
        }
        
        entrada_upload_atual <- function() {
            input[[id_upload_atual()]]
        }
        
        # ----------------------------------------
        # CARREGA DADOS DA EMPRESA ATUAL (login / troca de empresa)
        # ----------------------------------------
        
        # Carga SOB DEMANDA (ver mod_alertas.R): só quando o módulo é
        # aberto pela primeira vez para a empresa da sessão, em vez de logo
        # no login. Com a empresa vazia (logout), os dados são descartados.
        empresa_carregada <- reactiveVal(NULL)
        
        observeEvent(list(empresa(), ativo()), {
            
            if (!empresa_definida()) {
                if (!is.null(empresa_carregada())) {
                    dados_base(data.frame())
                    dados(data.frame())
                    lotacoes(NULL)
                    empresa_carregada(NULL)
                }
                return(invisible(NULL))
            }
            
            req(ativo())
            
            if (!identical(empresa_carregada(), empresa())) {
                recarregar()
                empresa_carregada(empresa())
            }
            
        }, ignoreInit = FALSE)
        
        # =================================================
        # COMBOS
        # =================================================
        
        # Opções de Status da base na última atualização: quando o
        # arquivo passa de "sem dados" para "com dados" (primeiro envio,
        # troca de empresa), o Status volta ao padrão "Ativo".
        status_base_anterior <- character(0)
        
        atualizar_combos_base <- function() {
            
            df <- dados_base()
            
            status <- opcoes_combo(valores_coluna_exibicao(df, REGEX_COL_STATUS))
            unidades <- opcoes_combo(valores_coluna_exibicao(df, REGEX_COL_UNIDADE_ORIGEM))
            entrancias <- opcoes_combo(valores_coluna_exibicao(df, REGEX_COL_ENTRANCIA))
            
            status_atual <- if (length(status_base_anterior) == 0) "Ativo" else input$base_status
            status_base_anterior <<- status
            
            updateSelectInput(
                session, "base_status",
                choices = c("Todos", status),
                selected = selecao_valida(status_atual, status, padrao = "Ativo")
            )
            
            updateSelectInput(
                session, "base_unidade",
                choices = c("Todos", unidades),
                selected = selecao_valida(input$base_unidade, unidades)
            )
            
            updateSelectInput(
                session, "base_entrancia",
                choices = c("Todos", entrancias),
                selected = selecao_valida(input$base_entrancia, entrancias)
            )
            
        }
        
        atualizar_combos <- function() {
            
            df <- dados()
            
            colunas_alerta <- colunas_com_dados(df, colunas_por_prefixo(df, "Alerta"))
            colunas_conflito <- colunas_com_dados(df, colunas_por_prefixo(df, "Conflito"))
            status <- opcoes_combo(valores_coluna_exibicao(df, REGEX_COL_STATUS))
            
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
            
            updateSelectInput(
                session, "status",
                choices = c("Todos", status),
                selected = selecao_valida(input$status, status)
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
            
            # "Atualizar Dados" sempre relê do disco.
            limpar_cache_arquivos(file.path(PASTA_ALERTAS_BASE, empresa()))
            recarregar()
            empresa_carregada(empresa())
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
            status <- opcoes_combo(valores_coluna_exibicao(dados_base(), REGEX_COL_STATUS))
            updateSelectInput(session, "base_status", selected = selecao_valida("Ativo", status))
            updateTextInput(session, "base_codigo", value = "")
            updateSelectInput(session, "base_unidade", selected = "Todos")
            updateSelectInput(session, "base_entrancia", selected = "Todos")
            updateTextInput(session, "base_detalhe", value = "")
        })
        
        observeEvent(input$limpar_filtros, {
            updateSelectInput(session, "alerta", selected = "Todos")
            updateSelectInput(session, "conflito", selected = "Todos")
            updateSelectInput(session, "status", selected = "Todos")
            updateTextInput(session, "codigo", value = "")
            updateTextInput(session, "nome", value = "")
        })
        
        # ----------------------------------------
        # EXCLUSÃO MÚTUA (Alerta x Conflito) — aba Alertas
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
        # Lista os dois arquivos (com botão Apagar para cada um) e permite
        # enviar um ou os dois de uma vez. O tipo de cada arquivo enviado
        # é identificado pelas colunas (identificar_tipo_arquivo_serventias).
        # =================================================
        
        linha_arquivo_modal <- function(pasta, tipo, id_apagar) {
            
            info <- TIPOS_ARQUIVO_SERVENTIAS[[tipo]]
            existentes <- arquivos_do_tipo(pasta, tipo)
            
            nome_exibido <- if (length(existentes) > 0) {
                basename(existentes[1])
            } else {
                paste0(info$radical, ".xlsx / .csv")
            }
            
            icone <- if (length(existentes) > 0 && extensao_arquivo(existentes[1]) == "xlsx") {
                "file-excel"
            } else {
                "file-csv"
            }
            
            div(
                class = "srv-arquivo-linha",
                div(
                    div(class = "srv-arquivo-nome", icon(icone, class = "me-1"), nome_exibido),
                    div(
                        class = "srv-arquivo-info",
                        info$rotulo, " — ",
                        if (length(existentes) > 0) {
                            sprintf(
                                "atualizado em %s",
                                format(file.info(existentes[1])$mtime, "%d/%m/%Y %H:%M")
                            )
                        } else {
                            "ainda não enviado"
                        },
                        # Se houver .xlsx e .csv do mesmo tipo, avisa qual
                        # está sendo ignorado (o Apagar remove os dois).
                        if (length(existentes) > 1) {
                            tags$div(
                                class = "text-warning",
                                sprintf("também na pasta (ignorado): %s", paste(basename(existentes[-1]), collapse = ", "))
                            )
                        }
                    )
                ),
                if (length(existentes) > 0) {
                    actionButton(
                        ns(id_apagar),
                        tagList(icon("trash", class = "me-1"), "Apagar"),
                        class = "btn btn-outline-danger btn-sm"
                    )
                }
            )
            
        }
        
        observeEvent(input$gerenciar_arquivos, {
            
            if (!empresa_definida()) {
                showNotification(
                    "Nenhuma empresa definida para esta sessão. Faça login novamente (ou use \"Trocar empresa\").",
                    type = "error",
                    duration = 8
                )
                return(invisible(NULL))
            }
            
            pasta <- pasta_serventias(empresa())
            
            contador_upload(contador_upload() + 1)
            
            showModal(modalDialog(
                title = div(
                    style = "position:relative; padding-right:28px;",
                    icon("folder-open", class = "me-2"),
                    sprintf("Gerenciar Arquivos de Serventias — %s", empresa()),
                    tags$button(
                        type = "button",
                        class = "btn-close",
                        style = "position:absolute; top:2px; right:0;",
                        `aria-label` = "Fechar",
                        onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
                            ns("fechar_modal_serventias")
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
                    
                    linha_arquivo_modal(pasta, "base", "apagar_base"),
                    linha_arquivo_modal(pasta, "alertas", "apagar_alertas")
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
                        "Selecione o arquivo de Serventias e/ou o de Alertas de Serventias ",
                        "gerados pelo MPM, em ", tags$b(".xlsx"), " ou ", tags$b(".csv"),
                        " (um de cada tipo, limite de ", MAX_UPLOAD_MB,
                        " MB no total). O tipo é identificado pelas colunas: com colunas ",
                        "\"Alerta: ...\" o arquivo é gravado como ", tags$code("alertas_serventias"),
                        "; com a coluna CEP e sem alertas, como ", tags$code("serventias"),
                        " — sempre com a extensão original. Um arquivo já existente do ",
                        "mesmo tipo é substituído."
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
                
                footer = actionButton(ns("fechar_modal_serventias"), "Fechar")
            ))
            
        })
        
        observeEvent(input$fechar_modal_serventias, {
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
        })
        
        # ----------------------------------------
        # APAGAR (um arquivo por vez)
        # ----------------------------------------
        
        tipo_para_apagar <- reactiveVal(NULL)
        
        pedir_confirmacao_apagar <- function(tipo) {
            req(empresa())
            
            info <- TIPOS_ARQUIVO_SERVENTIAS[[tipo]]
            
            existentes <- arquivos_do_tipo(pasta_serventias(empresa()), tipo)
            
            if (length(existentes) == 0) {
                showNotification(sprintf("Não há arquivo de %s na pasta.", info$rotulo), type = "warning")
                return(invisible(NULL))
            }
            
            tipo_para_apagar(tipo)
            
            # Fecha o modal atual antes de abrir a confirmação (evita o
            # backdrop "grudado" — ver mod_alertas.R).
            removeModal()
            
            showModal(modalDialog(
                title = "Confirmar exclusão",
                sprintf(
                    "Tem certeza que deseja apagar o arquivo %s (%s) da empresa %s? Esta ação não pode ser desfeita.",
                    paste(basename(existentes), collapse = " e "), info$rotulo, empresa()
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
            info <- TIPOS_ARQUIVO_SERVENTIAS[[tipo]]
            tipo_para_apagar(NULL)
            
            inicio <- Sys.time()
            existentes <- arquivos_do_tipo(pasta_serventias(empresa()), tipo)
            nomes_existentes <- paste(basename(existentes), collapse = " e ")
            
            if (length(existentes) == 0) {
                showNotification(sprintf("Não há arquivo de %s na pasta.", info$rotulo), type = "warning")
                return()
            }
            
            removidos <- tryCatch(
                file.remove(existentes),
                error = function(e) {
                    showNotification(sprintf("Erro ao apagar o arquivo: %s", conditionMessage(e)), type = "error")
                    NULL
                }
            )
            
            if (is.null(removidos)) {
                return()
            }
            
            if (all(removidos)) {
                showNotification(
                    sprintf("Arquivo %s apagado em %.2fs.", nomes_existentes, tempo_decorrido(inicio)),
                    type = "message"
                )
            } else {
                showNotification(
                    sprintf("O arquivo %s não pôde ser apagado (verifique se está aberto em outro programa).", nomes_existentes),
                    type = "error"
                )
            }
            
            recarregar()
            atualizar_todos_combos()
        })
        
        # ----------------------------------------
        # UPLOAD (um ou dois arquivos; tipo identificado pelas colunas)
        # ----------------------------------------
        
        # Arquivos já validados, aguardando a confirmação de substituição.
        upload_pendente <- reactiveVal(NULL)
        
        processar_upload <- function(pendente) {
            req(pendente, empresa())
            
            pasta <- pasta_serventias(empresa())
            req(pasta)
            
            inicio <- Sys.time()
            
            removeModal()
            session$sendCustomMessage("limpar-modal-backdrop", list())
            
            gravados <- character(0)
            
            for (i in seq_len(nrow(pendente))) {
                
                info <- TIPOS_ARQUIVO_SERVENTIAS[[pendente$tipo[i]]]
                
                # O arquivo anterior do mesmo tipo, em QUALQUER formato, é
                # substituído (senão um .xlsx antigo continuaria com prioridade
                # sobre um .csv novo) — mas só DEPOIS de o novo estar gravado
                # (gravar_arquivo_substituindo(), em R/utils.R). Antes, o
                # anterior era apagado primeiro e uma falha na cópia deixava a
                # empresa sem o arquivo.
                anteriores <- arquivos_do_tipo(pasta, pendente$tipo[i])

                destino <- paste0(info$radical, ".", pendente$extensao[i])

                if (gravar_arquivo_substituindo(pendente$datapath[i], file.path(pasta, destino), anteriores)) {
                    gravados <- c(gravados, sprintf("%s → %s", pendente$name[i], destino))
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
                        "Enviado(s) em %.2fs: %s. Atualizando as tabelas...",
                        tempo_decorrido(inicio),
                        paste(gravados, collapse = "; ")
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
                    sprintf("Arquivo(s) enviado(s), mas houve erro ao recarregar as tabelas: %s", conditionMessage(e)),
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
                    if (!(extensoes[i] %in% EXTENSOES_SERVENTIAS)) {
                        return(NA_character_)
                    }
                    identificar_tipo_arquivo_serventias(
                        ler_arquivo_serventias(up$datapath[i], extensoes[i], up$name[i], n_max = 5)
                    )
                },
                character(1)
            )
            
            invalidos <- up$name[is.na(tipos)]
            
            if (length(invalidos) > 0) {
                showNotification(
                    sprintf(
                        "Não reconhecido(s) como arquivo de Serventias ou de Alertas de Serventias em .xlsx ou .csv (ignorado[s]): %s.",
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
            
            if (any(duplicated(tipos[validos]))) {
                showNotification(
                    "Foram selecionados dois arquivos do mesmo tipo. Envie no máximo um arquivo de Serventias e um de Alertas de Serventias.",
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
            
            pasta <- pasta_serventias(empresa())
            
            a_substituir <- vapply(
                pendente$tipo,
                function(t) {
                    existentes <- arquivos_do_tipo(pasta, t)
                    if (length(existentes) > 0) paste(basename(existentes), collapse = ", ") else NA_character_
                },
                character(1)
            )
            a_substituir <- a_substituir[!is.na(a_substituir)]
            
            if (length(a_substituir) > 0) {
                
                removeModal()
                
                showModal(modalDialog(
                    title = "Arquivo(s) existente(s) na pasta",
                    sprintf(
                        "A pasta da empresa %s já contém: %s. Deseja substituir pelo(s) arquivo(s) selecionado(s)?",
                        empresa(),
                        paste(a_substituir, collapse = ", ")
                    ),
                    footer = tagList(
                        modalButton("Cancelar"),
                        actionButton(ns("confirmar_substituir"), "Substituir", class = "btn-danger")
                    )
                ))
                
            } else {
                processar_upload(pendente)
            }
        })
        
        observeEvent(input$confirmar_substituir, {
            processar_upload(upload_pendente())
        })
        
        # =================================================
        # ABA 1 — BASE DE DADOS - SERVENTIAS
        # =================================================
        
        MSG_SEM_BASE <- paste0(
            "Não existe arquivo de Serventias (serventias.xlsx ou serventias.csv)",
            " para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviá-lo."
        )
        
        MSG_BASE_SEM_RESULTADO <- "Nenhuma serventia encontrada com os filtros aplicados."
        
        # Texto de busca por linha (filtro Detalhe) — recalculado só
        # quando o arquivo muda, não a cada tecla digitada.
        busca_base <- reactive({
            texto_busca_linhas(dados_base())
        })
        
        # Espera o usuário parar de digitar (400 ms) antes de filtrar.
        base_detalhe_d <- debounce(reactive(input$base_detalhe), 400)
        base_codigo_d <- debounce(reactive(input$base_codigo), 400)
        
        # Filtros combinados como máscaras sobre a base completa; a tabela
        # mantém TODAS as colunas do arquivo, na ordem original.
        dados_base_filtrados <- reactive({
            df <- dados_base()
            
            if (nrow(df) == 0) {
                return(df)
            }
            
            manter <- rep(TRUE, nrow(df))
            
            if (!is.null(input$base_status) && input$base_status != "Todos") {
                v <- valores_coluna_exibicao(df, REGEX_COL_STATUS)
                if (!is.null(v)) manter <- manter & v == input$base_status
            }
            
            codigo <- str_trim(base_codigo_d() %||% "")
            col_codigo <- coluna_serventia(df, REGEX_COL_CODIGO_SERVENTIA)
            if (codigo != "" && !is.null(col_codigo)) {
                manter <- manter & str_detect(
                    str_to_upper(coalesce(df[[col_codigo]], "")),
                    fixed(str_to_upper(codigo))
                )
            }
            
            if (!is.null(input$base_unidade) && input$base_unidade != "Todos") {
                v <- valores_coluna_exibicao(df, REGEX_COL_UNIDADE_ORIGEM)
                if (!is.null(v)) manter <- manter & v == input$base_unidade
            }
            
            if (!is.null(input$base_entrancia) && input$base_entrancia != "Todos") {
                v <- valores_coluna_exibicao(df, REGEX_COL_ENTRANCIA)
                if (!is.null(v)) manter <- manter & v == input$base_entrancia
            }
            
            # Detalhe: o texto digitado em QUALQUER coluna (sem diferenciar
            # maiúsculas/minúsculas nem acentos).
            detalhe <- str_trim(base_detalhe_d() %||% "")
            if (detalhe != "") {
                manter <- manter & str_detect(busca_base(), fixed(normalizar_texto_busca(detalhe)))
            }
            
            df[manter, , drop = FALSE]
        })
        
        # ---- Tabela (mesma estrutura do arquivo) ----
        
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
        
        # ---- Gráfico por CEP (ggiraph) ----
        
        resumo_cep <- reactive({
            if (nrow(dados_base()) == 0) {
                return(list(resumo = NULL, mensagem = MSG_SEM_BASE))
            }
            
            df <- dados_base_filtrados()
            
            if (nrow(df) == 0) {
                return(list(resumo = NULL, mensagem = MSG_BASE_SEM_RESULTADO))
            }
            
            col_cep <- coluna_serventia(df, REGEX_COL_CEP)
            
            if (is.null(col_cep)) {
                return(list(resumo = NULL, mensagem = "Coluna \"CEP\" não encontrada no arquivo carregado."))
            }
            
            col_nome <- coluna_serventia(df, REGEX_COL_NOME_SERVENTIA)
            
            resumo <- tibble(
                CEP = formatar_cep(df[[col_cep]]),
                Nome = if (is.null(col_nome)) NA_character_ else df[[col_nome]]
            ) %>%
                group_by(CEP) %>%
                summarise(
                    Quantidade = n(),
                    Serventias = resumir_nomes(Nome),
                    .groups = "drop"
                ) %>%
                arrange(desc(Quantidade), CEP)
            
            list(resumo = resumo, mensagem = NULL)
        })
        
        output$base_grafico_cep <- renderGirafe({
            r <- resumo_cep()
            
            shiny::validate(need(is.null(r$mensagem), r$mensagem))
            
            total_ceps <- nrow(r$resumo)
            limite <- suppressWarnings(as.integer(input$base_grafico_qtd))
            
            resumo <- if (!is.na(limite) && limite > 0) head(r$resumo, limite) else r$resumo
            
            resumo <- resumo %>%
                arrange(Quantidade, desc(CEP)) %>%   # com coord_flip, a maior fica no topo
                mutate(
                    CEP = factor(CEP, levels = unique(CEP)),
                    dica = sprintf(
                        "<b>CEP %s</b><br/>%s serventia(s)%s",
                        htmltools::htmlEscape(as.character(CEP)),
                        format(Quantidade, big.mark = ".", decimal.mark = ","),
                        ifelse(Serventias == "", "", paste0("<hr style='margin:4px 0'/>", Serventias))
                    )
                )
            
            p <- ggplot(resumo, aes(x = CEP, y = Quantidade)) +
                geom_col_interactive(
                    aes(tooltip = dica, data_id = CEP),
                    fill = "#2C7FB8"
                ) +
                geom_text(aes(label = Quantidade), hjust = -0.2, size = 3.2) +
                coord_flip() +
                scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
                labs(
                    title = "Quantidade de Serventias por CEP",
                    subtitle = sprintf("Exibindo %d de %d CEP(s)", nrow(resumo), total_ceps),
                    x = "",
                    y = "Quantidade"
                ) +
                theme_minimal()
            
            girafe(
                ggobj = p,
                width_svg = 9,
                height_svg = max(3, 0.28 * nrow(resumo) + 1.6),
                options = list(
                    opts_hover(css = "fill:#0d6efd;cursor:pointer;"),
                    opts_tooltip(css = "background:#fff;color:#212529;padding:8px 10px;border:1px solid #dee2e6;border-radius:6px;font-size:12px;max-width:420px;"),
                    opts_sizing(rescale = TRUE, width = 1)
                )
            )
        })
        
        # ---- Gerar arquivo CSV ----
        
        output$base_csv_ui <- renderUI({
            ui_csv_serventias(ns, "base_download_csv", nrow(dados_base_filtrados()) > 0)
        })
        
        output$base_download_csv <- downloadHandler(
            
            filename = function() {
                paste0("base-serventias-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
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
        # ABA 2 — ALERTAS - SERVENTIAS
        # =================================================
        
        MSG_SEM_ARQUIVOS <- paste0(
            "Não existe arquivo de alertas de Serventias (alertas_serventias.csv ou alertas_serventias.xlsx)",
            " para processamento. Utilize o botão \"Gerenciar Arquivos\" para enviá-lo."
        )
        
        # Espera o usuário parar de digitar (400 ms) antes de filtrar —
        # antes, cada tecla refiltrava e redesenhava tabela e gráficos.
        codigo_d <- debounce(reactive(input$codigo), 400)
        nome_d <- debounce(reactive(input$nome), 400)
        
        dados_filtrados <- reactive({
            req(dados())
            df <- dados()
            
            col_codigo <- coluna_serventia(df, REGEX_COL_CODIGO_SERVENTIA)
            col_nome <- coluna_serventia(df, REGEX_COL_NOME_SERVENTIA)
            
            if (!is.null(codigo_d()) && str_trim(codigo_d()) != "" && !is.null(col_codigo)) {
                df <- df %>%
                    filter(str_detect(
                        str_to_upper(as.character(.data[[col_codigo]])),
                        fixed(str_to_upper(str_trim(codigo_d())))
                    ))
            }
            
            if (!is.null(nome_d()) && str_trim(nome_d()) != "" && !is.null(col_nome)) {
                df <- df %>%
                    filter(str_detect(
                        str_to_upper(as.character(.data[[col_nome]])),
                        fixed(str_to_upper(str_trim(nome_d())))
                    ))
            }
            
            if (!is.null(input$alerta) && input$alerta != "Todos" && input$alerta %in% names(df)) {
                df <- df %>%
                    filter(!is.na(.data[[input$alerta]]) & str_trim(.data[[input$alerta]]) != "")
            }
            
            if (!is.null(input$conflito) && input$conflito != "Todos" && input$conflito %in% names(df)) {
                df <- df %>%
                    filter(!is.na(.data[[input$conflito]]) & str_trim(.data[[input$conflito]]) != "")
            }
            
            if (!is.null(input$status) && input$status != "Todos") {
                status <- valores_coluna_exibicao(df, REGEX_COL_STATUS)
                if (!is.null(status)) {
                    df <- df[status == input$status, , drop = FALSE]
                }
            }
            
            df
        })
        
        tabela_exibicao <- reactive({
            consolidar_alertas_serventias_tabela(dados_filtrados())
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
        
        # ---- Gráficos: tipo de Alerta/Conflito (echarts4r) e Status (ggiraph) ----
        
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
                    mensagem = "Nenhuma coluna de Alerta/Conflito encontrada no arquivo carregado."
                ))
            }
            
            resumo <- map_dfr(colunas_tipo, function(col) {
                valores <- df[[col]]
                tibble(Tipo = col, Quantidade = sum(!is.na(valores) & str_trim(valores) != ""))
            }) %>%
                filter(Quantidade > 0) %>%
                arrange(Quantidade) %>%
                # Nomes longos (ex.: "Alerta: Geolocalização (...)") são
                # quebrados em linhas para não espremer as barras.
                mutate(Tipo = str_wrap(Tipo, 50))
            
            if (nrow(resumo) == 0) {
                return(list(resumo = NULL, mensagem = "Nenhum alerta/conflito encontrado nos dados atuais."))
            }
            
            list(resumo = resumo, mensagem = NULL)
        })
        
        resumo_status <- reactive({
            df <- dados_filtrados()
            
            if (nrow(df) == 0) {
                return(list(resumo = NULL, mensagem = MSG_SEM_ARQUIVOS))
            }
            
            valores <- valores_coluna_exibicao(df, REGEX_COL_STATUS)
            
            if (is.null(valores)) {
                return(list(resumo = NULL, mensagem = "Coluna \"Status\" não encontrada no arquivo carregado."))
            }
            
            resumo <- tibble(Status = valores) %>%
                count(Status, name = "Quantidade") %>%
                arrange(Quantidade)
            
            list(resumo = resumo, mensagem = NULL)
        })
        
        output$grafico_ui <- renderUI({
            r <- resumo_tipos()
            n <- if (is.null(r$resumo)) 0 else nrow(r$resumo)
            altura <- if (n == 0) 120 else max(320, 40 * n + 110)
            
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
        
        output$grafico_status <- renderGirafe({
            r <- resumo_status()
            
            shiny::validate(need(is.null(r$mensagem), r$mensagem))
            
            resumo <- r$resumo %>%
                mutate(
                    rotulo = str_wrap(Status, 45),
                    rotulo = factor(rotulo, levels = unique(rotulo)),
                    dica = sprintf(
                        "<b>%s</b><br/>%s registro(s)",
                        htmltools::htmlEscape(Status),
                        format(Quantidade, big.mark = ".", decimal.mark = ",")
                    )
                )
            
            p <- ggplot(resumo, aes(x = rotulo, y = Quantidade)) +
                geom_col_interactive(aes(tooltip = dica, data_id = Status), fill = "#2C7FB8") +
                geom_text(aes(label = Quantidade), hjust = -0.2) +
                coord_flip() +
                scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
                labs(title = "Quantidade de Registros por Status", x = "", y = "Quantidade") +
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
        
        output$csv_ui <- renderUI({
            df <- tabela_exibicao()
            ui_csv_serventias(ns, "download_csv", !is.null(df) && nrow(df) > 0)
        })
        
        output$download_csv <- downloadHandler(
            
            filename = function() {
                paste0("alertas-serventias-", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
            },
            
            content = function(file) {
                inicio <- Sys.time()
                
                df <- tabela_exibicao()
                
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
        
        # =================================================
        # ABA 3 — INCONSISTÊNCIAS - SERVENTIAS
        # -------------------------------------------------
        # Calculada sobre dados_base() (todos os registros, sem os
        # filtros da aba Base de Dados).
        # =================================================
        
        inconsistencias <- reactive({
            verificar_inconsistencias_serventias(dados_base(), lotacoes())
        })
        
        sem_lotacoes <- reactive({
            is.null(lotacoes()) || nrow(lotacoes()) == 0
        })
        
        sem_status <- reactive({
            is.null(coluna_serventia(dados_base(), REGEX_COL_STATUS))
        })
        
        mod_inconsistencias_server(
            "inc",
            dados = inconsistencias,
            verificacoes = reactive({
                lista <- list(
                    list(descricao = "O mesmo código em mais de um registro da base. Alta: dois ou mais registros ativos."),
                    list(
                        descricao = "Serventia ativa sem CEP (Média) ou com CEP que não tem 8 dígitos / é sequência repetida (Alta).",
                        indisponivel = is.null(coluna_serventia(dados_base(), REGEX_COL_CEP))
                    ),
                    list(descricao = "Duas ou mais serventias ativas, com códigos diferentes, com o mesmo nome."),
                    list(
                        descricao = paste0(
                            "Serventia com status diferente de \"Ativo\" que ainda é a lotação atual de magistrados ",
                            "(módulo Magistrados) ou servidores (Quadro de Pessoal)."
                        ),
                        indisponivel = sem_status() || sem_lotacoes()
                    )
                )
                setNames(lista, VERIFICACOES_SERVENTIAS)
            }),
            mensagem_sem_base = reactive({
                if (nrow(dados_base()) == 0) MSG_SEM_BASE
            }),
            avisos = reactive({
                if (nrow(dados_base()) > 0 && (sem_status() || sem_lotacoes())) {
                    div(
                        class = "alert alert-warning py-2",
                        style = "font-size: .85rem;",
                        icon("circle-info", class = "me-1"),
                        if (sem_status()) {
                            "A base de Serventias não tem a coluna Status: a verificação de serventia inativa com lotação ativa não pôde ser feita."
                        } else {
                            paste0(
                                "Nenhum arquivo de Magistrados ou do Quadro de Pessoal foi encontrado: a verificação de ",
                                "serventia inativa com lotação ativa não pôde ser feita. Envie-os nos respectivos módulos ",
                                "e clique em \"Atualizar Dados\"."
                            )
                        }
                    )
                }
            }),
            col_entidade = "Código Serventia",
            rotulo_entidade = "serventia(s)",
            filtros_texto = FILTROS_TEXTO_INC_SERVENTIAS,
            prefixo_csv = "inconsistencias-serventias"
        )
        
    })
}
