# =====================================================
# modules/mod_painel.R
# -----------------------------------------------------
# Módulo "Painel" — perfil do quadro ATUAL de Magistrados e Servidores
# (Quadro de Pessoal e Auxiliar), no estilo dos painéis temáticos do
# IBGE (ex.: Painel Cor ou Raça): destaques em cartões com o percentual
# de cada categoria, pirâmide etária por sexo, distribuições em barras e
# comparativo Magistrados × Servidores.
#
# Indicadores: Faixa etária, Sexo, Identidade de gênero, Raça/cor,
# Deficiência e Cargo.
#
# FONTES (as mesmas dos outros módulos, por empresa):
#   - Magistrados: <EMPRESA>/magistrados/magistrados.xlsx (ou .csv);
#   - Servidores:  <EMPRESA>/pessoal_auxiliar/pessoal_auxiliar.xlsx (ou
#     .csv) ou, sem essa base, os arquivos de alertas de Pessoal e
#     Auxiliar (mesma regra de registros_pessoal_empresa()).
#
# QUEM ENTRA NO PAINEL: cada CPF uma vez, pelo seu registro mais recente
# (maior "Data de início da situação"), desde que essa situação não seja
# de saída (aposentadoria, falecimento, exoneração/vacância, remoção,
# cessão... — SITUACOES_SERV_ENCERRADAS / SITUACOES_MAG_ENCERRADAS) e,
# havendo coluna Status, que ela não diga "Inativo". Registros marcados
# como "Exclusão de registro por erro" ficam de fora. Sexo, identidade
# de gênero, raça/cor, deficiência e data de nascimento vêm do registro
# mais recente do CPF em que o campo está preenchido.
#
# CÓDIGOS: conforme o Manual do MPM/CNJ (Sexo M/F/I/N; Identidade de
# gênero 1–9; Raça/cor 1–7; Deficiência 1–10, vários códigos separados
# por vírgula). Valores já por extenso também são reconhecidos. Para
# ajustar uma nomenclatura, edite os vetores abaixo.
#
# Os gráficos são HTML/CSS puro (sem pacote de gráficos), para não
# acrescentar dependência ao deploy.
#
# Depende de funções de mod_pessoal_auxiliar.R e mod_magistrados.R — por
# isso é carregado depois deles no app.R.
# =====================================================

local({

    dependencias <- c(
        "coluna_por_regex", "codigo_limpo", "converter_data", "status_ativo",
        "normalizar_nome_coluna", "normalizar_texto_busca", "remover_registros_excluidos",
        "codigo_para_nomenclatura", "carregar_base_pessoal_empresa", "carregar_alertas",
        "caminho_alertas", "registros_pessoal_empresa", "arquivo_base_pessoal",
        "pasta_pessoal", "carregar_arquivo_magistrados", "mag_arquivo_do_tipo",
        "pasta_magistrados", "CARGO_SERVIDOR", "CARGO_MAGISTRADO",
        "SITUACOES_SERV_ENCERRADAS", "SITUACOES_MAG_ENCERRADAS",
        "REGEX_COL_CPF", "REGEX_COL_CARGO", "REGEX_COL_SITUACAO", "REGEX_COL_SEXO",
        "REGEX_COL_INICIO_SITUACAO", "REGEX_COL_STATUS_PESSOAL", "REGEX_COL_RACA_PESSOAL"
    )

    faltando <- dependencias[!vapply(dependencias, exists, logical(1))]

    if (length(faltando) > 0) {
        stop(
            "mod_painel.R precisa ser carregado depois de mod_pessoal_auxiliar.R e ",
            "mod_magistrados.R. Não encontrado: ",
            paste(faltando, collapse = ", ")
        )
    }

})

# =====================================================
# PÚBLICOS E COLUNAS
# =====================================================

PAINEL_MAGISTRADOS <- "Magistrados"
PAINEL_SERVIDORES <- "Servidores"
PAINEL_TODOS <- "Todos"
PAINEL_PUBLICOS <- c(PAINEL_TODOS, PAINEL_MAGISTRADOS, PAINEL_SERVIDORES)

PAINEL_COR_PUBLICO <- c("Magistrados" = PALETA_GRAFICOS[["azul"]], "Servidores" = PALETA_GRAFICOS[["ciano"]])

# Nome normalizado (sem acento, minúsculo — normalizar_nome_coluna()).
REGEX_COL_NASCIMENTO <- "^data (de )?nascimento"
REGEX_COL_IDENTIDADE_GENERO <- "^identidade de genero"
REGEX_COL_DEFICIENCIA <- "^(tipo de )?deficiencia|^pessoa com deficiencia"
REGEX_COL_RACA_PAINEL <- "^raca|^cor( |$)"

# =====================================================
# CÓDIGO -> NOMENCLATURA (Manual do MPM/CNJ)
# =====================================================

PAINEL_EM_BRANCO <- "Em branco"
PAINEL_NAO_INFORMADO <- "Não informado"
PAINEL_NAO_DECLARADO <- "Não declarado pelo respondente"

# Categorias "sem informação": sempre no fim, em cinza.
PAINEL_SEM_INFO <- c(PAINEL_NAO_INFORMADO, PAINEL_NAO_DECLARADO, PAINEL_EM_BRANCO)

SEXO_PAINEL <- c(
    "M" = "Masculino",
    "F" = "Feminino",
    "I" = "Intersexo",
    "N" = PAINEL_NAO_INFORMADO,
    "NDR" = PAINEL_NAO_DECLARADO
)

IDENTIDADE_GENERO_PAINEL <- c(
    "1" = "Cisgênero",
    "2" = "Transgênero",
    "3" = "Transexual",
    "4" = "Travesti",
    "5" = "Gênero fluido",
    "6" = "Agênero",
    "7" = "Outra",
    "8" = PAINEL_NAO_INFORMADO,
    "9" = PAINEL_NAO_DECLARADO
)

RACA_COR_PAINEL <- c(
    "1" = "Amarelo(a)",
    "2" = "Branco(a)",
    "3" = "Indígena",
    "4" = "Negro(a)-Pardo(a)",
    "5" = "Negro(a)-Preto(a)",
    "6" = PAINEL_NAO_INFORMADO,
    "7" = PAINEL_NAO_DECLARADO
)

# Pessoas negras = pretas + pardas (mesmo critério do IBGE).
RACA_NEGROS_PAINEL <- c("Negro(a)-Pardo(a)", "Negro(a)-Preto(a)")

DEFICIENCIA_PAINEL <- c(
    "1"  = "Física/motora",
    "2"  = "Auditiva",
    "3"  = "Visual",
    "4"  = "Intelectual",
    "5"  = "Psicossocial",
    "6"  = "Mental",
    "7"  = "Outras deficiências",
    "8"  = "Não possui",
    "9"  = PAINEL_NAO_INFORMADO,
    "10" = PAINEL_NAO_DECLARADO
)

TIPOS_DEFICIENCIA_PAINEL <- unname(DEFICIENCIA_PAINEL[as.character(1:7)])
PAINEL_COM_DEFICIENCIA <- "Com deficiência"
PAINEL_SEM_DEFICIENCIA <- "Sem deficiência"

# Faixas etárias: de 10 em 10 anos (cartões) e de 5 em 5 (pirâmide).
FAIXAS_10_PAINEL <- list(
    "Até 29 anos" = c(0, 29),
    "30 a 39 anos" = c(30, 39),
    "40 a 49 anos" = c(40, 49),
    "50 a 59 anos" = c(50, 59),
    "60 a 69 anos" = c(60, 69),
    "70 anos ou mais" = c(70, Inf)
)

FAIXAS_5_PAINEL <- list(
    "Até 24" = c(0, 24),
    "25 a 29" = c(25, 29),
    "30 a 34" = c(30, 34),
    "35 a 39" = c(35, 39),
    "40 a 44" = c(40, 44),
    "45 a 49" = c(45, 49),
    "50 a 54" = c(50, 54),
    "55 a 59" = c(55, 59),
    "60 a 64" = c(60, 64),
    "65 a 69" = c(65, 69),
    "70 a 74" = c(70, 74),
    "75 ou mais" = c(75, Inf)
)

# Idades fora deste intervalo são tratadas como data de nascimento
# inválida (erro de digitação).
PAINEL_IDADE_MIN <- 14
PAINEL_IDADE_MAX <- 100

# Paleta categórica (distingue categorias; não representa cor de pele).
# Paleta padrão da aplicação (R/utils.R), com tons mais claros para
# as categorias que passarem de cinco.
PAINEL_PALETA <- paleta_graficos(15)
PAINEL_COR_SEM_INFO <- COR_GRAFICO_SEM_INFO

PAINEL_CORES_SEXO <- c(
    "Feminino" = PALETA_GRAFICOS[["laranja"]],
    "Masculino" = PALETA_GRAFICOS[["azul"]],
    "Intersexo" = PALETA_GRAFICOS[["verde"]]
)

PAINEL_CORES_RACA <- c(
    "Branco(a)" = PALETA_GRAFICOS[["azul"]],
    "Negro(a)-Pardo(a)" = PALETA_GRAFICOS[["laranja"]],
    "Negro(a)-Preto(a)" = PALETA_GRAFICOS[["grafite"]],
    "Amarelo(a)" = PALETA_GRAFICOS[["ciano"]],
    "Indígena" = PALETA_GRAFICOS[["verde"]]
)

# =====================================================
# TRADUÇÃO DE VALORES
# =====================================================

# Código -> nomenclatura; valores já por extenso (com ou sem acento)
# também casam com a nomenclatura. Vazio -> "Em branco". O que não for
# reconhecido fica como veio.
traduzir_dominio_painel <- function(valores, tabela) {

    v <- str_trim(as.character(valores))
    vazio <- is.na(v) | v == ""

    out <- codigo_para_nomenclatura(v, tabela)

    # Por extenso: compara sem acento/maiúsculas com as nomenclaturas.
    chave_tab <- normalizar_texto_busca(unname(tabela))
    pos <- match(normalizar_texto_busca(v), chave_tab)
    out <- ifelse(!is.na(pos), unname(tabela)[pos], out)

    # "Não informado"/"Não declarado" escritos de outro jeito.
    norm <- normalizar_texto_busca(v)
    out[!is.na(norm) & str_detect(norm, "^nao (informad|inform)")] <- PAINEL_NAO_INFORMADO
    out[!is.na(norm) & (str_detect(norm, "^nao declarad") | norm == "ndr")] <- PAINEL_NAO_DECLARADO

    out[vazio] <- PAINEL_EM_BRANCO
    out

}

traduzir_sexo_painel <- function(valores) {

    v <- toupper(str_trim(as.character(valores)))
    norm <- normalizar_texto_busca(v)

    cod <- dplyr::case_when(
        is.na(v) | v == "" ~ NA_character_,
        v %in% names(SEXO_PAINEL) ~ v,
        str_detect(norm, "^masc") ~ "M",
        str_detect(norm, "^fem") ~ "F",
        str_detect(norm, "^inter") ~ "I",
        str_detect(norm, "^nao declarad") ~ "NDR",
        str_detect(norm, "^nao inform") ~ "N",
        TRUE ~ NA_character_
    )

    out <- unname(SEXO_PAINEL[cod])
    out[is.na(out)] <- as.character(valores)[is.na(out)]
    out[is.na(v) | v == ""] <- PAINEL_EM_BRANCO
    out

}

# Deficiência pode trazer vários códigos ("1, 3"). Devolve uma lista com
# as nomenclaturas de cada pessoa (sem repetição).
tipos_deficiencia_painel <- function(valores) {

    lapply(as.character(valores), function(x) {
        if (is.na(x) || str_trim(x) == "") {
            return(PAINEL_EM_BRANCO)
        }
        partes <- str_trim(unlist(str_split(x, "[,;/|]+")))
        partes <- partes[partes != ""]
        if (length(partes) == 0) {
            return(PAINEL_EM_BRANCO)
        }
        out <- traduzir_dominio_painel(partes, DEFICIENCIA_PAINEL)
        # "Não", "N", "Nenhuma" = não possui deficiência.
        norm <- normalizar_texto_busca(partes)
        out[norm %in% c("n", "nao", "nenhuma", "nenhum", "nao possui")] <- "Não possui"
        unique(out)
    })

}

# Uma classificação por pessoa: "Com deficiência" se tiver algum tipo
# (códigos 1 a 7 ou outro texto não reconhecido como "sem informação");
# senão, "Sem deficiência" (código 8) ou a categoria sem informação.
classificar_deficiencia_painel <- function(tipos) {

    vapply(tipos, function(t) {
        if (any(t %in% TIPOS_DEFICIENCIA_PAINEL)) return(PAINEL_COM_DEFICIENCIA)
        if (any(t == "Não possui")) return(PAINEL_SEM_DEFICIENCIA)
        sem <- intersect(PAINEL_SEM_INFO, t)
        if (length(sem) > 0) return(sem[1])
        PAINEL_COM_DEFICIENCIA
    }, character(1))

}

idade_em_anos <- function(nascimento, referencia = Sys.Date()) {

    anos <- as.integer(format(referencia, "%Y")) - as.integer(format(nascimento, "%Y"))
    antes_aniversario <- format(referencia, "%m%d") < format(nascimento, "%m%d")
    idade <- anos - as.integer(antes_aniversario)

    idade[!is.na(idade) & (idade < PAINEL_IDADE_MIN | idade > PAINEL_IDADE_MAX)] <- NA_integer_
    idade

}

faixa_etaria_painel <- function(idade, faixas = FAIXAS_10_PAINEL) {

    out <- rep(NA_character_, length(idade))
    for (nome in names(faixas)) {
        lim <- faixas[[nome]]
        out[!is.na(idade) & idade >= lim[1] & idade <= lim[2]] <- nome
    }
    out

}

# =====================================================
# QUADRO ATUAL (uma linha por pessoa)
# =====================================================

pessoas_painel_vazio <- function() {
    tibble(
        publico = character(0), CPF = character(0), cargo = character(0),
        sexo = character(0), genero = character(0), raca = character(0),
        deficiencia = character(0), tipos_deficiencia = list(),
        idade = integer(0), faixa = character(0), faixa5 = character(0)
    )
}

ultimo_nao_vazio <- function(x) {
    x <- x[!is.na(x) & str_trim(x) != ""]
    if (length(x) > 0) x[length(x)] else NA_character_
}

# Devolve list(pessoas = tibble, colunas = vetor lógico com as colunas
# encontradas no arquivo).
pessoas_painel <- function(df, publico, encerradas, tabela_cargo, hoje = Sys.Date()) {

    colunas <- c(
        nascimento = FALSE, sexo = FALSE, genero = FALSE,
        raca = FALSE, deficiencia = FALSE, cargo = FALSE
    )

    vazio <- list(pessoas = pessoas_painel_vazio(), colunas = colunas)

    df <- remover_registros_excluidos(df)

    if (is.null(df) || nrow(df) == 0 || is.null(coluna_por_regex(df, REGEX_COL_CPF))) {
        return(vazio)
    }

    valor <- function(regex) {
        c <- coluna_por_regex(df, regex)
        if (is.null(c)) rep(NA_character_, nrow(df)) else as.character(df[[c]])
    }

    tem <- function(regex) !is.null(coluna_por_regex(df, regex))

    colunas[] <- c(
        tem(REGEX_COL_NASCIMENTO), tem(REGEX_COL_SEXO), tem(REGEX_COL_IDENTIDADE_GENERO),
        tem(REGEX_COL_RACA_PAINEL), tem(REGEX_COL_DEFICIENCIA), tem(REGEX_COL_CARGO)
    )

    tem_status <- tem(REGEX_COL_STATUS_PESSOAL)

    # CPF só com dígitos e 11 posições (o Excel tira o zero à esquerda).
    cpf <- gsub("\\D", "", valor(REGEX_COL_CPF))
    cpf <- ifelse(nchar(cpf) > 0 & nchar(cpf) < 11, formatC(cpf, width = 11, flag = "0"), cpf)
    cpf <- gsub(" ", "0", cpf)

    d <- tibble(
        CPF = cpf,
        situacao = codigo_limpo(valor(REGEX_COL_SITUACAO)),
        status = valor(REGEX_COL_STATUS_PESSOAL),
        inicio = converter_data(valor(REGEX_COL_INICIO_SITUACAO)),
        cargo = valor(REGEX_COL_CARGO),
        nascimento = valor(REGEX_COL_NASCIMENTO),
        sexo = valor(REGEX_COL_SEXO),
        genero = valor(REGEX_COL_IDENTIDADE_GENERO),
        raca = valor(REGEX_COL_RACA_PAINEL),
        deficiencia = valor(REGEX_COL_DEFICIENCIA)
    ) %>%
        filter(!is.na(CPF) & CPF != "")

    if (nrow(d) == 0) {
        return(vazio)
    }

    # Mesma ordem de ultimo_registro_por_cpf(): registros sem data antes,
    # depois os datados do mais antigo ao mais recente; o último vence.
    atual <- d %>%
        mutate(.ordem = row_number(), .tem_data = !is.na(inicio)) %>%
        group_by(CPF) %>%
        arrange(.tem_data, inicio, .ordem, .by_group = TRUE) %>%
        summarise(
            situacao = last(situacao),
            status = last(status),
            cargo = last(cargo),
            across(c(nascimento, sexo, genero, raca, deficiencia), ultimo_nao_vazio),
            .groups = "drop"
        ) %>%
        filter(is.na(situacao) | situacao == "" | !(situacao %in% encerradas))

    if (tem_status) {
        atual <- atual %>% filter(is.na(status) | str_trim(status) == "" | status_ativo(status))
    }

    if (nrow(atual) == 0) {
        return(list(pessoas = pessoas_painel_vazio(), colunas = colunas))
    }

    idade <- idade_em_anos(converter_data(atual$nascimento), hoje)
    tipos <- tipos_deficiencia_painel(atual$deficiencia)

    cargo <- codigo_para_nomenclatura(codigo_limpo(atual$cargo), tabela_cargo)
    cargo[is.na(cargo) | str_trim(cargo) == ""] <- PAINEL_EM_BRANCO

    pessoas <- tibble(
        publico = publico,
        CPF = atual$CPF,
        cargo = if (colunas[["cargo"]]) cargo else NA_character_,
        sexo = if (colunas[["sexo"]]) traduzir_sexo_painel(atual$sexo) else NA_character_,
        genero = if (colunas[["genero"]]) traduzir_dominio_painel(atual$genero, IDENTIDADE_GENERO_PAINEL) else NA_character_,
        raca = if (colunas[["raca"]]) traduzir_dominio_painel(atual$raca, RACA_COR_PAINEL) else NA_character_,
        deficiencia = if (colunas[["deficiencia"]]) classificar_deficiencia_painel(tipos) else NA_character_,
        tipos_deficiencia = if (colunas[["deficiencia"]]) tipos else rep(list(character(0)), nrow(atual)),
        idade = if (colunas[["nascimento"]]) idade else NA_integer_,
        faixa = faixa_etaria_painel(if (colunas[["nascimento"]]) idade else rep(NA_integer_, nrow(atual))),
        faixa5 = faixa_etaria_painel(if (colunas[["nascimento"]]) idade else rep(NA_integer_, nrow(atual)), FAIXAS_5_PAINEL)
    )

    list(pessoas = pessoas, colunas = colunas)

}

# Magistrados + Servidores da empresa. `colunas` é uma matriz lógica
# (público × coluna).
carregar_painel_empresa <- function(empresa, hoje = Sys.Date()) {

    mag <- tryCatch(
        pessoas_painel(
            carregar_arquivo_magistrados(mag_arquivo_do_tipo(pasta_magistrados(empresa), "base")),
            PAINEL_MAGISTRADOS, SITUACOES_MAG_ENCERRADAS, CARGO_MAGISTRADO, hoje
        ),
        error = function(e) pessoas_painel(NULL, PAINEL_MAGISTRADOS, character(0), CARGO_MAGISTRADO)
    )

    srv <- tryCatch(
        pessoas_painel(
            registros_pessoal_empresa(
                carregar_base_pessoal_empresa(empresa),
                carregar_alertas(caminho_alertas(empresa))
            ),
            PAINEL_SERVIDORES, SITUACOES_SERV_ENCERRADAS, CARGO_SERVIDOR, hoje
        ),
        error = function(e) pessoas_painel(NULL, PAINEL_SERVIDORES, character(0), CARGO_SERVIDOR)
    )

    list(
        pessoas = bind_rows(mag$pessoas, srv$pessoas),
        colunas = rbind(Magistrados = mag$colunas, Servidores = srv$colunas)
    )

}

# Arquivos que alimentam o painel (para saber se algo mudou e recarregar).
arquivos_painel_empresa <- function(empresa) {

    pasta_alertas <- caminho_alertas(empresa)

    arquivos <- c(
        mag_arquivo_do_tipo(pasta_magistrados(empresa), "base"),
        arquivo_base_pessoal(pasta_pessoal(empresa)),
        if (dir.exists(pasta_alertas)) list.files(pasta_alertas, full.names = TRUE)
    )

    if (length(arquivos) == 0) {
        return("")
    }

    info <- file.info(arquivos)
    paste(arquivos, info$size, as.numeric(info$mtime), collapse = "|")

}

# =====================================================
# RESUMOS
# =====================================================

# Quantidade e percentual por categoria. As maiores primeiro, as "sem
# informação" no fim (ou a ordem fixa `ordem`, quando informada).
resumo_painel <- function(x, ordem = NULL) {

    x <- x[!is.na(x)]
    total <- length(x)

    if (total == 0) {
        return(tibble(categoria = character(0), n = integer(0), pct = numeric(0)))
    }

    r <- tibble(categoria = x) %>%
        count(categoria, name = "n") %>%
        mutate(pct = 100 * n / total)

    if (!is.null(ordem)) {
        r <- r %>%
            mutate(.pos = match(categoria, ordem)) %>%
            arrange(is.na(.pos), .pos, desc(n)) %>%
            select(-.pos)
    } else {
        r <- r %>%
            mutate(.sem = match(categoria, PAINEL_SEM_INFO)) %>%
            arrange(!is.na(.sem), .sem, desc(n), categoria) %>%
            select(-.sem)
    }

    r

}

# Tipos de deficiência entre as pessoas com deficiência (uma pessoa pode
# ter mais de um tipo, então a soma pode passar de 100%).
resumo_tipos_deficiencia <- function(p) {

    com <- p %>% filter(deficiencia == PAINEL_COM_DEFICIENCIA)
    total <- nrow(com)

    if (total == 0) {
        return(tibble(categoria = character(0), n = integer(0), pct = numeric(0)))
    }

    tipos <- unlist(lapply(com$tipos_deficiencia, function(t) setdiff(t, c(PAINEL_SEM_INFO, "Não possui"))))

    tibble(categoria = tipos) %>%
        count(categoria, name = "n") %>%
        mutate(.pos = match(categoria, TIPOS_DEFICIENCIA_PAINEL), pct = 100 * n / total) %>%
        arrange(desc(n), .pos) %>%
        select(-.pos)

}

cores_painel <- function(categorias, fixas = NULL) {

    cores <- character(length(categorias))
    livres <- setdiff(PAINEL_PALETA, fixas)
    j <- 1

    for (i in seq_along(categorias)) {
        cat_i <- categorias[i]
        if (cat_i %in% PAINEL_SEM_INFO) {
            cores[i] <- PAINEL_COR_SEM_INFO
        } else if (!is.null(fixas) && cat_i %in% names(fixas)) {
            cores[i] <- fixas[[cat_i]]
        } else {
            cores[i] <- livres[(j - 1) %% length(livres) + 1]
            j <- j + 1
        }
    }

    stats::setNames(cores, categorias)

}

# =====================================================
# FORMATAÇÃO (padrão brasileiro)
# =====================================================

fmt_int <- function(x) {
    formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
}

fmt_pct <- function(x, digitos = 1) {
    ifelse(
        is.na(x), "–",
        paste0(formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ","), "%")
    )
}

fmt_num <- function(x, digitos = 1) {
    ifelse(is.na(x), "–", formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ","))
}

# =====================================================
# COMPONENTES VISUAIS (HTML/CSS)
# =====================================================

# Cartões de destaque, um por categoria (estilo IBGE): percentual grande,
# nome da categoria e quantidade.
painel_cartoes <- function(resumo, cores, max_cartoes = 8) {

    if (nrow(resumo) == 0) {
        return(NULL)
    }

    r <- head(resumo, max_cartoes)

    div(
        class = "pn-cartoes",
        lapply(seq_len(nrow(r)), function(i) {
            div(
                class = "pn-cartao",
                style = paste0("--pn-cor:", cores[[r$categoria[i]]], ";"),
                div(class = "pn-cartao-pct", fmt_pct(r$pct[i])),
                div(class = "pn-cartao-rotulo", r$categoria[i]),
                div(class = "pn-cartao-n", paste(fmt_int(r$n[i]), if (r$n[i] == 1) "pessoa" else "pessoas"))
            )
        })
    )

}

# Barras horizontais: comprimento relativo à maior categoria; o texto
# mostra quantidade e percentual.
painel_barras <- function(resumo, cores, rotulo_pct = "do total") {

    if (nrow(resumo) == 0) {
        return(div(class = "pn-vazio", "Sem dados para exibir."))
    }

    maior <- max(resumo$n)

    div(
        class = "pn-barras",
        role = "list",
        lapply(seq_len(nrow(resumo)), function(i) {
            largura <- if (maior > 0) 100 * resumo$n[i] / maior else 0
            div(
                class = "pn-barra-linha",
                role = "listitem",
                title = sprintf("%s: %s (%s %s)", resumo$categoria[i], fmt_int(resumo$n[i]), fmt_pct(resumo$pct[i]), rotulo_pct),
                div(class = "pn-barra-rotulo", resumo$categoria[i]),
                div(
                    class = "pn-barra-trilho",
                    div(
                        class = "pn-barra-preenchida",
                        style = sprintf("width:%.2f%%;background:%s;", largura, cores[[resumo$categoria[i]]])
                    )
                ),
                div(
                    class = "pn-barra-valor",
                    tags$b(fmt_pct(resumo$pct[i])),
                    tags$span(class = "pn-barra-n", fmt_int(resumo$n[i]))
                )
            )
        })
    )

}

# Rosca (conic-gradient) com o total no centro.
painel_rosca <- function(resumo, cores, rotulo_centro = "pessoas") {

    if (nrow(resumo) == 0) {
        return(NULL)
    }

    # Fatias pela participação de cada categoria na soma (o percentual
    # da legenda é o do resumo).
    parte <- if (sum(resumo$n) > 0) 100 * resumo$n / sum(resumo$n) else rep(0, nrow(resumo))
    fim <- cumsum(parte)
    ini <- c(0, head(fim, -1))

    gradiente <- paste(
        sprintf("%s %.3f%% %.3f%%", cores[resumo$categoria], ini, fim),
        collapse = ", "
    )

    div(
        class = "pn-rosca-bloco",
        div(
            class = "pn-rosca",
            role = "img",
            `aria-label` = paste(sprintf("%s %s", resumo$categoria, fmt_pct(resumo$pct)), collapse = "; "),
            style = sprintf("background: conic-gradient(%s);", gradiente),
            div(
                class = "pn-rosca-centro",
                div(class = "pn-rosca-total", fmt_int(sum(resumo$n))),
                div(class = "pn-rosca-sub", rotulo_centro)
            )
        ),
        div(
            class = "pn-legenda",
            lapply(seq_len(nrow(resumo)), function(i) {
                div(
                    class = "pn-legenda-item",
                    tags$span(class = "pn-legenda-cor", style = paste0("background:", cores[[resumo$categoria[i]]], ";")),
                    tags$span(resumo$categoria[i]),
                    tags$b(class = "ms-auto", fmt_pct(resumo$pct[i])),
                    tags$span(class = "pn-barra-n", fmt_int(resumo$n[i]))
                )
            })
        )
    )

}

# Barras verticais (7 a 8 categorias): percentual e quantidade acima da
# barra, nome embaixo; a maior à esquerda.
painel_colunas <- function(resumo, cores, rotulo_pct = "do total") {

    if (nrow(resumo) == 0) {
        return(div(class = "pn-vazio", "Sem dados para exibir."))
    }

    maior <- max(resumo$n)

    div(
        class = "pn-colunas",
        role = "list",
        lapply(seq_len(nrow(resumo)), function(i) {
            altura <- if (maior > 0) 100 * resumo$n[i] / maior else 0
            div(
                class = "pn-coluna",
                role = "listitem",
                title = sprintf("%s: %s (%s %s)", resumo$categoria[i], fmt_int(resumo$n[i]), fmt_pct(resumo$pct[i]), rotulo_pct),
                div(
                    class = "pn-coluna-area",
                    div(class = "pn-coluna-valor", tags$b(fmt_pct(resumo$pct[i])), tags$span(class = "pn-barra-n", fmt_int(resumo$n[i]))),
                    div(
                        class = "pn-coluna-barra",
                        style = sprintf("height:%.2f%%;background:%s;", max(altura, 1), cores[[resumo$categoria[i]]])
                    )
                ),
                div(class = "pn-coluna-rotulo", resumo$categoria[i])
            )
        })
    )

}

# Tipo do gráfico pela quantidade de categorias, como no resto da
# aplicação (tipo_grafico_automatico()): rosca até 6, barras verticais
# de 7 a 8 e horizontais de 9 em diante. `permite_pizza = FALSE` quando
# uma pessoa pode estar em mais de uma categoria (as fatias não somariam
# o todo) — fica em barras.
painel_grafico <- function(resumo, cores, rotulo_pct = "do total", rotulo_centro = "pessoas",
                           permite_pizza = TRUE) {

    tipo <- tipo_grafico_automatico(nrow(resumo))
    if (!permite_pizza && tipo == "pizza") tipo <- "barras"

    switch(
        tipo,
        pizza = painel_rosca(resumo, cores, rotulo_centro),
        colunas = painel_colunas(resumo, cores, rotulo_pct),
        painel_barras(resumo, cores, rotulo_pct)
    )

}

# Comparativo Magistrados × Servidores: uma barra 100% empilhada por
# público, com a mesma cor por categoria e legenda única.
painel_comparativo <- function(p, variavel, cores, ordem = NULL) {

    publicos <- intersect(c(PAINEL_MAGISTRADOS, PAINEL_SERVIDORES), unique(p$publico))

    if (length(publicos) < 2) {
        return(NULL)
    }

    resumos <- lapply(publicos, function(pub) {
        resumo_painel(p[[variavel]][p$publico == pub], ordem)
    })
    names(resumos) <- publicos

    if (all(vapply(resumos, nrow, integer(1)) == 0)) {
        return(NULL)
    }

    categorias <- unique(unlist(lapply(resumos, `[[`, "categoria")))
    geral <- resumo_painel(p[[variavel]], ordem)
    categorias <- c(intersect(geral$categoria, categorias), setdiff(categorias, geral$categoria))

    div(
        class = "pn-comparativo",
        div(class = "pn-subtitulo", "Comparativo Magistrados × Servidores"),
        lapply(publicos, function(pub) {
            r <- resumos[[pub]]
            r <- r[match(intersect(categorias, r$categoria), r$categoria), ]
            div(
                class = "pn-comp-linha",
                div(
                    class = "pn-comp-rotulo",
                    tags$b(pub),
                    tags$span(class = "pn-barra-n", paste(fmt_int(sum(r$n)), "pessoas"))
                ),
                div(
                    class = "pn-comp-barra",
                    role = "img",
                    `aria-label` = paste0(pub, ": ", paste(sprintf("%s %s", r$categoria, fmt_pct(r$pct)), collapse = "; ")),
                    lapply(seq_len(nrow(r)), function(i) {
                        div(
                            class = "pn-comp-seg",
                            title = sprintf("%s — %s: %s (%s)", pub, r$categoria[i], fmt_int(r$n[i]), fmt_pct(r$pct[i])),
                            style = sprintf("width:%.3f%%;background:%s;", r$pct[i], cores[[r$categoria[i]]]),
                            if (r$pct[i] >= 8) fmt_pct(r$pct[i], 0)
                        )
                    })
                )
            )
        }),
        div(
            class = "pn-legenda pn-legenda-linha",
            lapply(categorias, function(cat_i) {
                div(
                    class = "pn-legenda-item",
                    tags$span(class = "pn-legenda-cor", style = paste0("background:", cores[[cat_i]], ";")),
                    tags$span(cat_i)
                )
            })
        )
    )

}

# Pirâmide etária por sexo (Feminino à esquerda, Masculino à direita),
# em % do total de pessoas com sexo F/M e idade válida — como na
# pirâmide do IBGE.
painel_piramide <- function(p) {

    d <- p %>% filter(sexo %in% c("Feminino", "Masculino"), !is.na(faixa5))

    if (nrow(d) == 0) {
        return(div(class = "pn-vazio", "Sem pessoas com sexo e data de nascimento válidos para montar a pirâmide."))
    }

    total <- nrow(d)
    contagem <- d %>% count(faixa5, sexo)

    pct <- function(faixa, sx) {
        n <- contagem$n[contagem$faixa5 == faixa & contagem$sexo == sx]
        if (length(n) == 0) 0 else 100 * n / total
    }

    faixas <- rev(names(FAIXAS_5_PAINEL))
    valores <- vapply(faixas, function(f) c(pct(f, "Feminino"), pct(f, "Masculino")), numeric(2))
    maior <- max(valores)
    if (maior == 0) maior <- 1

    tot_f <- sum(d$sexo == "Feminino")
    tot_m <- sum(d$sexo == "Masculino")

    div(
        class = "pn-piramide",
        role = "img",
        `aria-label` = "Pirâmide etária por sexo",
        div(
            class = "pn-pir-cabecalho",
            div(class = "pn-pir-lado", style = paste0("color:", PAINEL_CORES_SEXO[["Feminino"]]),
                tags$b("Feminino"), " ", fmt_pct(100 * tot_f / total)),
            div(class = "pn-pir-meio", "Idade"),
            div(class = "pn-pir-lado text-start", style = paste0("color:", PAINEL_CORES_SEXO[["Masculino"]]),
                tags$b("Masculino"), " ", fmt_pct(100 * tot_m / total))
        ),
        lapply(seq_along(faixas), function(i) {
            f <- valores[1, i]
            m <- valores[2, i]
            div(
                class = "pn-pir-linha",
                div(
                    class = "pn-pir-lado",
                    tags$span(class = "pn-pir-valor", if (f > 0) fmt_pct(f)),
                    div(
                        class = "pn-pir-barra pn-pir-f",
                        title = sprintf("Feminino, %s anos: %s", faixas[i], fmt_pct(f)),
                        style = sprintf("width:%.2f%%;", 82 * f / maior)
                    )
                ),
                div(class = "pn-pir-meio", faixas[i]),
                div(
                    class = "pn-pir-lado text-start",
                    div(
                        class = "pn-pir-barra pn-pir-m",
                        title = sprintf("Masculino, %s anos: %s", faixas[i], fmt_pct(m)),
                        style = sprintf("width:%.2f%%;", 82 * m / maior)
                    ),
                    tags$span(class = "pn-pir-valor", if (m > 0) fmt_pct(m))
                )
            )
        })
    )

}

painel_indicador <- function(icone, valor, rotulo, detalhe = NULL, cor = COR_GRAFICO) {
    div(
        class = "pn-kpi",
        style = paste0("--pn-cor:", cor, ";"),
        div(class = "pn-kpi-icone", icon(icone)),
        div(
            div(class = "pn-kpi-valor", valor),
            div(class = "pn-kpi-rotulo", rotulo),
            if (!is.null(detalhe)) div(class = "pn-kpi-detalhe", detalhe)
        )
    )
}

# Seção temática (um indicador).
painel_secao <- function(id_secao, icone, titulo, descricao, ...) {
    div(
        class = "pn-secao",
        id = id_secao,
        div(
            class = "pn-secao-cabecalho",
            div(class = "pn-secao-icone", icon(icone)),
            div(
                tags$h5(class = "pn-secao-titulo", titulo),
                div(class = "pn-secao-desc", descricao)
            )
        ),
        ...
    )
}

# Aviso de coluna ausente / sem dados para o público selecionado.
painel_aviso_coluna <- function(colunas, coluna, nome_coluna, publicos) {

    faltando <- publicos[!colunas[publicos, coluna]]

    if (length(faltando) == 0) {
        return(NULL)
    }

    div(
        class = "pn-aviso",
        icon("circle-info", class = "me-2"),
        sprintf(
            "A coluna \"%s\" não foi encontrada no arquivo de %s; essas pessoas ficam fora deste indicador.",
            nome_coluna,
            paste(tolower(faltando), collapse = " e de ")
        )
    )

}

# =====================================================
# TABELA PARA EXPORTAÇÃO (CSV)
# =====================================================

tabela_exportacao_painel <- function(p) {

    indicadores <- c(
        "Faixa etária" = "faixa",
        "Sexo" = "sexo",
        "Identidade de gênero" = "genero",
        "Raça/cor" = "raca",
        "Deficiência" = "deficiencia",
        "Cargo" = "cargo"
    )

    ordem_faixa <- c(names(FAIXAS_10_PAINEL))

    grupos <- c(list(Todos = p), split(p, p$publico))

    linhas <- list()

    for (pub in names(grupos)) {
        g <- grupos[[pub]]
        for (ind in names(indicadores)) {
            var <- indicadores[[ind]]
            r <- resumo_painel(g[[var]], if (var == "faixa") ordem_faixa else NULL)
            if (nrow(r) > 0) {
                linhas[[length(linhas) + 1]] <- tibble(
                    `Público` = pub, Indicador = ind, Categoria = r$categoria,
                    Quantidade = r$n, `Percentual (%)` = round(r$pct, 2)
                )
            }
        }
        r <- resumo_tipos_deficiencia(g)
        if (nrow(r) > 0) {
            linhas[[length(linhas) + 1]] <- tibble(
                `Público` = pub, Indicador = "Tipo de deficiência (entre pessoas com deficiência)",
                Categoria = r$categoria, Quantidade = r$n, `Percentual (%)` = round(r$pct, 2)
            )
        }
    }

    bind_rows(linhas)

}

# =====================================================
# UI
# =====================================================

CSS_PAINEL <- "
#ID .al-header { display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:1rem; margin-bottom:1.25rem; }
#ID .al-titulo { display:flex; align-items:center; gap:.65rem; }
#ID .al-titulo-icone { width:44px; height:44px; border-radius:50%; display:flex; align-items:center; justify-content:center; background:linear-gradient(135deg,#003366,#0d6efd); color:#fff; font-size:1.1rem; flex-shrink:0; box-shadow:0 .3rem .8rem rgba(13,110,253,.2); }
#ID .al-titulo h4 { margin:0; font-weight:700; letter-spacing:-.01em; }
#ID .al-acoes { display:flex; gap:.6rem; flex-wrap:wrap; }
#ID .btn-acao { font-weight:600; border-radius:.6rem; padding:.55rem 1.1rem; }

#ID .pn-filtros { background:#fff; border:1px solid rgba(0,0,0,.06); border-radius:.9rem; box-shadow:0 .2rem .6rem rgba(15,23,42,.05); padding:1rem 1.25rem .25rem; margin-bottom:1rem; display:flex; flex-wrap:wrap; gap:1.5rem; align-items:flex-end; }
#ID .pn-filtros .form-group { margin-bottom:.75rem; }
#ID .pn-filtros .control-label { font-weight:600; font-size:.8rem; text-transform:uppercase; letter-spacing:.04em; color:#6c757d; }
#ID .pn-segmentado .shiny-input-container-inline .control-label { display:block; }
#ID .pn-segmentado .shiny-options-group { display:inline-flex; border:1px solid #ced4da; border-radius:.6rem; overflow:hidden; background:#fff; }
#ID .pn-segmentado label.radio-inline { margin:0; padding:0; cursor:pointer; position:relative; }
#ID .pn-segmentado label.radio-inline input { position:absolute; opacity:0; width:1px; height:1px; }
#ID .pn-segmentado label.radio-inline span { display:block; padding:.45rem 1.1rem; font-weight:600; color:#495057; border-right:1px solid #ced4da; }
#ID .pn-segmentado label.radio-inline:last-child span { border-right:0; }
#ID .pn-segmentado label.radio-inline:hover span { background:#eef3f9; }
#ID .pn-segmentado label.radio-inline input:checked + span { background:#003366; color:#fff; }
#ID .pn-segmentado label.radio-inline input:focus-visible + span { outline:2px solid #6ea8fe; outline-offset:-2px; }

#ID .pn-temas { position:sticky; top:0; z-index:5; display:flex; flex-wrap:wrap; gap:.5rem; padding:.6rem 0; margin-bottom:1rem; background:#f4f6f9; }
#ID .pn-tema { border:1px solid #d0d7e2; background:#fff; color:#003366; border-radius:999px; padding:.35rem .9rem; font-size:.85rem; font-weight:600; text-decoration:none; }
#ID .pn-tema:hover, #ID .pn-tema:focus-visible { background:#003366; color:#fff; }

#ID .pn-kpis { display:grid; grid-template-columns:repeat(auto-fit,minmax(190px,1fr)); gap:1rem; margin-bottom:1.5rem; }
#ID .pn-kpi { background:#fff; border:1px solid rgba(0,0,0,.06); border-radius:.9rem; box-shadow:0 .2rem .6rem rgba(15,23,42,.05); padding:1rem; display:flex; gap:.85rem; align-items:flex-start; border-top:4px solid var(--pn-cor); }
#ID .pn-kpi-icone { width:40px; height:40px; border-radius:50%; flex-shrink:0; display:flex; align-items:center; justify-content:center; color:var(--pn-cor); background:color-mix(in srgb, var(--pn-cor) 12%, #fff); }
#ID .pn-kpi-valor { font-size:1.6rem; font-weight:700; line-height:1.1; color:#1f2d3d; font-variant-numeric:tabular-nums; }
#ID .pn-kpi-rotulo { font-size:.85rem; color:#495057; font-weight:600; }
#ID .pn-kpi-detalhe { font-size:.78rem; color:#6c757d; margin-top:.15rem; }

#ID .pn-secao { background:#fff; border:1px solid rgba(0,0,0,.06); border-radius:.9rem; box-shadow:0 .2rem .6rem rgba(15,23,42,.05); padding:1.25rem; margin-bottom:1.5rem; scroll-margin-top:60px; }
#ID .pn-secao-cabecalho { display:flex; gap:.75rem; align-items:flex-start; margin-bottom:1rem; padding-bottom:.75rem; border-bottom:1px solid #eef1f4; }
#ID .pn-secao-icone { width:36px; height:36px; border-radius:.6rem; background:#e7eef7; color:#003366; display:flex; align-items:center; justify-content:center; flex-shrink:0; }
#ID .pn-secao-titulo { margin:0; font-weight:700; color:#003366; }
#ID .pn-secao-desc { font-size:.85rem; color:#6c757d; }
#ID .pn-subtitulo { font-weight:600; font-size:.8rem; text-transform:uppercase; letter-spacing:.04em; color:#6c757d; margin:1rem 0 .6rem; }
#ID .pn-grade { display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); gap:1.5rem; align-items:start; }
@media (max-width: 992px) { #ID .pn-grade { grid-template-columns:minmax(0,1fr); } }

#ID .pn-cartoes { display:grid; grid-template-columns:repeat(auto-fill,minmax(140px,1fr)); gap:.75rem; }
#ID .pn-cartao { border-radius:.75rem; padding:.85rem .9rem; background:color-mix(in srgb, var(--pn-cor) 10%, #fff); border-left:5px solid var(--pn-cor); }
#ID .pn-cartao-pct { font-size:1.55rem; font-weight:700; color:#1f2d3d; line-height:1.1; font-variant-numeric:tabular-nums; }
#ID .pn-cartao-rotulo { font-size:.85rem; font-weight:600; color:#343a40; margin-top:.2rem; }
#ID .pn-cartao-n { font-size:.75rem; color:#6c757d; }

#ID .pn-barras { display:flex; flex-direction:column; gap:.55rem; }
#ID .pn-barra-linha { display:grid; grid-template-columns:minmax(110px,38%) 1fr auto; gap:.6rem; align-items:center; font-size:.85rem; }
#ID .pn-barra-rotulo { color:#343a40; line-height:1.2; }
#ID .pn-barra-trilho { background:#eef1f4; border-radius:4px; height:18px; overflow:hidden; }
#ID .pn-barra-preenchida { height:100%; border-radius:4px; min-width:2px; }
#ID .pn-barra-valor { text-align:right; min-width:92px; font-variant-numeric:tabular-nums; }
#ID .pn-barra-n { color:#6c757d; font-size:.78rem; margin-left:.35rem; }

#ID .pn-rosca-bloco { display:flex; gap:1.5rem; align-items:center; flex-wrap:wrap; }
#ID .pn-rosca { width:170px; height:170px; border-radius:50%; position:relative; flex-shrink:0; }
#ID .pn-rosca-centro { position:absolute; inset:26px; background:#fff; border-radius:50%; display:flex; flex-direction:column; align-items:center; justify-content:center; }
#ID .pn-rosca-total { font-size:1.4rem; font-weight:700; }
#ID .pn-rosca-sub { font-size:.75rem; color:#6c757d; }

#ID .pn-legenda { display:flex; flex-direction:column; gap:.35rem; flex:1; min-width:180px; max-width:440px; font-size:.85rem; }
#ID .pn-legenda-linha { flex-direction:row; flex-wrap:wrap; gap:.4rem 1rem; margin-top:.6rem; font-size:.8rem; }
#ID .pn-legenda-item { display:flex; align-items:center; gap:.45rem; }
#ID .pn-legenda-cor { width:12px; height:12px; border-radius:3px; flex-shrink:0; }
#ID .pn-colunas { display:flex; gap:.6rem; align-items:stretch; padding-top:.25rem; }
#ID .pn-coluna { flex:1 1 0; min-width:0; display:flex; flex-direction:column; }
#ID .pn-coluna-area { height:190px; display:flex; flex-direction:column; justify-content:flex-end; align-items:stretch; border-bottom:1px solid #dee2e6; }
#ID .pn-coluna-valor { text-align:center; font-size:.8rem; line-height:1.15; margin-bottom:.25rem; font-variant-numeric:tabular-nums; }
#ID .pn-coluna-valor .pn-barra-n { display:block; margin-left:0; }
#ID .pn-coluna-barra { border-radius:4px 4px 0 0; min-height:2px; }
#ID .pn-coluna-rotulo { text-align:center; font-size:.78rem; color:#343a40; line-height:1.2; margin-top:.4rem; overflow-wrap:anywhere; }

#ID .pn-comparativo { margin-top:1.25rem; padding-top:.25rem; border-top:1px dashed #e3e7ec; }
#ID .pn-comp-linha { display:grid; grid-template-columns:150px 1fr; gap:.75rem; align-items:center; margin-bottom:.5rem; }
#ID .pn-comp-rotulo { display:flex; flex-direction:column; font-size:.85rem; line-height:1.2; }
#ID .pn-comp-rotulo .pn-barra-n { margin-left:0; }
#ID .pn-comp-barra { display:flex; height:26px; border-radius:5px; overflow:hidden; background:#eef1f4; }
#ID .pn-comp-seg { height:100%; color:#fff; font-size:.72rem; font-weight:700; display:flex; align-items:center; justify-content:center; white-space:nowrap; overflow:hidden; text-shadow:0 0 2px rgba(0,0,0,.45); }

#ID .pn-piramide { font-size:.8rem; }
#ID .pn-pir-cabecalho, #ID .pn-pir-linha { display:grid; grid-template-columns:1fr 72px 1fr; gap:.4rem; align-items:center; }
#ID .pn-pir-cabecalho { margin-bottom:.4rem; font-size:.85rem; }
#ID .pn-pir-linha { margin-bottom:3px; }
#ID .pn-pir-lado { display:flex; justify-content:flex-end; align-items:center; gap:.35rem; text-align:right; }
#ID .pn-pir-lado.text-start { justify-content:flex-start; }
#ID .pn-pir-meio { text-align:center; color:#495057; font-weight:600; font-size:.75rem; }
#ID .pn-pir-barra { height:16px; border-radius:3px; min-width:0; }
#ID .pn-pir-f { background:#FF8C42; }
#ID .pn-pir-m { background:#005B96; }
#ID .pn-pir-valor { color:#6c757d; font-size:.72rem; font-variant-numeric:tabular-nums; white-space:nowrap; }

#ID .pn-numeros { display:flex; flex-wrap:wrap; gap:1.5rem; margin-top:1rem; }
#ID .pn-numero b { display:block; font-size:1.25rem; color:#1f2d3d; }
#ID .pn-numero span { font-size:.78rem; color:#6c757d; }

#ID .pn-aviso { font-size:.82rem; color:#664d03; background:#fff8e1; border:1px solid #ffe69c; border-radius:.6rem; padding:.5rem .75rem; margin-bottom:.9rem; }
#ID .pn-nota { font-size:.78rem; color:#6c757d; margin-top:.75rem; }
#ID .pn-vazio { color:#6c757d; font-size:.9rem; padding:.5rem 0; }
#ID .pn-sem-dados { background:#fff; border:1px dashed #c5ced8; border-radius:.9rem; padding:2rem; text-align:center; color:#495057; }
#ID .pn-rodape { font-size:.78rem; color:#6c757d; margin:-.5rem 0 1.5rem; }
"

mod_painel_ui <- function(id) {

    ns <- NS(id)

    tagList(

        tags$head(tags$style(HTML(gsub("#ID", paste0("#", id), CSS_PAINEL, fixed = TRUE)))),

        div(
            id = id,

            div(
                class = "al-header",

                div(
                    class = "al-titulo",
                    div(class = "al-titulo-icone", icon("chart-pie")),
                    div(
                        tags$h4("Painel"),
                        div(class = "text-muted", style = "font-size:.85rem;",
                            "Perfil do quadro atual de Magistrados e Servidores")
                    )
                ),

                div(
                    class = "al-acoes",
                    actionButton(
                        ns("atualizar"),
                        tagList(icon("rotate", class = "me-2"), "Atualizar Dados"),
                        class = "btn btn-outline-primary btn-acao"
                    ),
                    downloadButton(
                        ns("baixar_csv"),
                        "Baixar indicadores (CSV)",
                        icon = icon("download"),
                        class = "btn btn-primary btn-acao"
                    )
                )
            ),

            div(
                class = "pn-filtros",
                div(
                    class = "pn-segmentado",
                    radioButtons(
                        ns("publico"),
                        "Público",
                        choices = PAINEL_PUBLICOS,
                        selected = PAINEL_TODOS,
                        inline = TRUE
                    )
                ),
                div(
                    style = "min-width:280px; flex:1; max-width:460px;",
                    selectInput(ns("cargo"), "Cargo", choices = c("Todos"), selected = "Todos", width = "100%")
                ),
                div(
                    class = "mb-3",
                    actionButton(
                        ns("limpar"),
                        tagList(icon("eraser", class = "me-2"), "Limpar filtros"),
                        class = "btn btn-outline-secondary btn-acao"
                    )
                )
            ),

            uiOutput(ns("conteudo"))
        )
    )

}

# =====================================================
# SERVER
# =====================================================

mod_painel_server <- function(id, ativo = reactive(TRUE), empresa = reactive(NULL), hoje = Sys.Date) {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        dados <- reactiveVal(list(
            pessoas = pessoas_painel_vazio(),
            colunas = rbind(
                Magistrados = pessoas_painel(NULL, PAINEL_MAGISTRADOS, character(0), CARGO_MAGISTRADO)$colunas,
                Servidores = pessoas_painel(NULL, PAINEL_SERVIDORES, character(0), CARGO_SERVIDOR)$colunas
            )
        ))
        chave_carregada <- reactiveVal(NULL)

        empresa_definida <- function() {
            e <- empresa()
            !is.null(e) && length(e) == 1 && !is.na(e) && trimws(e) != ""
        }

        # Lê os arquivos só quando algum mudou (ou em "Atualizar Dados"):
        # abrir o Painel depois de enviar uma base em outro módulo já
        # mostra os dados novos.
        carregar <- function(forcar = FALSE) {

            if (!empresa_definida()) {
                return(invisible(NULL))
            }

            e <- empresa()
            chave <- paste(e, arquivos_painel_empresa(e))

            if (!forcar && identical(chave, chave_carregada())) {
                return(invisible(NULL))
            }

            novos <- tryCatch(
                carregar_painel_empresa(e, hoje()),
                error = function(err) {
                    showNotification(
                        paste("Não foi possível montar o painel:", conditionMessage(err)),
                        type = "error", duration = 10
                    )
                    NULL
                }
            )

            if (!is.null(novos)) {
                dados(novos)
                chave_carregada(chave)
            }

        }

        observeEvent(list(ativo(), empresa()), {
            req(isTRUE(ativo()))
            carregar()
        })

        observeEvent(input$atualizar, {
            carregar(forcar = TRUE)
            showNotification("Painel atualizado.", type = "message", duration = 3)
        }, ignoreInit = TRUE)

        publico_atual <- reactive({
            p <- input$publico
            if (is.null(p) || !(p %in% PAINEL_PUBLICOS)) PAINEL_TODOS else p
        })

        pessoas_publico <- reactive({
            p <- dados()$pessoas
            if (publico_atual() == PAINEL_TODOS) p else p[p$publico == publico_atual(), ]
        })

        # Cargos do público escolhido (os de magistrados e os de servidores
        # são de tabelas diferentes, então não se confundem).
        observeEvent(pessoas_publico(), {
            cargos <- sort(unique(na.omit(pessoas_publico()$cargo)))
            atual <- isolate(input$cargo)
            updateSelectInput(
                session, "cargo",
                choices = c("Todos", cargos),
                selected = if (!is.null(atual) && atual %in% cargos) atual else "Todos"
            )
        })

        observeEvent(input$limpar, {
            updateRadioButtons(session, "publico", selected = PAINEL_TODOS)
            updateSelectInput(session, "cargo", selected = "Todos")
        }, ignoreInit = TRUE)

        pessoas_filtradas <- reactive({
            p <- pessoas_publico()
            c <- input$cargo
            if (!is.null(c) && c != "Todos") p <- p[!is.na(p$cargo) & p$cargo == c, ]
            p
        })

        publicos_ativos <- reactive({
            if (publico_atual() == PAINEL_TODOS) c(PAINEL_MAGISTRADOS, PAINEL_SERVIDORES) else publico_atual()
        })

        # =================================================
        # CONTEÚDO
        # =================================================

        output$conteudo <- renderUI({

            if (!empresa_definida()) {
                return(div(class = "pn-sem-dados", "Selecione uma empresa para ver o painel."))
            }

            todos <- dados()$pessoas
            colunas <- dados()$colunas
            p <- pessoas_filtradas()
            pubs <- intersect(publicos_ativos(), unique(todos$publico))
            if (length(pubs) == 0) pubs <- publicos_ativos()

            if (nrow(todos) == 0) {
                return(div(
                    class = "pn-sem-dados",
                    icon("folder-open", class = "fa-2x mb-3", style = "color:#6c757d;"),
                    tags$h5("Ainda não há dados para o painel"),
                    p(
                        "Envie a Base de Dados de Magistrados e/ou de Pessoal e Auxiliar em ",
                        tags$b("Gerenciar Arquivos"), " nos módulos ", tags$b("Magistrados"),
                        " e ", tags$b("Pessoal e Auxiliar"), " e volte aqui."
                    )
                ))
            }

            if (nrow(p) == 0) {
                return(div(
                    class = "pn-sem-dados",
                    tags$h5("Nenhuma pessoa com os filtros selecionados"),
                    p("Troque o público ou o cargo, ou use \"Limpar filtros\".")
                ))
            }

            n_total <- nrow(p)
            n_mag <- sum(p$publico == PAINEL_MAGISTRADOS)
            n_srv <- sum(p$publico == PAINEL_SERVIDORES)

            pct_de <- function(cond) if (n_total > 0) 100 * sum(cond, na.rm = TRUE) / n_total else NA

            idades <- p$idade[!is.na(p$idade)]

            # ---------------- Resumos ----------------
            r_faixa <- resumo_painel(p$faixa, names(FAIXAS_10_PAINEL))
            r_sexo <- resumo_painel(p$sexo)
            r_genero <- resumo_painel(p$genero)
            r_raca <- resumo_painel(p$raca)
            r_def <- resumo_painel(p$deficiencia, c(PAINEL_COM_DEFICIENCIA, PAINEL_SEM_DEFICIENCIA, PAINEL_SEM_INFO))
            r_tipos <- resumo_tipos_deficiencia(p)
            r_cargo <- resumo_painel(p$cargo)

            cores_faixa <- cores_painel(names(FAIXAS_10_PAINEL))
            cores_sexo <- cores_painel(unique(c(r_sexo$categoria, todos$sexo[!is.na(todos$sexo)])), PAINEL_CORES_SEXO)
            cores_genero <- cores_painel(unique(c(r_genero$categoria, todos$genero[!is.na(todos$genero)])))
            cores_raca <- cores_painel(unique(c(r_raca$categoria, todos$raca[!is.na(todos$raca)])), PAINEL_CORES_RACA)
            cores_def <- cores_painel(
                c(PAINEL_COM_DEFICIENCIA, PAINEL_SEM_DEFICIENCIA, PAINEL_SEM_INFO),
                c("Com deficiência" = PALETA_GRAFICOS[["laranja"]], "Sem deficiência" = PALETA_GRAFICOS[["azul"]])
            )
            cores_tipos <- cores_painel(TIPOS_DEFICIENCIA_PAINEL)
            # Mesmas cores de cargo dos gráficos de Magistrados e de Pessoal e Auxiliar.
            cores_cargo <- cores_painel(
                unique(c(r_cargo$categoria, todos$cargo[!is.na(todos$cargo)])),
                c(cores_fixas_magistrados("Cargo"), cores_fixas_pessoal("Cargo"))
            )

            comparar <- publico_atual() == PAINEL_TODOS && n_mag > 0 && n_srv > 0

            sem_idade <- sum(is.na(p$idade) & p$publico %in% pubs[colunas[pubs, "nascimento"]])

            tagList(

                # ---------------- Destaques ----------------
                div(
                    class = "pn-kpis",
                    painel_indicador(
                        "users", fmt_int(n_total), "Pessoas no quadro atual",
                        paste(fmt_int(n_mag), "magistrados ·", fmt_int(n_srv), "servidores"),
                        PALETA_GRAFICOS[["azul"]]
                    ),
                    painel_indicador(
                        "venus", fmt_pct(pct_de(p$sexo == "Feminino")), "Mulheres",
                        paste(fmt_int(sum(p$sexo == "Feminino", na.rm = TRUE)), "pessoas"),
                        PAINEL_CORES_SEXO[["Feminino"]]
                    ),
                    painel_indicador(
                        "people-group", fmt_pct(pct_de(p$raca %in% RACA_NEGROS_PAINEL)), "Pessoas negras",
                        "pretas e pardas", PALETA_GRAFICOS[["grafite"]]
                    ),
                    painel_indicador(
                        "wheelchair", fmt_pct(pct_de(p$deficiencia == PAINEL_COM_DEFICIENCIA)), "Pessoas com deficiência",
                        paste(fmt_int(sum(p$deficiencia == PAINEL_COM_DEFICIENCIA, na.rm = TRUE)), "pessoas"),
                        PALETA_GRAFICOS[["ciano"]]
                    ),
                    painel_indicador(
                        "cake-candles",
                        if (length(idades) > 0) paste(fmt_num(mean(idades)), "anos") else "–",
                        "Idade média",
                        if (length(idades) > 0) paste("mediana de", fmt_num(stats::median(idades), 0), "anos"),
                        PALETA_GRAFICOS[["verde"]]
                    )
                ),

                div(
                    class = "pn-rodape",
                    sprintf(
                        "Situação em %s. Cada pessoa é contada uma vez, pela situação mais recente; aposentados, falecidos, exonerados e quem saiu do órgão ficam de fora. Percentuais sobre o total de pessoas filtradas.",
                        format(hoje(), "%d/%m/%Y")
                    )
                ),

                # ---------------- Navegação por tema ----------------
                tags$nav(
                    class = "pn-temas",
                    `aria-label` = "Temas do painel",
                    lapply(
                        list(
                            c("pn-faixa", "Faixa etária"), c("pn-sexo", "Sexo"),
                            c("pn-genero", "Identidade de gênero"), c("pn-raca", "Raça/cor"),
                            c("pn-deficiencia", "Deficiência"), c("pn-cargo", "Cargo")
                        ),
                        function(t) {
                            tags$a(
                                class = "pn-tema", href = "#",
                                onclick = sprintf(
                                    "var el=document.getElementById('%s'); if(el){el.scrollIntoView({behavior:'smooth',block:'start'});} return false;",
                                    ns(t[1])
                                ),
                                t[2]
                            )
                        }
                    )
                ),

                # ---------------- Faixa etária ----------------
                painel_secao(
                    ns("pn-faixa"), "chart-simple", "Faixa etária",
                    "Idade calculada pela data de nascimento, na data de hoje.",
                    painel_aviso_coluna(colunas, "nascimento", "Data de nascimento", pubs),
                    div(
                        class = "pn-grade",
                        div(
                            div(class = "pn-subtitulo", style = "margin-top:0;", "Pirâmide etária por sexo"),
                            painel_piramide(p)
                        ),
                        div(
                            div(class = "pn-subtitulo", style = "margin-top:0;", "Distribuição por faixa"),
                            painel_cartoes(r_faixa, cores_faixa, max_cartoes = 6),
                            div(
                                class = "pn-numeros",
                                div(class = "pn-numero", tags$b(if (length(idades)) paste(min(idades), "anos") else "–"), tags$span("menor idade")),
                                div(class = "pn-numero", tags$b(if (length(idades)) paste(fmt_num(mean(idades)), "anos") else "–"), tags$span("idade média")),
                                div(class = "pn-numero", tags$b(if (length(idades)) paste(max(idades), "anos") else "–"), tags$span("maior idade"))
                            ),
                            if (sem_idade > 0) div(
                                class = "pn-nota",
                                sprintf("%s pessoa(s) sem data de nascimento válida ficam fora deste indicador.", fmt_int(sem_idade))
                            )
                        )
                    ),
                    if (comparar) painel_comparativo(p, "faixa", cores_faixa, names(FAIXAS_10_PAINEL))
                ),

                # ---------------- Sexo ----------------
                painel_secao(
                    ns("pn-sexo"), "venus-mars", "Sexo",
                    "Sexo informado no cadastro do MPM/CNJ (Masculino, Feminino, Intersexo ou Não informado).",
                    painel_aviso_coluna(colunas, "sexo", "Sexo", pubs),
                    div(
                        class = "pn-grade",
                        painel_grafico(r_sexo, cores_sexo),
                        painel_cartoes(r_sexo, cores_sexo)
                    ),
                    if (comparar) painel_comparativo(p, "sexo", cores_sexo)
                ),

                # ---------------- Identidade de gênero ----------------
                painel_secao(
                    ns("pn-genero"), "transgender", "Identidade de gênero",
                    "Autodeclaração de identidade de gênero (Cisgênero, Transgênero, Transexual, Travesti, Gênero fluido, Agênero, Outra).",
                    painel_aviso_coluna(colunas, "genero", "Identidade de gênero", pubs),
                    div(
                        class = "pn-grade",
                        painel_cartoes(r_genero, cores_genero),
                        painel_grafico(r_genero, cores_genero)
                    ),
                    if (comparar) painel_comparativo(p, "genero", cores_genero)
                ),

                # ---------------- Raça/cor ----------------
                painel_secao(
                    ns("pn-raca"), "palette", "Raça/cor",
                    "Autodeclaração de raça/cor. Pessoas negras são as pretas e as pardas, como nas estatísticas do IBGE.",
                    painel_aviso_coluna(colunas, "raca", "Raça / Cor", pubs),
                    painel_cartoes(r_raca, cores_raca),
                    div(
                        class = "pn-grade mt-3",
                        painel_grafico(r_raca, cores_raca),
                        div(
                            class = "pn-numeros",
                            style = "margin-top:0;",
                            div(class = "pn-numero", tags$b(fmt_pct(pct_de(p$raca %in% RACA_NEGROS_PAINEL))), tags$span("pessoas negras (pretas e pardas)")),
                            div(class = "pn-numero", tags$b(fmt_pct(pct_de(p$raca == "Branco(a)"))), tags$span("pessoas brancas")),
                            div(class = "pn-numero", tags$b(fmt_pct(pct_de(p$raca %in% PAINEL_SEM_INFO))), tags$span("sem informação"))
                        )
                    ),
                    if (comparar) painel_comparativo(p, "raca", cores_raca)
                ),

                # ---------------- Deficiência ----------------
                painel_secao(
                    ns("pn-deficiencia"), "wheelchair", "Deficiência",
                    "Pessoas com deficiência são as que têm algum dos tipos 1 a 7 do MPM/CNJ. Quem tem mais de um tipo aparece em cada um deles.",
                    painel_aviso_coluna(colunas, "deficiencia", "Deficiência", pubs),
                    div(
                        class = "pn-grade",
                        div(
                            div(class = "pn-subtitulo", style = "margin-top:0;", "Pessoas com e sem deficiência"),
                            painel_cartoes(r_def, cores_def)
                        ),
                        div(
                            div(class = "pn-subtitulo", style = "margin-top:0;", "Tipo de deficiência (entre as pessoas com deficiência)"),
                            if (nrow(r_tipos) > 0) painel_grafico(r_tipos, cores_tipos, "das pessoas com deficiência", permite_pizza = FALSE)
                            else div(class = "pn-vazio", "Nenhuma pessoa com deficiência nos filtros selecionados.")
                        )
                    ),
                    if (comparar) painel_comparativo(p, "deficiencia", cores_def, c(PAINEL_COM_DEFICIENCIA, PAINEL_SEM_DEFICIENCIA, PAINEL_SEM_INFO))
                ),

                # ---------------- Cargo ----------------
                painel_secao(
                    ns("pn-cargo"), "briefcase", "Cargo",
                    "Cargo da situação mais recente de cada pessoa, conforme as tabelas do MPM/CNJ.",
                    painel_aviso_coluna(colunas, "cargo", "Cargo", pubs),
                    if (publico_atual() == PAINEL_TODOS && n_mag > 0 && n_srv > 0) {
                        div(
                            class = "pn-grade",
                            div(
                                div(class = "pn-subtitulo", style = "margin-top:0;", "Magistrados"),
                                {
                                    r <- resumo_painel(p$cargo[p$publico == PAINEL_MAGISTRADOS])
                                    painel_grafico(r, cores_cargo, "dos magistrados", "magistrados")
                                }
                            ),
                            div(
                                div(class = "pn-subtitulo", style = "margin-top:0;", "Servidores"),
                                {
                                    r <- resumo_painel(p$cargo[p$publico == PAINEL_SERVIDORES])
                                    painel_grafico(r, cores_cargo, "dos servidores", "servidores")
                                }
                            )
                        )
                    } else {
                        painel_grafico(r_cargo, cores_cargo)
                    }
                )
            )

        })

        # =================================================
        # CSV
        # =================================================

        output$baixar_csv <- downloadHandler(
            filename = function() {
                paste0("painel_indicadores_", gsub("[^A-Za-z0-9_-]", "_", if (empresa_definida()) empresa() else "empresa"), "_", format(hoje(), "%Y%m%d"), ".csv")
            },
            content = function(file) {
                readr::write_excel_csv2(tabela_exportacao_painel(pessoas_filtradas()), file, na = "")
            }
        )

    })

}
