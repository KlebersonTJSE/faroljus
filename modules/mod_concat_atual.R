library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(writexl)

# Marca o tempo inicial de execução

tempo_inicio <- Sys.time()

# ------------------------------------------------------------------
# 1. Caminho dos arquivos
# ------------------------------------------------------------------

caminho_pasta <- "C:/Users/3894/Projetos R/MPM/Envio Mensal/202607/quadro pessoal e auxiliar"

arquivo_quadro <- file.path(
  caminho_pasta,
  "Quadro pessoal e auxiliar.xlsx"
)

arquivo_alertas <- file.path(
  caminho_pasta,
  "Alertas quadro pessoal e auxiliar II.csv"
)

arquivo_saida <- file.path(
  caminho_pasta,
  "Terceiro_Arquivo_CPFs_Alertas.xlsx"
)

# ------------------------------------------------------------------
# 2. Leitura dos arquivos mantendo todos os campos como texto
# ------------------------------------------------------------------

df_quadro <- read_excel(
  arquivo_quadro,
  col_types = "text"
)

df_alertas <- read_csv2(
  arquivo_alertas,
  col_types = cols(.default = col_character())
)

# ------------------------------------------------------------------
# 3. Função para higienizar e padronizar CPF
# ------------------------------------------------------------------

limpar_cpf <- function(cpf) {
  
  cpf_limpo <- cpf %>%
    str_replace_na("") %>%
    str_remove_all("\\D")
  
  if_else(
    nchar(cpf_limpo) > 0,
    str_pad(
      cpf_limpo,
      width = 11,
      side = "left",
      pad = "0"
    ),
    ""
  )
}

# ------------------------------------------------------------------
# 4. Função para converter datas com segurança
# ------------------------------------------------------------------

converter_para_data_2 <- function(dt_str) {
  if (
    is.na(dt_str) ||
    str_trim(dt_str) == ""
  ) {
    return(as.Date(NA))
  }
  dt <- suppressWarnings(
    parse_date_time(
      str_trim(dt_str),
      orders = c(
        "dmy",
        "ymd",
        "d/m/Y",
        "Y-m-d"
      )
    )
  )
  as.Date(dt)
}

converter_para_data <- function(dt_str) {
  if (
    is.na(dt_str) ||
    str_trim(dt_str) == "")
    return(NA)
  
    dt <- parse_date_time(
        str_trim(dt_str), 
        orders = c(
          "dmy", 
          "ymd", 
          "d/m/Y", 
          "Y-m-d"
          )
        )
  return(
    as.Date(dt)
    )
}

# ------------------------------------------------------------------

# 5. Higienização dos CPFs

# ------------------------------------------------------------------

df_quadro <- df_quadro %>%
  mutate(
    CPF_clean = limpar_cpf(CPF)
  )

df_alertas <- df_alertas %>%
  mutate(
    CPF_clean = limpar_cpf(CPF)
  )

# ------------------------------------------------------------------

# 6. Regra 3

#

# Considerar somente os CPFs distintos do arquivo de alertas.

# ------------------------------------------------------------------

cpfs_alertas_distintos <- df_alertas %>%
  
  filter(
    CPF_clean != ""
  ) %>%
  
  distinct(
    CPF_clean
  ) %>%
  
  pull(
    CPF_clean
  )

# ------------------------------------------------------------------

# 7. Regra 4

#

# Buscar no Quadro pessoal e auxiliar os CPFs encontrados

# no arquivo de alertas.

# ------------------------------------------------------------------

df_quadro_filtrado <- df_quadro %>%
  
  filter(
    CPF_clean %in% cpfs_alertas_distintos
  )

# ------------------------------------------------------------------

# 8. NOVA REGRA

#

# Se Status = INATIVO, a linha deve ser totalmente desconsiderada

# e não deve ser gravada no terceiro arquivo.

# ------------------------------------------------------------------

df_quadro_filtrado <- df_quadro_filtrado %>%
  
  filter(
    is.na(Status) |
      str_to_upper(str_trim(Status)) != "INATIVO"
  )

# ------------------------------------------------------------------

# 9. Processamento individual por CPF

# ------------------------------------------------------------------

processar_cpf <- function(df_bloco) {
  
  # --------------------------------------------------------------
  
  # Regra 6
  
  #
  
  # Se a Situação profissional atual for diferente de 1, 2 ou 3,
  
  # não aplicar qualquer regra de alteração de datas.
  
  # Os registros devem apenas ser gravados.
  
  # --------------------------------------------------------------
  
  situacoes_validas <- c(
    "1",
    "2",
    "3"
  )
  
  situacoes_cpf <- df_bloco$`Situação profissional atual` %>%
    str_trim()
  
  if (
    any(
      !is.na(situacoes_cpf) &
      situacoes_cpf != "" &
      !(situacoes_cpf %in% situacoes_validas)
    )
  ) {
    
    return(
      df_bloco
    )
    
  }
  
  # --------------------------------------------------------------
  
  # Para aplicar a regra de relacionamento entre as situações,
  
  # devem existir exatamente duas linhas para o CPF.
  
  # --------------------------------------------------------------
  
  if (
    nrow(df_bloco) != 2
  ) {
    
    return(
      df_bloco
    )
    
  }
  
  # --------------------------------------------------------------
  
  # Converter as datas de início para comparação
  
  # --------------------------------------------------------------
  
  datas_inicio <- vapply(
    df_bloco$`Data de início da situação`,
    converter_para_data,
    FUN.VALUE = as.Date(NA)
  )
  
  # Se não for possível determinar as datas, não alterar
  
  # os registros.
  
  if (
    all(is.na(datas_inicio))
  ) {
    
    return(
      df_bloco
    )
    
  }
  
  # --------------------------------------------------------------
  
  # Identificar a linha mais antiga e a linha mais recente
  
  # --------------------------------------------------------------
  
  idx_mais_antiga <- which.min(
    datas_inicio
  )
  
  idx_mais_recente <- which.max(
    datas_inicio
  )
  
  data_inicio_mais_recente <-
    datas_inicio[idx_mais_recente]
  
  # --------------------------------------------------------------
  
  # Verificar se ambas as linhas estão sem Data de saída
  
  # --------------------------------------------------------------
  
  data_saida_mais_antiga <-
    df_bloco$`Data de saída da situação`[idx_mais_antiga]
  
  data_saida_mais_recente <-
    df_bloco$`Data de saída da situação`[idx_mais_recente]
  
  sem_saida_mais_antiga <-
    is.na(data_saida_mais_antiga) ||
    str_trim(data_saida_mais_antiga) == ""
  
  sem_saida_mais_recente <-
    is.na(data_saida_mais_recente) ||
    str_trim(data_saida_mais_recente) == ""
  
  # --------------------------------------------------------------
  
  # Regra:
  
  #
  
  # Se as duas linhas não possuem Data de saída da situação,
  
  # preencher a linha com a menor Data de início com:
  
  #
  
  # Data de início da situação mais recente - 1 dia
  
  # --------------------------------------------------------------
  
  if (
    sem_saida_mais_antiga &&
    sem_saida_mais_recente
  ) {
    
    data_saida_calculada <- format(
      data_inicio_mais_recente - days(1),
      "%d/%m/%Y"
    )
    
    df_bloco$`Data de saída da situação`[
      idx_mais_antiga
    ] <- data_saida_calculada
    
  }
  
  # --------------------------------------------------------------
  
  # Caso somente a linha mais antiga esteja sem Data de saída,
  
  # também poderá ser encerrada um dia antes do início da situação
  
  # mais recente.
  
  #
  
  # Isso mantém a lógica de continuidade temporal entre situações.
  
  # --------------------------------------------------------------
  
  else if (
    sem_saida_mais_antiga &&
    !sem_saida_mais_recente
  ) {
    
    data_saida_calculada <- format(
      data_inicio_mais_recente - days(1),
      "%d/%m/%Y"
    )
    
    df_bloco$`Data de saída da situação`[
      idx_mais_antiga
    ] <- data_saida_calculada
    
  }
  
  return(
    df_bloco
  )
}

# ------------------------------------------------------------------

# 10. Aplicar o processamento para cada CPF

# ------------------------------------------------------------------

df_terceiro_arquivo <- df_quadro_filtrado %>%
  
  group_split(
    CPF_clean,
    .keep = TRUE
  ) %>%
  
  map_dfr(
    processar_cpf
  )

# ------------------------------------------------------------------

# 11. Remover a coluna auxiliar e manter exatamente as colunas

# originais do arquivo Quadro pessoal e auxiliar.xlsx

# ------------------------------------------------------------------

colunas_originais <- setdiff(
  colnames(df_quadro),
  "CPF_clean"
)

df_terceiro_arquivo <- df_terceiro_arquivo %>%
  
  select(
    all_of(colunas_originais)
  )

# ------------------------------------------------------------------

# 12. Exportação do terceiro arquivo

# ------------------------------------------------------------------

write_xlsx(
  df_terceiro_arquivo,
  arquivo_saida
)

# ------------------------------------------------------------------

# 13. Informações da execução

# ------------------------------------------------------------------

tempo_fim <- Sys.time()

tempo_execucao <- round(
  difftime(
    tempo_fim,
    tempo_inicio,
    units = "secs"
  ),
  2
)

cat(
  sprintf(
    paste0(
      "Arquivo 'Terceiro_Arquivo_CPFs_Alertas.xlsx' ",
      "gerado com sucesso!\n",
      "Linhas geradas: %d\n",
      "CPFs processados: %d\n",
      "Tempo total de execução: %s segundos.\n"
    ),
    nrow(df_terceiro_arquivo),
    length(cpfs_alertas_distintos),
    tempo_execucao
  )
)
