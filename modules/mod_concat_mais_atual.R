library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(lubridate)

tempo_inicio <- Sys.time()

# ------------------------------------------------------------------------------
# 1 e 2. Caminhos e Leitura dos Arquivos
# ------------------------------------------------------------------------------
caminho_quadro  <- "C:/Users/3894/Projetos R/MPM/Envio Mensal/202607/quadro pessoal e auxiliar/Quadro pessoal e auxiliar.xlsx"
caminho_alertas <- "C:/Users/3894/Projetos R/MPM/Envio Mensal/202607/quadro pessoal e auxiliar/Alertas quadro pessoal e auxiliar.csv"
caminho_saida   <- "C:/Users/3894/Projetos R/MPM/Envio Mensal/202607/quadro pessoal e auxiliar/Resultado_Quadro_Pessoal_Auxiliar.csv"

# Excluir o arquivo de saída caso ele já exista
if (file.exists(caminho_saida)) {
  file.remove(caminho_saida)
  cat("Arquivo anterior 'Resultado_Quadro_Pessoal_Auxiliar.csv' encontrado e excluído com sucesso.\n")
}

# Leitura do Quadro Pessoal (xlsx)
df_quadro <- read_excel(caminho_quadro, col_types = "text")

# Regra: Desconsiderar linhas com Status = Inativo
if ("Status" %in% colnames(df_quadro)) {
  df_quadro <- df_quadro %>%
    filter(is.na(Status) | str_to_upper(str_trim(Status)) != "INATIVO")
}

# Leitura do Arquivo de Alertas (csv)
df_alertas <- read_csv2(caminho_alertas, col_types = cols(.default = "c"))

# Função para higienizar e padronizar CPFs (11 dígitos)
limpar_cpf <- function(cpf) {
  cpf_limpo <- str_remove_all(str_replace_na(cpf, ""), "\\D")
  if_else(nchar(cpf_limpo) > 0, str_pad(cpf_limpo, 11, pad = "0"), "")
}

# Aplicar limpeza de CPF em ambas as bases
df_quadro <- df_quadro %>% mutate(CPF_clean = limpar_cpf(CPF))
df_alertas <- df_alertas %>% mutate(CPF_clean = limpar_cpf(CPF))

# ------------------------------------------------------------------------------
# 3. CPFs ÚNICOS do arquivo de Alertas
# ------------------------------------------------------------------------------
cpfs_alertas_unicos <- df_alertas %>% 
  filter(CPF_clean != "") %>% 
  pull(CPF_clean) %>% 
  unique()

# Helper para converter texto em Date com segurança
converter_para_data <- function(dt_vec) {
  as.Date(parse_date_time(str_trim(dt_vec), orders = c("dmy", "ymd", "d/m/Y", "Y-m-d")))
}

# ------------------------------------------------------------------------------
# 4, 5 e 6. Processamento das Regras
# ------------------------------------------------------------------------------
df_processado <- df_quadro %>%
  filter(CPF_clean %in% cpfs_alertas_unicos) %>%
  mutate(
    dt_inicio_obj = converter_para_data(`Data de início da situação`),
    sit_prof_limpa = str_trim(as.character(`Situação profissional atual`))
  )

df_final <- df_processado %>%
  group_by(CPF_clean) %>%
  group_modify(~ {
    n_linhas <- nrow(.x)
    
    # Aplica o cálculo de datas se houver 2 linhas para o mesmo CPF
    if (n_linhas == 2 && !any(is.na(.x$dt_inicio_obj))) {
      
      maior_dt_inicio <- max(.x$dt_inicio_obj)
      menor_dt_inicio <- min(.x$dt_inicio_obj)
      
      nova_dt_fim <- maior_dt_inicio - days(1)
      nova_dt_fim_str <- format(nova_dt_fim, "%d/%m/%Y")
      
      # Regra de preenchimento individual por tupla:
      # Preenche APENAS se a tupla tiver a MENOR data de início,
      # tiver SituacaoProfissional em ('1', '2', '3')
      # e a Data de Saida estiver em branco.
      .x <- .x %>%
        mutate(
          `Data de saída da situação` = if_else(
            dt_inicio_obj == menor_dt_inicio & 
              sit_prof_limpa %in% c("1", "2", "3") &
              (is.na(`Data de saída da situação`) | str_trim(`Data de saída da situação`) == ""),
            nova_dt_fim_str,
            `Data de saída da situação`
          )
        )
    }
    
    return(.x)
  }) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Limpeza de colunas auxiliares, supressão do 'Status' e Ordenação por Nome
# ------------------------------------------------------------------------------
colunas_finais <- setdiff(colnames(df_quadro), c("CPF_clean", "Status"))

df_final <- df_final %>%
  select(all_of(colunas_finais)) %>%
  arrange(Nome)

# ------------------------------------------------------------------------------
# Exportação do Arquivo Resultado
# ------------------------------------------------------------------------------
write_excel_csv2(df_final, caminho_saida)

tempo_fim <- Sys.time()
tempo_execucao <- round(difftime(tempo_fim, tempo_inicio, units = "secs"), 2)

cat(sprintf("Processamento concluído com sucesso em %s segundos!\nArquivo gerado em: %s\n", tempo_execucao, caminho_saida))