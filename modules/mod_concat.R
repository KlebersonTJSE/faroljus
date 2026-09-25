library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(purrr)
library(lubridate)

# Marca o tempo inicial de execução
tempo_inicio <- Sys.time()

# 1. Leitura dos arquivos mantendo todos os campos como texto
df_arq1 <- read_csv("C:/Users/3894/Projetos R/MPM/Envio Mensal/202607/quadro pessoal e auxiliar/20260721_Pessoal_Auxiliar.csv", col_types = cols(.default = "c"))
df_arq2 <- read_excel("C:/Users/3894/Projetos R/MPM/Envio Mensal/202607/quadro pessoal e auxiliar/Quadro pessoal e auxiliar.xlsx", col_types = "text")

# Função para higienizar e padronizar os CPFs (11 dígitos com zeros à esquerda)
limpar_cpf <- function(cpf) {
  cpf_limpo <- str_remove_all(str_replace_na(cpf, ""), "\\D")
  if_else(nchar(cpf_limpo) > 0, str_pad(cpf_limpo, 11, pad = "0"), "")
}

# Função auxiliar para converter strings em objeto Date com segurança
converter_para_data <- function(dt_str) {
  if (is.na(dt_str) || str_trim(dt_str) == "") return(NA)
  dt <- parse_date_time(str_trim(dt_str), orders = c("dmy", "ymd", "d/m/Y", "Y-m-d"))
  return(as.Date(dt))
}

# Adiciona identificação de origem e higieniza CPFs
df_arq1 <- df_arq1 %>% 
  mutate(
    Origem = "df_arq1",
    CPF_clean = limpar_cpf(CPF)
  )

df_arq2 <- df_arq2 %>% 
  mutate(
    Origem = "df_arq2",
    CPF_clean = limpar_cpf(CPF)
  )

# 2. Ignorar linhas do segundo arquivo onde Status seja 'Inativo'
if ("Status" %in% colnames(df_arq2)) {
  df_arq2 <- df_arq2 %>% 
    filter(is.na(Status) | str_to_upper(str_trim(Status)) != "INATIVO")
}

# 3. Mapeamento de colunas do segundo arquivo para a estrutura do primeiro
depara_colunas <- c(
  "CPF"                  = "CPF",
  "Nome"                 = "Nome",
  "Serventia"            = "Órgão de lotação do(a) Servidor(a) ou Auxiliar",
  "SituacaoProfissional" = "Situação profissional atual",
  "DataInicioSituacao"   = "Data de início da situação",
  "Naturalidade"         = "Naturalidade",
  "Nascimento"           = "Data Nascimento",
  "EmailInstitucional"   = "Email",
  "Sexo"                 = "Sexo",
  "Genero"               = "Identidade de gênero",
  "Raca"                 = "Raça / Cor",
  "Deficiencia"          = "Deficiência",
  "Cotas"                = "Foi aprovado(a) em Regime de Cotas",
  "Cargo"                = "Cargo",
  "AreaAtuacao"          = "Área de atuação",
  "DataPosse"            = "Data posse",
  "DataFimSituacao"      = "Data de saída da situação",
  "Exclusao"             = "Exclusão de registro por erro"
)

# Renomeia as colunas no df_arq2
df_arq2_mapeado <- df_arq2 %>% 
  rename(any_of(depara_colunas))

# Define colunas de saída (desconsiderando Referência, CadOrigem e Inicio)
colunas_desconsiderar <- c("Referência", "CadOrigem", "Inicio")
colunas_ordem_original <- setdiff(colnames(df_arq1), c("CPF_clean", colunas_desconsiderar))

# Garante a existência de todas as colunas necessárias em df_arq2
for (col in colunas_ordem_original) {
  if (!col %in% colnames(df_arq2_mapeado)) {
    df_arq2_mapeado[[col]] <- NA_character_
  }
}

df_arq2_mapeado <- df_arq2_mapeado %>% select(all_of(colunas_ordem_original), CPF_clean)
df_arq1_filtrado <- df_arq1 %>% select(all_of(colunas_ordem_original), CPF_clean)

# 4. Processamento dos blocos por CPF
cpfs_arq2 <- unique(df_arq2_mapeado$CPF_clean)
cpfs_unicos_arq1 <- unique(df_arq1_filtrado$CPF_clean[df_arq1_filtrado$CPF_clean != ""])

df_final <- map_dfr(cpfs_unicos_arq1, function(cpf_atual) {
  linhas_arq1 <- df_arq1_filtrado %>% filter(CPF_clean == cpf_atual)
  
  if (cpf_atual %in% cpfs_arq2) {
    linhas_arq2 <- df_arq2_mapeado %>% filter(CPF_clean == cpf_atual)
    
    # Verifica se existe alguma linha em df_arq2 com SituacaoProfissional em (1, 2, 3)
    situacoes_validas <- c("1", "2", "3")
    tem_situacao_valida <- any(str_trim(linhas_arq2$SituacaoProfissional) %in% situacoes_validas, na.rm = TRUE)
    
    # Executa o preenchimento da DataFimSituacao quando SituacaoProfissional == 1, 2 ou 3
    if (tem_situacao_valida) {
      dt_inicio_arq1_str <- linhas_arq1$DataInicioSituacao[1]
      dt_inicio_obj <- converter_para_data(dt_inicio_arq1_str)
      
      if (!is.na(dt_inicio_obj)) {
        dt_fim_calculada <- format(dt_inicio_obj - days(1), "%d/%m/%Y")
        
        linhas_arq2 <- linhas_arq2 %>%
          mutate(
            SituacaoProfissional_limpa = str_trim(SituacaoProfissional),
            DataFimSituacao = if_else(
              SituacaoProfissional_limpa %in% situacoes_validas & (is.na(DataFimSituacao) | str_trim(DataFimSituacao) == ""),
              dt_fim_calculada,
              DataFimSituacao
            )
          ) %>% 
          select(-SituacaoProfissional_limpa)
      }
    }
    
    # Combina todas as linhas do CPF para desduplicação
    bloco_combinado <- bind_rows(linhas_arq1, linhas_arq2)
    
    # Desduplicação aprimorada:
    # 1. Cria uma flag indicando se DataFimSituacao está preenchida (TRUE/FALSE)
    # 2. Ordena colocando as que possuem DataFimSituacao preenchida no topo e dando preferência a df_arq1
    # 3. Executa distinct() desconsiderando DataFimSituacao, Origem e CPF_clean
    bloco_deduplicado <- bloco_combinado %>%
      mutate(tem_data_fim = !is.na(DataFimSituacao) & str_trim(DataFimSituacao) != "") %>%
      arrange(
        desc(tem_data_fim),                         # Prioriza linhas com DataFimSituacao preenchida
        factor(Origem, levels = c("df_arq1", "df_arq2")) # Em empate, prioriza df_arq1
      ) %>%
      distinct(across(-c(DataFimSituacao, Origem, CPF_clean, tem_data_fim)), .keep_all = TRUE) %>%
      select(-tem_data_fim)
    
    # Reordena o bloco final mantendo df_arq2 primeiro e df_arq1 depois
    bloco_deduplicado %>% 
      arrange(factor(Origem, levels = c("df_arq2", "df_arq1")))
    
  } else {
    linhas_arq1
  }
})

# Remove a coluna temporária de controle
df_final <- df_final %>% select(-CPF_clean)

# 5. Exportação para CSV (separador ponto e vírgula)
write_excel_csv2(df_final, "C:/Users/3894/Projetos R/MPM/Envio Mensal/202607/quadro pessoal e auxiliar/Resultado_Pessoal_Auxiliar_R.csv")

# Calcula o tempo total de execução
tempo_fim <- Sys.time()
tempo_execucao <- round(difftime(tempo_fim, tempo_inicio, units = "secs"), 2)

cat(sprintf("Arquivo 'Resultado_Pessoal_Auxiliar_R.csv' gerado com sucesso!\nTempo total de execução: %s segundos.\n", tempo_execucao))