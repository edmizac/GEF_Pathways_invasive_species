# Script name: A3_Impacto
# Script purpose: Realizar análises relacionadas a quantificação do impacto de 
# vias e vetores (Produto 1: Atividade 4.3 e produtos seguintes)

# Date created:
# 17/08/2023
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("tidyverse")
library("readxl")
# library("ggalluvial")
# library("treemap")

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 


# Import and wrangle data ------
# vv <- read_xlsx(here("Entregas", "Arquivos", "Dados", "Database-consolidada_Sem-ocorrencias.xlsx"))
vv <- read_xlsx(here("Entregas", "Arquivos", "Dados", "00_Vias-vetores-corrigido.xlsx"))


# EICAT -----

## Carregar presentes e ausentes/contidas ----
le_present_eicat <- read_xlsx(here("Material enviado pelo MMA", "Impacto ambiental - EICAT", "EICAT presentes"
                                   , "Produto 13_2 EICAT completo Fauna jun 2023.xlsx")) %>% 
  rename(
    `EICAT_Classificação` = Resultado,
    `EICAT_Confiança` = `Qual o seu grau de confiança na análise?`
    # `Nomes populares` = `Nomes comuns (tudo minúsculo, separados por vírgula)`
  ) %>% 
  dplyr::select(c(`Nome científico`, EICAT_Classificação, EICAT_Confiança))
# names(le_present_eicat)


le_auscont_eicat <- read_xlsx(here("Material enviado pelo MMA", "Impacto ambiental - EICAT", "EICAT ausentes e contidos"
                                   , "Produto 8_2_Anexo_Relatório sobre o processo de priorização das espécies (2).xlsx")
                              , sheet = "EICAT") %>% 
  rename(
    `EICAT_Classificação` = `Resultado FINAL EICAT`,
    `EICAT_Confiança` = `Qual o seu grau de confiança na análise?`,
    `Nome científico` = Espécie
    # `Nomes populares` = `Nomes comuns (tudo minúsculo, separados por vírgula)`
  ) %>% 
  dplyr::select(c(`Nome científico`, EICAT_Classificação, EICAT_Confiança))

# names(le_auscont_eicat)

# Join:
le <- dplyr::full_join(le_present_eicat, le_auscont_eicat)#, by = c("Nome cientifico"))
le$EICAT_Classificação %>% unique()

# Tirar DD (são desconsideradas na nossa metodologia e no Produto 10.3 do Zenni)
le <- le %>% 
  dplyr::filter(EICAT_Classificação != "Data deficiency (DD): Dados insuficientes") %>% 
  dplyr::filter(EICAT_Classificação != "Data deficiency (DD): Deficiente em dados")

# Remover NAs:
# le <- dplyr::filter(le, !is.na(`Nome científico`)) # não tem NAs




# EICAT por via + padronização -----

# Pegar apenas dados do EICAT:
db <- dplyr::left_join(vv, le, relationship = "many-to-many") # many-to-many porque uma espécie pode estar em mais que uma via 

# Filtrar as que não tem EICAT
db_na <- db %>% dplyr::filter(is.na(EICAT_Classificação)) # mesma coisa que o inner_join, mas aqui consigo usar !=
# db <- inner_join(db, le)
db <- db %>% dplyr::filter(!is.na(EICAT_Classificação)) # mesma coisa que o inner_join, mas aqui consigo usar !=

# # Save:
# db %>%
#   writexl::write_xlsx(path = here("Entregas", "A3_Impacto", "Database_consolidada_EICAT.xlsx"))
# db_na %>%
#   writexl::write_xlsx(path = here("Entregas", "A3_Impacto", "Database_consolidada_EICAT-DD-ou-NA.xlsx"))

db %>% names()

db$EICAT_Classificação %>% unique()

db$`Nome científico` %>% unique() # 219 spps com EICAT
db_na$`Nome científico` %>% unique() # 342 spps sem EICAT



## Gerar valores conforme Produto 02: ----
db <- db %>% 
  mutate(
    EICAT_Classificação = case_when(
      # EICAT_Classificação == "Dados insuficientes"  ~ NA  # Desconsiderados (conforme proposto no Produto 02)
      EICAT_Classificação == "Minimal concern (MC): Preocupação mínima" ~  "Preocupação mínima"
      , EICAT_Classificação == "Minor (MN): Baixo"                      ~ "Baixo"
      , EICAT_Classificação == "Moderate (MO): Moderado"                ~ "Moderado"
      , EICAT_Classificação == "Major (MR): Alto"                       ~ "Alto"
      , EICAT_Classificação == "Massive (MV): Muito Alto"               ~ "Muito Alto"
      , TRUE ~ EICAT_Classificação
    )
  ) %>% 
  mutate(
    EICAT_valor = case_when(
      # EICAT_Classificação == "Dados insuficientes"  ~ NA  # Desconsiderados (conforme proposto no Produto 02)
      EICAT_Classificação == "Preocupação mínima" ~  0.2
      , EICAT_Classificação == "Baixo"              ~ 0.4
      , EICAT_Classificação == "Moderado"           ~ 0.6
      , EICAT_Classificação == "Alto"               ~ 0.8
      , EICAT_Classificação == "Muito Alto"         ~ 1
      # , TRUE ~ EICAT_Classificação
    )
  )

db$EICAT_valor %>% unique()
db %>% str()

db$`Subcategoria CDB` %>% unique()
a <- db %>% dplyr::filter(`Subcategoria CDB` == "Agricultura e biocombustíveis")


# Agrupar Ausentes e Contidas
db <- db %>% 
  mutate(
    Situação = case_when(
      Situação == "Ausente" ~ "Ausente/Contida",
      Situação == "Contida" ~ "Ausente/Contida",
      TRUE ~ Situação
    )
  )


## Sumarizar valores médios do EICAT por via: ----

db$EICAT_valor %>% mean(., na.rm = TRUE)
db$EICAT_valor %>% sd(., na.rm = TRUE)

EICAT_summary <- db %>% 
  group_by(Situação, `Categoria CDB`, `Subcategoria CDB`) %>% 
  summarise(
    EICAT_mean = mean(EICAT_valor, na.rm = TRUE),
    EICAT_sd = sd(EICAT_valor, na.rm = TRUE),
    EICAT_median = median(EICAT_valor, na.rm = TRUE),
    n_spp = n_distinct(`Nome científico`)
  ) #%>% 
  # # Tirar valores NaN (sem dados)
  # mutate(
  #   EICAT_mean = case_when(
  #     is.nan(EICAT_mean) ~ NA,
  #     TRUE ~ EICAT_mean
  #   )
  # )

# # Save EICAT por via:
# EICAT_summary %>%
#   writexl::write_xlsx(path = here("Entregas", "A3_Impacto", "Database_consolidada_EICAT_summary.xlsx"))



## Outros summaries: -----

# Situação:
EICAT_summary <- db %>% 
  group_by(Situação) %>% 
  summarise(
    EICAT_mean = mean(EICAT_valor, na.rm = TRUE),
    EICAT_sd = sd(EICAT_valor, na.rm = TRUE),
    EICAT_median = median(EICAT_valor, na.rm = TRUE),
    n_spp = n_distinct(`Nome científico`)
  )
# # Save EICAT
# EICAT_summary %>%
#   writexl::write_xlsx(path = here("Entregas", "A3_Impacto", "Database_consolidada_EICAT_summary-situacao.xlsx"))


# Ambiente:
EICAT_summary <- db %>% 
  group_by(Ambiente) %>% 
  summarise(
    EICAT_mean = mean(EICAT_valor, na.rm = TRUE),
    EICAT_sd = sd(EICAT_valor, na.rm = TRUE),
    EICAT_median = median(EICAT_valor, na.rm = TRUE),
    n_spp = n_distinct(`Nome científico`)
  )
# # Save EICAT
# EICAT_summary %>%
#   writexl::write_xlsx(path = here("Entregas", "A3_Impacto", "Database_consolidada_EICAT_summary-ambiente.xlsx"))


# Situação e Ambiente:
EICAT_summary <- db %>% 
  group_by(Situação, Ambiente) %>% 
  summarise(
    EICAT_mean = mean(EICAT_valor, na.rm = TRUE),
    EICAT_sd = sd(EICAT_valor, na.rm = TRUE),
    EICAT_median = median(EICAT_valor, na.rm = TRUE),
    n_spp = n_distinct(`Nome científico`)
  ) %>% 
  dplyr::arrange(Situação, EICAT_mean)
# # Save EICAT
# EICAT_summary %>%
#   writexl::write_xlsx(path = here("Entregas", "A3_Impacto", "Database_consolidada_EICAT_summary-ambiente-e-situacao.xlsx"))


# Via, Situação e Ambiente:
EICAT_summary <- db %>% 
  group_by(Situação, Ambiente, `Subcategoria CDB`) %>% 
  summarise(
    EICAT_mean = mean(EICAT_valor, na.rm = TRUE),
    EICAT_sd = sd(EICAT_valor, na.rm = TRUE),
    EICAT_median = median(EICAT_valor, na.rm = TRUE),
    n_spp = n_distinct(`Nome científico`)
  ) %>% 
  dplyr::arrange(Situação, EICAT_mean)
# # Save EICAT
# EICAT_summary %>%
#   writexl::write_xlsx(path = here("Entregas", "A3_Impacto", "Database_consolidada_EICAT_summary-ambiente-situacao-e-via.xlsx"))






