# Script name: Avaliacao-Risco.R
# Script purpose: Realizar análises relacionadas a quantificação do impacto de 
# vias e vetores (Produto 1: Atividade 4.3 e produtos seguintes)
# 
# *** Derivar métrica/score de priorização *** #
# Exemplo: Vector risk (VR) de Brancatelli & Zalba 2018

# Date created:
# 17/08/2023
# Última atualização:
# 09/01/2024
# Author: Eduardo Zanette

## Notes --------------------------- 


## Packages -------------------------
library("here")
library("tidyverse")
library("readxl")
library("scales")

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 


# Import and wrangle data ------
# db_all <- read_xlsx(here("Entregas", "Arquivos", "Dados", "Occorrencias-Vias-vetores.xlsx"))

# Por via -----

## Quantidade ----
# prio_qtdd <- read_xlsx(here("Entregas", "A2_Qtdd", "Vias-vetores-corrigido-summary_old.xlsx")) %>% 
prio_qtdd <- read_xlsx(here("Entregas", "A2_Qtdd", "00_Vias-vetores-corrigido_summary.xlsx")) %>% 
  # Esse não escala, é o número total mesmo
  # mutate(
  #   riqueza_scaled = scales::rescale(eventos_introducao, to=c(0.01, 1))
  # ) %>% 
  rename(riqueza = eventos_introducao) %>%
  dplyr::select(Situação, `Subcategoria CDB`, riqueza) %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )
prio_qtdd$`Subcategoria CDB` %>% unique()


## Impacto -----
prio_impacto <- read_xlsx(here("Entregas", "A3_Impacto", "Database_consolidada_EICAT_summary.xlsx"))

prio_impacto$EICAT_mean %>% summary() # (não está entre 0.01 e 1)

prio_impacto <- prio_impacto %>% 
  mutate(
    impacto_scaled = scales::rescale(EICAT_mean, to = c(0.01, 1))
  ) %>% 
  dplyr::select(Situação, `Subcategoria CDB`, impacto_scaled) %>% 
  arrange(`Subcategoria CDB`) %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )
prio_impacto$`Subcategoria CDB` %>% unique()
  

### **** PROBLEMA 1: BAGAGENS E TURISMO E INTRODUÇÃO PARA FINS DE CONSERV. SÓ TEM UMA SPP ####


## Custo ----

### Ausentes:
prio_custo_auscont <- read_xlsx(here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Aus-Cont_sem-Anoplophora.xlsx")) %>% 
  mutate(
    CUSTO_scaled = scales::rescale(custo_medio_BRL_mi_sum, to = c(0.01, 1))
  ) %>% 
  dplyr::select(`Subcategoria CDB`, CUSTO_scaled) %>% 
  mutate(
    Situação = "Ausente/Contida"
  )

### **** PROBLEMA 2: USAMOS SÓ O CUSTO MÉDIO OU O CUSTO MÉDIO * N° SPP AUSENTES?  #####
# R: acho que só o custo médio mesmo

### Contidas:
prio_custo_pres <- read_xlsx(here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Presentes_se_sem-Apis-e-Columba.xlsx")) %>% 
  mutate(
    CUSTO_scaled = scales::rescale(`custo_acumulado_BRL_bi_sum`, to = c(0.01, 1))
  ) %>% 
  dplyr::select(`Subcategoria CDB`, CUSTO_scaled) %>% 
  mutate(Situação = "Presente")

prio_custo <- bind_rows(prio_custo_pres, prio_custo_auscont)


## Sensibilidade ----
prio_sensi <- read_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências_summary.xlsx")) %>% 
  mutate(
    sensibilidade_scaled = scales::rescale(sens_media, to = c(0.01, 1))
  ) %>% 
  dplyr::select(`Subcategoria CDB`, sensibilidade_scaled) %>% 
  mutate(Situação = "Presente") %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )
prio_sensi$`Subcategoria CDB` %>% unique()

### PROBLEMA 3: TEM UMA VIA NA AQUI



## Variação temporal ----
prio_temp_pres <- read_csv(here("Entregas", "A6_Variacao-temporal", "A6_Acumulacao_vias.csv")
                           , locale = locale("br", encoding = 'latin1')) %>% 
  rename(`Subcategoria CDB` = via) %>% 
  rename(slope_scaled = slope_padronizado) %>% 
  dplyr::select(`Subcategoria CDB`, slope_scaled) %>% 
  mutate(Situação = "Presente") %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )



## JOIN -----
prio_risco <- dplyr::left_join(prio_qtdd, prio_impacto) %>% 
  dplyr::left_join(prio_custo) %>% 
  dplyr::left_join(prio_sensi) %>% 
  dplyr::left_join(prio_temp_pres)


## Ranquear -----
prio_risco <- prio_risco %>% 
  rowwise() %>%
  mutate(
    # risco_via = riqueza * impacto_scaled * CUSTO_scaled, sensibilidade_scaled, slope_scaled
    risco_via = prod(c_across(riqueza:slope_scaled), na.rm=TRUE)
  ) %>% 
  ungroup() # to remove rowise()

# padronizar 0 a 100
prio_risco <- prio_risco %>% 
  group_by(Situação) %>% 
  mutate(
    risco_padronizado = scales::rescale(risco_via, to = c(0, 100), na.rm=TRUE)
  ) %>% 
  arrange(desc(risco_padronizado), .by_group = TRUE) %>% 
  mutate(Posição = 1:n())


# # Save:
# prio_risco %>%
#   writexl::write_xlsx(path = here("Entregas", "1_Protocolo_Avaliacao-de-Risco", "Risco_vias-e-vetores.xlsx"))
































# Por via + ambiente -----

## Quantidade ----
# prio_qtdd <- read_xlsx(here("Entregas", "A2_Qtdd", "Vias-vetores-corrigido-summary_old.xlsx")) %>% 
prio_qtdd <- read_xlsx(here("Entregas", "Arquivos", "Dados", "00_Vias-vetores-corrigido.xlsx")) %>% 
  dplyr::select(`Nome científico`, Situação, Ambiente, `Subcategoria CDB`) %>% 
  # Fazer Ausente e Contidas uma única categoria:
  mutate(
    Situação = case_when(
      Situação == "Ausente" ~ "Ausente/Contida",
      Situação == "Contida" ~ "Ausente/Contida",
      Situação == "Presente" ~ "Presente"
      # TRUE ~ Situaçao
    )
  ) %>% 
  distinct() %>% 
  dplyr::group_by(Situação, Ambiente, `Subcategoria CDB`) %>% 
  summarise(
    riqueza = n()
  ) %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )
prio_qtdd$`Subcategoria CDB` %>% unique()
prio_qtdd$riqueza %>% sum()

## Impacto -----
prio_impacto <- read_xlsx(here("Entregas", "A3_Impacto", "Database_consolidada_EICAT_summary-ambiente-situacao-e-via.xlsx"))

prio_impacto$EICAT_mean %>% summary() # (não está entre 0.01 e 1)

prio_impacto <- prio_impacto %>% 
  mutate(
    impacto_scaled = scales::rescale(EICAT_mean, to = c(0.01, 1))
  ) %>% 
  dplyr::select(Situação, Ambiente, `Subcategoria CDB`, impacto_scaled) %>% 
  arrange(`Subcategoria CDB`) %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )
prio_impacto$`Subcategoria CDB` %>% unique()


### **** PROBLEMA 1: BAGAGENS E TURISMO E INTRODUÇÃO PARA FINS DE CONSERV. SÓ TEM UMA SPP ####


## Custo ----

### Ausentes:
prio_custo_auscont <- read_xlsx(here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Aus-Cont_sem-Anoplophora-Ambiente-e-Situacao.xlsx")) %>% 
  mutate(
    CUSTO_scaled = scales::rescale(custo_medio_BRL_mi_sum, to = c(0.01, 1))
  ) %>% 
  dplyr::select(Ambiente, `Subcategoria CDB`, CUSTO_scaled) %>% 
  mutate(
    Situação = "Ausente/Contida"
  )

### **** PROBLEMA 2: USAMOS SÓ O CUSTO MÉDIO OU O CUSTO MÉDIO * N° SPP AUSENTES?  #####
# R: acho que só o custo médio mesmo

### Contidas:
prio_custo_pres <- read_xlsx(here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Presentes_se_sem-Apis-e-Columba-ambiente.xlsx")) %>% 
  mutate(
    CUSTO_scaled = scales::rescale(`custo_acumulado_BRL_bi_sum`, to = c(0.01, 1))
  ) %>% 
  dplyr::select(Ambiente, `Subcategoria CDB`, CUSTO_scaled) %>% 
  mutate(Situação = "Presente")

prio_custo <- bind_rows(prio_custo_pres, prio_custo_auscont)


## Sensibilidade ----
prio_sensi <- read_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências_summary_via-ambiente.xlsx")) %>% 
  mutate(
    sensibilidade_scaled = scales::rescale(sens_mean, to = c(0.01, 1))
  ) %>% 
  dplyr::select(Ambiente, `Subcategoria CDB`, sensibilidade_scaled) %>% 
  mutate(Situação = "Presente") %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )
prio_sensi$`Subcategoria CDB` %>% unique()

### PROBLEMA 3: TEM UMA VIA NA AQUI



## Variação temporal ----
prio_temp_pres <- read_csv(here("Entregas", "A6_Variacao-temporal", "A6_Acumulacao_vias-ambiente.csv")
                           , locale = locale("br", encoding = 'latin1')) %>% 
  rename(
    `Subcategoria CDB` = via,
    Ambiente = ambiente) %>% 
  rename(slope_scaled = slope_padronizado) %>% 
  dplyr::select(Ambiente, `Subcategoria CDB`, slope_scaled) %>% 
  mutate(Situação = "Presente") %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  ) %>% 
  dplyr::filter(!is.na(slope_scaled))



## JOIN -----
prio_risco_ambiente <- dplyr::left_join(prio_qtdd, prio_impacto) %>% 
  dplyr::left_join(prio_custo) %>% 
  dplyr::left_join(prio_sensi) %>% 
  dplyr::left_join(prio_temp_pres)


## Ranquear -----
prio_risco_ambiente <- prio_risco_ambiente %>% 
  rowwise() %>%
  mutate(
    # risco_via = riqueza * impacto_scaled * CUSTO_scaled, sensibilidade_scaled, slope_scaled
    risco_via = prod(c_across(riqueza:slope_scaled), na.rm=TRUE)
  ) %>% 
  ungroup() # to remove rowise()

# padronizar 0 a 100
prio_risco_ambiente <- prio_risco_ambiente %>% 
  group_by(Situação) %>% # TODAS SÃO RANQUEADAS EM CONJUNTO -> NOPE, SE NÃO AS NA FICAM TUDO PRA CIMA (AUS/CONT)
  mutate(
    risco_padronizado = scales::rescale(risco_via, to = c(0, 100), na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(desc(risco_padronizado), .by_group = FALSE) %>% # igual no Produto 02
  mutate(Posição = 1:n())


# # # Save:
# prio_risco_ambiente %>%
#   writexl::write_xlsx(path = here("Entregas", "1_Protocolo_Avaliacao-de-Risco", "Risco_vias-e-vetores_situacao-e-ambiente.xlsx"))














# Plots ----





