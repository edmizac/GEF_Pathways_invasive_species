# Script name: A4_Custo
# Script purpose: Realizar análises relacionadas a quantificação do InvaCost de 
# vias e vetores (Produto 1: Atividade 4.4 e produtos seguintes)

# Date created:
# 17/08/2023
# Author: Eduardo Zanette

# Notes --------------------------- 
# Ver depois: https://borisleroy.com/invacost/global_invasion_costs_scripts.html#Appendix_to_figure_B
#

# Packages -------------------------
library("here")
library("tidyverse")
library("readxl")
library("invacost")
library("stringr")

# Options -------------------------
# (plotting, memory limit, decimal digits)


# Import and wrangle data ------

## Vias -----
vv <- read_xlsx(here("Entregas", "Arquivos", "Dados", "00_Vias-vetores-corrigido.xlsx")) %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )
vv$`Subcategoria CDB` %>% unique()

## eventDate -----

# Data de chegada da EEI no Brasil
evdate_fauna <- read_excel(path = here("Material enviado pelo MMA", "Listas", "Lista presentes"
                                       , "13_4_BD_ocorr_fauna.xlsx"))
evdate_flora <- read_excel(path = here("Material enviado pelo MMA", "Listas", "Lista presentes"
                                       , "13_4_BD_ocorr_flora.xlsx"))

evdate <- dplyr::bind_rows(evdate_fauna, evdate_flora) %>% 
  rename(
    `Nome científico` = scientificName
  ) %>% 
  mutate(
    Genero = str_extract(`Nome científico`, "^\\b\\w+")
  ) %>% 
  dplyr::select(`Nome científico`, Genero, eventDate, year, ID.)

evdate$Genero %>% unique()


# Arrumar nomes científicos e gêneros para bater com a vv:
evdate$`Nome científico` %>% unique()

# evdate %>% dplyr::filter(`Nome científico` == "Callithrix jacchus x Callithrix penicillata x Callithrix aurita")
evdate %>% dplyr::filter(`Nome científico` == "Callithrix jacchus x C. penicillata x C. aurita")
# evdate %>% dplyr::filter(`Nome científico` == "Nandayus nenday ")
evdate %>% dplyr::filter(`Nome científico` == "Nandayus nenday")
# evdate %>% dplyr::filter(`Nome científico` == "Cinnamomum burmanni ")
# evdate %>% dplyr::filter(`Nome científico` == "Cinnamomum burmannii")
evdate %>% dplyr::filter(`Nome científico` == "Pseudoplatystoma corruscans")
# evdate %>% dplyr::filter(`Nome científico` == "Pseudoplatystoma corruscans x Pseudoplatystoma reticulatum")
evdate %>% dplyr::filter(`Nome científico` == "Pseudoplatystoma corruscans x P. reticulatum")
evdate %>% dplyr::filter(`Nome científico` == "Crocosmia crocosmiiflora")
# evdate %>% dplyr::filter(`Nome científico` == "Crocosmia x crocosmiiflora")
# evdate %>% dplyr::filter(`Nome científico` == "Kappaphycus alvarezzi")
# evdate %>% dplyr::filter(`Nome científico` == "Kappaphycus alvarezii")
# evdate %>% dplyr::filter(`Nome científico` == "Neovison vison")
# evdate %>% dplyr::filter(`Nome científico` == "Mustela vison")
# 
# evdate <- evdate %>%
#   mutate(
#     `Nome científico` = case_when(
#       `Nome científico` == "Callithrix jacchus x Callithrix penicillata x Callithrix aurita"  ~ "Callithrix jacchus x C. penicillata x C. aurita",
#       `Nome científico` == "Nandayus nenday " ~ "Nandayus nenday",
#       `Nome científico` == "Cinnamomum burmanni " ~ "Cinnamomum burmannii",
#       `Nome científico` == "Pseudoplatystoma corruscans x Pseudoplatystoma reticulatum"  ~ "Pseudoplatystoma corruscans x P. reticulatum",
#       `Nome científico` == "Crocosmia crocosmiiflora" ~ "Crocosmia x crocosmiiflora",
#       `Nome científico` == "Kappaphycus alvarezii" ~ "Kappaphycus alvarezzi",
#       `Nome científico` == "Mustela vison" ~ "Neovison vison",
#       TRUE ~ `Nome científico`
#     )
#   )

# Pegar o menor ano de registro no Brasil:
evdate <- evdate %>% 
  group_by(`Nome científico`) %>% 
  slice_min(year) %>% 
  dplyr::filter(!is.na(year)) # 572 datas de primeira entrada pra todas as EEIs



## InvaCost -----

# Read: 
inva <- read_xlsx(path = here("Entregas", "A4_Custo", "Database_Invacost.xlsx"))

# Alterar valor de "Lonicera japonica" # Wang 2011: https://core.ac.uk/download/pdf/147136842.pdf
inva %>% dplyr::filter(`Nome científico` == "Lonicera japonica") -> a
inva <- inva %>% 
  mutate(
    Cost = case_when(
      `Nome científico` == "Lonicera japonica" & Authors == "Wang, H. H." ~ 188687966, # valor por ano corrigido]
      TRUE ~ Cost
    )
  )


# # Alternative: Get online:
# # https://borisleroy.com/invacost/global_invasion_costs_scripts.html
# # https://github.com/Farewe/invacost?tab=readme-ov-file#installation
# inva <- invacost::getInvaCostVersion()
# inva %>% dplyr::filter(Species == "Lonicera japonica") -> a
# names(inva)
# inva <- inva %>% 
#   dplyr::select(c(1:2, Species, Genus, Authors
#                   , Geographic_region, Official_country, Spatial_scale
#                   , Period_of_estimation
#                   # , Probable_starting_year_adjusted, Probable_ending_year_adjusted # Vou usar os do eventDate do Zenni, então não precisa
#                   , Method_reliability, Method_reliability_refined # Para saber se as estimativas são confiáveis
#                   , Occurrence
#                   # , Cost_estimate_per_year_2017_USD_PPP # Diagne usa o exchange porque o PPP nao tem pra todo país
#                   , Cost_estimate_per_year_2017_USD_exchange_rate # Vamos usar o exchange rate
#                   )) %>% 
#   rename(
#     `Nome científico` = Species,
#     Genero = Genus,
#     # Start_year = Probable_starting_year_adjusted,
#     # End_year = Probable_ending_year_adjusted,
#     Cost = Cost_estimate_per_year_2017_USD_exchange_rate
#   )
# 
# 
# # Remover NAs:
# inva <- inva %>% 
#   dplyr::filter(!is.na(Cost))
# 
# nrow(inva)
# 
# # Remove uncertain time estimates: (não tem)
# uncertain.starts <- inva[which(inva$Time_range == "Period" &
#                                      is.na(inva$Start_year)), ]
# nrow(uncertain.starts)
# 
# # No info about whether cost was annual or over a period: (não tem)
# unknown.periods <- inva[which(is.na(inva$Time_range)), ]
# nrow(unknown.periods) 
# 
# # # (Não precisa remover já que não tem nada errado) Applying the filter
# # if(nrow(uncertain.starts) + nrow(unknown.periods) > 0)
# # {
# #   inva <- inva[-which(inva$Cost_ID %in% c(uncertain.starts$Cost_ID,
# #                                                       unknown.periods$Cost_ID)), ]
# # }
# # nrow(inva)
# 
# 
# # # Usar estimativas apenas na escala de país:
# # inva <- inva %>% 
# #   dplyr::filter(Spatial_scale == "Country")


# # Save:
# inva %>% 
#   writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_Invacost.xlsx"))



# Join
vv_inva <- dplyr::left_join(vv, inva, relationship = "many-to-many")

# Checar quantas observações tem pra nossa database de EEIs:
vv_inva %>% nrow()
vv_inva %>% names()
vv_inva$Situação %>% unique()
vv_inva$Cost_ID %>% n_distinct()
vv_inva %>% dplyr::filter(Situação != "Presente")

## Pegar valores médios por espécie -----
inva_sp <- vv_inva %>% 
  group_by(`Categoria CDB`, `Subcategoria CDB`, `Nome científico`, Situação, Ambiente) %>% 
  summarise(
    custo_medio = mean(Cost, na.rm = TRUE),
    custo_mediano = median(Cost, na.rm = TRUE),
    custo_sd = sd(Cost, na.rm = TRUE),
    custo_se = sd(Cost)/sqrt(n()),
    # n = n_distinct(Cost_ID),
    na = sum(is.na(Cost)),
    n_countries = n_distinct(Official_country)
  ) %>% 
  # replace NaNs:
  mutate(
    custo_medio = case_when(
      is.nan(custo_medio) ~ NA,
      TRUE ~ custo_medio
    )
  ) %>% ungroup()
# #Save
# inva_sp %>%
#     writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-spps.xlsx"))


# Checar dados descritivos:
inva_sp %>% nrow()
inva_sp %>% names()
inva_sp$Situação %>% unique()
inva_sp$`Nome científico` %>% unique()
inva_sp %>% dplyr::filter(Situação != "Presente")


# Summary por via
inva_summary_via <- vv_inva %>% 
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
  summarise(
    custo_medio = mean(Cost, na.rm = TRUE),
    custo_mediano = median(Cost, na.rm = TRUE),
    custo_sd = sd(Cost, na.rm = TRUE),
    n = n(),
    na = sum(is.na(Cost)),
    n_countries = n_distinct(Official_country)
  ) %>% 
  # replace NaNs:
  mutate(
    custo_medio = case_when(
      is.nan(custo_medio) ~ NA,
      TRUE ~ custo_medio
    )
  )
# #Save
# inva_summary_via %>%
#     writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias.xlsx"))


# Summary por via + situação
inva_summary_via_situacao <- vv_inva %>% 
  group_by(`Categoria CDB`, `Subcategoria CDB`, Situação, Ambiente) %>% 
  summarise(
    custo_medio = mean(Cost, na.rm = TRUE),
    custo_mediano = median(Cost, na.rm = TRUE),
    custo_sd = sd(Cost, na.rm = TRUE),
    n = n(),
    na = sum(is.na(Cost)),
    n_countries = n_distinct(Official_country)
  ) %>% 
  # replace NaNs:
  mutate(
    custo_medio = case_when(
      is.nan(custo_medio) ~ NA,
      TRUE ~ custo_medio
    )
  )
# #Save
# inva_summary_via_situacao %>%
#     writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias-situação.xlsx"))


# Filtrar spps com NAs:
inva_sp <- inva_sp %>% filter(!is.na(custo_medio)) %>% ungroup()

# Numero de spps com invacost:
inva_sp$`Nome científico` %>% unique() # 147 spps com invacost

# Numero de vias com invacost:
# sum(!is.na(inva_summary_via$custo_medio)) # 37 vias
n_distinct(inva_sp$`Subcategoria CDB`) # 37 vias

inva_summary_via_n <- vv_inva %>% 
  group_by(Situação) %>% 
  dplyr::filter(!is.na(Cost)) %>% 
  summarise(
    n_vias = n_distinct(`Subcategoria CDB`),
    n_spp = n_distinct(`Nome científico`)
    )
# #Save
# inva_summary_via_n %>%
#     writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias-situação-n.xlsx"))


# Check NAs
inva_na <- inva_summary_via %>% filter(is.na(custo_medio)) 
# #Save
# inva_na %>%
#     writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias-situação-NA.xlsx"))

a <- inva_summary_via_situacao %>% dplyr::filter(is.na(custo_medio)) %>% 
  dplyr::filter(Situação == "Presente") ; a$`Subcategoria CDB` %>% unique()
a <- inva_summary_via_situacao %>% dplyr::filter(is.na(custo_medio)) %>% 
  dplyr::filter(Situação == "Ausente") ; a$`Subcategoria CDB` %>% unique()
a <- inva_summary_via_situacao %>% dplyr::filter(is.na(custo_medio)) %>% 
  dplyr::filter(Situação == "Contida") ; a$`Subcategoria CDB` %>% unique()


# Numero de estimativas com invacost:
# # Filtrar spps sem invacost:
nrow(inva_sp) # 437 estimativas (mas tem espécies em várias vias)

inva_sp$`Nome científico` %>% unique()
inva_sp$`Subcategoria CDB` %>% unique()


# Mudar nomes e levels (diferente dos outros plots dos outros)
inva_sp <- inva_sp %>% 
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Aquário, terrário e pet (incluindo comida viva para essas espécies)" ~ "Aquário, terrário e pet",
      `Subcategoria CDB` == "Jardim botânico, zoológico, aquário (não domésticos)" ~ "Jardim botânico, zoológico, aquário",
      # `Subcategoria CDB` == "Caça na natureza" ~ "Caça na natureza",
      # `Subcategoria CDB` == "Controle biológico" ~ "Controle biológico",
      # `Subcategoria CDB` == "Contaminantes em animais (exceto parasitas, espécies transportadas pelo hospedeiro / vetor)" ~ "Contaminantes em animais (exceto parasitas)",
      # `Subcategoria CDB` == "Equipamento de pesca / pesca com anzol" ~ "Equipamento de pesca",
      # `Subcategoria CDB` == "Comida viva e isca viva" ~ "Comida viva e isca viva",
      # `Subcategoria CDB` == "Contaminação em material para viveiros" ~ "Contaminação em material para viveiros",
      # `Subcategoria CDB` == "Introdução para fins de conservação" ~ "Introdução para fins de conservação",
      # `Subcategoria CDB` == "Parasitas em animais (incluindo espécies transportadas pelo hospedeiro e vetor)" ~ "Parasitas em animais",
      # `Subcategoria CDB` == "Presença clandestina sobre / dentro de avião" ~ "Presença em avião",
      # `Subcategoria CDB` == "Fazendas de peles de animais" ~ "Fazendas de peles de animais",
      # `Subcategoria CDB` == "Melhoramento de paisagem/flora/fauna na natureza" ~ "Melhoramento de paisagem",
      # `Subcategoria CDB` == "Pessoas e bagagens / equipamento (especialmente turismo)" ~ "Bagagens e turismo",
      # `Subcategoria CDB` == "Veículos (carro, trem...)" ~ "Veículos",    # modificação 3
      # `Subcategoria CDB` == "Veículos (carro, trem, ...)" ~ "Veículos",  # modificação 3
      # `Subcategoria CDB` == "Veículos (carro, trem,...)" ~ "Veículos",
      # `Subcategoria CDB` == "Contaminação de comida (incluindo comida viva)" ~ "Contaminação de comida incluindo comida viva",    # modificação 2
      # `Subcategoria CDB` == "Contaminação de comida" ~ "Contaminação de comida incluindo comida viva",                            # modificação 2
      # `Subcategoria CDB` == "Contaminação em sementes" ~ "Contaminação em sementes",
      # `Subcategoria CDB` == "Parasitas em plantas (incluindo espécies transportadas pelo hospedeiro e vetor)" ~ "Parasitas em plantas",
      # `Subcategoria CDB` == "Plantas cultivadas" ~ "Plantas cultivadas",
      # `Subcategoria CDB` == "Fins ornamentais (excluindo produção alimentar)" ~ "Fins ornamentais",
      # `Subcategoria CDB` == "Produção florestal (incluindo reflorestamento)" ~ "Produção florestal e reflorestamento",
      `Subcategoria CDB` == "Controle de erosão / estabilização de dunas (quebra vento, cerca viva...)" ~ "Controle de erosão, quebra vento ou cerca viva",
      TRUE ~ as.character(`Subcategoria CDB`)
    )
  ) %>% 
  ungroup()



# Espécies PRESENTES ------

## Puxar eventDate ----

inva_evdate <- dplyr::left_join(inva_sp, evdate, relationship = "many-to-many") %>% 
  dplyr::select(-c(ID., eventDate)) %>% 
  distinct()

# Filtrar NAs:
inva_evdate <- inva_evdate %>% filter(!is.na(year)) # --382-- 234 observações de invacost para...
inva_evdate$`Nome científico` %>% unique() # 84 spp
inva_evdate$`Subcategoria CDB` %>% unique() # de 35 vias



## Somar valores anuais -----
inva_evdate_pres <- inva_evdate %>% 
  dplyr::filter(Situação == "Presente") %>% 
  mutate(
    duração = 2023 - year
  ) %>% 
  # Corrigir durações = 0 -> 1
  mutate(
    duração = case_when(
      duração == 0 ~ 1,
      TRUE ~ duração
    )
  ) %>% 
  mutate(
    custo_acumulado = custo_medio * duração
  )



# Transformar para BRL (cotação de 2017)

# Usando Average exchange rate in 2017: 0.3134 USD # https://www.exchangerates.org.uk/BRL-USD-spot-exchange-rates-history-2017.html

inva_evdate_pres <- inva_evdate_pres %>% 
  mutate(
    custo_acumulado_BRL_bi = custo_acumulado * 0.3134 / 1000000000
  )

## Ordenar dados:
inva_evdate_pres <- inva_evdate_pres %>% 
  group_by(`Subcategoria CDB`) %>% 
  arrange(custo_acumulado_BRL_bi)

# # Qtdd de espécies pro plot
# n_spp <- inva_evdate_pres %>% 
#   group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
#   summarise(
#     n = n_distinct(`Nome científico`),
#     custo_acumulado_BRL_bi = sum(custo_acumulado_BRL_bi)
#   ) %>% 
#   arrange(fct_reorder(`Subcategoria CDB`, custo_acumulado_BRL_bi)) %>% 
#   mutate(
#     `Subcategoria CDB` = fct_reorder(`Subcategoria CDB`, custo_acumulado_BRL_bi, .fun = sum)
#   )

# Qtdd de espécies pro plot
inva_evdate_pres <- inva_evdate_pres %>% 
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
  mutate(
    n_spp = n_distinct(`Nome científico`)
  ) %>% 
  arrange(fct_reorder(`Subcategoria CDB`, custo_acumulado_BRL_bi)) %>% 
  mutate(
    `Subcategoria CDB` = fct_reorder(`Subcategoria CDB`, custo_acumulado_BRL_bi, .fun = sum)
  ) %>% 
  ungroup()

### Somar custos por via ----
inva_evdate_pres_summary <- inva_evdate_pres %>%
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>%
  summarise(
    custo_acumulado_BRL_bi = sum(custo_acumulado_BRL_bi)
  ) %>% 
  ungroup()



## Plots -----

theme_set(theme_bw(base_size = 18))
theme_update(
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  
)

options(scipen=999)

palCDB_cat <- rev(c("#9359a5", "#9c83bd", "#8c97c5" , "#26828f", "#52c56a", "#badd27"))




# Plot
inva_evdate_pres_summary %>% 
  ungroup() %>%
  mutate(
    `Subcategoria CDB` = fct_reorder(`Subcategoria CDB`, custo_acumulado_BRL_bi, .fun = sum)
  ) %>%
  # ggplot(aes(x = `Subcategoria CDB`
  ggplot(aes(x = fct_reorder(`Subcategoria CDB`, custo_acumulado_BRL_bi,  .fun = sum)
             , y = custo_acumulado_BRL_bi, fill = `Categoria CDB`)) +
  geom_col(
    # position = "dodge" # isso ferra o fct_reoder por algum motivo
    position = "dodge" # isso ferra o fct_reoder por algum motivo
    ) +
  scale_fill_manual(values = palCDB_cat) +
  coord_flip() +
  # geom_text(aes(label=n), stat = "count", colour="red", size=4) +
  theme_update(
    legend.position = "bottom"#,
    # axis.text.y = element_text(size = 14),
    # axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  xlab("Vias e vetores") +
  ylab("Custo acumulado (bilhões BRL)") +
  ggtitle("Vias e vetores com maior custo acumulado no Brasil")

# # Save:
# ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_medio_BRL_vias-Presentes.png"),
#        dpi = 300,  width = 13, height = 10, units = "in")


# Validando se o plot soma os valores:
inva_evdate_pres_summary %>% 
  dplyr::filter(`Subcategoria CDB` == "Pesquisa e criação ex-situ") %>% 
  # summarise(sum = sum(custo_acumulado_BRL_bi )) # 1186 -> bem perto do que parece no plot -> OK
  dplyr::select(custo_acumulado_BRL_bi) # 1186 -> bem perto do que parece no plot -> OK

n_spp <- inva_evdate_pres %>% 
  dplyr::select(`Subcategoria CDB`, n_spp) %>% 
  distinct()



### Plot com n espécies no label: ----

inva_evdate_pres_summary_se <- inva_evdate_pres %>%
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>%
  summarise(
    custo_acumulado_BRL_bi_sum = sum(custo_acumulado_BRL_bi, na.rm=TRUE),
    custo_acumulado_BRL_bi_mean = mean(custo_acumulado_BRL_bi, na.rm=TRUE),
    custo_acumulado_BRL_bi_sd = sd(custo_acumulado_BRL_bi, na.rm=TRUE),
    custo_acumulado_BRL_bi_se = custo_acumulado_BRL_bi_sd/sqrt(n()),
    n_spp = n_distinct(`Nome científico`)
  )
# # Save:
# inva_evdate_pres_summary_se %>%
#   mutate(
#     `Subcategoria CDB` = case_when(
#       `Subcategoria CDB` == "Aquário, terrário e pet" ~ "Aquário, terrário e pet (incluindo comida viva para essas espécies)",
#       `Subcategoria CDB` == "Jardim botânico, zoológico, aquário" ~ "Jardim botânico, zoológico, aquário (não domésticos)",
#       TRUE ~ `Subcategoria CDB`
#     )
#   ) %>%
#   writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Presentes_se.xlsx"))


inva_evdate_pres_summary_se %>%
# inva_evdate_pres_summary %>% 
  ungroup() %>% 
  mutate(
    `Subcategoria CDB` = fct_reorder(`Subcategoria CDB`, custo_acumulado_BRL_bi_sum, .fun = max)
  ) %>%
  ggplot(aes(x = `Subcategoria CDB`
             , y = custo_acumulado_BRL_bi_sum, fill = `Categoria CDB`)) +
  geom_col(position = "dodge") +
  # geom_errorbar(aes(ymin = custo_acumulado_BRL_bi_sum - custo_acumulado_BRL_bi_sd,
  #                   ymax = custo_acumulado_BRL_bi_sum + custo_acumulado_BRL_bi_sd)) +
  # geom_errorbar(aes(ymin = custo_acumulado_BRL_bi_sum - (custo_acumulado_BRL_bi_sum < 0)*custo_acumulado_BRL_bi_sd,
  #                   ymax = custo_acumulado_BRL_bi_sum + (custo_acumulado_BRL_bi_sum > 0)*custo_acumulado_BRL_bi_sd
  # ), lwd = 0.1
  # )+
  scale_fill_manual(values = palCDB_cat) +
  coord_flip() +
  # geom_text(aes(label=n), stat = "count", colour="red", size=4) +
  theme_update(
    legend.position = "bottom",
    # axis.text.y = element_text(size = 14)#,
    # axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  ggtitle("Vias e vetores com maior custo acumulado no Brasil") +
  xlab("Vias e vetores") +
  ylab("Custo acumulado (bilhões BRL)") +
  ggnewscale::new_scale_fill() +
  geom_label(#inherit.aes = FALSE,
    # , data = n_spp
    # , aes(x = `Subcategoria CDB`
    #     , y = custo_acumulado_BRL_bi
    #     , label = n_spp)
    aes(label = n_spp)#, fill = "white")
    , nudge_y = 5
    , label.padding = unit(0.05, "lines")
    , show.legend = FALSE
    , label.size = NA # tirar borda
    , size = 7
  )
# # # Save:
# ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_medio_BRL_vias-Presentes-n.png"),
#        dpi = 300,  width = 13, height = 10, units = "in")



# Checar spps responsáveis:
tgt <- c("Dispersão natural (vias 1 a 5) por fronteiras", "Animais domésticos",
         "Outra soltura intencional", "Pesquisa e criação ex-situ")

# inva_evdate_pres_main <- inva_evdate_pres_summary %>% 
  # dplyr::filter(`Subcategoria CDB` %in% tgt) %>% 
  # dplyr::select(1:Situação, duração:n_spp) %>%
  # distinct()

# inva_evdate_pres_main <- inva_evdate %>%
inva_evdate_pres_main <- inva_evdate_pres %>%
  dplyr::filter(`Subcategoria CDB` %in% tgt) %>% 
  dplyr::select(1:Situação, duração:n_spp) %>%
  distinct()

inva_evdate_pres_main %>% 
  # mutate(
    # `Nome científico` = fct_reorder(`Nome científico`, custo_acumulado_BRL_bi)
  # ) %>%
  ggplot(aes(y = fct_reorder(`Nome científico`, custo_acumulado_BRL_bi)
             , x = custo_acumulado_BRL_bi
             , fill = `Subcategoria CDB`)) +
  geom_col() +
  # # theme_bw() +
  theme_update(
    legend.position = "bottom"
  ) +
  # scale_fill_manual(values = c()) +
  ggtitle("EEIs das 4 vias e vetores com maior custo acumulado no Brasil") +
  ylab("") +
  xlab("Custo acumulado (bilhões BRL)") +
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
# # # Save:
# ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_medio_BRL_vias-spps-importantes-Presentes.png"),
#        dpi = 300,  width = 13, height = 8, units = "in")

spp_out <- c("Apis mellifera", "Columba livia") #, "Aedes aegypti")



### Sem espécies com maior dano ----
inva_evdate_pres_summary_se <- inva_evdate_pres %>%
  dplyr::filter(!`Nome científico` %in% spp_out) %>% 
  group_by(Situação, #Ambiente, 
           `Categoria CDB`, `Subcategoria CDB`) %>% 
  summarise(
    custo_acumulado_BRL_bi_sum = sum(custo_acumulado_BRL_bi, na.rm=TRUE),
    custo_acumulado_BRL_bi_mean = mean(custo_acumulado_BRL_bi, na.rm=TRUE),
    custo_acumulado_BRL_bi_sd = sd(custo_acumulado_BRL_bi, na.rm=TRUE),
    custo_acumulado_BRL_bi_se = custo_acumulado_BRL_bi_sd/sqrt(n()),
    n_spp = n_distinct(`Nome científico`)
  )
# # # Save:
# inva_evdate_pres_summary_se %>%
#   mutate(
#     `Subcategoria CDB` = case_when(
#       `Subcategoria CDB` == "Aquário, terrário e pet" ~ "Aquário, terrário e pet (incluindo comida viva para essas espécies)",
#       `Subcategoria CDB` == "Jardim botânico, zoológico, aquário" ~ "Jardim botânico, zoológico, aquário (não domésticos)",
#       TRUE ~ `Subcategoria CDB`
#     )
#   ) %>%
#   writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Presentes_se_sem-Apis-e-Columba-ambiente.xlsx"))






inva_evdate_pres_summary_se %>% 
  ungroup() %>% 
  mutate(
    `Subcategoria CDB` = fct_reorder(`Subcategoria CDB`, custo_acumulado_BRL_bi_sum, .fun = max)
  ) %>%
  ggplot(aes(x = `Subcategoria CDB`
             , y = custo_acumulado_BRL_bi_sum, fill = `Categoria CDB`)) +
  geom_col(position = "dodge") +
  # geom_errorbar(aes(ymin = custo_acumulado_BRL_bi_sum - custo_acumulado_BRL_bi_sd,
  #                   ymax = custo_acumulado_BRL_bi_sum + custo_acumulado_BRL_bi_sd)) +
  # geom_errorbar(aes(ymin = custo_acumulado_BRL_bi_sum - (custo_acumulado_BRL_bi_sum < 0)*custo_acumulado_BRL_bi_sd,
  #                   ymax = custo_acumulado_BRL_bi_sum + (custo_acumulado_BRL_bi_sum > 0)*custo_acumulado_BRL_bi_sd
  # ), lwd = 0.1
  # )+
  scale_fill_manual(values = palCDB_cat) +
  coord_flip() +
  # geom_text(aes(label=n), stat = "count", colour="red", size=4) +
  # theme_update(
  #   legend.position = "bottom"#,
  #   # axis.text.y = element_text(size = 14),
  #   # axis.text.x = element_text(angle = 30, hjust = 1)
  # ) +
  ggtitle("Vias e vetores com maior custo acumulado no Brasil",
          expression(paste("Desconsiderando ", italic("Apis mellifera, Columba livia")))) +#, " e ", italic("Aedes aegypti")))) +
  xlab("Vias e vetores") +
  ylab("Custo acumulado (bilhões BRL)") +
  ggnewscale::new_scale_fill() +
  geom_label(#inherit.aes = FALSE,
    # , data = n_spp
    # , aes(x = `Subcategoria CDB`
    #     , y = custo_acumulado_BRL_bi
    #     , label = n_spp)
    aes(label = n_spp)#, fill = "white")
    , nudge_y = 3
    , label.padding = unit(0.05, "lines")
    , show.legend = FALSE
    , label.size = NA # tirar borda
    , size = 7
  )
# # # Save:
# ggsave(filename = here("Entregas", "A4_Custo"
#                        , "InvaCost_medio_BRL_vias-n-errorbar-sd_sem-spps-importantes.png"),
#        dpi = 300,  width = 14, height = 10, units = "in")


### Checar spps responsáveis
# Checar spps responsáveis:
tgt <- c(#'Jardim botânico, zoológico e aquário', 'Fins ornamentais'
           'Presença em navio e embarcação', 'Container/volume')

inva_evdate_pres_main <- inva_evdate_pres %>% 
  dplyr::filter(`Subcategoria CDB` %in% tgt) %>% 
  dplyr::select(1:custo_se, duração:n_spp) %>%
  distinct()

inva_evdate_pres_main %>% 
  # mutate(
  # `Nome científico` = fct_reorder(`Nome científico`, custo_acumulado_BRL_bi)
  # ) %>%
  ggplot(aes(y = fct_reorder(`Nome científico`, custo_acumulado_BRL_bi)
             , x = custo_acumulado_BRL_bi
             , fill = `Subcategoria CDB`)) +
  geom_col() +
  # theme_bw() +
  theme_update(
    legend.position = "bottom",
  ) +
  # scale_fill_manual(values = c()) +
  ggtitle("EEIs das 2 vias e vetores com maior custo acumulado no Brasil",
          expression(paste("Desconsiderando ", italic("Apis mellifera, Columba livia")))) +#, " e ", italic("Aedes aegypti")))) +
  ylab("") +
  xlab("Custo acumulado (bilhões BRL)") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE))
# # # Save:
# ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_medio_BRL_vias-spps-importantes-spps-Presentes.png"),
#        dpi = 300,  width = 13, height = 8, units = "in")










# Espécies AUSENTES E CONTIDAS ------

## Somar valores anuais -----
inva_aus <- inva_sp %>% 
  dplyr::filter(Situação != "Presente")

inva_aus$`Nome científico` %>% unique()
inva_aus$`Subcategoria CDB` %>% unique()

# Transformar para BRL (cotação de 2017)

# Usando Average exchange rate in 2017: 0.3134 USD # https://www.exchangerates.org.uk/BRL-USD-spot-exchange-rates-history-2017.html

inva_aus <- inva_aus %>% 
  mutate(
    custo_medio_BRL_mi = custo_medio * 0.3134 / 1000000
  )

## Ordenar dados:
inva_aus <- inva_aus %>% 
  group_by(`Categoria CDB`) %>% 
  arrange(custo_medio_BRL_mi)


# Qtdd de espécies pro plot
inva_aus <- inva_aus %>% 
  group_by(Situação, `Categoria CDB`, `Subcategoria CDB`) %>% 
  mutate(
    n_spp = n_distinct(`Nome científico`)
  ) %>% 
  arrange(fct_reorder(`Subcategoria CDB`, custo_medio_BRL_mi)) %>% 
  mutate(
    `Subcategoria CDB` = fct_reorder(`Subcategoria CDB`, custo_medio_BRL_mi, .fun = sum)
  )

### Somar custos por via ----
inva_aus_summary <- inva_aus %>%
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>%
  summarise(
    custo_medio_BRL_mi = sum(custo_medio_BRL_mi),
    n_spp = n_distinct(`Nome científico`)
  )
# # Save:
# inva_aus_summary %>%
#   mutate(
#     `Subcategoria CDB` = case_when(
#       `Subcategoria CDB` == "Aquário, terrário e pet" ~ "Aquário, terrário e pet (incluindo comida viva para essas espécies)",
#       `Subcategoria CDB` == "Jardim botânico, zoológico, aquário" ~ "Jardim botânico, zoológico, aquário (não domésticos)",
#       TRUE ~ `Subcategoria CDB`
#     )
#   ) %>%
#   writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Aus-Cont_se.xlsx"))





## Plots -----

# theme_set(theme_bw(base_size = 20))
# theme_update(
#   axis.text.x = element_text(size = 12)
# )

options(scipen=999)

palCDB_cat <- rev(c("#9359a5", "#9c83bd", "#8c97c5" , "#26828f", "#52c56a", "#badd27"))




# Plot
inva_aus_summary %>% 
  # group_by(`Subcategoria CDB`) %>% 
  # mutate(
  #   n = n()
  #   ) %>% 
  mutate(
    `Subcategoria CDB` = fct_reorder(`Subcategoria CDB`, custo_medio_BRL_mi, .fun = sum)
  ) %>% 
  ggplot(aes(x = fct_reorder(`Subcategoria CDB`, custo_medio_BRL_mi)
             , y = custo_medio_BRL_mi, fill = `Categoria CDB`)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = palCDB_cat) +
  coord_flip() +
  # geom_text(aes(label=n), stat = "count", colour="red", size=4) +
  theme_update(
    legend.position = "bottom",
  #   axis.text.y = element_text(size = 14),
  #   axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  xlab("Vias e vetores") +
  ylab("Custo acumulado (milhões BRL)") +
  ggtitle("Custo acumulado de vias e vetores de EEIs Ausentes/Contidas")

# # # Save:
# ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_medio_BRL_vias_Aus-Cont.png"),
#        dpi = 300,  width = 13, height = 10, units = "in")



# Validando se o plot soma os valores:
inva_aus_summary %>% 
  dplyr::filter(`Subcategoria CDB` == "Transporte de material natural") %>% 
  summarise(sum = sum(custo_medio_BRL_mi )) # 987 -> bem perto do que parece no plot -> OK



### Plot com n espécies no label: ----

inva_aus_summary_se <- inva_aus %>%
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
  summarise(
    custo_medio_BRL_mi_sum = sum(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_mean = mean(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_sd = sd(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_se = custo_medio_BRL_mi_sd/sqrt(n()),
    n_spp = n_distinct(`Nome científico`)
  )
# # # Save:
# inva_aus_summary_se %>%
#     writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Aus-Cont_se.xlsx"))


inva_aus_summary_se %>%
  ungroup() %>% 
  mutate(
    `Subcategoria CDB` = fct_reorder(`Subcategoria CDB`, custo_medio_BRL_mi_sum, .fun = max)
  ) %>%
  ggplot(aes(x = `Subcategoria CDB`
             , y = custo_medio_BRL_mi_sum, fill = `Categoria CDB`)) +
  geom_col(position = "dodge") +
  # geom_errorbar(aes(ymin = custo_medio_BRL_mi_sum - custo_medio_BRL_mi_sd,
  #                   ymax = custo_medio_BRL_mi_sum + custo_medio_BRL_mi_sd)) +
  # geom_errorbar(aes(ymin = custo_medio_BRL_mi_sum - (custo_medio_BRL_mi_sum < 0)*custo_medio_BRL_mi_sd,
  #                   ymax = custo_medio_BRL_mi_sum + (custo_medio_BRL_mi_sum > 0)*custo_medio_BRL_mi_sd
  # ), lwd = 0.1
  # )+
  scale_fill_manual(values = palCDB_cat) +
  coord_flip() +
  # geom_text(aes(label=n), stat = "count", colour="red", size=4) +
  theme_update(
    legend.position = "bottom",
  #   axis.text.y = element_text(size = 14),
  #   axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  ggtitle("Custo acumulado de vias e vetores de EEIs Ausentes/Contidas") +
  xlab("Vias e vetores") +
  ylab("Custo acumulado (milhões BRL)") +
  ggnewscale::new_scale_fill() +
  geom_label(#inherit.aes = FALSE,
    # , data = n_spp
    # , aes(x = `Subcategoria CDB`
    #     , y = custo_acumulado_BRL_bi
    #     , label = n_spp)
    aes(label = n_spp)#, fill = "white")
    , nudge_y = 30
    , label.padding = unit(0.05, "lines")
    , show.legend = FALSE
    , label.size = NA # tirar borda
    , size = 7
  )
# # # Save:
# ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_medio_BRL_vias_Aus-Cont_n.png"),
#        dpi = 300,  width = 14.5, height = 10, units = "in")



# Checar spps responsáveis:
tgt <- c("Transporte de material natural", "Contaminantes em plantas (exceto parasitas)"
         , "Contaminação em material para viveiros", "Comércio de madeira"
        , "Material de embalagem orgânico")

inva_evdate_pres_main <- inva_aus %>% 
  dplyr::filter(`Subcategoria CDB` %in% tgt) %>% 
  # dplyr::select(1:custo_se, duração:n_spp) %>%
  distinct()

inva_evdate_pres_main %>% 
  # mutate(
  # `Nome científico` = fct_reorder(`Nome científico`, custo_medio_BRL_mi)
  # ) %>%
  ggplot(aes(y = fct_reorder(`Nome científico`, custo_medio_BRL_mi)
             , x = custo_medio_BRL_mi
             , fill = `Subcategoria CDB`)) +
  geom_col() +
  # theme_bw() +
  theme_update(
    legend.position = "bottom",
  ) +
  # scale_fill_manual(values = c()) +
  ggtitle("EEIs Ausentes/Contidas das 5 vias e vetores com maior custo acumulado") +
  
  ylab("") +
  xlab("Custo acumulado (milhões BRL)") +
  guides(fill=guide_legend(nrow=3, byrow=TRUE))
# # Save:
ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_medio_BRL_vias-spps-importantes-Aus-Cont.png"),
       dpi = 300,  width = 14.5, height = 8, units = "in")


inva_aus %>% dplyr::filter(`Nome científico` == "Anoplophora glabripennis")

spp_out <- "Anoplophora glabripennis"

### Sem espécies com maior dano ----
inva_aus_summary_se <- inva_aus %>%
  dplyr::filter(!`Nome científico` %in% spp_out) %>% 
  group_by(Situação, Ambiente, `Categoria CDB`, `Subcategoria CDB`) %>% 
  summarise(
    custo_medio_BRL_mi_sum = sum(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_mean = mean(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_sd = sd(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_se = custo_medio_BRL_mi_sd/sqrt(n()),
    n_spp = n_distinct(`Nome científico`)
  )
# # Save:
# inva_aus_summary_se %>%
#   mutate(
#     `Subcategoria CDB` = case_when(
#       `Subcategoria CDB` == "Aquário, terrário e pet" ~ "Aquário, terrário e pet (incluindo comida viva para essas espécies)",
#       `Subcategoria CDB` == "Jardim botânico, zoológico, aquário" ~ "Jardim botânico, zoológico, aquário (não domésticos)",
#       TRUE ~ `Subcategoria CDB`
#     )
#   ) %>%
#   writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Aus-Cont_sem-Anoplophora-Ambiente-e-Situacao.xlsx"))


### + Ambiente + Situação
### Sem espécies com maior dano ----
inva_aus_summary_se_amb_sit <- inva_aus %>%
  ungroup() %>% 
  dplyr::filter(!`Nome científico` %in% spp_out) %>% 
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
  summarise(
    custo_medio_BRL_mi_sum = sum(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_mean = mean(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_sd = sd(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_se = custo_medio_BRL_mi_sd/sqrt(n()),
    n_spp = n_distinct(`Nome científico`)
  )
# # Save:
# inva_aus_summary_se_amb_sit %>%
#   mutate(
#     `Subcategoria CDB` = case_when(
#       `Subcategoria CDB` == "Aquário, terrário e pet" ~ "Aquário, terrário e pet (incluindo comida viva para essas espécies)",
#       `Subcategoria CDB` == "Jardim botânico, zoológico, aquário" ~ "Jardim botânico, zoológico, aquário (não domésticos)",
#       TRUE ~ `Subcategoria CDB`
#     )
#   ) %>%
#   writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-vias_Aus-Cont_sem-Anoplophora-ambiente.xlsx"))




# inva_aus_summary_se %>% 
inva_aus_summary_se_amb_sit %>% 
  ungroup() %>% 
  mutate(
    `Subcategoria CDB` = fct_reorder(`Subcategoria CDB`, custo_medio_BRL_mi_sum, .fun = sum)
  ) %>%
  ggplot(aes(x = `Subcategoria CDB`
             , y = custo_medio_BRL_mi_sum, fill = `Categoria CDB`)) +
  geom_col(position = "dodge") +
  # geom_errorbar(aes(ymin = custo_medio_BRL_mi_sum - custo_medio_BRL_mi_sd,
  #                   ymax = custo_medio_BRL_mi_sum + custo_medio_BRL_mi_sd)) +
  # geom_errorbar(aes(ymin = custo_medio_BRL_mi_sum - (custo_medio_BRL_mi_sum < 0)*custo_medio_BRL_mi_sd,
  #                   ymax = custo_medio_BRL_mi_sum + (custo_medio_BRL_mi_sum > 0)*custo_medio_BRL_mi_sd
  # ), lwd = 0.1
  # )+
  scale_fill_manual(values = palCDB_cat) +
  coord_flip() +
  # geom_text(aes(label=n), stat = "count", colour="red", size=4) +
  # theme(
  #   legend.position = "bottom",
  #   axis.text.y = element_text(size = 14),
  #   axis.text.x = element_text(angle = 30, hjust = 1)
  # ) +
  ggtitle("Custo acumulado de vias e vetores de EEIs Ausentes/Contidas",
          expression(paste("Desconsiderando ", italic("Anoplophora glabripennis")))) +
  xlab("Vias e vetores") +
  ylab("Custo acumulado (milhões BRL)") +
  ggnewscale::new_scale_fill() +
  geom_label(#inherit.aes = FALSE,
    # , data = n_spp
    # , aes(x = `Subcategoria CDB`
    #     , y = custo_acumulado_BRL_bi
    #     , label = n_spp)
    aes(label = n_spp)#, fill = "white")
    , nudge_y = 3
    , label.padding = unit(0.05, "lines")
    , show.legend = FALSE
    , label.size = NA # tirar borda
    , size = 7
  )
# # # # Save:
# ggsave(filename = here("Entregas", "A4_Custo"
#                        , "InvaCost_medio_BRL_vias-n-errorbar-sd_sem-spps-importantes-Aus-Cont.png"),
#        dpi = 300,  width = 14.5, height = 10, units = "in")


### Checar spps responsáveis
# Checar spps responsáveis:
tgt <- c('Jardim botânico, zoológico, aquário', 'Plantas cultivadas'
         , 'Aquário, terrário e pet', 'Maquinário e equipamento', 'Água de lastro')

inva_aus_main <- inva_aus %>% 
  dplyr::filter(`Subcategoria CDB` %in% tgt) %>% 
  # dplyr::select(1:custo_se, duração:n_spp) %>%
  distinct()

inva_aus_main %>% 
  # mutate(
  # `Nome científico` = fct_reorder(`Nome científico`, custo_medio_BRL_mi)
  # ) %>%
  ggplot(aes(y = fct_reorder(`Nome científico`, custo_medio_BRL_mi, .fun = sum)
             , x = custo_medio_BRL_mi
             , fill = `Subcategoria CDB`)) +
  geom_col(#position = "dodge"
    ) +
  # theme_bw() +
  theme_update(
    legend.position = "bottom",
  ) +
  # scale_fill_manual(values = c()) +
  ggtitle("EEIs Ausentes/Contidas das 5 vias e vetores com maior custo acumulado",
          expression(paste("Desconsiderando ", italic("Anoplophora glabripennis")))) +
  ylab("") +
  xlab("Custo acumulado (milhões BRL)") +
  guides(fill=guide_legend(nrow=3, byrow=TRUE))
# # # # Save:
ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_medio_BRL_vias-spps-importantes-spps_Aus-Cont.png"),
       dpi = 300,  width = 13, height = 11, units = "in")



# Summary por ambiente e situação -----

# Juntar presentes e aus/cont numa planilha só
# Presentes = custo acumulado (bilhoes BRL); Ausentes = custo médio (milhoes BRL)

## Presentes
spp_out <- c("Apis mellifera", "Columba livia")
pres <- inva_evdate_pres %>%
  dplyr::filter(!`Nome científico` %in% spp_out) %>% 
  group_by(Situação, Ambiente) %>% 
  summarise(
    custo_acumulado_BRL_bi_sum = sum(custo_acumulado_BRL_bi, na.rm=TRUE),
    custo_acumulado_BRL_bi_mean = mean(custo_acumulado_BRL_bi, na.rm=TRUE),
    custo_acumulado_BRL_bi_sd = sd(custo_acumulado_BRL_bi, na.rm=TRUE),
    custo_acumulado_BRL_bi_se = custo_acumulado_BRL_bi_sd/sqrt(n()),
    n_spp = n_distinct(`Nome científico`)
  ) %>% 
  rename(
    custo = custo_acumulado_BRL_bi_sum
  )

## Ausentes
spp_out <- c("Anoplophora glabripennis")
aus <- inva_aus %>%
  dplyr::filter(!`Nome científico` %in% spp_out) %>% 
  group_by(Situação, Ambiente) %>% 
  summarise(
    custo_medio_BRL_mi_sum = sum(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_mean = mean(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_sd = sd(custo_medio_BRL_mi, na.rm=TRUE),
    custo_medio_BRL_mi_se = custo_medio_BRL_mi_sd/sqrt(n()),
    n_spp = n_distinct(`Nome científico`)
  ) %>% 
  rename(
    custo = custo_medio_BRL_mi_sum
  )


## Join
inva_summary_ambiente <- bind_rows(pres, aus) %>% 
  dplyr::select(Situação:custo)

# # Save:
# inva_summary_ambiente %>%
#   writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_summary-ambiente-situacao.xlsx"))



# Numero de estimativas InvaCost por espécie: ----
theme_set(theme_bw(base_size = 20))

options(scipen=999)

palCDB_cat <- rev(c("#9359a5", "#9c83bd", "#8c97c5" , "#26828f", "#52c56a", "#badd27"))

vv_inva_n <- vv_inva %>% 
  dplyr::filter(!is.na(Cost_ID)) %>% 
  dplyr::select(`Nome científico`, `Categoria CDB`, Cost_ID) %>% 
  group_by(`Nome científico`) %>% 
  mutate(
    n_estimativas_invacost = n_distinct(Cost_ID)
  ) %>% 
  dplyr::select(-Cost_ID) %>% 
  distinct()

# N estimativas invacost
a <- vv_inva %>% 
  dplyr::filter(!is.na(Cost_ID)) %>%
  dplyr::select(`Nome científico`, `Categoria CDB`, Cost_ID) %>% 
  group_by(`Nome científico`) %>% 
  summarise(
    n_estimativas_invacost = n_distinct(Cost_ID)
  ) 
# # Save:
#   a %>% 
#     writexl::write_xlsx(path = here("Entregas", "A4_Custo", "Database_consolidada_Invacost_n-estimativas.xlsx"))

a$n_estimativas_invacost %>% sum() # 3337 estimativas
a$`Nome científico` %>% unique() # 148 spps


# Plot 
vv_inva_n$n_estimativas_invacost %>% hist()
vv_inva_n$n_estimativas_invacost %>% sum()

# Histograma
vv_inva_n %>% 
  ggplot(aes(#y = fct_reorder(`Nome científico`, n_estimativas_invacost)
             x = n_estimativas_invacost, fill = `Categoria CDB`)) +
  geom_histogram() +
  scale_fill_manual(values = palCDB_cat) +
  facet_wrap(~`Categoria CDB`, scales = "free_y", nrow = 2) +
  theme(
    legend.position = "bottom"
  ) +
  xlab("Número de estimativas no InvaCost") +
  ylab("Contagem")
# # # # Save:
# ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_n-estimativas_hist.png"),
#        dpi = 300,  width = 12, height = 7, units = "in")


# Por especie
vv_inva_n %>% 
  ggplot(aes(y = fct_reorder(`Nome científico`, n_estimativas_invacost)
             , x = n_estimativas_invacost, fill = `Categoria CDB`)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = palCDB_cat) +
  facet_wrap(~`Categoria CDB`, scales = "free_y", nrow = 2) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 7)
  ) +
  xlab("Número de estimativas no InvaCost") +
  ylab("Nome científico") +
  theme(
    strip.text = element_text(size=12)
  )
# # # Save:
# ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_n-estimativas.png"),
#        dpi = 300,  width = 12, height = 18, units = "in")


# Separado em 2 plots:
vias_split <- c("Corredor", "Escape", "Sem ajuda humana")

vv_inva_n %>% 
  dplyr::filter(`Categoria CDB` %in% vias_split) %>% 
  ggplot(aes(y = fct_reorder(`Nome científico`, n_estimativas_invacost)
             , x = n_estimativas_invacost, fill = `Categoria CDB`)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = palCDB_cat) +
  facet_wrap(~`Categoria CDB`, scales = "free_y", nrow = 1) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 12)
  ) +
  xlab("Número de estimativas no InvaCost") +
  ylab("Nome científico") +
  theme(
    strip.text = element_text(size=12)
  )
# # # Save:
# ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_n-estimativas-1.png"),
#        dpi = 300,  width = 14, height = 18, units = "in")

vv_inva_n %>% 
  dplyr::filter(!`Categoria CDB` %in% vias_split) %>% 
  ggplot(aes(y = fct_reorder(`Nome científico`, n_estimativas_invacost)
             , x = n_estimativas_invacost, fill = `Categoria CDB`)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = palCDB_cat) +
  facet_wrap(~`Categoria CDB`, scales = "free_y", nrow = 1) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 12)
  ) +
  xlab("Número de estimativas no InvaCost") +
  ylab("Nome científico") +
  theme(
    strip.text = element_text(size=12)
  )
# # # Save:
ggsave(filename = here("Entregas", "A4_Custo", "InvaCost_n-estimativas-2.png"),
       dpi = 300,  width = 14, height = 18, units = "in")
