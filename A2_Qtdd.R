# Script name: A2_Qtdd
# Script purpose: Realizar análises relacionadas a quantificação
# de vias e vetores (Produto 1: Atividade 4.2 e seguintes)

# Date created:
# 10/08/2023
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("tidyverse")
library("readxl")
library("ggalluvial")
library("treemap")

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 


# Import and wrangle data ------
# vv <- read.csv2(file = here("Entregas", "Arquivos", "Dados", "Vias-vetores.csv")
# vv <- read.csv2(file = here("Entregas", "Arquivos", "Dados", "Database_consolidada.csv")
#                 , sep = ",", check.names=FALSE)
vv <- read_xlsx(here("Entregas", "Arquivos", "Dados", "00_Vias-vetores-corrigido.xlsx"))

# Agrupar Ausentes e Contidas
vv <- vv %>% 
  mutate(
    Situação = case_when(
      Situação == "Ausente" ~ "Ausente/Contida",
      Situação == "Contida" ~ "Ausente/Contida",
      TRUE ~ Situação
    )
  )


### Summaries -----

# N eventos 
db_summary  <-  vv %>%
  group_by(Situação, `Categoria CDB`, `Subcategoria CDB`) %>%
  summarise(
    eventos_introducao = n()
  )

db_summary$eventos_introducao %>% sum()

# # Save:
# db_summary %>%
#   writexl::write_xlsx(path = here("Entregas", "A2_Qtdd", "00_Vias-vetores-corrigido_summary.xlsx"))


# N de espécies por ambiente
db_summary  <-  vv %>%
  group_by(Ambiente) %>% 
  summarise(
    riqueza = n_distinct(`Nome científico`)
  )
db_summary$riqueza %>% sum()

# # Save:
# db_summary %>%
#   writexl::write_xlsx(path = here("Entregas", "A2_Qtdd", "00_Vias-vetores-corrigido_summary-ambiente.xlsx"))


# N de espécies por Situação e Ambiente
db_summary  <-  vv %>%
  group_by(Situação, Ambiente) %>% 
  summarise(
    riqueza = n_distinct(`Nome científico`)
  )
db_summary

# # Save:
# db_summary %>%
#   writexl::write_xlsx(path = here("Entregas", "A2_Qtdd", "00_Vias-vetores-corrigido_summary-ambiente-situacao.xlsx"))


# N de espécies por Mecanismo
db_summary  <-  vv %>%
  group_by(Mecanismo) %>% 
  summarise(
    riqueza = n_distinct(`Nome científico`)
  )
db_summary

# # Save:
# db_summary %>%
#   writexl::write_xlsx(path = here("Entregas", "A2_Qtdd", "00_Vias-vetores-corrigido_summary-mecanismo.xlsx"))






## Alluvial plot ----
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

options(scipen = 100)

ggplot(as.data.frame(vv),
       aes(y = count, axis1 = `Grau de confiança`, axis2 = `Categoria CDB`
           , axis3 = `Subcategoria CDB`
           # , axis4 = `Situação`
       )) +
  geom_alluvium(aes(fill = Situação), width = 1/12) +
  # geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_stratum(width = 1/12, aes(fill = `Grupo biológico`)) +
  # geom_label(stat = "stratum", aes(label = after_stat(stratum), label.padding = 3)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))
            # , nudge_x = 1
            , hjust = 0
  ) +
  # scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  # scale_fill_brewer(type = "qual", palette = "Set2")
  scale_fill_viridis_d()
# ggtitle("UC Berkeley admissions and rejections, by sex and department")

# ggsave(filename = here("Entregas", "1_Vias-Vetores", "Alluvial2.png"),
#        dpi = 300,  width = 297, height = 297, units = "mm")




## Treemap plot ----
# https://rpubs.com/tskam/treemap # -> tem como fazer interativo com d3tree()

# Include number of species


