# Script name: 00_prep-data
# Script purpose: Preparar os dados para cada atividade dos produtos 1 a 3 do MMA

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

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 



# Import and wrangle data ------

## Vias e vetores -----

# Contém Categorias e subcategorias CDB; nível de confiança
vv <- read_excel(path = here("Material enviado pelo MMA", "Vias_Vetores"
                       , "Produto 14.6 Anexo 1 Vias todas as espécies.xlsx"))


# Selecionar somente colunas de interesse
names(vv)
vv <- vv %>% 
  dplyr::select(-c(Reino:Família, `Autor/a`, `Grau de confiança`, `Grupo biológico`, `Comentários`, Referências))

# Encurtar nomes e unificar subcategorias:
vv$`Categoria CDB` %>% unique()
vv$`Subcategoria CDB` %>% unique()

vv$`Nome científico` %>% unique()
vv %>% group_by(Situação) %>% 
  dplyr::select(`Nome científico`) %>% 
  distinct() %>%
  summarise(
    riqueza = n()
    )

vv$Ambiente %>% unique()
vv$Situação %>% unique()


### Checar categorias e subcategorias: -----
# Correct database (de acordo com a classificação de Zenni et al 2023 (produto 14.3))

vv$`Categoria CDB` %>% unique() # 8 categorias, nao ok

vv$`Subcategoria CDB` %>% unique() # 44 categorias, nao ok

grep("1 a 5", vv$`Subcategoria CDB`)

# a <- vv %>% dplyr::filter(str_detect(`Subcategoria CDB`, "1 a 5")) %>% 
# a <- vv %>% dplyr::filter(str_detect(`Subcategoria CDB`, "intencional")) %>%
# a <- vv %>% dplyr::filter(str_detect(`Subcategoria CDB`, "além dos citados")) %>%
# a <- vv %>% dplyr::filter(str_detect(`Subcategoria CDB`, "navio")) %>%
# a <- vv %>% dplyr::filter(str_detect(`Subcategoria CDB`, "Veículos")) %>%
# a <- vv %>% dplyr::filter(str_detect(`Subcategoria CDB`, "comida")) %>%
# a <- vv %>% dplyr::filter(str_detect(`Subcategoria CDB`, "túneis")) %>% # via ausente ('Túneis e passagens terrestres')
# a <- vv %>% dplyr::filter(str_detect(`Subcategoria CDB`, "Contaminante")) %>% # via ausente ('Outro contaminante')
# a <- vv %>% dplyr::filter(str_detect(`Subcategoria CDB`, "isca")) %>% # via ausente ('Contaminante de isca')
  dplyr::select(`Subcategoria CDB`) %>% 
  distinct()

a$`Subcategoria CDB`

vv <- vv %>%
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Espécie de aquário / terrário / pet (incluindo comida viva para essas espécies)" ~ "Aquário, terrário e pet (incluindo comida viva para essas espécies)",
      `Subcategoria CDB` == "Pesca na natureza (incluindo pesca desportiva)" ~ "Pesca na natureza",
                                    # modificação 5?:
      `Subcategoria CDB` == "Dispersão natural de espécies exóticas invasoras introduzidas pelas vias 1 a 5 através de fronteiras" ~ "Dispersão natural (vias 1 a 5) por fronteiras",
      # `Subcategoria CDB` == "Dispersão natural de espécies exóticas invasoras introduzidas pelas vias 1 a 5 através de fronteiras " ~ "Dispersão natural (vias 1 a 5) por fronteiras",
      `Subcategoria CDB` == "Comércio de madeira" ~ "Comércio de madeira",
      `Subcategoria CDB` == "Contaminantes em plantas (exceto parasitas, espécies transportadas pelo hospedeiro / vetor)" ~ "Contaminantes em plantas (exceto parasitas)",
      `Subcategoria CDB` == "Maquinário / equipamento" ~ "Maquinário e equipamento",
      `Subcategoria CDB` == "Material de embalagem orgânico, em especial de madeira" ~ "Material de embalagem orgânico",
      `Subcategoria CDB` == "Presença clandestina em navio/embarcação (excluindo água de lastro e bioincrustação)" ~ "Presença em navio e embarcação",
      `Subcategoria CDB` == "Canais/bacias/mares interconectados" ~ "Canais, bacias e mares interconectados",
      `Subcategoria CDB` == "Outra soltura intencional" ~ "Outra soltura intencional",                                 # modificação 4
      `Subcategoria CDB` == "Soltura na natureza (outros fins além dos citados acima)" ~ "Outra soltura intencional",  # modificação 4
      `Subcategoria CDB` == "Água de lastro de navio/embarcação" ~ "Água de lastro",
      `Subcategoria CDB` == "Bioincrustação em navio/embarcação" ~ "Bioincrustação em navios", # modificação 1 (união)
      `Subcategoria CDB` == "BIoincrustação em navio/embarcação" ~ "Bioincrustação em navios", # modificação 1 (união)
      # `Subcategoria CDB` == "Bioincrustação em navio/embarcação;" ~ "Bioincrustação em navios", # modificação 1 (união)
      `Subcategoria CDB` == "Container/volume" ~ "Container/volume",
      `Subcategoria CDB` == "Outro meio de transporte" ~ "Outro meio de transporte",
      `Subcategoria CDB` == "Transporte de material natural (solo, vegetação, ...)" ~ "Transporte de material natural",
      `Subcategoria CDB` == "Transporte de material natural (solo, vegetação,...)" ~ "Transporte de material natural",
      `Subcategoria CDB` == "Animais domésticos (incluindo animais de criação sob controle limitado)" ~ "Animais domésticos",
      `Subcategoria CDB` == "Pesquisa e criação ex-situ (em instituições)" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Aquicultura / maricultura" ~ "Aquicultura/maricultura",
      `Subcategoria CDB` == "Jardim botânico / zoológico / aquário (excluindo aquários domésticos)" ~ "Jardim botânico, zoológico, aquário (não domésticos)",
      `Subcategoria CDB` == "Caça na natureza" ~ "Caça na natureza",
      `Subcategoria CDB` == "Controle biológico" ~ "Controle biológico",
      `Subcategoria CDB` == "Contaminantes em animais (exceto parasitas, espécies transportadas pelo hospedeiro / vetor)" ~ "Contaminantes em animais (exceto parasitas)",
      `Subcategoria CDB` == "Equipamento de pesca / pesca com anzol" ~ "Equipamento de pesca",
      `Subcategoria CDB` == "Comida viva e isca viva" ~ "Comida viva e isca viva",
      `Subcategoria CDB` == "Contaminação em material para viveiros" ~ "Contaminação em material para viveiros",
      `Subcategoria CDB` == "Introdução para fins de conservação" ~ "Introdução para fins de conservação",
      `Subcategoria CDB` == "Parasitas em animais (incluindo espécies transportadas pelo hospedeiro e vetor)" ~ "Parasitas em animais",
      `Subcategoria CDB` == "Presença clandestina sobre / dentro de avião" ~ "Presença em avião",
      `Subcategoria CDB` == "Fazendas de peles de animais" ~ "Fazendas de peles de animais",
      `Subcategoria CDB` == "Melhoramento de paisagem/flora/fauna na natureza" ~ "Melhoramento de paisagem",
      `Subcategoria CDB` == "Pessoas e bagagens / equipamento (especialmente turismo)" ~ "Bagagens e turismo",
      `Subcategoria CDB` == "Veículos (carro, trem...)" ~ "Veículos",    # modificação 3
      `Subcategoria CDB` == "Veículos (carro, trem, ...)" ~ "Veículos",  # modificação 3
      `Subcategoria CDB` == "Veículos (carro, trem,...)" ~ "Veículos",
      `Subcategoria CDB` == "Contaminação de comida (incluindo comida viva)" ~ "Contaminação de comida incluindo comida viva",    # modificação 2
      `Subcategoria CDB` == "Contaminação de comida" ~ "Contaminação de comida incluindo comida viva",                            # modificação 2
      `Subcategoria CDB` == "Contaminação em sementes" ~ "Contaminação em sementes",
      `Subcategoria CDB` == "Parasitas em plantas (incluindo espécies transportadas pelo hospedeiro e vetor)" ~ "Parasitas em plantas",
      `Subcategoria CDB` == "Plantas cultivadas" ~ "Plantas cultivadas",
      `Subcategoria CDB` == "Fins ornamentais (excluindo produção alimentar)" ~ "Fins ornamentais",
      `Subcategoria CDB` == "Produção florestal (incluindo reflorestamento)" ~ "Produção florestal e reflorestamento",
      `Subcategoria CDB` == "Controle de erosão / estabilização de dunas (quebra vento, cerca viva...)" ~ "Controle de erosão, quebra vento ou cerca viva",
      `Subcategoria CDB` == "Agricultura (incluindo biocombustíveis a partir de biomassa)" ~ "Agricultura e biocombustíveis",
      TRUE ~ as.character(`Subcategoria CDB`)
    )
  ) %>%
  mutate(
    `Categoria CDB` = case_when(
      `Categoria CDB` == "Transporte - clandestinidade" ~ "Transporte clandestino",
      `Categoria CDB` == "Transporte" ~ "Transporte clandestino",
      TRUE ~ as.character(`Categoria CDB`)
    )
  ) %>%
  mutate(
    `Categoria CDB` = case_when(
      `Subcategoria CDB` == "Comércio de madeira" ~ "Transporte como contaminante",
      `Subcategoria CDB` == "Contaminantes em animais (exceto parasitas)" ~ "Transporte como contaminante",
      `Subcategoria CDB` == "Contaminantes em plantas (exceto parasitas)" ~ "Transporte como contaminante",
      `Subcategoria CDB` == "Contaminação de comida incluindo comida viva" ~ "Transporte como contaminante",
      `Subcategoria CDB` == "Contaminação em material para viveiros" ~ "Transporte como contaminante",
      `Subcategoria CDB` == "Contaminação em sementes" ~ "Transporte como contaminante",
      `Subcategoria CDB` == "Maquinário e equipamento" ~ "Transporte clandestino",
      `Subcategoria CDB` == "Transporte de material natural" ~ "Transporte como contaminante",

      `Subcategoria CDB` == "Aquicultura/maricultura" ~ "Escape",
      `Subcategoria CDB` == "Transporte de material natural" ~ "Transporte como contaminante",

      TRUE~ as.character(`Categoria CDB`)
    )
  )

# Arrumar ambiente:
vv <- vv %>% 
  mutate(
    Ambiente = case_when(
      # Ambiente == "Terrestre"  ~ "Terrestre",
      # Ambiente == "Terreste."  ~ "Terrestre",
      Ambiente == "Água Doce"  ~ "Água doce",
      TRUE ~ Ambiente
    )
  )

vv$`Categoria CDB` %>% unique() # 6 categorias! OK
b <- vv %>% dplyr::filter(is.na(`Categoria CDB`))

vv$`Subcategoria CDB` %>% unique() # 40 categorias! OK
b <- vv %>% dplyr::filter(is.na(`Subcategoria CDB`))

vv$Ambiente %>% unique() # 40 categorias! OK
b <- vv %>% dplyr::filter(is.na(Ambiente))


### Checar espécies:
vv %>% dplyr::filter(`Nome científico` == "Callithrix jacchus x Callithrix penicillata x Callithrix aurita")
vv %>% dplyr::filter(`Nome científico` == "Callithrix jacchus x C. penicillata x C. aurita")
vv %>% dplyr::filter(`Nome científico` == "Nandayus nenday ")
vv %>% dplyr::filter(`Nome científico` == "Nandayus nenday")
vv %>% dplyr::filter(`Nome científico` == "Cinnamomum burmanni ")
vv %>% dplyr::filter(`Nome científico` == "Cinnamomum burmannii")
vv %>% dplyr::filter(`Nome científico` == "Pseudoplatystoma corruscans")
vv %>% dplyr::filter(`Nome científico` == "Pseudoplatystoma corruscans x Pseudoplatystoma reticulatum")
vv %>% dplyr::filter(`Nome científico` == "Pseudoplatystoma corruscans x P. reticulatum")
vv %>% dplyr::filter(`Nome científico` == "Crocosmia crocosmiiflora")
vv %>% dplyr::filter(`Nome científico` == "Crocosmia x crocosmiiflora")
vv %>% dplyr::filter(`Nome científico` == "Kappaphycus alvarezzi")
vv %>% dplyr::filter(`Nome científico` == "Kappaphycus alvarezii")
vv %>% dplyr::filter(`Nome científico` == "Neovison vison")
vv %>% dplyr::filter(`Nome científico` == "Mustela vison")


### Adicionar mecanismo (= Turbelin et al. 2022)
vv$`Categoria CDB` %>% unique()

vv <- vv %>%
  mutate(
    Mecanismo = case_when(
      `Categoria CDB` == "Corredor" ~ "Espalhamento",
      `Categoria CDB` == "Sem ajuda humana" ~ "Espalhamento",
      `Categoria CDB` == "Transporte clandestino" ~ "Vetor",
      TRUE ~ "Commodity"
    )
  )
vv$Mecanismo %>% unique()


# # Check duplicates (sem duplicates):
# vv_dup <- vv %>%
#   # dplyr::select(c(`Grau de confiança`:`Grupo biológico`)) %>%
#   dplyr::select(c(`Categoria CDB`, `Subcategoria CDB`)) %>%
#   mutate(id = dplyr::row_number()) %>% 
#   dplyr::distinct(across(-id), .keep_all = TRUE) %>%
#   group_by(`Subcategoria CDB`) %>%
#   mutate(
#     count = n()
#   ) %>% 
#   dplyr::filter(count > 1) %>% 
#   arrange(`Subcategoria CDB`, .by_group = TRUE)
# 
# # Duplicatas:
# vv_dup <- vv_dup %>% 
#   mutate(id = dplyr::row_number()) %>% 
#   # dplyr::inner_join(vv_dup, by = c("id") # use anti_join with select() instead:
#   dplyr::anti_join(select(vv_dup, -count)) # https://stackoverflow.com/questions/75855496/how-to-join-by-everything-except-specified-columns-in-dplyr
# 
# #Save for further addressing it:
# #vv_dup %>%
# #  write.csv(file = here("Entregas", "A1_Vias-Vetores", "Vias-vetores-repetidos.csv")
# #            , row.names = FALSE
# #            , fileEncoding = "latin1")




### Salvar database consolidada -----
# Save file:
vv %>%
  # write.csv(file = here("Entregas", "Arquivos", "Dados",, "00_Vias-vetores-corrigido.csv")
  #           , row.names = FALSE
  #           , fileEncoding = "latin1")
  writexl::write_xlsx(path = here("Entregas", "Arquivos", "Dados", "00_Vias-vetores-corrigido.xlsx"))


# Check CDB categories:
db_summary  <-  vv %>%
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>%
  summarise(
    eventos_introducao = n()
  )
# 40 categorias! All done





#### A PRINCÍPIO ESSE CÓDIGO AQUI PRA BAIXO É PORQUE EU TAVA TENTANDO UNIR VÁRIAS 
#### PLANILHAS E FAZER UMA PLANILHA SÓ, MAS DEU MUITO PROBLEMA COM NOME DE 
#### CATEGORIAS COM DIFERENÇAS MÍNIMAS DE CARACTERES E ISSO FEZ EU PERDER MUITO
#### TEMPO TENTANDO ARRUMAR


# XUNXO:
vv_correct <- vv

vv_correct$`Subcategoria CDB` %>% unique() # 40 categorias! good to go.
vv$`Categoria CDB` %>% unique()



# ## Outras listas: ------
# files <- list.files(here("Material enviado pelo MMA", "Listas", "Lista Presentes"))
# 
# ### Dataset ocorrencias: -----
# # Não será incluído
# 
# ### Presentes ------
# # Contem: Ano e Local primeiro registro, reprodução, dieta, dispersão, classific EICAT,
# 
# le_present_fauna <- read_excel(path = paste0(here("Material enviado pelo MMA", "Listas", "Lista Presentes"
#                              ), "/", files[2])) %>% 
#   dplyr::slice(1:254) # Linhas vazias após 254
# # names(le_present_fauna)
# 
# le_present_flora <- read_excel(path = paste0(here("Material enviado pelo MMA", "Listas", "Lista Presentes"
#                         ), "/", files[3])) %>% 
#   rename(
#     `Nome científico` = `Nome Científico`
#   )
# # names(le_present_flora)
# 
# le_present <- bind_rows(le_present_fauna, le_present_flora) #; rm(le_present_fauna); rm(le_present_flora) 
# le_present$`Nome científico` %>% unique()
# le_present$`Ambiente afetado` %>% unique()
# names(le_present)
# 
# le_present <- le_present %>% 
#   mutate(
#     Situação = "Presente"
#   ) %>% 
#   dplyr::select(c(`Nome científico`, Situação, `Ambiente afetado`
#                   , ViasVetores_Categoria, ViasVetores_Subcategoria))
# 
# # Separar ambientes:
# le_present <- le_present %>% 
#   rename(
#     "Ambiente" = "Ambiente afetado"
#   ) %>% 
#   separate_longer_delim(cols = Ambiente, delim = ", ") %>% 
#   mutate(
#     Ambiente = case_when(
#       Ambiente == "Terrestre."  ~ "Terrestre",
#       Ambiente == "Terreste."  ~ "Terrestre",
#       Ambiente == "Água doce."  ~ "Água doce",
#       Ambiente == "Marinho."  ~ "Marinho",
#       TRUE ~ Ambiente
#     )
#   ) 
# 
# le_present$`Nome científico` %>% unique()
# le_present$Ambiente %>% unique()
# 
# 
# ### Ausentes/Contidas -----
# # Contem: Lista/escore EICAT
# le_auscont <- read.csv2(here("Material enviado pelo MMA", "Listas", "Lista ausentes e contidas"
#                              , "10.3_Ausentes_e_contidas.csv")
#                 , check.names = F, encoding = "latin1") %>% 
#   rename(
#     `Nome científico` = Espécie,
#     # Priorizacao_score_ausentes = ESCORE,
#     "Situação" = "SITUAÇÃO",
#     Ambiente = categoria
#     ) %>%
#   mutate(
#       Ambiente = case_when(
#         Ambiente == "terrestres" ~ "Terrestre",
#         Ambiente == "marinhas"   ~ "Marinho",
#         Ambiente == "agua doce"  ~ "Água doce"
#       )
#   ) %>% 
#   dplyr::select(`Nome científico`, Situação, Ambiente)
# 
# le_auscont$Situação %>% unique()  
# le_auscont$Ambiente %>% unique()  
# le_auscont$`Nome científico` %>% unique()  
# 
# 
# # Bind:
# le <- bind_rows(le_present, le_auscont)
# 
# le$Ambiente %>% unique() # check
# le$Situação %>% unique() # check
# 
# 
# # Checar categorias CDB:
# le <- le %>% 
#   rename(
#     `Categoria CDB` = ViasVetores_Categoria,
#     `Subcategoria CDB` = ViasVetores_Subcategoria
#   )
# 
# le$`Subcategoria CDB` %>% unique() # categorias misturadas. Requer um separate_delim
# le$`Categoria CDB` %>% unique()
# 
# le_b <- le %>% 
#   separate_longer_delim(cols = `Categoria CDB`,
#                         delim = "; ") %>% 
#   separate_longer_delim(cols = `Subcategoria CDB`,
#                         delim = "; ")
# 
# le_b$`Subcategoria CDB` %>% unique() # 44 subcategorias + NA!! -> errado
# le_b$`Categoria CDB` %>% unique()    # 6 Categorias + NA!! -> errado
# a <- le_b %>% dplyr::filter(is.na(`Categoria CDB`))    # nrow = 112 -> sempre das ausentes
# a <- le_b %>% dplyr::filter(is.na(`Subcategoria CDB`)) # nrow = 112 -> sempre das ausentes
# 
# # Para corrigir, teremos que fazer um left_join pois vv_correct já está com nomes certos
# # Mas ainda temos os nomes cientificos (key pro join) erradis entre as duas dbs. Vamos corrigir isso prmeiro:
# 
# 
# ### Corrigir Nomes científicos das espécies da lista Zenni et al e das planilhas presentes/ausentes e contidas: ----
# le_b %>% dplyr::select(c(`Nome científico`)) %>% distinct()
# vv_correct %>% dplyr::select(c(`Nome científico`)) %>% distinct() # 557 é pra ser o numero correto de spps
# 
# #esses resultados estão diferentes de vv # Eram as espécies que estavam como NA em A1_Vias-Vetores.R -> Vias-Vetores-NA.csv:
# a <- vv_correct[!vv_correct$`Nome científico` %in% le_b$`Nome científico`, ] 
# a$`Nome científico` %>% unique()
# a <- le_b[!le_b$`Nome científico` %in% vv_correct$`Nome científico`, ] 
# a$`Nome científico` %>% unique()
# 
# # Arrumar nomes científicos em comum com as duas listas acima:
# le_b <- le_b %>% 
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
# 
# le_b$`Nome científico` %>% unique() 
# le_b %>% dplyr::filter(`Nome científico` == "Mustela vison")
# le_b %>% dplyr::filter(`Nome científico` == "Neovison vison")
# vv_correct %>% dplyr::filter(`Nome científico` == "Mustela vison")
# vv_correct %>% dplyr::filter(`Nome científico` == "Neovison vison")
# 
# 
# # Corrigir subcategorias CDB (= vv) ------
# # Agora que temos os nomes cientificos corretos, vamos fazer o join
# vv_correct$`Subcategoria CDB` %>% unique() # 40 categorias! (OK!)
# le_b$`Subcategoria CDB` %>% unique() # 44 categorias! + NA = vv (nomes errados -> 3 e 22 são a mesma, BIoincrustaçao etc)
# le_b$`Categoria CDB` %>% unique()
# 
# 
# le_b %>% dplyr::filter(is.na(`Categoria CDB`)) # 112 ausentes e contidas
# le_b %>% dplyr::filter(is.na(`Subcategoria CDB`)) # 112 ausentes e contidas
# 
# 
# ### Join databases: lista Zenni et al + planilhas presentes/ausentes e contidas:
# names(vv_correct)
# names(le_b)
# 
# 
# # # full_join (pegar espécies que faltaram):
# # db_consolidada <- dplyr::full_join(vv_correct, le_b) # funcionando com 561 spp
# 
# vv_miss <- vv_correct %>% 
#   dplyr::filter(Situação != "Presente")
# 
# db_consolidada <- dplyr::full_join(le_b, vv_miss) # funcionando com 557 spp -> checar
# 
# db_consolidada %>% dplyr::filter(`Nome científico` == "Mustela vison")
# db_consolidada %>%  dplyr::filter(`Nome científico` == "Neovison vison")
# db_consolidada %>% dplyr::filter(`Nome científico` =='Codium fragile')
# 
# vv_miss %>%  dplyr::filter(`Nome científico` == "Neovison vison")
# vv_miss %>% dplyr::filter(`Nome científico` =='Codium fragile')
# 
# le_b %>%  dplyr::filter(`Nome científico` == "Neovison vison")
# le_b %>% dplyr::filter(`Nome científico` =='Codium fragile')
# 
# # Como o full join (e o left_join com many to many) mantem as linhas com NA, vamos retirá-las
# db_consolidada <- dplyr::filter(db_consolidada, !is.na(`Categoria CDB`)) # - 112! OK
# 
# # Checar duplicatas
# db_consolidada[duplicated(db_consolidada) | duplicated(db_consolidada,fromLast = TRUE), ]
# db_consolidada <- distinct(db_consolidada) # (removeu as linhas acima)
# 
# db_consolidada$`Nome científico` %>% unique() # 561 espécies, não 557! 
# db_consolidada %>%  dplyr::filter(`Nome científico` == "Neovison vison")
# db_consolidada %>% dplyr::filter(`Nome científico` =='Codium fragile')
# 
# 
# ### Checar categorias e subcategorias: -----
# # Correct database (de acordo com a classificação de Zenni et al 2023 (produto 14.3))
# 
# db_consolidada$`Categoria CDB` %>% unique() # 8 categorias, nao ok
# 
# db_consolidada$`Subcategoria CDB` %>% unique() # 48 categorias, nao ok
# 
# db_consolidada <- db_consolidada %>% 
#   mutate(
#     `Subcategoria CDB` = case_when(
#       `Subcategoria CDB` == "Espécie de aquário / terrário / pet (incluindo comida viva para essas espécies)" ~ "Aquário, terrário e pet (incluindo comida viva para essas espécies)",
#       `Subcategoria CDB` == "Pesca na natureza (incluindo pesca desportiva)" ~ "Pesca na natureza",
#       `Subcategoria CDB` == "Dispersão natural de espécies exóticas invasoras introduzidas pelas vias 1 a 5 através de fronteiras" ~ "Dispersão natural (vias 1 a 5) por fronteiras",
#       `Subcategoria CDB` == "Dispersão natural de espécies exóticas invasoras introduzidas pelas vias 1 a 5 através de fronteiras " ~ "Dispersão natural (vias 1 a 5) por fronteiras",
#       `Subcategoria CDB` == "Comércio de madeira" ~ "Comércio de madeira",
#       `Subcategoria CDB` == "Contaminantes em plantas (exceto parasitas, espécies transportadas pelo hospedeiro / vetor)" ~ "Contaminantes em plantas (exceto parasitas)",
#       `Subcategoria CDB` == "Maquinário / equipamento" ~ "Maquinário e equipamento",
#       `Subcategoria CDB` == "Material de embalagem orgânico, em especial de madeira" ~ "Material de embalagem orgânico",
#       `Subcategoria CDB` == "Presença clandestina em navio/embarcação (excluindo água de lastro e bioincrustação)" ~ "Presença em navio e embarbação",
#       `Subcategoria CDB` == "Canais/bacias/mares interconectados" ~ "Canais, bacias e mares interconectados",
#       `Subcategoria CDB` == "Outra soltura intencional" ~ "Outra soltura intencional",                                 # modificação 4
#       `Subcategoria CDB` == "Soltura na natureza (outros fins além dos citados acima)" ~ "Outra soltura intencional",  # modificação 4
#       `Subcategoria CDB` == "Água de lastro de navio/embarcação" ~ "Água de lastro",
#       `Subcategoria CDB` == "Bioincrustação em navio/embarcação" ~ "Bioincrustação em navios", # modificação 1 (união)
#       `Subcategoria CDB` == "BIoincrustação em navio/embarcação" ~ "Bioincrustação em navios", # modificação 1 (união)
#       `Subcategoria CDB` == "Bioincrustação em navio/embarcação;" ~ "Bioincrustação em navios", # modificação 1 (união)
#       `Subcategoria CDB` == "Container/volume" ~ "Container/volume",
#       `Subcategoria CDB` == "Outro meio de transporte" ~ "Outro meio de transporte",
#       `Subcategoria CDB` == "Transporte de material natural (solo, vegetação, ...)" ~ "Transporte de material natural",
#       `Subcategoria CDB` == "Transporte de material natural (solo, vegetação,...)" ~ "Transporte de material natural",
#       `Subcategoria CDB` == "Animais domésticos (incluindo animais de criação sob controle limitado)" ~ "Animais domésticos",
#       `Subcategoria CDB` == "Pesquisa e criação ex-situ (em instituições)" ~ "Pesquisa e e criação ex-situ",
#       `Subcategoria CDB` == "Aquicultura / maricultura" ~ "Aquicultura/maricultura",
#       `Subcategoria CDB` == "Jardim botânico / zoológico / aquário (excluindo aquários domésticos)" ~ "Jardim botânico, zoológico, aquário (não domésticos)",
#       `Subcategoria CDB` == "Caça na natureza" ~ "Caça na natureza",
#       `Subcategoria CDB` == "Controle biológico" ~ "Controle biológico",
#       `Subcategoria CDB` == "Contaminantes em animais (exceto parasitas, espécies transportadas pelo hospedeiro / vetor)" ~ "Contaminantes em animais (exceto parasitas)",
#       `Subcategoria CDB` == "Equipamento de pesca / pesca com anzol" ~ "Equipamento de pesca",
#       `Subcategoria CDB` == "Comida viva e isca viva" ~ "Comida viva e isca viva",
#       `Subcategoria CDB` == "Contaminação em material para viveiros" ~ "Contaminação em material para viveiros",
#       `Subcategoria CDB` == "Introdução para fins de conservação" ~ "Introdução para fins de conservação",
#       `Subcategoria CDB` == "Parasitas em animais (incluindo espécies transportadas pelo hospedeiro e vetor)" ~ "Parasitas em animais",
#       `Subcategoria CDB` == "Presença clandestina sobre / dentro de avião" ~ "Presença em avião",
#       `Subcategoria CDB` == "Fazendas de peles de animais" ~ "Fazendas de peles de animais",
#       `Subcategoria CDB` == "Melhoramento de paisagem/flora/fauna na natureza" ~ "Melhoramento de paisagem",
#       `Subcategoria CDB` == "Pessoas e bagagens / equipamento (especialmente turismo)" ~ "Bagagens e turismo",
#       `Subcategoria CDB` == "Veículos (carro, trem...)" ~ "Veículos",    # modificação 3
#       `Subcategoria CDB` == "Veículos (carro, trem, ...)" ~ "Veículos",  # modificação 3
#       `Subcategoria CDB` == "Veículos (carro, trem,...)" ~ "Veículos",
#       `Subcategoria CDB` == "Contaminação de comida (incluindo comida viva)" ~ "Contaminação de comida incluindo comida viva",    # modificação 2
#       `Subcategoria CDB` == "Contaminação de comida" ~ "Contaminação de comida incluindo comida viva",                            # modificação 2
#       `Subcategoria CDB` == "Contaminação em sementes" ~ "Contaminação em sementes",
#       `Subcategoria CDB` == "Parasitas em plantas (incluindo espécies transportadas pelo hospedeiro e vetor)" ~ "Parasitas em plantas",
#       `Subcategoria CDB` == "Plantas cultivadas" ~ "Plantas cultivadas",
#       `Subcategoria CDB` == "Fins ornamentais (excluindo produção alimentar)" ~ "Fins ornamentais",
#       `Subcategoria CDB` == "Produção florestal (incluindo reflorestamento)" ~ "Produção florestal e reflorestamento",
#       `Subcategoria CDB` == "Controle de erosão / estabilização de dunas (quebra vento, cerca viva...)" ~ "Controle de erosão, quebra vento ou cerca viva",
#       `Subcategoria CDB` == "Agricultura (incluindo biocombustíveis a partir de biomassa)" ~ "Agricultura e biocombustíveis",
#       TRUE ~ as.character(`Subcategoria CDB`)
#     )
#   ) %>% 
#   mutate(
#     `Categoria CDB` = case_when(
#       `Categoria CDB` == "Transporte - clandestinidade" ~ "Transporte clandestino",
#       `Categoria CDB` == "Transporte" ~ "Transporte clandestino",
#       TRUE ~ as.character(`Categoria CDB`)
#     )
#   ) %>% 
#   mutate(
#     `Categoria CDB` = case_when(
#       `Subcategoria CDB` == "Comércio de madeira" ~ "Transporte como contaminante",
#       `Subcategoria CDB` == "Contaminantes em animais (exceto parasitas)" ~ "Transporte como contaminante",
#       `Subcategoria CDB` == "Contaminantes em plantas (exceto parasitas)" ~ "Transporte como contaminante",
#       `Subcategoria CDB` == "Contaminação de comida incluindo comida viva" ~ "Transporte como contaminante",
#       `Subcategoria CDB` == "Contaminação em material para viveiros" ~ "Transporte como contaminante",
#       `Subcategoria CDB` == "Contaminação em sementes" ~ "Transporte como contaminante",
#       `Subcategoria CDB` == "Maquinário e equipamento" ~ "Transporte clandestino",
#       `Subcategoria CDB` == "Transporte de material natural" ~ "Transporte como contaminante",
#       
#       `Subcategoria CDB` == "Aquicultura/maricultura" ~ "Escape",
#       `Subcategoria CDB` == "Transporte de material natural" ~ "Transporte como contaminante",
#       
#       TRUE~ as.character(`Categoria CDB`)
#     )
#   )
# 
# db_consolidada$`Categoria CDB` %>% unique() # 6 categorias! OK
# b <- db_consolidada %>% dplyr::filter(is.na(`Categoria CDB`))
# 
# db_consolidada$`Subcategoria CDB` %>% unique() # 40 categorias! OK
# b <- db_consolidada %>% dplyr::filter(is.na(`Subcategoria CDB`))
# 
# 
# ### Adicionar mecanismo (= Turbelin et al. 2022)
# db_consolidada$`Categoria CDB` %>% unique()
# 
# db_consolidada <- db_consolidada %>% 
#   mutate(
#     Mecanismo = case_when(
#       `Categoria CDB` == "Corredor" ~ "Espalhamento",
#       `Categoria CDB` == "Sem ajuda humana" ~ "Espalhamento",
#       `Categoria CDB` == "Transporte clandestino" ~ "Vetor",
#       TRUE ~ "Commodity"
#     )
#   )
# db_consolidada$Mecanismo %>% unique()
# 
# # Summary:
# db_summary  <-  db_consolidada %>% 
#   group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
#   summarise(
#     riqueza = n()
#   )
# db_summary
# 
# 
# ### Salvar database consolidada -----
# Save file:
# db_consolidada %>%
#   # write.csv(file = here("Entregas", "Arquivos", "Dados",, "00_Vias-vetores-corrigido.csv")
#   #           , row.names = FALSE
#   #           , fileEncoding = "latin1")
#   writexl::write_xlsx(path = here("Entregas", "Arquivos", "Dados", "00_Vias-vetores-corrigido.xlsx"))
