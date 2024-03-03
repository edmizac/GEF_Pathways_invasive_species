# Script name: A6_Temporal
# Script purpose: Realizar análises relacionadas a temporalidade de registros 
# de EEI em cada via e vetore (Produto 1: Atividade 4.6 e produtos seguintes)

# Date created:
# 22/08/2023
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("tidyverse")
library("readxl")
library("lubridate")
library("scales")

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 
theme_set(theme_bw(base_size = 20))
theme_update(
    axis.text.x = element_text(size = 12)
)



# Example accumulation slopes (Produto 2) -----

# Cumsum of records per pathway through time
## Simulate data
set.seed(44)

n_reg <- 2000 # numbero de registros

accum <- data.frame(
  registros = sample(1:10, n_reg, replace=T),
  # alguns anos:
  Data = sample(seq(as.Date('2019-01-01'), as.Date('2020-01-01'), by = "day"), n_reg, replace = TRUE)
  # um mês:
  # Data = sample(seq(as.Date('2023-01-01'), as.Date('2023-01-15'), by = "day"), n_reg, replace = TRUE)
)

# Número de dias desde 01/01/1970
#convert date object to number of days since 1/1/1970 # https://www.statology.org/r-convert-date-to-numeric/
accum <- accum %>% 
  mutate(
    n_dias = # número de dias de 1970 até hoje - número de dias até a Data especificada
      (today() %>% days() %>% time_length(., unit = "days")) 
    - (lubridate::days(Data) %>% time_length(., unit = "days"))
  )

accum %>% str()

accum <- accum %>%
  # mutate(Date = lubridate::dmy(as.character(Data))) %>%
  arrange(Data)    # Just in case not ordered already

accum <- accum %>%
  mutate(
    soma_registros = cumsum(registros)
  )
  
p <- accum %>% 
  ggplot(aes(x= Data, y = soma_registros)) +
  geom_line() +
  geom_smooth(stat = "smooth", span = 1.0, n = nrow(accum))
p

# loessdata <- ggplot_build(p)$data[[1]] # TRUE DATA
loessdata <- ggplot_build(p)$data[[2]] # PREDICTED
# loessdata.last <- loessdata[nrow(loessdata):(nrow(loessdata)-1), ] # last two lines of the stat_smooth data (predicted)

loessdata <- loessdata %>% 
  rename(
    x_predict = x,
    y_predict = y
  )

# Estimate slopes of the last observation
slope.loess <- with(loessdata,diff(y_predict)/diff(x_predict))
# slope.loess <- diff(loessdata.last$y) # errado (ver comentário do Bolker no stackoverflow)
slope.loess.last <- tail(slope.loess, 1)

# ângulo a partir do slope
angle.loess.last <-  atan(slope.loess.last) * 180 / pi # ou: dendrometry::slope2angle(slope.loess.last)

loessdata <- loessdata %>% 
  dplyr::select(x_predict, y_predict) %>% 
  mutate(
    slope = slope.loess.last,
    angle = angle.loess.last
    )

## Add to dataframe
accumdf <-  bind_cols(accum, loessdata)

accumdf <- accumdf %>%
  mutate(
    b = slope * ( - as.numeric(x_predict)) + y_predict  # https://stackoverflow.com/questions/59892472/how-to-draw-a-line-based-on-a-point-and-a-slope-sd-line
  ) %>% 
  mutate(
    Data = as.Date(Data)
  )



## Plot
yrng <- range(accumdf$soma_registros)
xrng <- range(accumdf$Data)

accumdf %>% 
  ggplot(aes(x = Data, y = soma_registros)) + 
  geom_point(
    # aes(size = 1.2)
    ) +
  geom_line(
    # lwd = 1.2, color = "blue"
    ) +
  scale_x_date(date_labels = "%d-%m-%Y", date_breaks = '6 months') +
  # scale_x_date(date_labels = "%d-%m-%Y", date_breaks = '1 week') +
  # stat_smooth(method = "lm", col = "red") +
  geom_smooth(stat = "smooth", alpha = 0.7) +#, color = "black", alpha = 0.7) +
  geom_abline(
    data = accumdf %>% dplyr::filter(soma_registros == max(soma_registros, na.rm = T))
    , aes(intercept = b, slope = slope, color = slope),  lwd = 1.2
    ) +
  # scale_color_gradient(limits = c(0, 1)
  #                      , low = "grey50", high = "red"
  # ) +
  # SLOPE COLOR GRADIENT:
  scale_color_gradient2(limits = c(0, max(slope.loess, na.rm=TRUE))
                        # limits = c(min(slope.loess, na.rm=TRUE), max(slope.loess, na.rm=TRUE))
                        # limits = c(0, 1)
                        , low = "blue"
                        , mid = "yellow"
                        , high = "red"#,
                        # , midpoint = 0.5
                        , midpoint = quantile(slope.loess, probs = 0.5, na.rm = TRUE)
  ) +
  # # ANGLE COLOR GRADIENT:
  # scale_color_gradient2(limits = c(0, 90)
  #                       # limits = c(min(slope.loess, na.rm=TRUE), max(slope.loess, na.rm=TRUE))
  #                       # limits = c(0, 1)
  #                       , low = "blue"
  #                       , mid = "yellow"
  #                       , high = "red"#,
  #                       # , midpoint = 0.5
  #                       , midpoint = 45
  # ) +
  geom_text(aes(x = xrng[1],
                y = yrng[2],
                label = paste0(
                  "slope = ", round(slope.loess.last, 2)
                  # , "\nângulo = ", round(angle.loess.last, 2)
                  )
                , hjust = 0
                , vjust = 1
                )
                , size = 5
  ) +
  ylab("Acumulação de registros") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )

# ggsave(filename = here("Entregas", "A6_Variacao-temporal", "Exemplo_acumulacao_registros2.png"),
#        dpi = 300,  width = 7, height = 5, units = "in")









# Produto 03 ------



# Import and wrangle data ------

db_vias <- read_xlsx(here("Entregas", "Arquivos", "Dados", "Vias-vetores-corrigido.xlsx")) %>% 
  dplyr::select(`Nome científico`, `Categoria CDB`, `Subcategoria CDB`, Ambiente
                , `Grupo biológico`, Situação) %>% 
  dplyr::distinct() %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )

db_occ <- read_xlsx(here("Entregas", "Arquivos", "Dados", "Occorrencias-Vias-vetores.xlsx"))

str(db_occ)

# Estimar datas NAs:
db_occ %>% dplyr::filter(is.na(eventDate)) %>% nrow() # 19424 registros sem data!


## Summary por ambiente ----
### Apenas para spps presentes (não tem registros pra ausentes e contidas) 
accum_ambiente <- db_occ %>% 
  dplyr::select(`Nome científico`, ID.)

db_ambiente <- db_vias %>% 
  dplyr::select(`Nome científico`, Ambiente)

db_ambiente <- dplyr::left_join(accum_ambiente, db_ambiente, relationship = 'many-to-many') %>% 
  distinct() 

# Check NAs:
db_ambiente %>% dplyr::filter(is.na(Ambiente)) %>% 
  dplyr::select(`Nome científico`) %>% 
  distinct()

# Adicionar ambientes pras que estavam NAs
db_ambiente <- db_ambiente %>% 
  mutate(
    Ambiente = case_when(
      `Nome científico` == "Ciona intestinalis" ~ "Marinho",     # algum invertebrado marinho
      `Nome científico` == "Nandayus nenday" ~ "Terrestre",      # é um psitacídeo
      `Nome científico` == "Nymphaea maculata" ~ "Água doce",    # planta aquática
      `Nome científico` == "Crocosmia crocosmiiflora" ~ "Terrestre", # planta terrestre
      TRUE ~ as.character(Ambiente)
    )
  )


# Summarise
db_ambiente %>% 
  group_by(Ambiente) %>%
  summarise(
    n_registros_ambiente = n()
  ) #%>% 
  # # Save:
  # writexl::write_xlsx(path = here("Entregas", "A6_Variacao-temporal"
  #                                 , "A6_Ocorrencias_n-por-ambiente.xlsx"))








# Checar número de registros máximos por espécie e por via (o n determinará o n máximo da 
# escala do slope das figuras):

db_occ_summary_spp <- db_occ %>% 
  group_by(`Nome científico`) %>% 
  summarise(
    n_reg = n(),
    sem_data = sum(is.na(eventDate))
  )
# # Save:
# db_occ_summary_spp %>%
#   write.csv(file = here("Entregas", "A6_Variacao-temporal", "A6_Ocorrencias_n-por-spp.csv")
#             , fileEncoding = "latin1")



# Retirar registros sem data antes do join:
db_occ <- db_occ %>% dplyr::filter(!is.na(eventDate))


# Join nome da via:
db_all <- dplyr::left_join(db_occ, db_vias, relationship = 'many-to-many')

db_all <- db_all %>% 
  rename(
    Nome_científico = `Nome científico`
  )

str(db_all)


# Retirar NAs (explorar em A1_Vias-Vetores.R):

db_all$`Categoria CDB` %>% unique()

db_all <- db_all %>% dplyr::filter(!is.na(`Categoria CDB`))



# Checar número de registros máximos por via (o n determinará o n máximo da 
# escala do slope das figuras). Isso é importante porque o left_join teve que ser
# many-to-many uma vez que cada registro de ocorrência pode ter ocorrido por uma 
# via ou outra (Subcategoria CDB) e isso é impossível de determinar

db_all_summary_vias <- db_all %>% 
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
  summarise(
    n_reg = n()#,
    # sem_data = sum(is.na(eventDate))
  )
## db_all_summary_vias$`Subcategoria CDB` %>% duplicated()
# # Save:
# db_all_summary_vias %>%
#   write.csv(file = here("Entregas", "A6_Variacao-temporal", "A6_Ocorrencias_n-por-vias.csv")
#             , fileEncoding = "latin1")

db_all_summary_cat <- db_all %>% 
  group_by(`Categoria CDB`) %>% 
  summarise(
    n_reg = n()#,
    # sem_data = sum(is.na(eventDate))
  )
# # Save:
# db_all_summary_cat %>%
#   write.csv(file = here("Entregas", "A6_Variacao-temporal", "A6_Ocorrencias_n-por-categoriaCDB.csv")
#             , fileEncoding = "latin1")


### Transform dates: -----

# Primeiro, separar coluna com mais que uma observação
# Como não sabemos qual data é a verdadeira ou se são dois registros em cada data,
# Vou considerar apenas o primeiro registro

# Vamos usar o comprimento (em caracteres) do eventDate pq o str_dected() é 
# pésimo de usar com "/": 

# stringr::str_detect(db_all$eventDate, "/")
# stringr::str_detect(db_all$eventDate, "-/")
# stringr::str_detect(db_all$eventDate, "\\/{2, }") # {1, } = one or more
# stringr::str_detect(db_all$eventDate, "/||-")
# stringr::str_detect(db_all$eventDate, "(/)|(-)")


# Adicionando o length para saber qual tipo de erro tenho que resolver:
db_all <- db_all %>% 
  mutate(
    eventDate_length = stringr::str_length(eventDate)
  )

# Erros pra corrigir em eventDate:
stringr::str_length(db_all$eventDate) %>% unique()

db_all$eventDate_length %>% unique()
b <- db_all %>% dplyr::filter(eventDate_length == 4) # apenas ano 
b <- db_all %>% dplyr::filter(eventDate_length == 5) # numerico
b <- db_all %>% dplyr::filter(eventDate_length == 8) # micos (data incompleta)
b <- db_all %>% dplyr::filter(eventDate_length == 9) # range exemplo '2012-2015'
b <- db_all %>% dplyr::filter(eventDate_length == 10) # dd/mm/YYYY # OK
b <- db_all %>% dplyr::filter(eventDate_length == 11) # range exemplo '2012 e 2013' e '2013 a 2018'
b <- db_all %>% dplyr::filter(eventDate_length == 12) # range exemplo '2012 to 2013' (mesmo caso do == 9)
b <- db_all %>% dplyr::filter(eventDate_length == 13) # range exemplo '2012 and 2013' (mesmo caso do == 11)
b <- db_all %>% dplyr::filter(eventDate_length == 15) # range exemplo '2012-to present' (mesmo caso do == 9)
b <- db_all %>% dplyr::filter(eventDate_length == 16) # range exemplo '2007, 2008, 2009'
b <- db_all %>% dplyr::filter(eventDate_length == 18) # todos 2017 (substituí abaixo)
b <- db_all %>% dplyr::filter(eventDate_length == 19) # as_date()
b <- db_all %>% dplyr::filter(eventDate_length == 21) # range com data completa exemplo '"2015-06-27/2015-06-28' -> usar duas datas

b$eventDate %>% unique()

# str_extract(b$eventDate, "[:digits:]$")
# str_extract(b$eventDate, "(?<=-)[0-9]{4}")
# str_extract(b$eventDate, "")




# Corrigir datas com '2012 e 2015' (length = 11) e '2012 and 2015' (length = 13):
db_all <- db_all %>% 
  tidyr::separate_longer_delim(eventDate, ' e ') # +9 registros
db_all <- db_all %>% 
  tidyr::separate_longer_delim(eventDate, ' and ') # +6 registros
# Total = 15

# Update eventDate_length:
db_all <- db_all %>% 
  mutate(
    eventDate_length = stringr::str_length(eventDate)
  )

a <- db_all %>% dplyr::filter(eventDate_length == 11)
a$eventDate_length
a$eventDate # sobraram os formatos'2013 a 2018'. Como é uma sequencia, vou consertá-los junto com length = 9, 12 e 15


# Corrigir datas com length = 9, 11, 12, 15:
db_all <- db_all %>% 
  mutate(
    seq = case_when(
      str_detect(eventDate, ' a ') == TRUE ~ "sequência", # length = 9
      str_detect(eventDate, ' to ') == TRUE ~ "sequência", # lenth = 12
      str_detect(eventDate, ' to present ') == TRUE ~ "sequência", # lenth = 15
      TRUE ~ "não"
    )
  )


# Check NA's:
a <- db_all %>% dplyr::filter(is.na(`Categoria CDB`)) # 0 = OK

# ae <- db_all %>%
#   dplyr::filter(str_detect(eventDate, ' a ')) %>%
#   tidyr::separate_longer_delim(eventDate, ' a ') %>%
#   mutate(
#     # create unique id
#     # id_a = paste0(rep(c("A", "B"), times = nrow(.) / 2),  "_", dplyr::cur_group_rows()) #https://stackoverflow.com/questions/74480834/r-split-a-row-into-multiple-rows-and-then-split-the-column-into-multiple-colum
#     # id_a = paste0(rep(LETTERS, each = 2),  "_", rep(c(1:2), times = nrow(.) / 2))
#     # id_a = rep(LETTERS[1:2], times = nrow(.) / 4, each = 2),
#     # id_b = rep(c(1:2), times = nrow(.) / 2)
#     # id_a = rep(LETTERS[1:21], each = 2, times = nrow(.) / 42),
#     id_a = rep(LETTERS[1:14], each = 2, length.out = nrow(.)),
#     id_b = rep(c(1:(nrow(.)/2)), each = 2)
#   )

# ae <- ae %>%
#   group_by(id_a, id_b) %>%
#   mutate(
#     # specify nubmer of rows:
#     n.times = case_when(
#       id_a == lag(id_a) ~ "teste"
#     ),
#     n.times.years = case_when(
#       !is.na(n.times) ~ as.numeric(eventDate) - lag(as.numeric(eventDate))
#     )
#   ) #%>%
#   # slice(rep(as.numeric(eventDate[1]):as.numeric(eventDate[2])))
#   # tidyr::expand(eventDate)
# 
# ae <- ae %>% 
#   group_by(id_a, id_b) %>%
#   # complete(teste = seq(min(ymd(eventDate, truncated = 2L)), max(ymd(eventDate, truncated = 2L)), by = "years"))
#   complete(teste = seq.Date(min(ymd(eventDate, truncated = 2L)), max(ymd(eventDate, truncated = 2L)), by = "years"))

ae <- db_all %>%
  # dplyr::filter(seq == "sequência") %>%
  dplyr::filter(str_detect(eventDate, ' a ')) %>%
  tidyr::separate_wider_delim(eventDate, ' a ', names = c('start', 'end')) %>% 
  mutate(
    start = as.Date(ISOdate(start, 1, 1)),
    end = as.Date(ISOdate(end, 1, 1))
  )

ae <- db_all %>%
  # dplyr::filter(seq == "sequência") %>%
  dplyr::filter(str_detect(eventDate, '-to present')) %>%
  tidyr::separate_wider_delim(eventDate, '-to present', names = c('start', 'end')) %>% 
  mutate(
    start = as.Date(ISOdate(start, 1, 1)),
    end = as.Date(ISOdate(2023, 1, 1))
  ) %>% 
  bind_rows(ae)

ae <- db_all %>%
  # dplyr::filter(seq == "sequência") %>%
  dplyr::filter(str_detect(eventDate, ' to ')) %>%
  tidyr::separate_wider_delim(eventDate, ' to ', names = c('start', 'end')) %>% 
  mutate(
    start = as.Date(ISOdate(start, 1, 1)),
    end = as.Date(ISOdate(end, 1, 1))
  ) %>% 
  bind_rows(ae)

ae_l <- nrow(ae)

ae <- ae %>% 
  # mutate(
  #   id_a = rep(LETTERS[1:14], each = 2, length.out = nrow(.)),
  #       id_b = rep(c(1:(nrow(.)/2)), each = 2)
  # ) %>% 
  # group_by(id_a, id_b) %>%
  group_by(row_number()) %>%
  # complete(teste = seq(min(ymd(start, truncated = 2L)), max(ymd(end, truncated = 2L)), by = "years"))
  tidyr::complete(start = seq.Date(max(start), max(end), by = "year")) %>%
  rename(eventDate = start) %>% 
  # tidyr::fill(c(`Nome científico`, Local))
  tidyr::fill(c(`Nome_científico`, Local, Município, decimalLatitude
                , decimalLongitude, measurementDeterminedBy, coordinate.type.
                , coordinateUncertaintyInMeters, rightsHolder, sigla.UF., ID.
                , Situação, `Categoria CDB`, `Subcategoria CDB`, Ambiente
                , `Grupo biológico`, eventDate_length, seq
  )) %>% 
  mutate(eventDate = as.character(eventDate)) %>% 
  dplyr::select(-c(end)) %>% 
  ungroup()

ae <- ae[, 2:ncol(ae)]

# Filtrar linhas excluídas (length = 9, 11, 12 e 15)
db_all <- db_all %>% 
  # dplyr::filter(!str_detect(eventDate, ' a ')) %>% # length = 9
  # dplyr::filter(!str_detect(eventDate, ' to ')) %>%  # lenth = 12
  # dplyr::filter(!str_detect(eventDate, ' to present ')) # lenth = 15
  dplyr::filter(eventDate_length != 9) %>% # length = 9
  dplyr::filter(eventDate_length != 11) %>%  # lenth = 11
  dplyr::filter(eventDate_length != 12) %>%  # lenth = 12
  dplyr::filter(eventDate_length != 15) # lenth = 15
  
nrow(ae) - ae_l # numero total adicionado nessa correção = 3769

# Put the corrected lines of ae back:
# db_all %>% nrow()
# db_all %>%
#   # dplyr::filter(str_detect(eventDate, ' a ')) %>% nrow()
#   dplyr::filter(!str_detect(eventDate, ' a ')) %>% nrow()
#   # dplyr::filter(str_detect(eventDate, ' a ', negate = TRUE)) %>% nrow()
unmatches <- ae$ID. %>% unique()

db_all <- db_all %>%
  # dplyr::filter(!str_detect(eventDate, ' a ')) %>%  # era pra ser nrow() - 714 -> nao consegui entender pq n funciona
  dplyr::filter(!ID. %in% unmatches) %>% 
  bind_rows(ae)

# Check NA's:
a <- db_all %>% dplyr::filter(is.na(eventDate)) # 0 = OK



# Corrigir datas com length = 16:
avirg <- db_all %>%
  # dplyr::filter(seq == "sequência") %>%
  dplyr::filter(str_detect(eventDate, ',')) %>%
  tidyr::separate_wider_delim(eventDate, ',', names = c('start', 'mid',  'end')) %>% 
  mutate(
    start = as.Date(ISOdate(start, 1, 1)),
    mid = as.Date(ISOdate(mid, 1, 1)),
    end = as.Date(ISOdate(2023, 1, 1))
  ) %>% 
  pivot_longer(
    cols = c(start, mid, end),
    values_to = 'eventDate',
    names_to = 'whatever'
  ) %>% 
  dplyr::select(-whatever) %>% 
  mutate(eventDate = as.character(eventDate)) 

nrow(avirg) # registros adicionais = 96

# Filtrar do original:
db_all <- db_all %>%
  dplyr::filter(!str_detect(eventDate, ',')) 

# Bind all together
db_all <- db_all %>% 
  bind_rows(avirg)

# Check NA's:
a <- db_all %>% dplyr::filter(is.na(eventDate)) # 0 = OK


# Corrigir datas com length = 21 (inicio e final) -< até linha 7526 funciona, 
# dps vira tudo NA (por causa do formato mm/dd/YYYY): 

# parse_date_time(c('12/05/2016','25/11/2017','12/12/2000'), orders= c('dmy', 'mdy')

a21 <- db_all %>%
  dplyr::filter(eventDate_length == 21) %>% 
  tidyr::separate_wider_delim(eventDate, '/', names = c('start',  'end'), cols_remove = TRUE) %>% 
  mutate(
    # start = as.Date(ISOdate(start, 1, 1)),
    # end = as.Date(ISOdate(end, 1, 1))
    # start = as.Date(start, tryFormats = c("dd-mm-%Y","%m/%d/%Y")),
    # end = as.Date(end, tryFormats = c("%d-%m-%Y","%m/%d/%Y"))
    start = parse_date_time(start, orders= c('dmy', 'mdy', 'ymd', 'ydm')),
    end = parse_date_time(end, orders= c('dmy', 'mdy', 'ymd', 'ydm'))
  ) #%>%

# # Identificar os que não estão transformando:
# a21_na <- a21 %>% dplyr::filter(is.na(start))
# a21_na <- a21 %>% dplyr::filter(is.na(end))
# out <- a21_na$ID. %>% unique()
# a21_na <- db_all %>% dplyr::filter(ID. %in% out)
# a21_na$eventDate %>% unique() # Só formatos zuados:
# # [1] "2010-09-01/1900-01-00" "2010-03-01/1900-01-00"
# # [3] "06-10-2009/01-00-1900"
# 
# # db_all <- db_all %>% 
# #   mutate(
# #     eventDate = case_when(
# #       eventDate == "2010-09-01/1900-01-00" ~ "2010-09-01",
# #       eventDate == "2010-03-01/1900-01-00" ~ "2010-03-01",
# #       eventDate == "06-10-2009/01-00-1900" ~ "06-10-2009",
# #       TRUE ~ eventDate
# #     )
# #   )

a21 <- a21 %>% 
  mutate(
    diff = end - start # difftime -> entre 1 e 90 dias: a21$diff %>% max(., na.rm=T)
  ) %>%  
  pivot_longer(
    cols = c(start, end),
    values_to = 'eventDate',
    names_to = 'whatever'
  ) %>% 
  dplyr::select(-whatever, diff) %>% 
  mutate(eventDate = as.character(eventDate))


# Check NA's:
a <- a21 %>% dplyr::filter(is.na(eventDate)) # 19 NAs! (Corrigidos diretamente em db_all acima)
# # Registros de 1900 apenas:
outs <- a$ID. %>% unique()
outs
# # remove dates = 1900
a21 <- a21 %>%  dplyr::filter(!ID. %in% outs) # NAs removidos

nrow(a21) - dplyr::filter(db_all, eventDate_length == 21) %>% nrow() # registros adicionais = 9347


# Tirar esses registros de db_all:
db_all <- db_all %>%
  dplyr::filter(!eventDate_length == 21) %>%
  dplyr::filter(!ID. %in% outs)

# Bind together:
db_all <- db_all %>% 
  bind_rows(a21)

# Check NA's:
a <- db_all %>% dplyr::filter(is.na(eventDate)) # 0 NAs!

# Update length and check length == 21
db_all <- db_all %>% 
  mutate(
    eventDate_length = stringr::str_length(eventDate)
  )
db_all %>%
  dplyr::filter(eventDate_length == 21) # length = 21 GONE

# a <- db_all %>%
#   dplyr::filter(eventDate_length == 19) %>%
#   mutate(eventDate == ymd(eventDate))
# ymd_hms("2017-08-25T00:00:00", truncated = 3)
# as_date("2017-08-25T00:00:00")
# as.POSIXct("2017-08-25T00:00:00", tz = "")

# Ter certeza que tudo em db_all$eventDate é charactere:
db_all$eventDate %>% str()
# db_all <- db_all %>% 
#   mutate(eventDate = as.character(eventDate))



# Corrigir todos os outros casos:
db_all$eventDate_length %>% unique()
b <- db_all %>% dplyr::filter(eventDate_length == 4) # apenas ano 
b <- db_all %>% dplyr::filter(eventDate_length == 5) # numerico
b <- db_all %>% dplyr::filter(eventDate_length == 8) # micos (data incompleta)
# b <- db_all %>% dplyr::filter(eventDate_length == 9) # range exemplo '2012-2015'
b <- db_all %>% dplyr::filter(eventDate_length == 10) # dd/mm/YYYY # OK
# b <- db_all %>% dplyr::filter(eventDate_length == 11) # range exemplo '2012 e 2013'
# b <- db_all %>% dplyr::filter(eventDate_length == 12) # range exemplo '2012 to 2013' (mesmo caso do == 9)
# b <- db_all %>% dplyr::filter(eventDate_length == 13) # range exemplo '2012 and 2013' (mesmo caso do == 11)
# b <- db_all %>% dplyr::filter(eventDate_length == 15) # range exemplo '2012 to present' (mesmo caso do == 9)
# b <- db_all %>% dplyr::filter(eventDate_length == 16) # range exemplo '2012 to present' (mesmo caso do == 9)
b <- db_all %>% dplyr::filter(eventDate_length == 18) # todos 2017 (substituí abaixo)
b <- db_all %>% dplyr::filter(eventDate_length == 19) # as_date()
# b <- db_all %>% dplyr::filter(eventDate_length == 21) # range com data completa exemplo '"2015-06-27/2015-06-28' -> usar duas datas


b$eventDate
# b$eventDate %>% as.numeric() / 365

# a <- db_all %>% 
#   mutate(
#     contem_charact = str_detect(" ", eventDate)
#   )
# 
# a %>% summary()
    
db_all <- db_all %>% 
  mutate(
    eventDate_correct = case_when(
      eventDate_length == 4 ~ as.Date(ISOdate(eventDate, 1, 1)) %>% as.character(), # apenas o ano
      eventDate_length == 5 ~ as.Date(as.numeric(eventDate), origin = "1900-01-01") %>% as.character(), # check
      eventDate_length == 8 ~ as.character("01-01-2017"),
      eventDate_length == 18 ~ as.Date(ISOdate(2017, 1, 1)) %>% as.character(), # todos 2017
      eventDate_length == 19 ~ as_date(eventDate) %>% as.character(), #as_date("2017-08-25T00:00:00")
      TRUE ~ as.character(eventDate)
    )
  )    

# Check wrong conversions:
a <- db_all %>% dplyr::filter(is.na(eventDate_correct)) # 0 NAs

# Update eventDate_length:
db_all <- db_all %>% 
  mutate(
    eventDate_length = stringr::str_length(eventDate_correct)
  )

# Checar se todos tem o mesmo formato
db_all$eventDate_length %>% unique()
db_all %>% dplyr::select(eventDate_correct) %>% print(n = 200) # nem todo formato é igual (possui - em vez de /)

# Trocar - por /:
db_all <- db_all %>% 
  mutate(
    eventDate_correct = str_replace_all(eventDate_correct, '-', '/')
  )


### Convert all to the same date format:  -----
db_all <- db_all %>% 
  mutate(
    eventDate_correct = parse_date_time(eventDate_correct
                                          , orders= c('dmy','ymd', 'mdy', 'ydm')
                                          , select_formats = 'dmY'
                                          ) %>% 
      format('%d/%m/%Y')
    )
  
  

db_all %>% dplyr::select(eventDate_correct) %>% print(n = 200) # nem todo formato é igual

str(db_all)


# Check NA's:
a_na <- db_all %>% dplyr::filter(is.na(eventDate_correct)) # 0 NAs
# a_na <- a_na %>% dplyr::filter(eventDate_length == 21) # = NAs for length == 21


# Rename column names
db_all <- db_all %>% 
  rename(
    eventDate_original = eventDate,
    eventDate = eventDate_correct
  )


### Salvar planilhas com datas corrigidas: ----

db_all_summary_vias <- db_all %>% 
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
  summarise(
    n_reg = n()#,
    # sem_data = sum(is.na(eventDate))
  )
# # Save:
# db_all_summary_vias %>%
#   write.csv(file = here("Entregas", "A6_Variacao-temporal", "A6_Ocorrencias_n-por-vias_corrigidos.csv")
#             , fileEncoding = "latin1")

db_all_summary_cat <- db_all %>% 
  group_by(`Categoria CDB`) %>% 
  summarise(
    n_reg = n()#,
    # sem_data = sum(is.na(eventDate))
  )
# # Save:
# db_all_summary_cat %>%
#   write.csv(file = here("Entregas", "A6_Variacao-temporal", "A6_Ocorrencias_n-por-categoriaCDB_corrigido.csv")
#             , fileEncoding = "latin1")

bpk <- db_all

rm(a)
rm(a21)
rm(a_na)
rm(a21_na)
rm(ae)
rm(avirg)
rm(b)


gc()






# Calculo da temporalidade das vias -----

theme_set(theme_bw(base_size = 20))
theme_update(
  axis.text.x = element_text(size = 12, angle = 30, hjust = 0.5)
  # plot.title = element_text(size = 12)
)

db_all$eventDate

# Renomear colunas
accum <- db_all %>% 
  mutate(
    Data = eventDate
  )

# Tornar tudo as.Date no formato correto:
accum <- accum %>% 
  mutate(
    Data = as.Date(eventDate, format = "%d/%m/%Y")
  )

# Rearranjar por data (ordem)
accum <- accum %>%
  # mutate(Date = lubridate::dmy(as.character(Data))) %>%
  arrange(Data)    # Just in case not ordered already


# POR VIA -----

# A análise será feita por via. Logo, agruparemos o dataset por via/vetor e realizaremos o acumulo dos registros

# Número de dias desde 01/01/1970: https://www.statology.org/r-convert-date-to-numeric/
accum <- accum %>% 
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
  mutate(
    n_dias = # número de dias de 1970 até 31/12/2023 - número de dias até a Data especificada
      # (today() %>% days() %>% time_length(., unit = "days")) 
      (dmy("31/12/2023") %>% days() %>% time_length(., unit = "days")) 
    - (days(as.Date(eventDate)) %>% time_length(., unit = "days"))
  )

accum %>% str()


# Acumular registros:
accum <- accum %>%
  mutate(
    registros = 1, # sempre um registro por linha
    soma_registros = cumsum(registros)
  )

max(accum$soma_registros, na.rm=TRUE)
summary(accum$soma_registros)

# str(accum_via)


### Pegar todos os valores de slope para padronizar

slopes_list <- data.frame(
  via = character(),
  slope = c(),
  angulo = c()
)

for (cat in unique(accum$`Subcategoria CDB`)) {
  cat_name <- cat
  # cat_name <- accum$`Subcategoria CDB` %>% unique() %>% nth(3)
  
  accum_via <- accum %>%
    dplyr::filter(`Subcategoria CDB` == cat_name) ; nrow(accum_via)
  
  p <- accum_via %>% 
    ggplot(aes(x= Data, y = soma_registros)) +
    geom_smooth(method = 'glm', method.args=list(family="poisson")
                , n = nrow(accum_via)
    )
  
  # p
  # ggplot_build(p)
  
  loessdata <- ggplot_build(p)$data[[1]] # PREDICTED
  # loessdata.last <- loessdata[nrow(loessdata):(nrow(loessdata)-1), ] # last two lines of the stat_smooth data (predicted)
  
  if (nrow(loessdata) == 0) {
    
    info_via <- data.frame(via = cat_name, slope = NA, angle = NA)
    
  } else {
    
    loessdata <- loessdata %>% 
      rename(
        x_predict = x,
        y_predict = y
      )
    
    # Estimate slopes of the last observation
    slope.loess <- with(loessdata,diff(y_predict)/diff(x_predict))
    # slope.loess <- diff(loessdata.last$y) # errado (ver comentário do Bolker no stackoverflow)
    slope.loess.last <- tail(slope.loess, 1)
    # slope.loess.last <- tail(slope.loess, 2)
    
    # ângulo a partir do slope
    angle.loess.last <-  atan(slope.loess.last) * 180 / pi # ou: dendrometry::slope2angle(slope.loess.last)
    

    # Bind
    info_via <- data.frame(via = cat_name
                           , slope = slope.loess.last
                           , angulo = angle.loess.last)
    
    slopes_list <- bind_rows(slopes_list, info_via)
    
  }
  
}



# Padronizar valores entre 0.01 e 1:
# scale2 <- function(x, na.rm = FALSE) { (x - min(x, na.rm = FALSE)) / (max(x, na.rm = FALSE) - min(x, na.rm = FALSE))}

slopes_list <- slopes_list %>% 
  mutate(
    slope_padronizado = scales::rescale(slope, to=c(0.01, 1))
  )

nvias <- nrow(slopes_list)

# Incluir categoria CDB:
vv <- read_xlsx(here("Entregas", "Arquivos", "Dados", "Vias-vetores-corrigido.xlsx")) %>% 
  rename(
    # catCDB = `Categoria CDB`,
    via = `Subcategoria CDB`
    ) %>% 
  dplyr::select(c(`Categoria CDB`, via)) %>% 
  distinct() %>% 
  mutate(
    `Categoria CDB` = forcats::fct_relevel(`Categoria CDB`,
                                           "Sem ajuda humana", "Corredor",
                                           "Transporte como contaminante",
                                           "Soltura na natureza", "Escape", 
                                           "Transporte clandestino")
  )

vias_missing <- vv$via %in% slopes_list$via
vias_missing <- vv[!vias_missing, ]
vias_missing # Vias que não tem observação suficiente no dataset

# Vias que não tem registros suficientes:
via_sem_occ <- vias_missing$via[vias_missing$via %in% accum$`Subcategoria CDB`]
via_sem_occ
# numero de registros:
a <- accum %>% dplyr::filter(`Subcategoria CDB` == via_sem_occ) ; a ; nrow(a)

# Vias que nao aparecem no dataset de ocorrencias:
via_sem_n_suf <- vias_missing$via[!vias_missing$via %in% accum$`Subcategoria CDB`]
via_sem_n_suf
a <- accum %>% dplyr::filter(`Subcategoria CDB` %in% via_sem_n_suf) ; a ; nrow(a)


# Juntar vias com categoria cdb para o plot:
slopes_list <- dplyr::left_join(slopes_list, vv)
slopes_list <- bind_rows(slopes_list, vias_missing)

palCDB_cat <- rev(c("#9359a5", "#9c83bd", "#8c97c5" , "#26828f", "#52c56a", "#badd27"))

### Plot distribuição de slopes padronizados -----
p_slopes <- slopes_list %>% 
  ggplot() +
  geom_histogram(aes(x = slope_padronizado, fill = `Categoria CDB`)) +
  scale_fill_manual(values = palCDB_cat) +
  ggtitle(paste("Variação temporal de", nvias, "vias e vetores")) +
  xlab("Slope padronizado") +
  ylab("Contagem") +
  theme(
    axis.text.x = element_text(size = 20, angle = 0),
    # legend.position = "bottom"
    legend.position = c(0.58, 0.82),
    legend.key.size = unit(0.4, 'cm'),
    legend.text = element_text(size=12),
  ) + 
  ylim(0, 14) +
  guides(fill = guide_legend(nrow = 3))

p_slopes

# # Save:
# p_slopes %>% 
#   ggsave(filename = here("Entregas", "A6_Variacao-temporal", "Distribuicao_slopes.png"),
#          dpi = 300,  width = 7, height = 5, units = "in")


# # # Save:
# slopes_list %>%
#   write.csv(file = here("Entregas", "A6_Variacao-temporal", "A6_Acumulacao_vias.csv")
#             , fileEncoding = "latin1")


### Plots exploratórios ----
i <- 1
for (cat in unique(accum$`Subcategoria CDB`)) {
  cat_name <- cat
  # cat_name <- accum$`Subcategoria CDB` %>% unique() %>% nth(34)
  
  accum_via <- accum %>%
    dplyr::filter(`Subcategoria CDB` == cat_name) ; nrow(accum_via)
  
  if (nrow(accum_via) >= 10) {
  
  p <- accum_via %>% 
    ggplot(aes(x= Data, y = soma_registros)) +
    geom_line() +
    geom_point() +
    geom_smooth(method = 'glm', method.args=list(family="poisson")
                , n = nrow(accum_via)
                )
    # geom_smooth(stat = "smooth"
    #             # , span = 0.5
    #             # , n = nrow(accum_via)/2
    #             )
    # scale_x_date(date_labels = "%d-%m-%Y", date_breaks = '10 years') # Isso muda os dados
    # facet_wrap(~`Subcategoria CDB`)

  # p
  # ggplot_build(p)
  
  # loessdata <- ggplot_build(p)$data[[1]] # TRUE DATA
  loessdata <- ggplot_build(p)$data[[3]] # PREDICTED
  # loessdata.last <- loessdata[nrow(loessdata):(nrow(loessdata)-1), ] # last two lines of the stat_smooth data (predicted)
  
  loessdata <- loessdata %>% 
    rename(
      x_predict = x,
      y_predict = y
    )
  
  # Estimate slopes of the last observation
  slope.loess <- with(loessdata,diff(y_predict)/diff(x_predict))
  # slope.loess <- diff(loessdata.last$y) # errado (ver comentário do Bolker no stackoverflow)
  slope.loess.last <- tail(slope.loess, 1)
  
  # # (with GAM loess): It has generated Infinite values. Let's drop them out (don't know what is best to do)
  # slope.loess[is.infinite(slope.loess)] <- NA
  
  # ângulo a partir do slope
  angle.loess.last <-  atan(slope.loess.last) * 180 / pi # ou: dendrometry::slope2angle(slope.loess.last)
  
  loessdata <- loessdata %>% 
    dplyr::select(x_predict, y_predict) %>% 
    mutate(
      slope = slope.loess.last,
      angle = angle.loess.last
    )
  
  ## Add to dataframe
  accumdf <-  bind_cols(accum_via, loessdata)
  
  accumdf <- accumdf %>%
    mutate(
      b = slope * ( - as.numeric(x_predict)) + y_predict  # https://stackoverflow.com/questions/59892472/how-to-draw-a-line-based-on-a-point-and-a-slope-sd-line
    )
  
  
  
  ## Plot
  yrng <- range(accumdf$soma_registros)
  xrng <- range(accumdf$Data)
  
  accumdf %>% 
    ggplot(aes(x = Data, y = soma_registros)) + 
    geom_point(
      # aes(size = 1.2)
    ) +
    geom_line(
      # lwd = 1.2, color = "blue"
    ) +
    # scale_x_date(date_labels = "%Y", date_breaks = '10 years') +
    # scale_x_date(date_labels = "%d-%m-%Y", date_breaks = '1 week') +
    # stat_smooth(method = "lm", col = "red") +
    stat_smooth(method = 'glm', method.args=list(family="poisson")
                # , n = nrow(accum_via)
                , fullrange = TRUE
                , alpha = 0.7) +#, color = "black", alpha = 0.7) +
    geom_abline(
      data = accumdf %>% dplyr::filter(soma_registros == max(soma_registros, na.rm = T))
      , aes(intercept = b, slope = slope, color = slope),  lwd = 1.2  # slope
      # , aes(intercept = b, slope = slope, color = angle),  lwd = 1.2  # angulo
    ) +
    # scale_color_gradient(limits = c(0, 1)
    #                      , low = "grey50", high = "red"
    # ) +
    # SLOPE COLOR GRADIENT:
    scale_color_gradient2(limits = c(0, max(slopes_list$slope, na.rm=TRUE)) # slope
    # scale_color_gradient2(limits = c(0, 90)                               # angulo
                          # limits = c(min(slope.loess, na.rm=TRUE), max(slope.loess, na.rm=TRUE))
                          # limits = c(0, 1)
                          , low = "#488f31"
                          , mid = "#ffeb8f"
                          # , mid = "#fbd163"
                          , high = "#f51616"#,
                          # , midpoint = 0.5
                          , midpoint = quantile(slope.loess, probs = 0.5, na.rm = TRUE)
    ) +
    # # ANGLE COLOR GRADIENT:
    # scale_color_gradient2(limits = c(0, 90)
    #                       # limits = c(min(slope.loess, na.rm=TRUE), max(slope.loess, na.rm=TRUE))
    #                       # limits = c(0, 1)
    #                       , low = "blue"
    #                       , mid = "yellow"
    #                       , high = "red"#,
    #                       # , midpoint = 0.5
    #                       , midpoint = 45
    # ) +
    geom_text(aes(x = xrng[1],
                  y = yrng[2],
                  label = paste0(
                    "slope = ", round(slope.loess.last, 4)
                    # , "\nângulo = ", round(angle.loess.last, 2)
                  )
                  , hjust = 0
                  , vjust = 1
    )
    , size = 5
    ) +
    ylab("Acumulação de registros") +
    ggtitle(cat_name)

  # ggsave(filename = here("Entregas", "A6_Variacao-temporal", "Plots_acumulacao"
  #                        , paste0("Acumulacao_vias_", i, "-", cat_name, "_slope.png")),
  #        dpi = 300,  width = 7, height = 5, units = "in")
  
  }
  
  i <- i + 1
}










# POR VIA + AMBIENTE  -----

# Via + ambiente pra poder priorizar as vias conforme exigido pelo MMA

# Número de dias desde 01/01/1970: https://www.statology.org/r-convert-date-to-numeric/
accum <- accum %>% 
  group_by(Ambiente, `Categoria CDB`, `Subcategoria CDB`) %>% 
  mutate(
    n_dias = # número de dias de 1970 até 31/12/2023 - número de dias até a Data especificada
      # (today() %>% days() %>% time_length(., unit = "days")) 
      (dmy("31/12/2023") %>% days() %>% time_length(., unit = "days")) 
    - (days(as.Date(eventDate)) %>% time_length(., unit = "days"))
  )

accum %>% str()


# Acumular registros:
accum <- accum %>%
  mutate(
    registros = 1, # sempre um registro por linha
    soma_registros = cumsum(registros)
  )

max(accum$soma_registros, na.rm=TRUE)
summary(accum$soma_registros)

# str(accum_via)


### Pegar todos os valores de slope para padronizar

slopes_list <- data.frame(
  ambiente = character(),
  via = character(),
  slope = c(),
  angulo = c()
)

rm(amb_name)
rm(cat_name)

for (amb in unique(accum$Ambiente)) {
  
  amb_name <- amb
  # amb_name <- accum$Ambiente %>% unique() %>% nth(3)
  
  accum_amb <- accum %>% 
    dplyr::filter(Ambiente == amb_name)
  
  for (cat in unique(accum_amb$`Subcategoria CDB`)) {
    cat_name <- cat
    # cat_name <- accum_amb$`Subcategoria CDB` %>% unique() %>% nth(3)
    
    accum_via <- accum_amb %>%
      dplyr::filter(`Subcategoria CDB` == cat_name) ; nrow(accum_via)
    
    p <- accum_via %>% 
      ggplot(aes(x= Data, y = soma_registros)) +
      geom_smooth(method = 'glm', method.args=list(family="poisson")
                  , n = nrow(accum_via)
      )
    
    # p
    # ggplot_build(p)
    
    loessdata <- ggplot_build(p)$data[[1]] # PREDICTED
    # loessdata.last <- loessdata[nrow(loessdata):(nrow(loessdata)-1), ] # last two lines of the stat_smooth data (predicted)
    
    if (nrow(loessdata) == 0) {
      
      info_via <- data.frame(ambiente = amb_name, via = cat_name, slope = NA, angle = NA)
      
    } else {
      
      loessdata <- loessdata %>% 
        rename(
          x_predict = x,
          y_predict = y
        )
      
      # Estimate slopes of the last observation
      slope.loess <- with(loessdata,diff(y_predict)/diff(x_predict))
      # slope.loess <- diff(loessdata.last$y) # errado (ver comentário do Bolker no stackoverflow)
      slope.loess.last <- tail(slope.loess, 1)
      # slope.loess.last <- tail(slope.loess, 2)
      
      # ângulo a partir do slope
      angle.loess.last <-  atan(slope.loess.last) * 180 / pi # ou: dendrometry::slope2angle(slope.loess.last)
      
      
      # Bind
      info_via <- data.frame(ambiente = amb_name
                             , via = cat_name
                             , slope = slope.loess.last
                             , angulo = angle.loess.last)
      
      slopes_list <- bind_rows(slopes_list, info_via)
      
    }
    
  }

}


# Padronizar valores entre 0.01 e 1:
# scale2 <- function(x, na.rm = FALSE) { (x - min(x, na.rm = FALSE)) / (max(x, na.rm = FALSE) - min(x, na.rm = FALSE))}

slopes_list <- slopes_list %>% 
  # por ambiente
  group_by(ambiente) %>% 
  mutate(
    slope_padronizado = scales::rescale(slope, to=c(0.01, 1))
  ) %>% 
  arrange(desc(slope_padronizado), .by_group = TRUE)

# nvias <- nrow(slopes_list)
nvias <- slopes_list %>% dplyr::select(ambiente, via) %>% distinct() %>% nrow()

# Incluir categoria CDB:
vv <- read_xlsx(here("Entregas", "Arquivos", "Dados", "Vias-vetores-corrigido.xlsx")) %>% 
  rename(
    # catCDB = `Categoria CDB`,
    via = `Subcategoria CDB`
  ) %>% 
  dplyr::select(c(`Categoria CDB`, via)) %>% 
  distinct() %>% 
  mutate(
    `Categoria CDB` = forcats::fct_relevel(`Categoria CDB`,
                                           "Sem ajuda humana", "Corredor",
                                           "Transporte como contaminante",
                                           "Soltura na natureza", "Escape", 
                                           "Transporte clandestino")
  )

vias_missing <- vv$via %in% slopes_list$via
vias_missing <- vv[!vias_missing, ]
vias_missing # Vias que não tem observação suficiente no dataset

# Vias que não tem registros suficientes:
via_sem_occ <- vias_missing$via[vias_missing$via %in% accum$`Subcategoria CDB`]
via_sem_occ
# numero de registros:
a <- accum %>% dplyr::filter(`Subcategoria CDB` == via_sem_occ) ; a ; nrow(a)

# Vias que nao aparecem no dataset de ocorrencias:
via_sem_n_suf <- vias_missing$via[!vias_missing$via %in% accum$`Subcategoria CDB`]
via_sem_n_suf
a <- accum %>% dplyr::filter(`Subcategoria CDB` %in% via_sem_n_suf) ; a ; nrow(a)


# Juntar vias com categoria cdb para o plot:
slopes_list <- dplyr::left_join(slopes_list, vv)
slopes_list <- bind_rows(slopes_list, vias_missing)

palCDB_cat <- rev(c("#9359a5", "#9c83bd", "#8c97c5" , "#26828f", "#52c56a", "#badd27"))

### Plot distribuição de slopes padronizados -----
p_slopes <- slopes_list %>% 
  # filtrar NAs:
  dplyr::filter(!is.na(slope_padronizado)) %>%
  ggplot() +
  geom_histogram(aes(x = round(slope_padronizado, 2), fill = `Categoria CDB`)) +
  scale_fill_manual(values = palCDB_cat) +
  ggtitle(paste("Variação temporal de", nvias, "vias e vetores em três ambientes")) +
  xlab("Slope padronizado") +
  ylab("Contagem") +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    legend.position = "bottom",
    # legend.position = c(0.58, 0.82),
    legend.key.size = unit(0.4, 'cm'),
    legend.text = element_text(size=12),
    plot.title = element_text(size = 16)
  ) + 
  ylim(0, 14) +
  facet_wrap(~ambiente, ncol = 3) +
  guides(fill = guide_legend(nrow = 3))

p_slopes

# # Save:
# p_slopes %>%
#   ggsave(filename = here("Entregas", "A6_Variacao-temporal", "Distribuicao_slopes-ambiente.png"),
#          dpi = 300,  width = 8, height = 5, units = "in")


# # # Save:
# slopes_list %>%
#   write.csv(file = here("Entregas", "A6_Variacao-temporal", "A6_Acumulacao_vias-ambiente.csv")
#             , fileEncoding = "latin1")



# ### Plots exploratórios ----
# i <- 1
# for (cat in unique(accum$`Subcategoria CDB`)) {
#   cat_name <- cat
#   # cat_name <- accum$`Subcategoria CDB` %>% unique() %>% nth(34)
#   
#   accum_via <- accum %>%
#     dplyr::filter(`Subcategoria CDB` == cat_name) ; nrow(accum_via)
#   
#   if (nrow(accum_via) >= 10) {
#     
#     p <- accum_via %>% 
#       ggplot(aes(x= Data, y = soma_registros)) +
#       geom_line() +
#       geom_point() +
#       geom_smooth(method = 'glm', method.args=list(family="poisson")
#                   , n = nrow(accum_via)
#       )
#     # geom_smooth(stat = "smooth"
#     #             # , span = 0.5
#     #             # , n = nrow(accum_via)/2
#     #             )
#     # scale_x_date(date_labels = "%d-%m-%Y", date_breaks = '10 years') # Isso muda os dados
#     # facet_wrap(~`Subcategoria CDB`)
#     
#     # p
#     # ggplot_build(p)
#     
#     # loessdata <- ggplot_build(p)$data[[1]] # TRUE DATA
#     loessdata <- ggplot_build(p)$data[[3]] # PREDICTED
#     # loessdata.last <- loessdata[nrow(loessdata):(nrow(loessdata)-1), ] # last two lines of the stat_smooth data (predicted)
#     
#     loessdata <- loessdata %>% 
#       rename(
#         x_predict = x,
#         y_predict = y
#       )
#     
#     # Estimate slopes of the last observation
#     slope.loess <- with(loessdata,diff(y_predict)/diff(x_predict))
#     # slope.loess <- diff(loessdata.last$y) # errado (ver comentário do Bolker no stackoverflow)
#     slope.loess.last <- tail(slope.loess, 1)
#     
#     # # (with GAM loess): It has generated Infinite values. Let's drop them out (don't know what is best to do)
#     # slope.loess[is.infinite(slope.loess)] <- NA
#     
#     # ângulo a partir do slope
#     angle.loess.last <-  atan(slope.loess.last) * 180 / pi # ou: dendrometry::slope2angle(slope.loess.last)
#     
#     loessdata <- loessdata %>% 
#       dplyr::select(x_predict, y_predict) %>% 
#       mutate(
#         slope = slope.loess.last,
#         angle = angle.loess.last
#       )
#     
#     ## Add to dataframe
#     accumdf <-  bind_cols(accum_via, loessdata)
#     
#     accumdf <- accumdf %>%
#       mutate(
#         b = slope * ( - as.numeric(x_predict)) + y_predict  # https://stackoverflow.com/questions/59892472/how-to-draw-a-line-based-on-a-point-and-a-slope-sd-line
#       )
#     
#     
#     
#     ## Plot
#     yrng <- range(accumdf$soma_registros)
#     xrng <- range(accumdf$Data)
#     
#     accumdf %>% 
#       ggplot(aes(x = Data, y = soma_registros)) + 
#       geom_point(
#         # aes(size = 1.2)
#       ) +
#       geom_line(
#         # lwd = 1.2, color = "blue"
#       ) +
#       # scale_x_date(date_labels = "%Y", date_breaks = '10 years') +
#       # scale_x_date(date_labels = "%d-%m-%Y", date_breaks = '1 week') +
#       # stat_smooth(method = "lm", col = "red") +
#       stat_smooth(method = 'glm', method.args=list(family="poisson")
#                   # , n = nrow(accum_via)
#                   , fullrange = TRUE
#                   , alpha = 0.7) +#, color = "black", alpha = 0.7) +
#       geom_abline(
#         data = accumdf %>% dplyr::filter(soma_registros == max(soma_registros, na.rm = T))
#         , aes(intercept = b, slope = slope, color = slope),  lwd = 1.2  # slope
#         # , aes(intercept = b, slope = slope, color = angle),  lwd = 1.2  # angulo
#       ) +
#       # scale_color_gradient(limits = c(0, 1)
#       #                      , low = "grey50", high = "red"
#       # ) +
#       # SLOPE COLOR GRADIENT:
#       scale_color_gradient2(limits = c(0, max(slopes_list$slope, na.rm=TRUE)) # slope
#                             # scale_color_gradient2(limits = c(0, 90)                               # angulo
#                             # limits = c(min(slope.loess, na.rm=TRUE), max(slope.loess, na.rm=TRUE))
#                             # limits = c(0, 1)
#                             , low = "#488f31"
#                             , mid = "#ffeb8f"
#                             # , mid = "#fbd163"
#                             , high = "#f51616"#,
#                             # , midpoint = 0.5
#                             , midpoint = quantile(slope.loess, probs = 0.5, na.rm = TRUE)
#       ) +
#       # # ANGLE COLOR GRADIENT:
#       # scale_color_gradient2(limits = c(0, 90)
#       #                       # limits = c(min(slope.loess, na.rm=TRUE), max(slope.loess, na.rm=TRUE))
#       #                       # limits = c(0, 1)
#       #                       , low = "blue"
#       #                       , mid = "yellow"
#       #                       , high = "red"#,
#       #                       # , midpoint = 0.5
#       #                       , midpoint = 45
#       # ) +
#       geom_text(aes(x = xrng[1],
#                     y = yrng[2],
#                     label = paste0(
#                       "slope = ", round(slope.loess.last, 4)
#                       # , "\nângulo = ", round(angle.loess.last, 2)
#                     )
#                     , hjust = 0
#                     , vjust = 1
#       )
#       , size = 5
#       ) +
#       ylab("Acumulação de registros") +
#       ggtitle(cat_name)
#     
#     # ggsave(filename = here("Entregas", "A6_Variacao-temporal", "Plots_acumulacao"
#     #                        , paste0("Acumulacao_vias_", i, "-", cat_name, "_slope.png")),
#     #        dpi = 300,  width = 7, height = 5, units = "in")
#     
#   }
#   
#   i <- i + 1
# }




