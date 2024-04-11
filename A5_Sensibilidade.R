# Script name: A5_Sensibilidade
# Script purpose: Realizar análises relacionadas a quantificação de sensibilidade
# de vias e vetores (Produto 1: Atividade 4.5 e produtos seguintes)

# Date created:
# 17/08/2023
# Last edit:
# 10/04/2024

# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("tidyverse")
library("readxl")
library("scales")
library("cowplot")
library("sf")
library("mapview")
library("terra")
library("scales")
library("writexl")
library("geobr")

path <- "D:/Data/Documentos/Consultoria/SelecaoNatural/Dados_Espaciais/"


## Options -------------------------
# (plotting, memory limit, decimal digits)
# theme_set(theme_bw(base_size = 20))
custom_theme <- theme_set(theme_bw(base_size = 20)) +
  theme(
    title = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    # axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 18)
    )
# theme_update(title = element_text(size = 14),
#              )

br <- geobr::read_country() %>%
  sf::st_transform(crs = 4326) %>% 
  rename(geometry = geom)





# Exemplo de valores de sensibilidade -----

idx.sens <- expand.grid(
  HFI = seq(0, 50, length.out = 15),
  HMI = seq(0, 15, length.out = 15),
  UC = seq(1, 8, by = 1),
  APri = seq(1, 4, by = 1)
) %>% 
  mutate(
    Marinho = HMI * UC * APri,
    Terrestre = HFI * UC * APri
  ) %>% 
  pivot_longer(
    cols = c("Marinho", "Terrestre"),
    names_to = "Ambiente",
    values_to = "Sensibilidade"
  )
  
# Sem padronizar HFI e HMI (um varia de 0 a 50 e outro 0 a 15)
p1 <- idx.sens %>% 
  ggplot() +
  custom_theme +
  geom_histogram(aes(x = Sensibilidade, fill = Ambiente), alpha = 0.5
                 , position = "dodge2", bins = 20# ,binwidth = 20
                 ) +
  scale_fill_manual(values = c("darkblue", "#D7C350")) +
  ggtitle("Sem padronizar HFI e HMI, Sensibilidade não padronizada") +
  ylab("Frequência") +
  xlab("") +
  theme(legend.position = "none")

# Sensibilidade sem escalar HFI e HMI e padronizado pra 0.1 e 1
idx.sens <- idx.sens %>% 
  mutate(
    Sensibilidade = scales::rescale(Sensibilidade, to=c(0.01, 1))
  )

idx.sens$Sensibilidade %>% summary()

p2 <- idx.sens %>% 
  ggplot() +
  custom_theme +
  geom_histogram(aes(x = Sensibilidade, fill = Ambiente), alpha = 0.5
                 , position = "dodge2", bins = 20
  ) +
  scale_fill_manual(values = c("darkblue", "#D7C350")) +
  ggtitle("Sem padronizar HFI e HMI, Sensibilidae de 0.01 a 1") +
  ylab("Frequência") +
  xlab("") +
  theme(legend.position = "none")
# p2




# Padronizando HFI e HMI
# scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
scale2 <- function(x, na.rm = FALSE) { (x - min(x, na.rm = FALSE)) / (max(x, na.rm = FALSE) - min(x, na.rm = FALSE))}

idx.sens.scaled <- expand.grid(
  HFI = seq(0, 50, length.out = 15),
  HMI = seq(0, 15, length.out = 15),
  UC = seq(1, 8, by = 1),
  APri = seq(1, 4, by = 1)
) %>% 
  mutate(
    HFI_scaled = scales::rescale(HFI, to=c(0.01, 1)),
    HMI_scaled = scales::rescale(HMI, to=c(0.01, 1))
  ) %>% 
  mutate(
    Marinho = HMI_scaled * UC * APri,
    Terrestre = HFI_scaled * UC * APri
  ) %>% 
  pivot_longer(
    cols = c("Marinho", "Terrestre"),
    names_to = "Ambiente",
    values_to = "Sensibilidade"
  )

idx.sens.scaled$Sensibilidade %>% summary()

p3 <- idx.sens.scaled %>% 
  ggplot() +
  custom_theme +
  geom_histogram(aes(x = Sensibilidade, fill = Ambiente), alpha = 0.5
                 , position = "dodge2", bins = 20
                 ) +
  scale_fill_manual(values = c("darkblue", "#D7C350")) +
  ggtitle("HFI e HMI padronizado, Sensibilidade não padronizada") +
  ylab("Frequência") +
  theme(legend.position = "none")


# Sensibilidade final padronizado pra 0.1 e 1
idx.sens.scaled <- idx.sens.scaled %>% 
  mutate(
    Sensibilidade = scales::rescale(Sensibilidade, to=c(0.1, 1))
  )

idx.sens.scaled$Sensibilidade %>% summary()

p4 <- idx.sens.scaled %>% 
  ggplot() +
  custom_theme +
  geom_histogram(aes(x = Sensibilidade, fill = Ambiente), alpha = 0.5
                 , position = "dodge2", bins = 20
                 ) +
  scale_fill_manual(values = c("darkblue", "#D7C350")) +
  ggtitle("HFI e HMI padronizado, Sensibilidade de 0.01 a 1") +
  ylab("Frequência") +
  theme(legend.position = c(0.85, 0.85))
# p4

plot_grid(p1, p2, p3, p4, labels = "AUTO")

# # Save
plot_grid(p1, p2, p3, p4, labels = "AUTO", label_size = 20) %>% 
  ggsave(filename = here("Entregas", "A5_Sensibilidade", "Sensibilidade_score_padronizacao.png"),
         dpi = 600,  width = 15, height = 10)




# Layers ------


## Layer 2a: UCs BR ----

# (Ver prints na pasta A5_Sensibilidade)
# Durante a consulta pública foi comentado que os shapes do Protected Planet não 
# possuíam todas as UCs do Brasil. Assim, utilizei também da database da USP de 
# UCs do Brasil enviado pelo João (link:  https://jornal.usp.br/universidade/base-de-dados-apresenta-mapas-de-todas-as-unidades-de-conservacao-do-brasil/)
# Também identifiquei problemas de polígonos na database do Protected Planet:
# por exemplo, a RESEX Guariba-Roosevelt (1996) não está no Protected Planet (amarelo), 
# mas está no da USP (vermelho). No entanto, o polígono é diferente do relatado nesse 
# site aqui: https://uc.socioambiental.org/pt-br/arp/1141


### Protected Planet ----
shp_pp <- sf::st_read(paste0(path, "ProtectedAreas_BRA_shp/"
                             , "WDPA_WDOECM_Sep2023_Public_BRA_shp_0/"
                             , "WDPA_WDOECM_Sep2023_Public_BRA_shp-polygons.shp")) #%>% 
# Checar dados:
shp_pp$IUCN_CAT %>% unique() # Categorias IUCN

table(shp_pp$WDPAID == shp_pp$WDPA_PID) # Mesma informação?

sf::st_crs(shp_pp)

shp_pp <- shp_pp %>% 
  mutate(
    IUCN_CAT = case_when(
      IUCN_CAT == "Not Reported" ~ NA_character_,
      IUCN_CAT == "Not Applicable" ~ NA_character_,
      TRUE ~ IUCN_CAT
    )
  )


### UCs da USP -----
shp_br <- sf::st_read(paste0(path, "UCs_Brasil_USP/"
                             , "UNC_21_BR_CEM_V2.shp")
                      , options = "ENCODING=WINDOWS-1252")
# Checar dados:
sf::st_crs(shp_br)


# Limpar dados: 
shp_pp <- shp_pp %>% 
  # dplyr::select(WDPAID, NAME, DESIG, IUCN_CAT, geometry) %>% 
  dplyr::select(WDPAID, NAME, DESIG, geometry) %>% 
  rename(
    id = WDPAID,
    nome = NAME,
    categoria = DESIG#,
    # categoria_IUCN = IUCN_CAT
  ) %>% 
  mutate(
    origem = "Protected Planet"
  )

shp_br <- shp_br %>% 
  # dplyr::select(ID, NOM_UC_A, CATEG, IUCN_CAT, geometry) %>% 
  dplyr::select(ID, NOM_UC_A, CATEG, geometry) %>% 
  rename(
    id = ID,
    nome = NOM_UC_A,
    categoria = CATEG
  ) %>% 
  mutate(
    origem = "USP"
  )



# Intersect
sf_use_s2(FALSE)
st_make_valid(shp_pp)
st_make_valid(shp_br)

# Não funcionou com o st por alguma razão:
# exclusivas_br <- sf::st_intersects(shp_br, shp_pp, sparse=FALSE)
# exclusivas_br
# # exclusivas_br <- st_intersection(shp_br, shp_pp, sparse=FALSE)
# exclusivas_br <- shp_pp %>% 
#   # dplyr::filter(lengths(st_intersects(., shp_br, sparse = FALSE)) > 0)
#   dplyr::filter(st_intersects(., shp_br, sparse = FALSE))

exclusivas_br <- lengths(st_intersects(shp_br, shp_pp)) > 0
exclusivas_br <- shp_br[!exclusivas_br, ]


# Join:
# shp_ucs <- st_join(shp_pp, exclusivas_br, left = FALSE)  # full join, not left join
# shp_ucs <- dplyr::full_join(shp_pp, exclusivas_br)  # trying dplyr instead of sf
shp_ucs <- dplyr::bind_rows(shp_pp, exclusivas_br)  # dur, I needed a bind_rows()

st_make_valid(shp_ucs)


# Transformar para Categorias IUCN:
shp_pp$categoria %>% unique()
shp_br$categoria %>% unique()
shp_ucs$categoria %>% unique() 

shp_pp[str_detect(shp_pp$categoria, "Biológica"), ]


shp_ucs <- shp_ucs %>%
  # Filtrar categorias que não fazem sentido pro Brasil
  dplyr::filter(
    categoria != "World Heritage Site (natural or mixed)" &
      categoria != "Ramsar Site, Wetland of International Importance"
  ) %>% 
  mutate(
    categoria_IUCN = case_when(
      
      # Dataset USP:
      categoria == "PARNA" ~ "II",
      categoria == "APA" ~ "V",
      categoria == "FLOES" ~ "VI", # = Floresta Nacional ou Municipal
      categoria == "FLONA" ~ "VI",
      categoria == "REBIO" ~ "Ia",
      categoria == "RESEX" ~ "VI",
      categoria == "ESEC" ~ "Ia",
      categoria == "PARES" ~ "II", # ~ Parque Nacional
      categoria == "RDS" ~ "VI",
      categoria == "REVIS" ~ "III",
      categoria == "ARIE" ~ "IV",
      categoria == "PNM" ~ "II",    # ~ Parque Nacional
      categoria == "RPPN" ~ "IV",
      categoria == "MONA" ~ "III",
      
      # Dataset Protected Planet:
      categoria == "Reserva Biológica" ~ "Ia", #
      categoria == "Estação Ecológica" ~ "Ia", #
      categoria == "Parque" ~ "II", # Nacional, Estadual ou Municipal
      categoria == "Área de Proteção Ambiental" ~ "V",
      categoria == "Área de Relevante Interesse Ecológico" ~ "IV",
      categoria == "Floresta" ~ "IV", # Nacional, Estadual ou Municipal
      categoria == "Refugio de Vida Silvestre" ~ "III",
      categoria == "Reserva Particular do Patrimônio Natural" ~ "IV",
      categoria == "Reserva de Desenvolvimento Sustentável" ~ "VI",
      categoria == "Reserva Extrativista" ~ "VI",
      categoria == "Monumento Natural" ~ "III",
      
      # Terra Indígena = categoria VI IUCN
      categoria == "Terra Indígena" ~ "VI",
      categoria == "Reserva Indígena" ~ "VI",
      is.na(categoria) ~ NA_character_
    )
  ) 

shp_ucs$categoria_IUCN %>% unique() 

# Check NAs:
a <- shp_ucs %>% dplyr::filter(is.na(categoria_IUCN))


### Pontuação Layer 2a ----

# De acordo com os valores sugeridos em 'Sensibilidade_exemplo.xlsx'
# Não tem categoria 1b no Brasil

shp_ucs <- shp_ucs %>%
  mutate(
    value2a = case_when(
      categoria_IUCN == "Ia" ~ 7,
      categoria_IUCN == "II" ~ 6,
      categoria_IUCN == "III" ~ 5,
      categoria_IUCN == "IV" ~ 4,
      categoria_IUCN == "V" ~ 3,
      categoria_IUCN == "VI" ~ 2
    )
  )


# Visualizar exclusivas do br:
# st_make_valid(exclusivas_br)
# st_is_valid(exclusivas_br)
# m <- mapview(shp_ucs, zcol = "categoria_IUCN", alpha.regions = 0.5) +
m <- mapview(shp_ucs, zcol = "categoria_IUCN") +
  mapview(exclusivas_br, col.regions = "red")
m

# # Save
# mapshot(m, url="Entregas/A5_Sensibilidade/Layer2/Layer2a_UCs-BR-fora-da-Protected-Planet.html")



# Save final shp with all UCs considered:
shp_ucs %>% 
  # sf::st_write(dsn = "Entregas/A5_Sensibilidade/Layer2/UCs-BR.shp"
  sf::st_write(dsn = here("Entregas", "A5_Sensibilidade", "Layer2", "Layer2a.shp")
               , append = FALSE # to overwrite
  )


# Save as csv
shp_ucs %>% 
  sf::st_drop_geometry() %>% 
  write.csv(file = here("Entregas", "A5_Sensibilidade", "Layer2", "Layer2a.csv")
            , fileEncoding = "latin1")




## Layer 2b: Areas prioritarias ------

layer2b_files <- list.files(paste0(path, "Areas-Prioritarias-Brasil"), ".shp$"
                            , full.names = TRUE)

layer2b_Amaz <- layer2b_files[1] %>% st_read() %>% 
  dplyr::select(COD_area, Import_bio, Prior_acao, geometry) %>% 
  mutate(
    COD_area = as.character(COD_area),
    bioma = "Amazônia"
    )

layer2b_Caat <- layer2b_files[2] %>% st_read() %>% 
  dplyr::select(COD_area, Import_bio, Prior_acao, geometry) %>% 
  mutate(
    COD_area = as.character(COD_area),
    bioma = "Caatinga"
    )

layer2b_CerrPant <- layer2b_files[3] %>% st_read() %>% 
  dplyr::select(COD_area, Import_bio, Prior_acao, geometry) %>% 
  mutate(
    COD_area = as.character(COD_area),
    bioma = "Cerrado_Pantanal"
  )

layer2b_MaAt <- layer2b_files[4] %>% st_read() %>% 
  rename(
    Import_bio = ImportBio_,
    Prior_acao = Prioridade
  ) %>% 
  dplyr::select(COD_area, Import_bio, Prior_acao, geometry) %>% 
  mutate(
    COD_area = as.character(COD_area),
    bioma = "Mata Atlântica"
  )

layer2b_Pamp <- layer2b_files[5] %>% st_read() %>% 
  dplyr::select(COD_area, Import_bio, Prior_acao, geometry) %>% 
  mutate(
    COD_area = as.character(COD_area),
    bioma = "Pampas"
  )

layer2b_Mar <- layer2b_files[6] %>% st_read() %>% 
  rename(
    Import_bio = IMP,
    Prior_acao = PRIO,
    COD_area = ID_AP
  ) %>% 
  dplyr::select(COD_area, Import_bio, Prior_acao, geometry) %>% 
  mutate(
    COD_area = as.character(COD_area),
    bioma = "Marinho"
  )

# Area prioritária sem classificação: https://repositorio.ufsc.br/bitstream/handle/123456789/231093/PPCA0057-D.pdf?sequence=-1&isAllowed=y
layer2b_Mar %>% dplyr::filter(is.na(Prior_acao)) 


# Bind :
layer2b_shps <- bind_rows(layer2b_Amaz, layer2b_Caat, layer2b_CerrPant,
                          layer2b_MaAt, layer2b_Pamp, layer2b_Mar)
rm(layer2b_Amaz, layer2b_Caat, layer2b_CerrPant,
   layer2b_MaAt, layer2b_Pamp, layer2b_Mar) ; gc()

# set crs:
st_crs(layer2b_shps)
layer2b_shps <- st_transform(layer2b_shps, crs = 4326)

# Drop Z dimension
layer2b_shps <- st_zm(layer2b_shps)


# Checar levels:
layer2b_shps$Import_bio %>% unique()
layer2b_shps$Prior_acao %>% unique()

layer2b_shps <- layer2b_shps %>% 
  mutate(
    Import_bio = case_when(
      Import_bio == "Muita Alta" ~ "Muito Alta",
      TRUE ~ as.character(Import_bio)
    )
  )

# check NA:
a <- layer2b_shps %>% dplyr::filter(is.na(Prior_acao))


### Pontuação Layer 2b -----
layer2b_shps <- layer2b_shps %>% 
  mutate(
    value2b = case_when(
      Import_bio == "Extremamente Alta" ~ 7,         
      Import_bio == "Muito Alta" ~ 6,         
      Import_bio == "Alta" ~ 5,         
      Import_bio == "Insuficientemente conhecida" ~ 4         
    )
  )

# layer2b_shps$value2b %>% unique()


# Visualizar Layer2b:

# m <- mapview(shp_ucs, zcol = "Import_bio", alpha.regions = 0.5) +
m <- mapview(layer2b_shps, zcol = "Import_bio")
m

# # Save
# mapshot(m, url="Entregas/A5_Sensibilidade/Layer2/Layer2b_ImportanciaBiologica.html")


# Save final shp:
layer2b_shps %>% 
  sf::st_write(dsn = here("Entregas", "A5_Sensibilidade", "Layer2", "Layer2b.shp")
               , append = TRUE # to overwrite
  )

# Save as csv
layer2b_shps %>% 
  sf::st_drop_geometry() %>% 
  write.csv(file = here("Entregas", "A5_Sensibilidade", "Layer2", "Layer2b.csv")
            , fileEncoding = "latin1")




# Consolidar Layer Sensibilidade final -----


## Layer 1: HMI e HFI -----

### FMI ----
rast_HFI <- terra::rast(x = paste0(path
                             , "HFI_Williams_etal_2019/"
                             , "Human_footprint_maps/"
                             , "hfp2013_merisINT.tif")
                        # , crs = "+proj=longlat +datum=WGS84" #crs = 4362
                       )
rast_HFI
plot(rast_HFI)

terra::crs(rast_HFI)
terra::ext(rast_HFI)

rast_HFI <- terra::project(rast_HFI, "epsg:4326")

plot(rast_HFI)

# Crop extent: 
terra::ext(rast_HFI)

# Adicionar ucs marinhas
br_ext <- layer2b_shps %>% 
  dplyr::filter(bioma == "Marinho") %>% 
  bind_rows(br)

ggplot() + geom_sf(data = br_ext)

# br_ext <- sf::st_buffer(br_ext, 50) # add 50 km buffer # only works in UTM

e <- terra::ext(br_ext)

# add extent of sea:
# e[1, ] #xmim
# e[2, ] <- -25 #xmax
# e[3, ] #ymim
# e[4, ] <- 0#ymax

e <- terra::extend(e, 2)

e

# crop
rast_HFI_la <- terra::crop(rast_HFI, e)
terra::ext(rast_HFI_la)
plot(rast_HFI_la)

# Save
terra::writeRaster(rast_HFI_la, filename = here("Entregas"
                                                , "A5_Sensibilidade"
                                                , "Layer1"
                                                , "Layer1_HFI.tif")
                   , gdal="COMPRESS=NONE"
                   , overwrite = TRUE
)


### HMI ----
rast_HMI <- terra::rast(x = paste0(path
                                   , "HMI/"
                                   , "2013_pressure_and_cumulative/"
                                   , "data/"
                                   , "global_cumul_impact_2013_all_layers.tif")
                        # , crs = "+proj=longlat +datum=WGS84" #crs = 4362
)
rast_HMI
# plot(rast_HMI)

terra::crs(rast_HMI)
terra::ext(rast_HMI)

rast_HMI <- terra::project(rast_HMI, "epsg:4326")

plot(rast_HMI)

# Crop extent: 
terra::ext(rast_HMI)

library("geobr")
br <- geobr::read_country() %>%
  sf::st_transform(crs = 4326) %>% 
  rename(geometry = geom)
# Adicionar ucs marinhas
br_ext <- layer2b_shps %>% 
  dplyr::filter(bioma == "Marinho") %>% 
  bind_rows(br)

ggplot() + geom_sf(data = br_ext)


e <- terra::ext(br_ext)
e <- terra::extend(e, 2)
e

# crop
rast_HMI_la <- terra::crop(rast_HMI, e)
terra::ext(rast_HMI_la)
plot(rast_HMI_la)

# Save
terra::writeRaster(rast_HMI_la, filename = here("Entregas"
                                                , "A5_Sensibilidade"
                                                , "Layer1"
                                                , "Layer1_HMI.tif")
                   , gdal="COMPRESS=NONE"
                   , overwrite = TRUE
)




# Carregar e Padronizar valores das layers ------
range02 <- function(x){(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

vec <- 1:15
range01(vec)
range02(vec)


## Layer 1: -----

### Marinho
rast_HMI <- terra::rast(x = here("Entregas", "A5_Sensibilidade"
                                       , "Layer1" , "Layer1_HMI.tif"))

rast_HMI
rast_HMI[1000:2000]
table(rast_HMI[10000:20000] > 0)
names(rast_HMI)

plot(rast_HMI)
# library("tmap")
# tmap::tm_shape(rast_HMI) +
#   tm_raster()

rast_HMI$global_cumul_impact_2013_all_layers %>% summary()
minmax <- terra::minmax(rast_HMI$global_cumul_impact_2013_all_layers) # https://gis.stackexchange.com/questions/437520/normalize-raster-in-r

HMI_scaled <- terra::app(rast_HMI, fun = range02)
plot(HMI_scaled)

# scales package does not work:
# HMI_scaled <- range02(rast_HMI, 2, 5)
# HMI_scaled <- terra::app(rast_HMI, fun = scales::rescale(rast_HMI, to = c(0.01, 1)))
# scales::rescale(rast_HMI, to = c(0.01, 1))

# # Save
# terra::writeRaster(HMI_scaled, filename = here("Entregas"
#                                                 , "A5_Sensibilidade"
#                                                 , "Layer1"
#                                                 , "Layer1_HMI_scaled.tif")
#                    , gdal="COMPRESS=NONE"
#                    , overwrite = TRUE
# )


### Terrestre
rast_HFI <- terra::rast(x = here("Entregas", "A5_Sensibilidade"
                                 , "Layer1" , "Layer1_HFI.tif"))

rast_HFI
rast_HFI[1000:2000]
table(rast_HFI[10000:20000] > 0)
names(rast_HFI)

plot(rast_HFI)

HFI_scaled <- terra::app(rast_HFI, fun = range02)
plot(HFI_scaled)

# # Save
# terra::writeRaster(HFI_scaled, filename = here("Entregas"
#                                                , "A5_Sensibilidade"
#                                                , "Layer1"
#                                                , "Layer1_HFI_scaled.tif")
#                    , gdal="COMPRESS=NONE"
#                    , overwrite = TRUE
# )


## Layer 2a: -----
layer2a <- sf::st_read(dsn = here("Entregas", "A5_Sensibilidade", "Layer2", "Layer2a.shp"))

### Rasterize:
layer2a_rast <- terra::rasterize(x = layer2a, y = HFI_scaled, field = "value2a")
plot(layer2a_rast)

### Add values = 1 (none)
# layer2a_rast[1:1000]
layer2a_rast <- terra::subst(layer2a_rast, NA, 1)

# png(file = here("Entregas", "A5_Sensibilidade", "Layer2", "Layer2a.png")
#     , width = 750, height = 750
#     , bg = "white")
plot(layer2a_rast, main = "Layer 2a - UCs")
# dev.off()


## Layer 2b: -----
layer2b <- sf::st_read(dsn = here("Entregas", "A5_Sensibilidade", "Layer2", "Layer2b.shp"))

### Rasterize:
layer2b_rast <- terra::rasterize(x = layer2b, y = HFI_scaled, field = "value2b")
plot(layer2b_rast)

### Add values = 1 (none)
# layer2a_rast[1:1000]
layer2b_rast <- terra::subst(layer2b_rast, NA, 1)

# png(file = here("Entregas", "A5_Sensibilidade", "Layer2", "Layer2b.png")
#     , width = 750, height = 750
#     , bg = "white")
plot(layer2b_rast, main = "Layer 2b - Áreas prioritárias")
# dev.off()





# Criar layer final ----


### Layer 1 ----
HMI_scaled <- terra::resample(x = HMI_scaled, y = HFI_scaled, method = "bilinear")
is.nan(HMI_scaled)
is.na(HMI_scaled)
crs(HMI_scaled) == crs(HFI_scaled)

plot(HMI_scaled)

# HFI_scaled <- terra::crop(HFI_scaled, HMI_scaled, mask = TRUE)
# HMI_scaled <- terra::crop(HMI_scaled, HFI_scaled, mask = TRUE)
# plot(HMI_scaled)

# Criar outro raster com informação do bioma:
HMI_scaled_b <- HMI_scaled
HMI_scaled_b <- app(HMI_scaled_b, function(x) { x[x < 0.01] <- NA; x }) # 1 = Marinho
HMI_scaled_b <- app(HMI_scaled_b, function(x) { x[x >= 0.01] <- 1; x }) # 1 = Marinho
# HMI_scaled_b[HMI_scaled_b >= 0] <- 1 # 1 = Marinho #pq nao funciona?
plot(HMI_scaled_b)
# HMI_scaled_b$lyr.1 %>% summary()
# HMI_scaled_b[is.na(HMI_scaled_b)]


HFI_scaled_b <- HFI_scaled
HFI_scaled_b <- app(HFI_scaled_b, function(x) { x[x > 0.01] <- 2; x }) # 2 = Terrestre
# values(HFI_scaled_b) <- 2 # 2 = Terrestre # pq n funciona?
plot(HFI_scaled_b)

HFI_scaled <- c(HFI_scaled, HFI_scaled_b)
names(HFI_scaled) <- c("lyr.1", "bioma")

HMI_scaled <- c(HMI_scaled, HMI_scaled_b)
names(HMI_scaled) <- c("lyr.1", "bioma")



# Merge:
layer1 <- terra::merge(HFI_scaled, HMI_scaled)
layer1


# png(file = here("Entregas", "A5_Sensibilidade", "Layer1", "Layer1.png")
#     , width = 750, height = 750
#     # , bg = "transparent")
#     , bg = "white")
plot(layer1[[1]], main = "Layer 1 - Ecossistemas Intactos")
# dev.off()

# # Save
# terra::writeRaster(layer1[[1]], filename = here("Entregas"
#                                                , "A5_Sensibilidade"
#                                                , "Layer1"
#                                                , "Layer1.tif")
#                    , gdal="COMPRESS=NONE"
#                    , overwrite = TRUE
# )

tm_shape(layer1[[1]]) +
  tm_raster()



# Layer 2
# layer2 <- layer2a_rast * layer2b_rast # Gera uns valores zuados
# layer2 <- terra::merge(layer2a_rast, layer2b_rast, fun = sum) # merge does not work
layer2 <- terra::mosaic(layer2a_rast, layer2b_rast, fun="max")# Maior valor, nao a multiplicação

# png(file = here("Entregas", "A5_Sensibilidade", "Layer2", "Layer2_unscaled.png")
#     , width = 750, height = 750
#     , bg = "white")
plot(layer2, main = "Layer 2 - Áreas Mantenedoras de Espécies")
# dev.off()

# # Scale from 0.01 to 1
# layer2 <- terra::app(layer2, fun = range02)
# 
# # png(file = here("Entregas", "A5_Sensibilidade", "Layer1", "Layer2.png")
# #     , width = 750, height = 750
# #     , bg = "white")
# plot(layer2, main = "Layer 2 - Áreas Mantenedoras de Espécies")
# # dev.off()


# Layer final
layer_sens <- layer1[[1]] * layer2
layer_sens <- terra::app(layer_sens, fun = range02)


# png(file = here("Entregas", "A5_Sensibilidade", "Layer_Sensibilidade.png")
#     , width = 1000, height = 1000
#     , bg = "white")
plot(layer_sens, main = "Layer Sensibilidade", res = 600)
plot(br, add = TRUE)
# dev.off()
# # sf_use_s2(FALSE)
# # # a <- br_ext %>% dplyr::filter(bioma == "Marinho") %>% sf::st_union()
# # # lines(a, add = TRUE, col = "black")


# set.seed(2)
# hist(log10(layer_sens), maxcell = Inf, main = "log10(Sensibilidade)")
# hist(log(layer_sens), maxcell = Inf, main = "log(Sensibilidade)")
# # hist(sqrt(layer_sens), maxcell = Inf)
hist(layer_sens, maxcell = Inf)
hist(layer_sens[])

table(as.vector(layer_sens$lyr.1) > 0.25)
table(as.vector(layer_sens$lyr.1) > 0.5)
table(as.vector(layer_sens$lyr.1) > 0.6)

sens_summary <- data.frame(
  dados = "todas as células",
  media = mean(as.vector(layer_sens$lyr.1), na.rm=TRUE),
  sd = sd(as.vector(layer_sens$lyr.1), na.rm=TRUE),
  q10 = quantile(as.vector(layer_sens$lyr.1), 0.10, na.rm=TRUE),
  q25 = quantile(as.vector(layer_sens$lyr.1), 0.25, na.rm=TRUE),
  q50 = quantile(as.vector(layer_sens$lyr.1), 0.50, na.rm=TRUE),
  q75 = quantile(as.vector(layer_sens$lyr.1), 0.75, na.rm=TRUE),
  q90 = quantile(as.vector(layer_sens$lyr.1), 0.90, na.rm=TRUE)
) 

rownames(sens_summary) <- 1

# # Save xlsx com valores de sensibilidade pra janela (America Latina)
# sens_summary %>% 
#   writexl::write_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_summary.xlsx"))






# Extrair dados de sensibilidade para pontos de ocorrência ------
db_all <- read_xlsx(here("Entregas", "Arquivos", "Dados", "Occorrencias-Vias-vetores.xlsx")) %>% 
  # sf::st_as_sf(coords = c("decimalLatitude", "decimalLongitude"), crs = 4326) %>% 
  terra::vect(geom = c("decimalLongitude", "decimalLatitude"), crs = "epsg:4326") 

db_all



# png(file = here("Entregas", "A5_Sensibilidade", "Layer_Sensibilidade_ocorrencias.png")
#     , width = 1000, height = 1000
#     , bg = "white")
plot(layer_sens, main = "Layer Sensibilidade", res = 600)
plot(br, add = TRUE)
points(db_all, col= "red", cex = 0.05)
# dev.off()

# tm_shape(layer_sens) +
#   tm_raster("lyr.1") #+
#   # tm_shape(db_all) +
#   # tm_dots()


# Extrair valor de sensibilidade dos pontos com extract:

# xy <- spatSample(layer_sens, 2000, "random", na.rm=TRUE, xy=TRUE)
# points(xy, pch=20, col="red", cex=2)

sens_val <- terra::extract(layer_sens, db_all, na.rm=TRUE, bind = TRUE) ## sensibilidade
sens_bio <- terra::extract(layer1[[2]], db_all, na.rm=TRUE, bind = TRUE) %>% ## bioma
  terra::as.data.frame(geom = "XY") %>% 
  dplyr::select("bioma")
# plot(sens_val)

sens_db <- sens_val %>% 
  terra::as.data.frame(geom = "XY") %>%  # keep x and y coordinates
  rename(sensibilidade = lyr.1) %>% 
  bind_cols(sens_bio) %>% 
  mutate(
    bioma = case_when(
      bioma == 2 ~ "Terrestre",
      bioma == 1 ~ "Marinho"
    )
  )

sens_db$bioma %>% unique()
sens_db$bioma %>% table()
sum(sens_db$bioma %>% is.na() == TRUE) # 3472 NAs...



sens1 <- sens_db %>% 
  ggplot() +
  geom_histogram(aes(x = sensibilidade)) +
  labs(title = "Sensibilidade dos pontos de ocorrência"
     , x = "Sensibilidade"
     , y = "Frequência"
     )
sens1


# Save plot
sens1 %>% 
  ggsave(filename = here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências.png"),
         dpi = 600,  width = 7, height = 5)


sens2 <- sens_db %>% 
  ggplot() +
  geom_density(aes(x = sensibilidade)) +
  labs(title = "Sensibilidade dos pontos de ocorrência"
       , x = "Sensibilidade"
       , y = "Frequência"
  )

sens2


# Save plot
sens2 %>% 
  ggsave(filename = here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências_density.png"),
         dpi = 600,  width = 7, height = 5)


# Gerar summary dos valores para as ocorrencias e comparar com todo o dataset
sens_summary

sens_summary_occ <- sens_db %>% 
  summarise(
    dados = "ocorrências",
    media = mean(sensibilidade, na.rm=TRUE),
    sd = sd(sensibilidade, na.rm=TRUE),
    q10 = quantile(sensibilidade, 0.10, na.rm=TRUE),
    q25 = quantile(sensibilidade, 0.25, na.rm=TRUE),
    q50 = quantile(sensibilidade, 0.50, na.rm=TRUE),
    q75 = quantile(sensibilidade, 0.75, na.rm=TRUE),
    q90 = quantile(sensibilidade, 0.90, na.rm=TRUE)
  ) 

sens_summary <- bind_rows(sens_summary, sens_summary_occ)


# Save xlsx com valores de sensibilidade pra janela (America Latina) e pras ocorrências
sens_summary %>% 
  writexl::write_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_summary.xlsx"))


sens_cells <- as.data.frame(layer_sens$lyr.1) %>%
  mutate(origem = "todas as células") %>% 
  rename(sensibilidade = lyr.1)

sens_all <- sens_db %>%
  dplyr::select(sensibilidade) %>% 
  mutate(origem = "ocorrências") %>% 
  bind_rows(sens_cells)


## Teste de normalidade dos resíduos
sens_all_sample <- sens_all %>% slice_sample(n = 20000)

lm <- lm(sensibilidade ~ origem, data = sens_all_sample)
car::qqPlot(lm)
# sjPlot::plot_residuals(lm)


## Plot
sens3 <- sens_all %>% 
  ggplot() +
  geom_density(aes(x = sensibilidade, fill = origem), alpha = 0.5) +
  labs(title = "Valores de sensibilidade"
       , x = "Sensibilidade"
       , y = "Frequência"
  ) +
  # theme(legend.position = "bottom")
  theme(legend.position = c(0.8, 0.8))
  # scale_color_manual(values = c())
sens3


# Save plot
sens3 %>% 
  ggsave(filename = here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências_density_comparacao.png"),
         dpi = 600,  width = 7, height = 5)


# Diferenças por bioma (marinho, terrestre)
sens_db$bioma %>% unique()

sens4 <- sens_db %>% 
  dplyr::filter(bioma == "Marinho" | bioma == "Terrestre") %>% 
  ggplot() +
  geom_density(aes(x = sensibilidade, fill = bioma), alpha = 0.7) +
  labs(title = "Sensibilidade dos pontos de ocorrência"
       , x = "Sensibilidade"
       , y = "Frequência"
  ) +
  # theme(legend.position = "bottom")
  theme(legend.position = c(0.8, 0.8)) +
  scale_fill_manual(values = c("darkblue", "#D7C350"))

sens4


# Save plot
sens4 %>% 
  ggsave(filename = here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências_density_comparacao.png"),
         dpi = 600,  width = 7, height = 5)






# # Save xlsx com ocorrencias + valores de sensibilidade das ocorrências + origem
# sens_db %>% 
#   terra::as.data.frame() %>%
#   writexl::write_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências.xlsx"))



# Summary por via -----
sens_db <- readxl::read_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências.xlsx")) %>% 
  group_by(`Nome científico`) %>% 
  summarise(
    sensibilidade_media = mean(sensibilidade, na.rm=TRUE),
    sensibilidade_mediana = median(sensibilidade, na.rm=TRUE),
    sensibilidade_sd = sd(sensibilidade, na.rm=TRUE),
    sensibilidade_min = min(sensibilidade, na.rm=TRUE),
    sensibilidade_max = max(sensibilidade, na.rm=TRUE),
    sensibilidade_n = n()
  )

vv <- read_xlsx(here("Entregas", "Arquivos", "Dados", "00_Vias-vetores-corrigido.xlsx"))
vv$`Subcategoria CDB` %>% unique()

db <- dplyr::left_join(sens_db, vv, relationship = "many-to-many") # many-to-many porque uma espécie pode estar em mais que uma via 


db_summary <- db %>% 
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>% 
  summarise(
    sens_media = mean(sensibilidade_mediana, na.rm=TRUE),
    sens_sd = sd(sensibilidade_mediana, na.rm=TRUE),
    sens_min = min(sensibilidade_mediana, na.rm=TRUE),
    sens_max = max(sensibilidade_mediana, na.rm=TRUE),
    sens_n = sum(sensibilidade_n)
  )

db$sensibilidade_n %>% sum() #ok
db_summary$sens_n %>% sum() #ok
  
# # Save
# db_summary %>% 
#   writexl::write_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências_summary.xlsx"))

# Qual via está faltando?



# Summary por ambiente (nao tem como fazer por situação pq é pra ausente) ----
sens <- read_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências.xlsx")) %>% 
  dplyr::select(`Nome científico`, sensibilidade_media)


vv <- read_xlsx(here("Entregas", "Arquivos", "Dados", "00_Vias-vetores-corrigido.xlsx")) %>% 
  # mudar nome de categorias com erros de digitação
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` == "Pesquisa e e criação ex-situ" ~ "Pesquisa e criação ex-situ",
      `Subcategoria CDB` == "Presença em navio e embarbação" ~ "Presença em navio e embarcação",
      TRUE ~  as.character(`Subcategoria CDB`)
    )
  )

sens_vv <- dplyr::left_join(sens, vv)

# Ambiente:
sens_vv_summary <- sens_vv %>% 
  group_by(Ambiente) %>% 
  summarise(
    sens_mean = mean(sensibilidade_media, na.rm = TRUE),
    sens_sd = sd(sensibilidade_media, na.rm = TRUE)
    # sens_median = median(sensibilidade_media, na.rm = TRUE)
    # n_points = n_distinct(`Nome científico`)
  ) %>% 
  dplyr::arrange(sens_mean)
# # Save
# sens_vv_summary %>%
#   writexl::write_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências_summary_ambiente.xlsx"))


# Ambiente + Via (n tem situação pq só tem presentes)
sens_vv_summary <- sens_vv %>% 
  group_by(Ambiente, `Subcategoria CDB`) %>% 
  summarise(
    sens_mean = mean(sensibilidade_media, na.rm = TRUE),
    sens_sd = sd(sensibilidade_media, na.rm = TRUE)
    # sens_median = median(sensibilidade_media, na.rm = TRUE)
    # n_points = n_distinct(`Nome científico`)
  ) %>% 
  dplyr::arrange(sens_mean)
# # Save
# sens_vv_summary %>%
#   writexl::write_xlsx(here("Entregas", "A5_Sensibilidade", "Sensibilidade_ocorrências_summary_via-ambiente.xlsx"))


