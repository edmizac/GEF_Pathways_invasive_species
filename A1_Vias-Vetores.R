# Script name: A1_Vias-Vetores
# Script purpose: Realizar análises relacionadas a classificação e descrição
# de vias e vetores (Produto 1: Atividade 4.1 e seguintes)

# Date created:
# 10/08/2023
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("tidyverse")
# library("readxl")
# library("webr")         # piedonut plots
# library("ggpie")        # piedonut plots
library("treemap")        # treemap plots
library("hues")           # for plotting palettes
library("geomtextpath")   # better labels
library("ggalluvial")   # alluvial plots

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

# Import and wrangle data ------
vv <- read_xlsx(here("Entregas", "Arquivos", "Dados", "Vias-vetores-corrigido.xlsx"))

vv$`Categoria CDB` %>% unique()
vv$`Subcategoria CDB` %>% unique() # 41??? nao eram 49? -> eram 41 mesmo, corrigi em 00_prep-data.R -> na vdd eram 40
vv$`Grau de confiança` %>% unique()

vv %>%
  dplyr::select(c(`Categoria CDB`, `Subcategoria CDB`)) %>%
  dplyr::distinct() %>% 
  select(`Subcategoria CDB`) %>% 
  duplicated()

# Número de categorias para derivar valores gráficos (angulo, posições, paletas de cores etc)
nsub <- vv$`Subcategoria CDB` %>% unique() %>%  length()


# Checar NAs:

vv$`Categoria CDB` %>% unique()
vv_na <- db_all %>% dplyr::filter(is.na(`Categoria CDB`))

# # Save:
# vv_na %>%
#   write.csv(file = here("Entregas", "A1_Vias-Vetores", "Vias-Vetores-NA.csv")
#             , fileEncoding = "latin1")





### Nested Piechart plot -----
# = Fig. 1 Turbelin et al. 2022

# Make each subcategory unique and add counter
vv_pie <- vv %>%
  # dplyr::select(c(`Grau de confiança`:`Grupo biológico`)) %>%
  dplyr::select(c(`Categoria CDB`, `Subcategoria CDB`)) %>%
  dplyr::distinct() %>%
  mutate(qtdd = 1) %>%
  mutate(
    sum = case_when(
      `Categoria CDB` == "Corredor" ~ 1,
      `Categoria CDB` == "Sem ajuda humana" ~ 2,
      `Categoria CDB` == "Soltura na natureza" ~ 3,
      `Categoria CDB` == "Transporte como contaminante" ~ 4,
      `Categoria CDB` == "Transporte clandestino" ~ 5,
      `Categoria CDB` == "Escape" ~ 6,
      TRUE ~ 0
    )
  ) %>%
  arrange(sum) %>% 
  # group_by(`Categoria CDB`) %>%
  # mutate(sum = n()) %>%
  # group_by(`Categoria CDB`, `Subcategoria CDB`) %>%
  # arrange(as.numeric(sum), .by_group = TRUE) #%>%
  # arrange(sum, .by_group = FALSE) %>%
  # arrange(`Subcategoria CDB`, as.numeric(sum), .by_group = TRUE) %>%
  # arrange(`Subcategoria CDB`, -sum, .by_group = TRUE) #%>%
  # arrange(`Subcategoria CDB`, .by_group = TRUE) %>%
  # arrange(desc(`Subcategoria CDB`), .by_group = FALSE) %>%
  ungroup() %>%
  # Compute the position of subcategory labels: 
  mutate(
    x = 1:nrow(.),
    x_angle =  360/(2*pi)*rev( pi/2 + seq( pi/nsub, 2*pi-pi/nsub, len=nsub)), # https://stackoverflow.com/questions/19438638/rotation-of-labels-to-follow-x-axis-in-ggplot2-polar-projection
    x_angle = case_when(
      x >= 20 ~ (x_angle - 180),
      TRUE ~ as.numeric(x_angle)
        ),
    hjust = case_when(
      x >= 20 ~ as.numeric(1),
      TRUE ~ as.numeric(0)
    )
  ) %>% 
  # Compute the position of category labels: 
  group_by(`Categoria CDB`) %>% 
  mutate(
    order_cat = cur_group_id(),
    x_cat =  mean(x)
    ) %>% 
  ungroup()


# Map palette to categories

# for categories
vv_pie <- vv_pie %>%
  group_by(`Categoria CDB`) %>%
  arrange(sum, .by_group = FALSE)
  # arrange(`Subcategoria CDB`, sum, .by_group = TRUE)
  # arrange(desc(`Subcategoria CDB`), .by_group = TRUE)
  # arrange(`Categoria CDB`)#, .by_group = TRUE)
ncolors_cat <- vv_pie$`Categoria CDB` %>% unique()
# palCDB_cat <- rev(viridis::viridis(n=ncolors_cat))
palCDB_cat <- rev(c("#450154", "#88419d", "#8c97c5" , "#26828f", "#52c56a", "#badd27"))
palCDB_cat <- rev(c("#622b6f", "#88419d", "#8c97c5" , "#26828f", "#52c56a", "#badd27"))
palCDB_cat <- rev(c("#9359a5", "#9c83bd", "#8c97c5" , "#26828f", "#52c56a", "#badd27"))
swatch(palCDB_cat)
cat_names <- vv_pie %>%
  dplyr::select(`Categoria CDB`) %>% unique()
cat_names <- bind_cols(cat_names, palCDB_cat) %>% rename("pal_cat" = `...2`)
vv_pie <- vv_pie %>%
  dplyr::left_join(cat_names)
swatch(vv_pie$pal_cat)
# vv_pie <- vv_pie %>%
#   ungroup()


# for subcategories
vv_pie <- vv_pie %>%
  group_by(`Categoria CDB`, `Subcategoria CDB`) %>%
  arrange(sum, .by_group = FALSE)
#   ungroup()
  # arrange(`Subcategoria CDB`, sum, .by_group = TRUE)
  # arrange(desc(`Subcategoria CDB`), .by_group = TRUE) 
ncolors_subcat <- nrow(vv_pie)
# Interpolate
palCDB_subcat <- colorRampPalette(vv_pie$pal_cat)(ncolors_subcat)
# or use viridis
# palCDB_subcat <- viridis::viridis(n=ncolors_subcat)
swatch(palCDB_subcat)
# subcat_names <- vv_pie %>% 
#   dplyr::select(`Subcategoria CDB`) %>% unique()
# # ncolors_subcat <- vv_pie %>% dplyr::select(`Subcategoria CDB`) %>% unique()
# # ncolors_subcat <- vv_pie %>% 
# #   dplyr::select(`Categoria CDB`) %>% 
# #   n_distinct() %>% 
# #   arrange(sum, .by_group = FALSE)
# 
# 
# subcat_names <- bind_cols(subcat_names, palCDB_subcat) %>% 
#   rename("pal_subcat" = `...3`)
# vv_pie <- vv_pie %>%
#   dplyr::left_join(subcat_names) 
# # vv_pie <- vv_pie %>%
# #   dplyr::bind_cols(palCDB_subcat) %>%
#   # rename("palCDB_subcat" = `...9`)
# swatch(vv_pie$pal_subcat)
# swatch(vv_pie$pal_cat)

swatch(palCDB_subcat)
swatch(palCDB_cat)



# library("pals")
# p1 <- cubehelix(25)
# swatch(p1)
# cubebasis <- pal.compress(cubehelix, n = 6)
# swatch(cubebasis)
# cubebasis
# palCDB_cat <- rev(c("#450154", "#88419d", "#8c97c5" , "#26828f", "#52c56a", "#badd27"))
# cat_names <- vv_pie %>% dplyr::select(`Categoria CDB`) %>% unique()
# cat_names <- bind_cols(cat_names, palCDB_cat) %>% rename("palCDB_cat" = `...2`)
# vv_pie <- vv_pie %>% 
#   dplyr::left_join(cat_names)

##### With ggplot2 only -----
# Sources:
# https://stackoverflow.com/questions/26748069/ggplot2-pie-and-donut-chart-on-same-plot
# https://stackoverflow.com/questions/72423398/plot-a-wheel-pie-chart-in-r

# create center pie (white)
vv_pie_blob <- vv_pie %>% 
  dplyr::select(c(`Categoria CDB`, `Subcategoria CDB`, x)) %>% 
  mutate(
    qtdd = 1
  )

# # create center pie (mecanismo = Turbelin et al 2022)
# vv_pie_blob <- read.csv2(file = here("Entregas", "Arquivos", "Dados", "Vias-vetores-corrigido.csv")
#                 , sep = ",", check.names=FALSE)
# vv_pie_blob <- vv_pie_blob %>% 
#   dplyr::select(c(`Categoria CDB`, `Subcategoria CDB`, Mecanismo)) %>% 
#   distinct() %>% 
#   mutate(
#     x = 1:max(vv_pie$x),
#     y = 1
#   
# )

# create CDB category pie
vv_pie_cat <- vv_pie %>% 
  ungroup() %>% 
  dplyr::select(c(`Categoria CDB`, x, qtdd, x_cat, sum)) %>% 
  distinct()

# Break text to fit plot
vv_pie_cat <- vv_pie_cat %>%
  mutate(
    `Categoria CDB` = case_when(
      `Categoria CDB` ==  "Escape" ~ "Escape",
      `Categoria CDB` ==  "Soltura na natureza" ~ "Soltura",
      `Categoria CDB` ==  "Sem ajuda humana" ~ "Sem ajuda",
      `Categoria CDB` ==  "Transporte clandestino" ~ "Transporte\nclandestino",
      `Categoria CDB` ==  "Corredor"  ~ "Corredor",
      `Categoria CDB` ==  "Transporte como contaminante"  ~ "Transporte\ncomo contaminante"
    )
  ) %>% 
  mutate(
    angle_cat = case_when(
      `Categoria CDB` ==  "Corredor"  ~ as.numeric(90),
      `Categoria CDB` ==  "Sem ajuda" ~ as.numeric(90),
      `Categoria CDB` ==  "Escape" ~ as.numeric(0),
      `Categoria CDB` ==  "Soltura" ~ as.numeric(0),
      `Categoria CDB` ==  "Transporte\nclandestino" ~ as.numeric(0),
      `Categoria CDB` ==  "Transporte\ncomo contaminante"  ~ as.numeric(0)
    )
  )

# create CDB subcategory pie
vv_pie_sub <- vv_pie %>% 
  ungroup() %>% 
  dplyr::select(c(`Categoria CDB`, `Subcategoria CDB`
                  , x, qtdd, x_cat, x_angle, hjust
                  , sum)) %>% 
  distinct()

# Break text to fit plot
vv_pie_sub <- vv_pie_sub %>%
  mutate(
    `Subcategoria CDB` = case_when(
      `Subcategoria CDB` ==  "Jardim botânico, zoológico, aquário (não domésticos)" ~ "Jardim botânico, zoológico,\naquário (não domésticos)",
      `Subcategoria CDB` ==  "Canais, bacias e mares interconectados" ~ "Canais, bacias e\nmares interconectados",
      `Subcategoria CDB` ==  "Dispersão natural (vias 1 a 5) por fronteiras" ~ "Dispersão natural (vias 1\na 5)por fronteiras",
      `Subcategoria CDB` ==  "Aquário, terrário e pet (incluindo comida viva para essas espécies)" ~ "Aquário, terrário e pet (incluindo\ncomida viva para essas espécies)",
      `Subcategoria CDB` ==  "Controle de erosão, quebra vento ou cerca viva" ~ "Controle de erosão, quebra\nvento ou cerca viva",
      `Subcategoria CDB` ==  "Contaminantes em animais (exceto parasitas)"  ~ "Contaminantes em animais\n(exceto parasitas)",
      `Subcategoria CDB` ==  "Contaminantes em plantas (exceto parasitas)"  ~ "Contaminantes em plantas\n(exceto parasitas)",
      `Subcategoria CDB` ==  "Contaminação de comida incluindo comida viva"  ~ "Contaminação de comida\nincluindo comida viva",
      `Subcategoria CDB` ==  "Contaminação em material para viveiros"  ~ "Contaminação em material\npara viveiros",
      `Subcategoria CDB` ==  "Produção florestal e reflorestamento"  ~ "Produção florestal\ne reflorestamento",
      `Subcategoria CDB` ==  "Introdução para fins de conservação"  ~ "Introdução para fins\nde conservação",
      TRUE ~ as.character(`Subcategoria CDB`)
      
      
                                                  # , "Agricultura e biocombustíveis"
                                                  # , "Animais domésticos"
                                                  # , "Aquicultura/maricultura"
                                                  # , "Comida viva e isca viva"
                                                  # , "Criação ex-situ"
                                                  # , "Fazendas de peles de animais"
                                                  # , "Fins ornamentais"
                                                  # , "Plantas cultivadas"
                                                  # , "Produção florestal e reflorestamento"
                                                  # , "Caça na natureza"
                                                  # , "Controle biológico"
                                                  # , "Controle de erosão, quebra vento ou cerca viva"
                                                  # , "Introdução para fins de conservação"
                                                  # , "Melhoramento de paisagem"
                                                  # , "Outra soltura intencional"
                                                  # , "Pesca na natureza"
                                                  # , "Soltura na natureza (outros)"
                                                  # 
                                                  # , "Bagagens e turismo"
                                                  # , "Bioincrustação em navios"
                                                  # , "Container/volume"
                                                  # , "Comércio de madeira"
                                                  # , ""
                                                  # , ""
                                                  # , "Contaminação em material para viveiros"
                                                  # , "Contaminação em sementes"
                                                  # , "Equipamento de pesca"
                                                  # , "Maquinário e equipamento"
                                                  # , "Material de embalagem orgânico"
                                                  # , "Outro meio de transporte"
                                                  # , "Parasitas em animais"
                                                  # , "Parasitas em plantas"
                                                  # , "Presença em avião"
                                                  # , "Presença em navio e embarcação"
                                                  # , "Transporte de material natural"
                                                  # , "Veículos"
                                                  # 
                                                  # , "Água de lastro"
      
      
    )
  )




# Plot it
# geom_rect starts with y = 0
# geom_tile has size = 1 with y = central position, so if y = 0.25, ymin = -0.25 and ymax = 0.75 

png(filename=paste0("Entregas/A1_Vias-Vetores/Pie_ggplot_Turbelin-palette.png")
    , width = 3400, height = 3400, res = 300)

ggplot() + 
  # geom_rect(data = vv_pie_blob, 
  #           aes(xmin = 0, xmax = max(x), 
  #               ymin = 0, ymax = max(qtdd) / 3
  #               , fill = "white"         # placeholder color only for inputting fill in geom_rect. Change the color in scale_fill_manual()
  #               )) +
  geom_tile(data = vv_pie_blob, 
            aes(x = x, y = 0.25
                , fill = "white"         # placeholder color only for inputting fill in geom_rect. Change the color in scale_fill_manual()
            )) +
  scale_fill_manual(values = "white") +  # change the color here
  
  ggnewscale::new_scale_fill() +
# ggplot() +
  # geom_rect(data = vv_pie_cat,
  #           aes(xmin = 0, xmax = max(x),
  #               ymin = max(qtdd) / 3, ymax = qtdd
  #               , fill=forcats::fct_reorder(`Categoria CDB`, sum)
  #               # , color = "white"
  #           )) +
  geom_tile(data = vv_pie_cat,
            aes(x = x, y = 0.75#qtdd
                , fill=forcats::fct_reorder(`Categoria CDB`, sum)
            # , color = "white"
            )) +
  
  # scale_fill_viridis_d() +
  scale_fill_manual(values = palCDB_cat) +
  # coord_polar()
  
  ggnewscale::new_scale_fill() +
# ggplot() +
  # geom_rect(data = vv_pie_sub,
  #           aes(xmin = 0, xmax = max(x),
  #               ymin = 0.75, ymax = qtdd + 1
  #               , fill=forcats::fct_reorder(`Subcategoria CDB`, sum)
  #               # , fill=`Subcategoria CDB`
  #               # , width = 0.97
  #               # , height = 0.97
  #               # , color = "white"
  #           )) +
  geom_tile(data = vv_pie_sub,
            aes(x = x, y = 2  # qttd + 1
                , fill=forcats::fct_reorder(`Subcategoria CDB`, sum)
                # , fill=`Subcategoria CDB`
                , width = 0.97
                # , height = 0.97
                , height = 2
                # , color = "white"
                )) +
                # , color = NA)) +
  # scale_color_manual(values = "white") +
  # scale_fill_viridis_d(option = "mako") +
  # scale_fill_viridis_d() +
  scale_fill_manual(values = palCDB_subcat) +
  theme(legend.position = "none") +
  # geom_rect(aes(fill=`Subcategoria CDB`
  #               , ymin = min(qtdd), ymax = 2 #max(qtdd)
  #               , xmin = min(x), xmax = max(x)
  #               )) +
  # geom_rect(aes(fill=`Categoria CDB`, ymax=1, ymin=0, xmax=4, xmin=3)) +
  # geom_rect(aes(fill=`Subcategoria CDB`, ymax=2, ymin=1, xmax=3, xmin=0)) +
  # xlim(c(0, 50)) +
  theme(aspect.ratio=1) +
  # coord_polar(theta="y")
  coord_polar() +
  
  # remove background and other elements:
  theme_void() +
  
  theme(legend.position = "none") +
  # geom_label(aes(x=x_cat, y = 1, fill = `Categoria CDB`, label=`Categoria CDB`, hjust=0.5), size = 4) +
  geom_text(data = vv_pie_blob, aes(x = 1, y = -0.25, label = "Categorias e\nsubcategorias\nCDB")) +
  geom_textpath(data = vv_pie_cat, aes(x=x_cat, y = 1, label = `Categoria CDB`
                                       , angle = angle_cat, hjust = 0.5) , size = 6) +
  # ggplot() +
  geom_text(data = vv_pie_sub, aes(x=x
                                   , y= 2.15  #when hjust = 0.5
                                   # , y = 1.25  #when hjust = hjust
                                   , label=`Subcategoria CDB`
                                   , angle=x_angle 
                                   , hjust = 0.5
                                   # , hjust=hjust
                                   ), size = 4)
  


dev.off()


# Com fade de cores (= Turbelin et al. 2022) 
# https://stackoverflow.com/questions/73829561/ggplot-geom-rect-color-gradient-without-reference-to-data

scale_fill_gradient(low = 'yellow', high = 'white')


##### With ggplot2 and sunburst package -----
# install.packages("ggsunburst")
# library("ggsunburst")
# Sources:
# https://stackoverflow.com/questions/26748069/ggplot2-pie-and-donut-chart-on-same-plot


##### With plotrix package -----
# Sources:
# https://www.r-bloggers.com/2014/10/shopping-cart-analysis-with-r-multi-layer-pie-chart/
# https://stackoverflow.com/questions/68970261/how-to-center-pie-chart-and-nested-pie-for-a-subsection











##### With baseplot and custom function -----
# # Sources:
# # https://stackoverflow.com/questions/26748069/ggplot2-pie-and-donut-chart-on-same-plot
# 
# source(here("Entregas/A1_Vias-Vetores/base_pie_chart.R"))
# 
# # Example data
# # par(mfrow = c(1,1), mar = c(0,4,0,4))
# with(browsers,
#      donuts(share, browser, sprintf('%s: %s%%', version, share),
#             col = c('cyan2','red','orange','green','dodgerblue2'))
# )
# 
# with(mtcars,
#      donuts(mpg, interaction(gear, cyl), rownames(mtcars))
# )
# 
# # par(mfrow = c(1, 1))
# 
# # EEI data
# with(vv_pie,
#      donuts(value, `Categoria CDB`, `Subcategoria CDB`, #, rownames(`Subcategoria CDB`),
#             col = c('cyan2','red','orange','green','dodgerblue2'))
# )



##### With ggpie package -----
# # https://cran.r-project.org/web/packages/ggpie/vignettes/ggpie.html
# 
# # Plot
# ggnestedpie(data = vv_pie, group_key = c("Categoria CDB", "Subcategoria CDB")
#             , count_type = "full"
#             , r0 = 1, r1 = 2.5, r2 = 2.5
#             , inner_label_info = "all", inner_label_split = NULL,inner_label_size = 2
#             , outer_label_type = "circle", outer_label_pos = "in", outer_label_info = "all"
#             )


##### With Plotly package -----
# # https://stackoverflow.com/questions/52582394/how-to-do-nested-pie-chart-in-r-where-the-outer-ring-data-is-a-subset-of-inner-r
# 
# source(here("Entregas/A1_Vias-Vetores/plotly_pie_chart.R"))


##### With webr package -----
# (não funciona muito bem)
# 

# # Remove spaces because PieDonut can't handle it:
# vv_pie <- vv_pie %>% 
#   dplyr::rename_with(~ str_replace_all( ., " ", "_"))
# 
# png(filename="Entregas/A1_Vias-Vetores/Pie_webr.png"
#     , width = 4000, height = 4000, res = 300)
# PieDonut(vv_pie, 
#          aes(Categoria_CDB, Subcategoria_CDB, count = qtdd)
#          , showRatioPie = FALSE
#          , ratioByGroup = FALSE
#          # , showRatioThreshold = 0.01 # drop labels too
#          , labelposition = 10
#          , donutLabelSize = 5
#          , explode = c(1, 3)
#          , explodeDonut = TRUE
#          , r0 = 0.2
#          , r1 = 0.8
#          # , r2 = 0.5
#          , color = "black"
#          , title = "Classificação de vias e vetores CDB"
#          , titlesize = 10
#          # , use.labels = TRUE # does not work
# )
# dev.off()





### Treemap plot ----
# https://rpubs.com/tskam/treemap # -> tem como fazer interativo com d3tree()

### Todos os ambientes -----

# Make each subcategory unique and add counter
vv_cat <- vv %>% 
  # dplyr::select(c(`Grau de confiança`:`Grupo biológico`)) %>% 
  dplyr::select(c(`Grau de confiança`:`Subcategoria CDB`)) %>% 
  dplyr::distinct() %>% 
  mutate(
    value = 1:nrow(.),
    qtdd = 1
    )

# Reorder and relevel by category
vv_cat <- vv_cat %>% 
  mutate(
    `Categoria CDB` = forcats::fct_relevel(`Categoria CDB`,  
                                           "Soltura na natureza", "Escape", "Transporte clandestino",
                                           "Transporte como contaminante", "Sem ajuda humana", "Corredor")
  ) %>% 
  mutate(
    `Subcategoria CDB` = forcats::fct_relevel(`Subcategoria CDB`, sort)
  ) #%>% 
  # # Fazer subcategorias dar match na ordem das categorias (= Fig.1 Turbelin et al. 2022)
  # mutate(
  #   `Subcategoria CDB` = forcats::fct_reorder(`Subcategoria CDB`, `Subcategoria CDB`)
  # )

# Create palette
ncolors <- nrow(vv_cat)
palCDB <- viridis::viridis(n=ncolors)
swatch(palCDB)

vv_cat <- vv_cat %>% 
  mutate(palCDB = palCDB)


# 1) Squarified:

png(filename="Entregas/A1_Vias-Vetores/Treemap_squarified.png"
    , width = 2400, height = 3000, res = 300)
treemap(vv_cat,
        index=c("Categoria CDB", "Subcategoria CDB"),
        vSize="qtdd",
        vColor="palCDB",
        type = "color",
        algorithm = "squarified",
        # algorithm = "pivotSize",
        # fontsize and color:
        fontsize.labels=c(15,10),
        # fontcolor.labels=c("white","white"),
        # border colors:
        border.col=c("black","white"),             # Color of borders
        border.lwds=c(3,1),                         # Width of colors
        # labels:
        align.labels = list(c("left", "top"), c("center", "center")),
        bg.lables = 0, # DOES NOT WORK
        overlap.labels = 1,
        # titles:
        title="Categorias e subcategorias de vias e vetores de EEI presentes" #,
        # title.legend = "Consultoria MMA-WWF. Produção: Seleção Natural"
)
dev.off()

png(filename="Entregas/A1_Vias-Vetores/Treemap_pivotSize.png"
    , width = 2400, height = 3000, res = 300)
treemap(vv_cat,
        index=c("Categoria CDB", "Subcategoria CDB"),
        vSize="qtdd",
        vColor="palCDB",
        type = "color",
        # algorithm = "squarified",
        algorithm = "pivotSize",
        # fontsize and color:
        fontsize.labels=c(15,10),
        # fontcolor.labels=c("white","white"),
        # border colors:
        border.col=c("black","white"),             # Color of borders
        border.lwds=c(3,1),                         # Width of colors
        # labels:
        align.labels = list(c("left", "top"), c("center", "center")),
        bg.lables = 0, # DOES NOT WORK
        # titles:
        title="Categorias e subcategorias de vias e vetores de EEI presentes" #,
        # title.legend = "Consultoria MMA-WWF. Produção: Seleção Natural"
)
dev.off()



### Por ambiente -----

vv$Ambiente %>% unique()


#### Agua doce ------

amb.x <- "Água doce"

# Make each subcategory unique
vv_cat <- vv %>% 
  dplyr::select(c(`Grau de confiança`:Ambiente)) %>%
  dplyr::filter(Ambiente == amb.x) %>% 
  dplyr::distinct() %>% 
  mutate(
    value = 1:nrow(.),
    qtdd = 1
  )

# Reorder and relevel by category
vv_cat <- vv_cat %>% 
  mutate(
    `Categoria CDB` = forcats::fct_relevel(`Categoria CDB`,  
                                           "Soltura na natureza", "Escape", "Transporte clandestino"
                                           # , "Transporte como contaminante", "Sem ajuda humana"
                                           , "Corredor"
                                           )
  ) %>% 
  mutate(
    `Subcategoria CDB` = forcats::fct_relevel(`Subcategoria CDB`, sort)
  ) #%>% 
# # Fazer subcategorias dar match na ordem das categorias (= Fig.1 Turbelin et al. 2022)
# mutate(
#   `Subcategoria CDB` = forcats::fct_reorder(`Subcategoria CDB`, `Subcategoria CDB`)
# )

# Create palette
ncolors <- nrow(vv_cat)
palCDB <- viridis::mako(n=ncolors)
swatch(palCDB)

vv_cat <- vv_cat %>% 
  mutate(palCDB = palCDB)


# pivotSize

png(filename=paste0("Entregas/A1_Vias-Vetores/Treemap_pivotSize_", amb.x, ".png")
    , width = 2400, height = 2400, res = 300)
treemap(vv_cat,
        index=c("Categoria CDB", "Subcategoria CDB"),
        vSize="qtdd",
        vColor="palCDB",
        type = "color",
        # algorithm = "squarified",
        algorithm = "pivotSize",
        # fontsize and color:
        fontsize.labels=c(15,10),
        # fontcolor.labels=c("white","white"),
        # border colors:
        border.col=c("black","white"),             # Color of borders
        border.lwds=c(3,1),                         # Width of colors
        # labels:
        align.labels = list(c("left", "top"), c("center", "center")),
        bg.lables = 0, # DOES NOT WORK
        # titles:
        title=paste0("Categorias e subcategorias de vias e vetores de EEI presentes de ambiente ", amb.x) #,
        # title.legend = "Consultoria MMA-WWF. Produção: Seleção Natural"
)
dev.off()


#### Terrestre ------

amb.x <- "Terrestre"

# Make each subcategory unique
vv_cat <- vv %>% 
  dplyr::select(c(`Grau de confiança`:Ambiente)) %>%
  dplyr::filter(Ambiente == amb.x) %>% 
  dplyr::distinct() %>% 
  mutate(
    value = 1:nrow(.),
    qtdd = 1
  )

# Reorder and relevel by category
vv_cat <- vv_cat %>% 
  mutate(
    `Categoria CDB` = forcats::fct_relevel(`Categoria CDB`,  
                                           "Soltura na natureza", "Escape", "Transporte clandestino"
                                           , "Transporte como contaminante", "Sem ajuda humana"
                                           # , "Corredor"
    )
  ) %>% 
  mutate(
    `Subcategoria CDB` = forcats::fct_relevel(`Subcategoria CDB`, sort)
  ) #%>% 
# # Fazer subcategorias dar match na ordem das categorias (= Fig.1 Turbelin et al. 2022)
# mutate(
#   `Subcategoria CDB` = forcats::fct_reorder(`Subcategoria CDB`, `Subcategoria CDB`)
# )

# Create palette
ncolors <- nrow(vv_cat)
palCDB <- viridis::cividis(n=ncolors)
swatch(palCDB)

vv_cat <- vv_cat %>% 
  mutate(palCDB = palCDB)


# pivotSize

png(filename=paste0("Entregas/A1_Vias-Vetores/Treemap_pivotSize_", amb.x, ".png")
    , width = 2400, height = 2400, res = 300)
treemap(vv_cat,
        index=c("Categoria CDB", "Subcategoria CDB"),
        vSize="qtdd",
        vColor="palCDB",
        type = "color",
        # algorithm = "squarified",
        algorithm = "pivotSize",
        # fontsize and color:
        fontsize.labels=c(15,10),
        # fontcolor.labels=c("white","white"),
        # border colors:
        border.col=c("black","white"),             # Color of borders
        border.lwds=c(3,1),                         # Width of colors
        # labels:
        align.labels = list(c("left", "top"), c("center", "center")),
        bg.lables = 0, # DOES NOT WORK
        # titles:
        title=paste0("Categorias e subcategorias de vias e vetores de EEI presentes de ambiente ", amb.x) #,
        # title.legend = "Consultoria MMA-WWF. Produção: Seleção Natural"
)
dev.off()


#### Marinho ------

amb.x <- "Marinho"

# Make each subcategory unique
vv_cat <- vv %>% 
  dplyr::select(c(`Grau de confiança`:Ambiente)) %>%
  dplyr::filter(Ambiente == amb.x) %>% 
  dplyr::distinct() %>% 
  mutate(
    value = 1:nrow(.),
    qtdd = 1
  )

# Reorder and relevel by category
vv_cat <- vv_cat %>% 
  mutate(
    `Categoria CDB` = forcats::fct_relevel(`Categoria CDB`,  
                                           # "Soltura na natureza", 
                                           "Escape", "Transporte clandestino"
                                           , "Transporte como contaminante", "Sem ajuda humana"
                                           , "Corredor"
    )
  ) %>% 
  mutate(
    `Subcategoria CDB` = forcats::fct_relevel(`Subcategoria CDB`, sort)
  ) #%>% 
# # Fazer subcategorias dar match na ordem das categorias (= Fig.1 Turbelin et al. 2022)
# mutate(
#   `Subcategoria CDB` = forcats::fct_reorder(`Subcategoria CDB`, `Subcategoria CDB`)
# )

# Create palette
ncolors <- nrow(vv_cat)
palCDB <- viridis::turbo(n=ncolors)
swatch(palCDB)

vv_cat <- vv_cat %>% 
  mutate(palCDB = palCDB)


# pivotSize

png(filename=paste0("Entregas/A1_Vias-Vetores/Treemap_pivotSize_", amb.x, ".png")
    , width = 2400, height = 2400, res = 300)
treemap(vv_cat,
        index=c("Categoria CDB", "Subcategoria CDB"),
        vSize="qtdd",
        vColor="palCDB",
        type = "color",
        # algorithm = "squarified",
        algorithm = "pivotSize",
        # fontsize and color:
        fontsize.labels=c(15,10),
        # fontcolor.labels=c("white","white"),
        # border colors:
        border.col=c("black","white"),             # Color of borders
        border.lwds=c(3,1),                         # Width of colors
        # labels:
        align.labels = list(c("left", "top"), c("center", "center")),
        bg.lables = 0, # DOES NOT WORK
        # titles:
        title=paste0("Categorias e subcategorias de vias e vetores de EEI presentes de ambiente ", amb.x) #,
        # title.legend = "Consultoria MMA-WWF. Produção: Seleção Natural"
)
dev.off()


