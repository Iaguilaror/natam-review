
# SCRIPT PARA GENERAR UN CHOROPLETH MAP DE AMERICA ------------------------


# librerias ---------------------------------------------------------------
library("pacman")

# cargamos los demas paquetes con p_load de pacman
p_load( "vroom",
        "ggplot2",
        "dplyr",
        "rnaturalearth",
        "sf",
        "tmap",
        "rgeos",
        "ggrepel",
        "tidyr",
        "scales",
        "cowplot",
        "openxlsx")



# cargar data -------------------------------------------------------------

# america_natam_data.df <- vroom("natam_groups_and_estimated_pop_perhome_country.tsv")
america_natam_data.df <- read.xlsx( xlsxFile = "Tables A tale of native american whole genome sequencing.xlsx",
                                    sheet = "Sup Table XXX",
                                    startRow = 2 ) %>% 
  rename( America_Region = America.Region )


# del paquete rnaturalearth se extraen los datos de todos
# los paises del mundo para obtener las coordenadas de los paises

world_map_coords_data.df <- ne_countries(returnclass = "sf") %>%
  select(name,
         Isocode = iso_a3,
         geometry,
         continent,
         America_Region = subregion,
         lastcensus)




# manejo de datos ---------------------------------------------------------

# unimos los datos de censos con los datos de coordenadas
america_map_data_1.df <- world_map_coords_data.df %>% 
  right_join(
    america_natam_data.df,
    by = c("Isocode",
           "America_Region"))


# Las regiones del Caribe no seran graficadas ya que no forman parte del registro
# del paquete rnaturalearth


# Guyana Francesa no aparece individualmente en el registro
# del paquete rnaturalearth
# es parte de Francia (Al graficar Francia, se grafica tambien Guyana Francesa)
# Francia si aparece
# Hay que convertir los
# identificadores de Guyana Francesa a Francia 
# extraemos Guyana Francesa del censo

french_guiana.df <- america_natam_data.df %>%
  filter(Isocode == "GUF")


# extraemos la data de Francia
france.df <- world_map_coords_data.df %>%
  filter(name == "France")


# convertimos los identificadores de GUF a FRA
# cambiamos el Isocode
french_guiana.df$Isocode[french_guiana.df$Isocode == "GUF"] <- "FRA"
# cambiamos la region de America
french_guiana.df$America_Region[french_guiana.df$America_Region == "South America"] <- "Western Europe"

french_guiana.df


# Unimos los datos de Guyana Francesa y Francia para tener en una sola fila
# los datos de coordenadas con los datos del censo de Guyana
partial_GUF_map_data.df <- france.df %>% left_join(
  french_guiana.df,
  by = c("Isocode",
         "America_Region"))


# ahora si unimos todos los datos de Guyana y el resto
# de america
# eliminamos las regiones de America (Caribe) sin datos de coordenadas
# en el paquete rnaturalearth
america_map_data_2.df <- rbind(america_map_data_1.df,
                               partial_GUF_map_data.df) %>% 
  drop_na(continent)



# Graficar mapa de America ------------------------------------------------

america_map.p <- ggplot(america_map_data_2.df) +
  geom_sf(aes(fill = as.integer(Total.NatAm.Population.reported)),
          color = "black",
          size = 0.2)  + 
  scale_fill_gradient(low = "white",  #gris 
                      high = "#ff9900", #orange
                      breaks = seq( from = 0, to = 8e6, by = 2e6 ),
                      limits = c (0, 8e6),
                      labels = seq( from = 0, to = 8, by = 2 ) %>% paste(., "M")  ) +
  guides(fill = guide_colourbar(ticks.colour = "black",
                                frame.colour = "black")) +
  coord_sf(xlim = c(-170, -15),           
           ylim = c(-90, 120)) +
  labs(title = "NatAm Population",
       fill = "habitants",
       caption = "Data Sources = \n Official Censuses Data of NatAm Pop of each country, \n IGWIA 2021, \n Labels show those countries with more than 1 million Native American population") +
  geom_sf_label(aes(label = ifelse(as.integer(Total.NatAm.Population.reported) > 1e6,
                                   Isocode,
                                   NA)),
                nudge_x = 0,
                nudge_y = 0,
                size = 3,
                fill = NA,
                color = "black",
                label.size = 0 ) +
  theme_void(base_size = 16)

# Vis
america_map.p

# guardar plot ------------------------------------------------------------

# salvar mapa 

# png
ggsave( filename = "Estimated_NatAm_Pop_in_America_Countries.png",    # nombre del archivo de salida
        plot = america_map.p,                   # cual grafico guardamos
        width = 15,                     # ancho de 7 pulgadas
        height = 8,                    # alto de 7 pulgadas
        dpi = 400 )                    # resolucion de 300 dpi



# svg
ggsave(filename = "Estimated_NatAm_Pop_in_America_Countries.svg", 
       plot = america_map.p,                
       device = "svg",                         
       width = 15, height = 8, units = "in",   
       dpi = 400)                              




