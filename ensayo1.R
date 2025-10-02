library(tidyverse)
library(openxlsx)

df1 <- read.xlsx("productos/aaa_amazonas.xlsx")
df2 <- read.xlsx("productos/aaa_caplina_ocoña.xlsx")
df3 <- read.xlsx("productos/AAA_JZ_filtros.xlsx", sheet = 1)

combinado <- rbind(df1, df2, df3) %>% 
  mutate(fecha_larga = as.POSIXct(
    fecha_larga * 86400,
    origin = "1899-12-30", tz = "UTC"))

source("scripts/filtrado.R")


combinado_filtrado <- combinado %>% filtrado() %>%
  filter(fecha_larga > "2020-01-01")

espacial <- combinado %>% 
    select(-c(PARAMETROS, valor, fecha_larga)) %>% 
    distinct() %>% 
    filter(!is.na(zona)) %>% 
  mutate(zona = zona + 32700) %>% 
  group_split(zona) %>%
  map_dfr(~ .x %>%
            st_as_sf(coords = c("este", "norte"), 
                     crs = unique(.x$zona)) %>%
            st_transform(crs = 4326))

library(sf)
library(geoidep)
library(ggspatial)
dep <- get_departaments(show_progress = FALSE)

plot(espacial$geometry)
st_write(tab1, "coliformes2.shp",)

g1 <- ggplot() + 
  geom_sf(color = "darkblue", size = 4, alpha = 0.2, data = espacial) + 
  geom_sf(color = "gray10", size = 4, alpha = 0.2, data = dep) + 
  labs(title = "Zonas con alta concentración de coliformes",
       subtitle = "Valores mayores a 1000 NMP/100ml") +
  annotation_scale(style = "ticks", location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering()) +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 11, hjust = 0.5),
        plot.background = element_rect(fill = "#fff1cc", colour = "#fff1cc"),
        panel.background = element_rect(fill = "#fff1cc", colour = "gray50"),
        panel.grid = element_line(linewidth = 0.1, colour = "gray50"))

g1

ggsave("mapa_coliformes.png", width = 9.5, height = 12,
       dpi = 300, bg = "#fff1cc", units = "cm")


get_data_sources() %>% view()
