#* Generación de dataset espacial de la data casi completa:
library(tidyverse)
library(sf)
df <- read.csv("data_agua_casi_completa.csv") %>%
  select(-c(X, PARAMETROS, valor, fecha_larga,
     tipo, cuerpo_agua, descripcion)) %>%
  distinct() %>%
  filter(!is.na(zona) | !is.na(este)) %>%
  mutate(zona = 32700 + zona)

espacial1 <- df %>% 
  group_split(zona) %>%
  map_dfr(~ .x %>%
            st_as_sf(coords = c("este", "norte"), 
                     crs = unique(.x$zona)) %>%
            st_transform(crs = 4326))

write_sf(espacial1, "peru4.gpkg")

write_rds(df, "data_avance_setiembre.rds", compress = "gz")

df <- readRDS("data_avance_setiembre.rds")
