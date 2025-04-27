# Paso1 carga de plantillas (scripts) -------------------------------------

source("scripts/data_espacial.R")
source("scripts/generacion de tabla1.R")

# Paso2 carga de librerías ------------------------------------------------

library(tidyverse)
library(openxlsx)
library(zoo)
library(readxl)

# 
# install.packages("openxlsx")
# install.packages("zoo")


# Paso 3 ejecución de las funciones ---------------------------------------

tabla_final <- dataset_agua2(ruta = "data", coordenadas = "CUENCAS/CaplinaOcoña")

view(tabla_final)
str(tabla_final)

# paso 4 Guardado del dataset ----------------------------------------------------

write.xlsx(tabla_final, "resultados/resultados_agua.xlsx")



# paso extra: convertir de tabla larga a tabla ancha ----------------------

tabla_ancha <- tabla_final %>% 
  pivot_wider(names_from = "PARAMETROS",
              values_from = "valor",
              id_cols = c(codigo, cuenca, fecha_larga, descripcion,
                          zona, este, norte, categoria, tipo, cuerpo_agua))










xd <- openxlsx::read.xlsx("resultados/resultados_agua.xlsx")


xd %>%
  mutate(zona = zona + 32700) %>% 
  st_as_sf(coords = c("este", "norte"), crs = "zona") -> spatial


plot(spatial$geometry)



