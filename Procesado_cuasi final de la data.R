# Procesado final de la data:
# Compilando las tablas de datos con errores:
source("scripts/data_espacial.R")
source("scripts/generacion de tabla1.R")

# Una tabla con pocos vacíos de fecha
tabla_final <- dataset_agua2(
  ruta = "params_varios", # coloca aquí la ruta de la carpeta
  coordenadas = "espacial_varios") # coloca aquí la otra carpeta

# Corrección de detalles:
tabla_final <- tabla_final %>%
  mutate(cuerpo_agua = str_extract(descripcion, "^[^,]+"),  # Extrae desde el inicio hasta la primera coma
         cuerpo_agua = str_replace_all(cuerpo_agua, ",", ""),  # Elimina cualquier coma restante
         cuerpo_agua = str_replace(cuerpo_agua, "^Rio\\b", "Río"),  # Corrige 'Rio' a 'Río' al inicio
         cuerpo_agua = str_squish(cuerpo_agua))
# Corrección utilizando la tabla de fechas corregidas:
df <- read.delim("clipboard") %>%
        mutate(fecha_larga = as.POSIXct(
            fecha_larga,format = "%m/%d/%Y %H:%M", tz = "UTC"))

tabla_final_ok <- tabla_final %>%
  left_join(df, by = "codigo", suffix = c("", "_nuevo")) %>%
  mutate(fecha_larga = coalesce(fecha_larga, fecha_larga_nuevo)) %>%
  select(-ends_with("_nuevo"))

# Compilando las tablas de datos con la data histórica procesada:
lista1 <- list.files(path = "resultados", pattern = "*.xlsx", full.names = TRUE)
listado2 <- map(lista1, read_xlsx)
tabla_unificada <- listado2 %>% bind_rows()
reemplazos <- unique(tabla_final_ok$cuenca)

tabla_unificada <- tabla_unificada %>% filter(!cuenca %in% reemplazos)

tabla_unificada2 <- bind_rows(tabla_unificada,
  tabla_final_ok %>% select(all_of(colnames(tabla_unificada)))
)

dxx <- read.delim("clipboard") %>% select(codigo, descripcion, zona, este, norte, categoria)
df_final <- tabla_unificada2 %>%
  left_join(dxx, by = "codigo", suffix = c(".grande", ".pequena"), relationship = "many-to-many") %>%
  mutate(
    descripcion  = coalesce(descripcion.grande, descripcion.pequena),
    zona      = coalesce(zona.grande, zona.pequena),
    este      = coalesce(este.grande, este.pequena),
    norte     = coalesce(norte.grande, norte.pequena),
    categoria = coalesce(categoria.grande, categoria.pequena)
  ) %>%
  select(-ends_with(".pequena"), -ends_with(".grande"))

write.csv(df_final, "data_agua_casi_completa.csv")


xdd <- read.csv("data_agua_casi_completa.csv")