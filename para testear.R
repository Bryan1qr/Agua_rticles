source("scripts/generacion de tabla1.R")
library(tidyverse)
# caplina_ocoña <- dataset_water(ruta = "CaplinaOcoña")
# 
# caplina_ocoña %>% filter(codigo == "RCapi1" & PARAMETROS == "Aceites y Grasas (mg/L) ") %>% 
#   ggplot(aes(x = fecha_larga, y = valor)) + geom_line() + geom_point() +
#   geom_label(aes(label = paste(valor, format(fecha_larga, format = "%Y"), sep = "\n")), size = 3, alpha = 0.5) +
#   ggthemes::theme_wsj()

origen_df <- "CaplinaOcoña/USHUSUMA_2011_I.xls"
a <- readxl::read_xls(origen_df, skip = 19) %>%
  filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>% 
  select(where(~ any(!is.na(.)))) %>% select(-starts_with("Cat")) %>% mutate(periodo = origen_df) %>% 
  pivot_longer(cols = -c(PARAMETROS, UNIDAD, periodo), names_to = "codigo", values_to = "valor") %>% 
  mutate(cuenca = str_extract(periodo, "(?<=/)(.*?)(?=_)"),
         año = str_extract(periodo, "(?<=_)(.*?)(?=_)"),
         periodo = str_extract(periodo, "(?<=_)([^_\\.]+)(?=\\.)")) %>%
  filter(valor != "----") %>%
  select(codigo, cuenca, año, periodo, PARAMETROS, UNIDAD, valor)


b <- readxl::read_xls(path = "CaplinaOcoña/USHUSUMA_2011_I.xls", skip = 15, col_names = TRUE) %>% 
  filter(`Fecha monitoreo` %in% c("Hora Monitoreo", "PARAMETROS")) %>% 
  select(where(~ any(!is.na(.)))) %>% 
  select(-starts_with(c( "DD", "..."))) %>% 
  pivot_longer(cols = -`Fecha monitoreo`, names_to = "fecha", values_to = "hora") %>% 
  mutate(
    # Reemplazar NA por "12:00"
    hora = ifelse(is.na(hora), "12:00", hora),
    
    # Convertir decimales a formato HH:MM:SS
    hora = ifelse(
      grepl("^\\d*\\.\\d+$", hora),  # Verificar si 'hora' es un decimal
      sapply(hora, function(x) {
        decimal_time <- as.numeric(x)  # Convertir el valor a numérico
        hours <- floor(decimal_time * 24)  # Obtener horas
        minutes <- round((decimal_time * 24 - hours) * 60)  # Obtener minutos
        sprintf("%02d:%02d:%02d", hours, minutes, 0)  # Formato HH:MM:SS
      }),
      hora  # Si no es decimal, mantener el valor original
    )
  ) %>% fill(fecha, hora, .direction = "downup") %>% 
  pivot_wider(id_cols = fecha, names_from = `Fecha monitoreo`, values_from = hora) %>% 
  mutate(fecha = str_replace(fecha, "\\.\\.\\.\\d+$", ""),
         fecha_larga = as.POSIXct(paste(fecha, `Hora Monitoreo`), format = "%d/%m/%Y %H:%M")) %>%
  select(fecha_larga, PARAMETROS) %>% rename("codigo" = "PARAMETROS")


x1 <- a %>% 
  left_join(b, by = "codigo") %>% 
  # Crear una bandera para saber si el valor fue completado
  mutate(fecha_larga_completada = is.na(fecha_larga)) %>% 
  fill(fecha_larga, .direction = "downup") %>%  # Completar con valores vecinos
  # Restar 2 días solo a las celdas que fueron completadas
  mutate(fecha_larga = ifelse(fecha_larga_completada, fecha_larga - 172800, fecha_larga)) %>%
  # Eliminar la columna de bandera
  select(-fecha_larga_completada) %>% 
  mutate(fecha_larga = as.POSIXct(fecha_larga, origin = "1970-01-01"))
