# Creación del nuevo dataset de agua --------------------------------------
dataset_water <- function(ruta){
  # Creamos una funciónn para la selección de los valores excel deseados:
  agua <- function(origen_df){
    a <- readxl::read_xls(origen_df, skip = 19) %>%
      filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>% 
      select(where(~ any(!is.na(.)))) %>% select(-starts_with("Cat")) %>% mutate(periodo = origen_df) %>% 
      pivot_longer(cols = -c(PARAMETROS, UNIDAD, periodo), names_to = "codigo", values_to = "valor") %>% 
      mutate(cuenca = str_extract(periodo, "(?<=/)(.*?)(?=_)"),
             año = str_extract(periodo, "(?<=_)(.*?)(?=_)"),
             periodo = str_extract(periodo, "(?<=_)([^_\\.]+)(?=\\.)")) %>%
      filter(valor != "----") %>%
      select(codigo, cuenca, año, periodo, PARAMETROS, UNIDAD, valor)
    
    b <- readxl::read_xls(origen_df, skip = 15, col_names = TRUE) %>% 
      filter(`Fecha monitoreo` %in% c("Hora Monitoreo", "PARAMETROS")) %>% 
      select(where(~ any(!is.na(.)))) %>% 
      select(-starts_with(c( "DD", "..."))) %>% 
      pivot_longer(cols = -`Fecha monitoreo`, names_to = "fecha", values_to = "hora") %>% 
      mutate(
        hora = ifelse(is.na(hora), "12:00", hora),
        hora = ifelse(
          grepl("^\\d*\\.\\d+$", hora),
          sapply(hora, function(x) {
            decimal_time <- as.numeric(x)
            hours <- floor(decimal_time * 24)
            minutes <- round((decimal_time * 24 - hours) * 60)
            sprintf("%02d:%02d:%02d", hours, minutes, 0)
          }),
          hora
        )
      ) %>% fill(fecha, hora, .direction = "downup") %>% 
      pivot_wider(id_cols = fecha, names_from = `Fecha monitoreo`, values_from = hora) %>% 
      mutate(fecha = str_replace(fecha, "\\.\\.\\.\\d+$", ""),
             fecha_larga = as.POSIXct(paste(fecha, `Hora Monitoreo`), format = "%d/%m/%Y %H:%M")) %>%
      select(fecha_larga, PARAMETROS) %>% rename("codigo" = "PARAMETROS")
    
    a %>% 
      left_join(b, by = "codigo") %>% 
      mutate(fecha_larga_completada = is.na(fecha_larga)) %>% 
      fill(fecha_larga, .direction = "downup") %>% 
      mutate(fecha_larga = ifelse(fecha_larga_completada, fecha_larga - 172800, fecha_larga)) %>%
      select(-fecha_larga_completada) %>% 
      mutate(fecha_larga = as.POSIXct(fecha_larga, origin = "1970-01-01"))}
  
  archivos_xls <- list.files(path = ruta, pattern = "\\.xls$", full.names = TRUE)
  listita <- map(archivos_xls, agua)
  
  df_combinado <- bind_rows(listita)%>% 
    mutate(
      valor = if_else(valor != "Ausencia", valor %>%
                        str_replace_all("[^0-9,.-]", "") %>%
                        str_replace(",", ".") %>% as.numeric(), 0)) %>% 
    mutate(PARAMETROS = paste0(PARAMETROS, " (", UNIDAD, ") ")) %>% 
    select(-UNIDAD, año, periodo)
  
  as_tibble(df_combinado)
}