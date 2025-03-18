# Creación del nuevo dataset de agua --------------------------------------
dataset_water <- function(ruta = "data"){
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
      select(where(~ any(!is.na(.)))) %>% select(-starts_with(c( "DD", "..."))) %>% 
      pivot_longer(cols = -`Fecha monitoreo`, names_to = "fecha", values_to = "hora") %>% 
      pivot_wider(id_cols = fecha, names_from = `Fecha monitoreo`, values_from = hora) %>% 
      mutate(fecha = str_replace(fecha, "\\.\\.\\.\\d+$", ""),
             fecha_larga = as.POSIXct(paste(fecha, `Hora Monitoreo`), format = "%d/%m/%Y %H:%M")) %>%
      select(fecha_larga, PARAMETROS) %>% rename("codigo" = "PARAMETROS")
    
    a %>% left_join(b, by = "codigo")}
  
  archivos_xls <- list.files(path = "data", pattern = "\\.xls$", full.names = TRUE)
  listita <- map(archivos_xls, agua)
  
  df_combinado <- bind_rows(listita)%>% 
    mutate(
      valor = if_else(valor != "Ausencia", valor %>%
                        str_replace_all("[^0-9,.-]", "") %>%
                        str_replace(",", ".") %>% as.numeric(), 0)) %>% 
    mutate(PARAMETROS = paste0(PARAMETROS, " (", UNIDAD, ") ")) %>% 
    select(-UNIDAD, -año, -periodo)
  
  as_tibble(df_combinado)
}