# filtro ------------------------------------------------------------------

filtrado <- function(tabla){
  
  tabla %>% 
  filter(!cuenca %in% c(
    "Ilo-Moquegua", "CAMANA")) %>% 
    filter(PARAMETROS %in% c("Coliformes Termotolerantes (NMP/100ml)",
                             "Nitratos (NO3-) (mg/L)",
                             "Demanda Bioquímica de Oxígeno (DBO5) (mg/L)",
                             "Demanda Química de Oxígeno (DQO) (mg/L)",
                             "pH (Unidad de PH)",
                             "Temperatura (°C)",
                             "Conductividad ((µS/cm))",
                             "Oxígeno Disuelto (mg/L)")) %>% 
    mutate(tipo = case_when(
      grepl("^Q", codigo) ~ "lótico",
      grepl("^R", codigo) ~ "lótico",
      TRUE ~ "léntico")) %>% 
    filter(tipo != "léntico") %>% 
    filter(!(PARAMETROS == "Coliformes Termotolerantes (NMP/100ml)" & valor < 1000))
}