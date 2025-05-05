# Coliformes, nitratos, dbo, dqo, ph, temperatura, conductividad y oxigeno

b1 <- tabla_final %>% 
  filter(!cuenca %in% c(
    "QUILCA-VICTOR-CHILI", "TAMBO", "Ilo-Moquegua", "CAMANA")) %>% 
  filter(PARAMETROS %in% c("Coliformes Termotolerantes (NMP/100ml)",
                            "Nitratos (NO3-) (mg/L)",
                            "Demanda Bioquímica de Oxígeno (DBO5) (mg/L)",
                            "Demanda Química de Oxígeno (DQO) (mg/L)",
                            "pH (Unidad de PH)",
                            "Temperatura (°C)",
                            "Conductividad ((µS/cm))",
                            "Oxígeno Disuelto (mg/L)")) %>% 
  filter(tipo != "léntico") %>% 
  filter(!(PARAMETROS == "Coliformes Termotolerantes (NMP/100ml)" & valor < 2))


view(b1)


test <- b1 %>% filter(PARAMETROS == "Coliformes Termotolerantes (NMP/100ml)")


range(test$valor, na.rm = T)


moda1 <- function(x){
  frecuencias <- table(x)
  moda <- as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
  moda
}


b1 %>% 
  filter(PARAMETROS == "Coliformes Termotolerantes (NMP/100ml)") %>% 
  summarise(moda = moda1(valor))


b2 <- b1 %>% 
  filter(PARAMETROS == "Coliformes Termotolerantes (NMP/100ml)")


b3 <- b2 %>% 
  summarise(q3 = quantile(valor, 0.75))


b2 %>% filter(valor > 330) %>% 
  ggplot() + 
  geom_violin(aes(x = PARAMETROS, y = valor)) +
  geom_boxplot(aes(x = PARAMETROS, y = valor),outliers = F, alpha = 0.5)


# agregar el estado activo/inactivo de los puntos de muestreo
# considerar una frecuencia de monitoreo y **continuidad freq > 50%**



# Analizando DQO ----------------------------------------------------------

b1 %>% 
  filter(PARAMETROS == "Demanda Química de Oxígeno (DQO) (mg/L)") %>% 
  summarise(moda = moda1(valor))


a2 <- b1 %>% 
  filter(PARAMETROS == "Demanda Química de Oxígeno (DQO) (mg/L)")


a3 <- a2 %>% 
  summarise(q3 = quantile(valor, 0.75))



dqo1 <- a2 %>% filter(valor > 12)

a2 %>% filter(valor > 12) %>% 
  ggplot() + 
  geom_violin(aes(x = PARAMETROS, y = valor)) +
  geom_boxplot(aes(x = PARAMETROS, y = valor),outliers = F, alpha = 0.5) + theme_wsj()



a2 %>% filter(valor > 12) %>% 
  ggplot(aes(x = valor)) + geom_histogram(fill = "aquamarine", color = "gray20") + 
  labs(title = "Histograma de DQO", x = "Concentración (mg/L)", y = "Frecuencia") +
  theme_bw()


# considerar comparación de quinquenios