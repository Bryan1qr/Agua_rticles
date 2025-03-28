source("scripts/generacion de tabla1.R")
library(tidyverse)
df <- dataset_water(ruta = "CaplinaOcoña")
View(df)
openxlsx::write.xlsx(df %>% mutate(fecha_larga = as.character.POSIXt(fecha_larga)), "AAA_Caplina_Ocoña.xlsx")


df %>% mutate(fecha_larga = as.character.POSIXt(fecha_larga)) %>% str()

library(tidyverse)
df <- openxlsx::read.xlsx("AAA_Caplina_Ocoña.xlsx")

df %>% 
  filter(PARAMETROS == "Coliformes Totales (NMP/100ml) " & valor > 0) %>% 
  ggplot(aes(x = log1p(valor))) + geom_histogram() + 
  facet_grid(año~cuenca) + labs(x = "log(NMP/100ml)", y = "Conteos")
