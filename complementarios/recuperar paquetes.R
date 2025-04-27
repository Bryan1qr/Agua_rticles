paquetes_anteriores <- installed.packages()[, "Package"]
writeLines(paquetes_anteriores, "paquetes_anteriores.txt")
paquetes <- readLines("paquetes_anteriores.txt")

# Leer la lista de paquetes
paquetes <- readLines("paquetes_anteriores.txt")
# Instalar los paquetes (si no están ya instalados)

paquetes_a_instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
install.packages(paquetes_a_instalar)
