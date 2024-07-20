install.packages("MVN")
library("MVN")


setwd("/Users/astro/Desktop/MCD/Tetra 2/MEM")
datos <- read.table("Datos MEM.csv", sep = ",", header = T)
mvn(datos, mvnTest = "hz")


help("mvn")
"Si usamos la prube Henze-Zirkler, hay que buscar cuál es su estadístico de prueba y su
región de rechazo"