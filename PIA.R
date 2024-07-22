#install.packages("MVN")
#install.packages("MASS")
#install.packages("dplyr")
#Realizamos las configuraciones iniciales.
setwd("D:/Documents/MASTER/2do Cuatri/MEM/PIA/MEM")
datos <- read.csv("DatosMEM.csv", sep = ",")[c(1:50),-c(6:10)]

#Cargamos las librerias necesarias.
library(MVN)
library(MASS)
library(dplyr)
library(ggplot2)


# Obtenemos graficas para mejorar el analisis exploratorio de los datos.
# Definimos la ruta para guardar las imágenes
output_dir <- "images/"


########## Parte 1. Analisis exploratorio de datos.  ##############################

summary(datos)


# Función para crear y guardar histogramas
create_histogram <- function(data, column_name) {
  histograma <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(bins = 9, fill = "skyBlue", color = "black") +
    labs(title = paste("Distribución de los datos de", column_name), x = "Precio de la acción", y = "Frecuencia") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5)  # Centramos el título
    )
  
  # Mostrar y guardar el histograma
  print(histograma)
  ggsave(paste(output_dir, "histograma_", tolower(column_name), ".png", sep = ""), plot = histograma)
}

# Crear y guardar histogramas para cada columna de nuestros datos.
create_histogram(datos, "Tesla")
create_histogram(datos, "Meta")
create_histogram(datos, "Amazon")
create_histogram(datos, "Microsoft")
create_histogram(datos, "CEMEX")

#Vector de medias.
mean_vector <- colMeans(datos)
#Matriz de covarianzas
cov_matrix <- cov(datos)
#Matriz de correlación
cor_matrix<-cor(datos)

#Vector de medias para la grafica.
mean_data <- data.frame(
  Empresa = c("Tesla", "Meta", "Amazon", "Microsoft", "CEMEX"),
  Media = c(mean_vector[1], mean_vector[2], mean_vector[3], mean_vector[4], mean_vector[5])
)

# Crear el gráfico de barras
histograma_medias<-ggplot(mean_data, aes(x = Empresa, y = Media, fill = Empresa)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Vector de Medias de las Variables", x = "Empresa", y = "Media") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

ggsave("images/histograma_medias.png", plot = histograma_medias)




heatmap_correlation<-ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  ggtitle("Mapa de Calor de Correlación") 
ggsave("images/heatmap_correlation.png", plot = heatmap_correlation)


cov_matrix_melt <- melt(cov_matrix)

# Graficar el heatmap de la matriz de covarianza
heatmap_cov <- ggplot(cov_matrix_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(min(cov_matrix_melt$value), max(cov_matrix_melt$value)), 
                       space = "Lab", name="Covarianza") +
  labs(title = "Matriz de Covarianza", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

print(heatmap_cov)
ggsave("images/heatmap_covarianza.png", plot = heatmap_cov)



########## Parte 2. Pruebas de bondad de ajuste.  ##############################
mvn(datos, mvnTest = "royston")
# Al realizar la prueba de normalidad en el conjunto de datos obtenemos que las 5 columnas del conjunto de datos tienen una distribución normal.
# Ademas, por medio de la prueba de Royston podemos ver que todo nuestro conjunto cuenta con una distribución normal multivariada.



########## Parte 3. Planteamientos de comparativas.  ##############################
### Empezamos a hacer el planteamiento de variables para la prueba de Barlett. ###
p <- ncol(datos)
n <- nrow(datos)
R <- cor(datos)
# H0: Las variables son independientes.
# Ha: Las variables no son independientes
# Calcular estadístico de prueba
EP<- -2 * (1 - ((2 * p + 11) / (6 * n))) * log(det(R)**(50 / 2)) 
# Calcular valor crítico
qchisq(1 - 0.05, p * (p - 1) / 2) 
# Se rechaza H0 si EP=86.24749 > 18.30704.
# Por lo tanto, se rechaza la hipotesis nula. Las variables no son independientes
### Podemos ver que 
p_valor <- 1 - pchisq(EP, p * (p - 1) / 2)


### Prueba de medias ###############
#H0: Las medias para las distintas compañias seran Tesla=250, Meta=300,Amazon=130, Microsoft=330, CEMEX=8
#Ha: Las medias para las distintas compañias seran difererentes a Tesla=250, Meta=300,Amazon=130, Microsoft=330, CEMEX=8









