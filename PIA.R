setwd()
datos <- read.csv("DatosMEM.csv", sep = ",")[c(1:37),-c(6:10)]

install.packages("MVN")
install.packages("dplyr")
install.packages("MASS")
library(MASS)
library(dplyr)
library(MVN)

#Utilizamos la librería MVN para comprobar que nuestros datos se distribuyan normal multivariada
mvn(datos, mvnTest = "mardia")

#Utilizamos la librería MASS para poder generar los elementos restantes a nuestro conjunto de datos
#y que se distribuyan también normal multivariada
mean_vector <- colMeans(datos)
cov_matrix <- cov(datos)
n_new_samples <- 25
set.seed(123)
new_data <- mvrnorm(n = n_new_samples, mu = mean_vector, Sigma = cov_matrix)

new_data_df <- as.data.frame(new_data)
colnames(new_data_df) <- colnames(datos)

#Utilizamos la librería dplyr para poder anexar los nuevos elementos generados a los elementos
#del conjunto original
expanded_data <- bind_rows(datos, new_data_df)

#Comprobamos si el conjunto de datos nuevo sigue una distribución normal multivariada
mvn(expanded_data, mvnTest = "royston")
mvn(expanded_data, mvnTest = "mardia")

mean_vector <- colMeans(expanded_data)
cov_matrix <- cov(expanded_data)
cor_matrix <- cor(expanded_data)

# Al realizar la prueba de normalidad en el conjunto de datos obtenemos que las 5 columnas del conjunto de datos tienen una distribución normal multivariada.
# Los datos tambien cuentan con una curtosis que es consistente con una distribución normal multivariada.

# Obtenemos graficas para mejorr el analisis exploratorio de los datos.
# Definimos la ruta para guardar las imágenes
output_dir <- "images/"

install.packages("ggplot2")
library(ggplot2)

#empezamos con estadísticas descriptivas

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

# Crear y guardar histogramas para cada columna
create_histogram(expanded_data, "Tesla")
create_histogram(expanded_data, "Meta")
create_histogram(expanded_data, "Amazon")
create_histogram(expanded_data, "Microsoft")
create_histogram(expanded_data, "CEMEX")

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

print(histograma_medias)
ggsave("images/histograma_medias.png", plot = histograma_medias)

install.packages("reshape2")
library(reshape2)

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

cor_matrix_melt <- melt(cor_matrix)

# Graficar el heatmap de la matriz de correlación
heatmap_cor <- ggplot(cor_matrix_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +  # Delineado negro
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(min(cor_matrix_melt$value), max(cor_matrix_melt$value)), 
                       space = "Lab", name="Correlación") +
  labs(title = "Matriz de Correlación", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

print(heatmap_cor)
ggsave("images/heatmap_correlacion.png", plot = heatmap_cor)
