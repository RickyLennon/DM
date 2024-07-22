#install.packages("MVN")
#install.packages("dplyr")
#Realizamos las configuraciones iniciales.
setwd("D:/Documents/MASTER/2do Cuatri/MEM/PIA/MEM")
datos <- read.csv("DatosMEM.csv", sep = ",")[c(1:50),-c(6:10)]

#Cargamos las librerias necesarias.
library(MVN)
library(MASS)
library(dplyr)
library(ggplot2)
library(reshape2)

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
p <- ncol(datos)  # número de variables
n <- nrow(datos) # número de observaciones
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

mu_hipotesis <- matrix(c(250,300,130,330,8), ncol = 1)
mu_verdadera<-matrix(colMeans(datos), ncol = 1)

#Obtenemos la inversa de la matriz de covarianza
inv_cov<-solve(cov_matrix)
#Calculamos el estadistico T²
t2 <- n * t(mu_verdadera - mu_hipotesis) %*% inv_cov %*% (mu_verdadera - mu_hipotesis)

#Calculamos el valor p
1 - pf((n - p) * t2 / ((n - 1) * p), p, n - p)



# Grados de libertad
df1 <- p # Grados de libertad del numerador
df2 <- n - p # Grados de libertad del denominador
alpha<-0.05 #nivel de significancia

# Convertir el estadístico T^2 a F
F_stat <- ( (n - p) / (p * (n - 1)) ) * t2

# Obtener el valor crítico de F
F_crit <- qf(1 - alpha, df1, df2)


# Calcular factor auxiliar para los intervalos de confianza
aux <- qf(1 - 0.05, p, n - p) * p * (n - 1) / (n * (n - p))

# Calcular e imprimir intervalos de confianza
ic <- function(media, varianza) {
  sqrt_aux <- sqrt(aux * varianza)
  c(media - sqrt_aux, media + sqrt_aux)
}

ic_values <- sapply(1:p, function(i) ic(mu_verdadera[i], cov_matrix[i, i]))
rownames(ic_values) <- c("Límite inferior", "Límite superior")
colnames(ic_values) <- c("Tesla", "Meta", "Amazon", "Microsoft","CEMEX")

#############Prueba de varianzas #################
# Las varianzas de los precios de las acciones son iguales entre las compañías Tesla, Meta, Amazon, Microsoft y CEMEX.
# Las varianzas de los precios de las acciones no son iguales entre las compañías Tesla, Meta, Amazon, Microsoft y CEMEX.
library(tidyr)
datos_largos <- pivot_longer(datos, cols = everything(), names_to = "Compania", values_to = "Precio")

# Realizar la prueba de Bartlett
resultado_bartlett <- bartlett.test(Precio ~ Compania, data = datos_largos)

# Mostrar los resultados
print(resultado_bartlett)
# Nivel de significancia (alpha)
alpha <- 0.05

# Grados de libertad
df <- 4

# Valor crítico
valor_critico <- qchisq(1 - alpha, df)
print(valor_critico)

# Función para calcular intervalos de confianza para varianzas
ic_varianza <- function(varianza, n, alpha) {
  chi2_lower <- qchisq(alpha / 2, df = n - 1)
  chi2_upper <- qchisq(1 - alpha / 2, df = n - 1)
  
  intervalo_inferior <- (n - 1) * varianza / chi2_upper
  intervalo_superior <- (n - 1) * varianza / chi2_lower
  
  c(intervalo_inferior, intervalo_superior)
}
# Nivel de significancia
alpha <- 0.05

# Número de compañías
compañias <- colnames(datos)

# Inicializar una matriz para los intervalos de confianza
ic_varianzas <- matrix(nrow = 2, ncol = length(compañias))
rownames(ic_varianzas) <- c("Límite Inferior", "Límite Superior")
colnames(ic_varianzas) <- compañias

# Calcular intervalos de confianza para cada compañía
for (compania in compañias) {
  datos_compania <- datos[[compania]]
  varianza_compania <- var(datos_compania)
  n_compania <- length(datos_compania)
  
  ic <- ic_varianza(varianza_compania, n_compania, alpha)
  ic_varianzas[, compania] <- ic
}

# Imprimir los intervalos de confianza
print(ic_varianzas)




###########################################
####  Parte 4 ####

#Prueba de componentes principales (4a)
fviz_eig(prcomp(scale(datos)), addlabels = T)
summary(prcomp(scale(datos)))

Itemx <- matrix(c(230.039993, 298.346924, 139.940002, 318.518341, 6.658184), ncol = 5)

e <- prcomp(scale(datos), rank. = 3)
e1 <- as.data.frame(e$x) #nuevos items, ya transformados
e1$Fecha <- datosn$Fecha

x_prime_scaled <- scale(Itemx, center = attr(scale(datos), "scaled:center"), scale = attr(scale(datos), "scaled:scale"))
x_prime_pca <- as.data.frame(x_prime_scaled %*% e$rotation[, 1:3])

#7.23
m1 <- datos[1:9,] 
mm1 <- matrix(colMeans(m1), ncol = 1)
sm1 <- cov(m1)

#8.23
m2 <- datos[10:32,]
mm2 <- matrix(colMeans(m2), ncol = 1)
sm2 <- cov(m2)

#9.23
m3 <- datos[33:50,]
mm3 <- matrix(colMeans(m3), ncol = 1)
sm3 <- cov(m3)

n <- matrix(c(230.039993, 298.346924, 139.940002, 318.518341, 6.658184), ncol = 1)
Sp <- ((8*sm1)+(22*sm2)+(17*sm3))/(50-5)

#7.23 
t(mm1)%*%solve(Sp)%*%n-(1/2)*t(mm1)%*%solve(Sp)%*%(mm1)
#8.23
t(mm2)%*%solve(Sp)%*%n-(1/2)*t(mm2)%*%solve(Sp)%*%(mm2)
#9.23 
t(mm3)%*%solve(Sp)%*%n-(1/2)*t(mm3)%*%solve(Sp)%*%(mm3)

n7.23 <- e1[e1$Fecha == "7.23", ]
n8.23 <- e1[e1$Fecha == "8.23", ]
n9.23 <- e1[e1$Fecha == "9.23", ]
colnames(x_prime_pca) <- c("PC1", "PC2", "PC3")

# Crear el gráfico 3D con plot3d
plot3d(x = n7.23$PC1, y = n7.23$PC2, z = n7.23$PC3, col = "blue", size = 10, 
       xlab = "Principal Componente 1", ylab = "Principal Componente 2", zlab = "Principal Componente 3", 
       xlim = c(min(e1$PC1) - 1, max(e1$PC1) + 1), 
       ylim = c(min(e1$PC2) - 1, max(e1$PC2) + 1), 
       zlim = c(min(e1$PC3) - 1, max(e1$PC3) + 1))

text3d(x_prime_pca$PC1, x_prime_pca$PC2, x_prime_pca$PC3, texts = "*", col = "black", cex = 2)
points3d(n8.23$PC1, n8.23$PC2, n8.23$PC3, col = "red", size = 10)
points3d(n9.23$PC1, n9.23$PC2, n9.23$PC3, col = "green", size = 10)

legend_x <- max(e1$PC1) 
legend_y <- max(e1$PC2)
legend_z <- max(e1$PC3)

# Agregar texto de leyenda
text3d(legend_x + 0.5, legend_y, legend_z, texts = "n7.23", col = "blue")
text3d(legend_x + 0.5, legend_y - 0.5, legend_z, texts = "n8.23", col = "red")
text3d(legend_x + 0.5, legend_y - 1, legend_z, texts = "n9.23", col = "green")

# Definir las coordenadas de los vértices del cubo
xrange <- range(e1$PC1) + c(-1, 1)
yrange <- range(e1$PC2) + c(-1, 1)
zrange <- range(e1$PC3) + c(-1, 1)

vertices <- expand.grid(x = xrange, y = yrange, z = zrange)

# Dibujar el cubo con líneas más gruesas
segments3d(rbind(vertices[c(1, 2), ], vertices[c(1, 3), ], vertices[c(1, 5), ],
                 vertices[c(2, 4), ], vertices[c(2, 6), ], vertices[c(3, 4), ],
                 vertices[c(3, 7), ], vertices[c(4, 8), ], vertices[c(5, 6), ],
                 vertices[c(5, 7), ], vertices[c(6, 8), ], vertices[c(7, 8), ]),
           lwd = 3)  # Ajusta el grosor de las líneas


#Prueba de equicorrelación (4b)
#matriz de correlación

#r barra
r.b <- 2*(sum(cor_matrix[cor_matrix!=1]))/(5*4)
gam <- ((5-1)**2)*(1-(1-r.b)**2)/(5-(5-2)*(1-r.b)**2)
r1 <- sum(cor_matrix[-1,1])/(5-1)
r2 <- sum(cor_matrix[-2,2])/(5-1) 
r3 <- sum(cor_matrix[-3,3])/(5-1)
r4 <- sum(cor_matrix[-4,4])/(5-1)
r4 <- sum(cor_matrix[-5,5])/(5-1)

T2 <- ((50-1)/(1-r.b)**2)*(sum((cor_matrix[row(cor_matrix) > col(cor_matrix)]-r.b)**2))-gam
#RR
qchisq(1-0.05,(5+1)*(5-2)/2)
#H0: Solo hay un valor propio en la matriz de correlación 
#H0: No hay solo un valor propio en la matriz de correlación 
#Rechazamos H0 si T2 (137.5317) > RR (16.91898)
#Rechazamos H0
#No hay solo un valor propio en la matriz de correlación 
#las correlaciones entre las variables no son todas iguales; 
#hay una variabilidad en cómo cada par de variables se correlaciona, 
#lo que se refleja en la presencia de múltiples valores propios distintos de cero.

#Aquí nos referimos a la distribución de los valores propios de dicha matriz