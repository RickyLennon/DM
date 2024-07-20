setwd()
datos <- read.csv("DatosMEM.csv", sep = ",")[c(1:37),-c(6:10)]

install.packages("MVN")
library(MVN)

mvn(datos, mvnTest = "mardia")

install.packages("MASS")
install.packages("dplyr")
library(MASS)
library(dplyr)

mean_vector <- colMeans(datos)
cov_matrix <- cov(datos)
n_new_samples <- 25
set.seed(123)
new_data <- mvrnorm(n = n_new_samples, mu = mean_vector, Sigma = cov_matrix)

new_data_df <- as.data.frame(new_data)
colnames(new_data_df) <- colnames(datos)

expanded_data <- bind_rows(datos, new_data_df)
mvn(expanded_data, mvnTest = "mardia")

