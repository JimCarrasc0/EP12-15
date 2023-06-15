
sem15 <- c(67.0975, 65.20517, 66.46108, 69.68791, 67.25581, 76.00934, 66.03579, 75.89793, 69.54054, 64.58422, 66.01125)
sem20 <- c(68.53608, 68.70861, 67.6798, 71.75897, 71.24894, 77.59157, 67.70297, 77.45975, 71.33677, 66.71235, 69.30179)

datos <- data.frame(sem15, sem20)

# Hipótesis
# h0: En promedio, no existe diferencia en el peso de las peras entre semana 15 y semana 20
# hA: En promedio, existe diferencia en el peso de las peras entre semana 15 y 20
# 
# Hipótesis matemática
# h0: p15 - p20 = 0
# hA: p15 - p20 != 0

# Se pretende usar una prueba paramétrica 
# Se testea normalidad utilizando shapiro wilk
shapiro.test(sem15)
shapiro.test(sem20)

# Se testea normalidad con Q-Q
library(ggpubr)

ggqqplot(sem15)
ggqqplot(sem20)

# Como no pasa ninguna prueba de normalidad, se procede a utilizar la transformación
# Escalera de potencias de Tukey
library(rcompanion)
tukey15<- transformTukey(sem15)
tukey20<- transformTukey(sem20)

ggqqplot(tukey15)
ggqqplot(tukey20)

# Una vez transformados los datos, se procede a utilizar pruebas paramétricas

library(dplyr)
alfa <- 0.05
t.test(tukey15,tukey20, paired = TRUE, alternative = "two.sided", conf.level = 1 - alfa)

# se rechaza la hipótesis nula en favor de la hipótesis alternativa.
  

#Pregunta 2.
# Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
# método robusto adecuado.


# En promedio, la edad de hombres y mujeres solteros/as pertenecientes a la RM ¿es el mismo?

# Importando el archivo 
dir <- "C:/Users/Jx/Documents/Universidad/IME/EPs/EP12-15"
basename <- "EP11 Datos.csv"
file <- file.path(dir, basename)
poblacion <- read.csv2(file = file)

# Para realizar la pregunta

muestra <- poblacion %>% filter(ecivil == "Soltero(a)" &
                                  region == "Region Metropolitana de Santiago")
muestra <- muestra %>% select(sexo, ecivil, edad)

#Se prueba normalidad utilizando gráfico Q-Q
ggqqplot(muestra[["edad"]])

#Como no sigue una distribución normal se procede a usar el método de Yuen para muestras pareadas

#Pregunta 3.
# Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
# método robusto adecuado.

# En promedio, la edad de las personas es igual en las regiones de Tarapacá, Valparaíso, La Araucanía y Metropolitana de santiago
# ¿es el mismo?

