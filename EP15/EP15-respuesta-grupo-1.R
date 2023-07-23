# Integrantes - Grupo 1
# Jaime Carrasco Quintrequeo
# Andrés Haussmann Garín
# Benjamín González Hurtado
# Ángel Vilches Urrutia

# Librerías

if (!require(caret)) {
  install.packages("caret",
                   dependencies = TRUE)
  require(caret)
}

if (!require(leaps)) {
  install.packages("leaps",
                   dependencies = TRUE)
  require(leaps)
}

# Primeramente, se obtienen los datos de la actividad

datos <- read.csv2(file.choose(),
                   stringsAsFactors = TRUE)

################################################################################
############################# Pregunta 1 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 1 - Grupo 1 ########################\n")

# Definir la semilla a utilizar, que corresponde a los primeros cinco dígitos
# del RUN del integrante de mayor edad del equipo

# El integrante de mayor edad es                       : Andrés Haussmann G. 
# El rut del integrante de menor edad es               : 18.972.186-2
# Los últimos cuatro dígitos del RUN son               : 1 8 9 7 2
# A continuación se define la semilla según lo indicado:
set.seed(18972)
cat("Semilla establecida: 1 8 9 7 2\n")

################################################################################
############################# Pregunta 2 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 2 - Grupo 1 ########################\n")

# Seleccionar una muestra de 100 vinos, asegurando que la mitad sean blancos y
# la otra mitad, tintos.

vinos_blancos <- subset(datos,
                        clase == "Blanco")
vinos_tintos <- subset(datos,
                       clase == "Tinto")
muestra_blancos <- vinos_blancos[sample(nrow(vinos_blancos),
                                        50), ]
muestra_tintos <- vinos_tintos[sample(nrow(vinos_tintos),
                                      50), ]

# En este punto, se dispone de dos muestras cuya calidad refiere a blanco y 
# tinto. Ambas muestras de tamaño: 50. Se procede a unificar las muestras

muestra_final <- rbind(muestra_blancos,
                       muestra_tintos)

################################################################################
############################# Pregunta 3 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 3 - Grupo 1 ########################\n")

# Usando las herramientas del paquete leaps, realizar una búsqueda exhaustiva 
# para seleccionar entre 2 y 6 predictores que ayuden a estimar la variable 
# calidad y luego utilizar las funciones del paquete caret para construir un
# modelo de regresión lineal múltiple con los predictores escogidos y evaluarlo
# usando bootstrapping.

res <- regsubsets(calidad ~ .,
           data   = datos,
           nbest  = 6,
           nvmax  = 6,
           method = "exhaustive")
plot(res)

# Seleccionar el mejor conjunto de predictores basado en el rendimiento
mjrs_predictores <- names(coef(res,
                               which.max(summary(res)$adjr2)))

# Omitir la variable "Intercept" del conjunto de predictores
mjrs_predictores <- mjrs_predictores[mjrs_predictores != "(Intercept)"]

# Construir el modelo de regresión lineal múltiple utilizando caret
modelo_RLM <- lm(calidad ~ .,
                 data = muestra_final[, c("calidad",
                                          mjrs_predictores)])

# Evaluar el modelo utilizando bootstrapping con caret
boot_control <- trainControl(method = "boot",
                             number = 100)
modelo_RLM_boot <- train(calidad ~ .,
                         data = muestra_final[, c("calidad",
                                                  mjrs_predictores)],
                         method = "lm", trControl = boot_control)

# Ver el resumen del rendimiento del modelo con bootstrapping
print(modelo_RLM_boot)


################################################################################
############################# Pregunta 4 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 4 - Grupo 1 ########################\n")

# Haciendo un poco de investigación sobre el paquete caret, en particular cómo 
# hacer Recursive Feature Elimination (RFE), construir un modelo de regresión 
# lineal múltiple para predecir la variable calidad que incluya entre 2 y 6 
# predictores, seleccionando el conjunto de variables que maximice R2 y que use 
# cinco repeticiones de validación cruzada de cinco pliegues para evitar el 
# sobreajuste.

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

rlm <- rfe(x=muestra_final[, c("calidad")], y=muestra_final$calidad, sizes = 2:6, rfeControl = ctrl)

print(rlm)
seleccion <- predict(result, type = "variables", fit = TRUE)

modelo_final <- lm(calidad ~ ., data = datos[, c("calidad", seleccion)])
summary(modelo_final)

print(modelo_final)