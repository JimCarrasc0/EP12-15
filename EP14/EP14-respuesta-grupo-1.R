# Integrantes - Grupo 1
# Jaime Carrasco Quintrequeo
# Andrés Haussmann Garín
# Benjamín González Hurtado
# Ángel Vilches Urrutia

# Librerías

if (!require(dplyr)) {
  install.packages("dplyr",
                   dependencies = TRUE)
  require(dplyr)
}

# Primeramente, se obtienen los datos de la actividad

datos <- read.csv2(file.choose(),
                   stringsAsFactors = TRUE)

cat("Se construirá un modelo de regresión logística para predecir la variable clase, de acuerdo con las siguientes instrucciones otorgadas en el enunciado de la presente actividad")

################################################################################
############################# Pregunta 1 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 1 - Grupo 1 ########################\n")

# Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos 
# del RUN (sin considerar el dígito verificador) del integrante de mayor edad
# del equipo.

# El integrante de mayor edad es                       : Andrés Haussmann G. 
# El rut del integrante de menor edad es               : 18.972.186-2
# Los últimos cuatro dígitos del RUN son               : 2 1 8 6
# A continuación se define la semilla según lo indicado:
set.seed(2186)
cat("Semilla establecida: 2 1 8 6\n")

################################################################################
############################# Pregunta 2 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 2 - Grupo 1 ########################\n")

# Seleccionar una muestra de 120 vinos, asegurando que la mitad sean blancos y
# la otra mitad, tintos. Dividir esta muestra en dos conjuntos: los datos de 80
# vinos (40 con clase “Blanco”) para utilizar en la construcción de los modelos
# y 40 vinos (20 con clase “Blanco”) para poder evaluarlos.

vinos_blancos <- subset(datos,
                        clase == "Blanco")
vinos_tintos <- subset(datos,
                       clase == "Tinto")
muestra_blancos <- vinos_blancos[sample(nrow(vinos_blancos),
                                        60), ]
muestra_tintos <- vinos_tintos[sample(nrow(vinos_tintos),
                                      60), ]

# En este punto, se dispone de dos muestras cuya calidad refiere a blanco y 
# tinto. Ambas muestras de tamaño: 60. Se procede a unificar las muestras

muestra_final <- rbind(muestra_blancos,
                       muestra_tintos)

# A continuación se integra una columna cuyo valor varía entre 0 y 1 en función
# de la clase del vino. Si es tinto el valor es 1, por el contrario, si es
# blanco el valor es 0.

muestra_final$clase_num <- as.numeric(muestra_final$clase == "Tinto")

# Se divide la muestra en dos conjuntos: 80 vinos (40 de clase "Blanco") y 40
# vinos (20 de clase "Blanco")

conjunto_entrenamiento <- muestra_final %>% 
                          group_by(clase) %>% 
                          sample_n(size = 40)

conjunto_evaluacion <- setdiff(muestra_final,
                               conjunto_entrenamiento)

cat("Se confeccionan dos conjuntos de datos, uno para utilizar en la construcción (con 80 datos de vinos donde 40 de ellos son de clase 'Blanco') y otro para su respectiva evaluación (con 40 datos de vinos donde 20 de ellos son de clase 'Blanco')")

################################################################################
############################# Pregunta 3 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 3 - Grupo 1 ########################\n")

# Seleccionar 6 variables predictoras de manera aleatoria (al igual que en el
# ejercicio anterior).

variables_predictoras <- sample(names(muestra_final)[3:13], 6)
cat("Variables predictoras seleccionadas: \n")
print(variables_predictoras)

################################################################################
############################# Pregunta 4 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 4 - Grupo 1 ########################\n")

# Seleccionar, de las otras variables, una que el equipo considere que podría
# ser útil para predecir la clase, justificando bien esta selección.

# Se opta por seleccionar la variable "densidad".
# En este sentido, la densidad es una variable útil para predecir la clase de un
# vino debido a que está relacionada con factores como el contenido de azúcar y
# alcohol, los cuales pueden variar entre vinos "Blancos" y "Tintos". Por lo 
# tanto, la variable "densidad" se considera útil para predecir la clase.

variable_seleccionada <- names(muestra_final)[10]
cat("Variable predictora seleccionada: \n")
print(variable_seleccionada)

################################################################################
############################# Pregunta 5 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 5 - Grupo 1 ########################\n")

# Usando el entorno R y paquetes estándares, construir un modelo de regresión
# logística con el predictor seleccionado en el paso anterior y utilizando de la
# muestra obtenida.

# Crear el modelo de regresión logística
modelo_rlogistico <- glm(clase_num ~ densidad,
                         data = conjunto_entrenamiento,
                         family = binomial(link = "logit"))
print(summary(modelo_rlogistico))

plot(modelo_rlogistico)

################################################################################
############################# Pregunta 6 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 6 - Grupo 1 ########################\n")

# Agregue la variable seleccionada en el paso 4 al conjunto obtenido en el punto
# 3.

variables_predictoras <- c(variables_predictoras, variable_seleccionada)
cat("Conjunto de variables predictoras final: \n")
print(variables_predictoras)

################################################################################
############################# Pregunta 7 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 7 - Grupo 1 ########################\n")

# Usando herramientas estándares para la exploración de modelos del entorno R,
# buscar entre dos y cinco predictores de entre las variables presentes en el
# conjunto obtenido en el paso anterior para construir un modelo de regresión
# logística múltiple.

variables_predictoras_seleccionadas <- sample(variables_predictoras, 3)
modelo_rlogm <- glm(calidad ~ ., data = muestra_final[,c("calidad", variables_predictoras_seleccionadas)])
print(summary(modelo_rlogm))
plot(modelo_rlogm)


################################################################################
############################# Pregunta 8 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 8 - Grupo 1 ########################\n")

# Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de
# ajuste y son generalizables) y “arreglarlos” en caso de que tengan algún
# problema.


################################################################################
############################# Pregunta 9 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 9 - Grupo 1 ########################\n")

# Usando herramientas del paquete caret, evaluar el poder predictivo de los
# modelos con los datos de los 40 vinos que no se incluyeron en su construcción
# en términos de sensibilidad y especificidad.















