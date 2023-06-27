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
if (!require(ggpubr)) {
  install.packages("ggpubr",
                   dependencies = TRUE)
  require(ggpubr)
}

# Primeramente, se obtienen los datos de la actividad

datos <- read.csv2(file.choose(),
                   stringsAsFactors = TRUE)

################################################################################
############################# Pregunta 1 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 1 - Grupo 1 ########################\n")

# Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos
# del RUN (sin considerar el dígito verificador) del integrante de menor edad
# del equipo.

# El integrante de menor edad es                       : Ángel Vilches U.
# El rut del integrante de menor edad es               : 20.996.064-8
# Los últimos cuatro dígitos del RUN son               : 6 0 6 4
# A continuación se define la semilla según lo indicado:
set.seed(6064)
cat("Semilla establecida: 6 0 6 4\n")

################################################################################
############################# Pregunta 2 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 2 - Grupo 1 ########################\n")

# Seleccionar una muestra de 100 vinos.

# A continuación se extrae una muestra de 100 vinos de acuerdo con los datos
# obtenidos (EP13 Datos.csv)

muestra <- sample_n(datos, 100)
cat("Extracción de muestra de 100 vinos de acuerdo con los datos obtenidos\n")

################################################################################
############################# Pregunta 3 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 3 - Grupo 1 ########################\n")

# Seleccionar de forma aleatoria 6 posibles variables predictoras.

# Debemos considerar que se trabaja en torno a la predicción de la
# variable calidad. Por lo tanto, debemos evaluar qué variables podrían influir
# en este aspecto.

# La primera variable "clase", representa la categoría del vino por lo que no
# parece tener una relación directa con la calidad del vino y, por ende, no
# corresponde a una variable predictora.
# La segunda variable "calidad", corresponde a la variable objetivo que se desea
# predecir por lo que no responde a una variable predictora.
# La tercera variable "acidez.fija", corresponde a la la cantidad de ácido fijo
# en el vino. Dado que la ácidez podría influir en la calidad del vino, esta 
# variable corresponde a una variable predictora.
# La cuarta variable "acidez.volatil",  representa la cantidad de ácido volátil
# en el vino. Al igual que la variable anterior, la acidez podría afectar la
# calidad del vino, por lo que esta variable corresponde a una variable
# predictora.
# La quinta variable "acidez.citrico", representa la cantidad de ácido cítrico 
# en el vino. Al ser otro tipo de ácido, también podría tener un impacto en la
# calidad del vino y por ende responde a una variable predictora.
# La sexta variable "azucar.residual", representa la cantidad de azúcar residual
# en el vino. El contenido de azúcar podría influir en la percepción de la 
# calidad del vino, por lo que esta variable corresponde a una variable 
# predictora.
# La septima variable "cloruros", representa la cantidad de cloruros en el vino.
# Los cloruros podrían afectar el sabor y la calidad del vino, por lo que esta
# variable también responde a una variable predictora.
# La octava variable "dioxido.azufre.libre", representa la cantidad de dióxido
# de azufre libre en el vino. El dióxido de azufre se utiliza como conservante
# y puede influir en la calidad y estabilidad del vino, por lo que esta
# variable corresponde a una variable predictora.
# La novena variable "dioxido.azufre.total", representa la cantidad total de
# dióxido de azufre en el vino. Al igual que la variable anterior, el dióxido
# de azufre podría tener un impacto en la calidad del vino y responde a una
# variable predictora.
# La décima variable "densidad", mantiene relación con la calidad del vino, por
# ello, puede ser considerada una variable predictora.
# La décimo primera variable "ph", representa el pH del vino. El pH puede
# afectar el sabor del vino, por lo que esta variable corresponde a una 
# variable predictora.
# La décimo segunda variable "sulfatos", representa la cantidad de sulfatos en
# el vino. Los sulfatos también se utilizan como conservantes y podrían tener un
# impacto en la calidad del vino, por lo que esta variable responde a una
# variable predictora.
# La décimo tercera variable "alcohol", representa el contenido de alcohol en el
# vino. El contenido de alcohol podría ser una característica importante para
# determinar la calidad del vino, por lo que esta variable corresponde a una
# variable predictora.

variables_predictoras <- sample(names(muestra)[3:13], 6)
cat("Variables predictoras seleccionadas: \n")
print(variables_predictoras)

################################################################################
############################# Pregunta 4 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 4 - Grupo 1 ########################\n")

# Seleccionar, entre las variables que no fueron escogidas en el punto anterior,
# una que el equipo considere que podría ser útil para predecir la variable
# calidad, justificando bien esta selección.

# Se opta por seleccionar la variable "alcohol".
# En este sentido, el alcohol puede influir en varios aspectos de la experiencia
# de degustación, como el sabor, el cuerpo y la sensación. Por lo tanto, 
# seleccionar el contenido de alcohol como una variable adicional para predecir
# la calidad del vino es razonable y relevante para la predicción de la calidad.

variable_seleccionada <- names(muestra)[13]
cat("Variable predictora seleccionada: \n")
print(variable_seleccionada)

################################################################################
############################# Pregunta 5 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 5 - Grupo 1 ########################\n")

# Usando el entorno R, construir un modelo de regresión lineal simple con el
# predictor seleccionado en el paso anterior.

# Previo a la realización del modelo, se calcula la correlación entre la 
# variable calidad y el predictor seleccionado, es decir, pH

R <- cor(muestra[["alcohol"]], muestra[["calidad"]])
cat("Correlación: ", R)

# El valor de correlación anteriormente obtenido, este es, 0.418955. Sugiere una
# correlación positiva moderada entre el contenido de alcohol y la calidad del
# vino.

modelo_RLS <- lm(calidad ~ alcohol,
                 data = muestra)
print(summary(modelo_RLS))

# Se grafica el modelo
grafico_modelo_RLS <- ggscatter(datos,
                                x = "alcohol",
                                y = "calidad",
                                color = "purple",
                                fill = "purple",
                                xlab = "Alcohol",
                                ylab = "Calidad",
                                xticks.by = 1)
grafico_modelo_RLS <- grafico_modelo_RLS + geom_smooth(method = lm,
                                                       se = FALSE,
                                                       colour = "red")
print(grafico_modelo_RLS)

# Según lo graficado, se observa una que la línea roja indica la tendencia
# estimada de la relación entre la variable "alcohol" en el eje x y la variable
# "calidad" en el eje y. Dado que la línea roja se extiende desde el punto 5
# hasta el punto 7 en el eje y, se sugiere que a medida que aumenta el contenido
# de alcohol, se espera un aumento en la calidad del vino.
# La concentración de observaciones en los valores centrales del eje y (5, 6 y
# 7) indica que hay una mayor cantidad de vinos con calidades en ese rango
# específico en la muestra. Es decir, se indica que los vinos de calidad
# promedio (representados por los valores centrales) son más comunes en la
# muestra que los vinos de baja o alta calidad.
# La dispersión de puntos alrededor de la línea de regresión indica que, si bien
# existe una tendencia general de aumento en la calidad del vino con el
# contenido de alcohol, todavía hay cierta variabilidad en los datos. 

################################################################################
############################# Pregunta 6 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 6 - Grupo 1 ########################\n")

# Agregue la variable seleccionada en el paso 4 al conjunto obtenido en el
# punto 3.

variables_predictoras <- c(variables_predictoras, variable_seleccionada)

# Variables predictoras actualizadas
cat("Variables predictoras seleccionadas actualizadas: \n")
print(variables_predictoras)

################################################################################
############################# Pregunta 7 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 7 - Grupo 1 ########################\n")

# Usando herramientas para la exploración de modelos del entorno R, escoger 
# entre dos y cinco predictores de entre las variables presentes en el conjunto
# obtenido en el paso anterior para construir un modelo de regresión lineal
# múltiple.

modelo_RLM <- lm(calidad ~ .,
                 data = muestra[, c("calidad", variables_predictoras)])
print(summary(modelo_RLM))
grafico_modelo_RLM <- ggplot(data = muestra,
                             aes(x = calidad,
                                 y = predict(modelo_RLM))) +
                      geom_point(color = "purple",
                                 fill = "purple") +
                      geom_smooth(method = "lm",
                                  se = FALSE,
                                  color = "red") +
                      xlab("Calidad") +
                      ylab("Predicción") +
                      ggtitle("Modelo de Regresión Lineal Múltiple")
print(grafico_modelo_RLM)

################################################################################
############################# Pregunta 8 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 8 - Grupo 1 ########################\n")



################################################################################
############################# Pregunta 9 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 9 - Grupo 1 ########################\n")



















