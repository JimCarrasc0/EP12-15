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
# variable calidad y el predictor seleccionado, es decir, 

R <- cor(muestra[["alcohol"]], muestra[["calidad"]])
cat("Correlación: ", R)

# El valor de correlación anteriormente obtenido, este es, 0.418955. Sugiere una
# correlación positiva moderada entre el contenido de alcohol y la calidad del
# vino.

modelo_RLS <- lm(calidad ~ alcohol,
                 data = muestra)
print(summary(modelo_RLS))

# Se grafica el modelo
plot(modelo_RLS)

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

plot(modelo_RLM)

################################################################################
############################# Pregunta 8 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 8 - Grupo 1 ########################\n")

# Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con
# las condiciones que deben cumplir.


# ############### Modelo de regresión lineal simple. ##########################
# Debido a que se utilizó la función lm() de R, se deduce que se realizó
# una regresión lineal mediante mínimos cuadrados. Además, se puede observar
# que este método fue utilizado sin verificar sus condiciones iniciales.
# En consecuencia a lo anterior, se realizara la evaluación del modelo desde
# ese aspecto.

# 1. Los datos deben representar una relación líneal.
#   Si se analiza el primer gráfico "Residuals vs Fitted", podemos notar
#   que ciertos grupos de los datos de la muestra presentan un patrón
#   de rectas descendentes a lo largo de la recta 0. Por consecuencia,
#   se puede concluir que los datos no representan una relación líneal.

# 2. La distribución de los residuos debe ser cercana a la normal.
#   Analizando el segundo gráfico "Normal Q-Q" podemos suponer que los
#   residuos siguen aproximadamente una distribución normal. No obstante,
#   la existencia de valores atípicos hace que esta conclusión no sea del
#   todo correcta.

# 3. La variabilidad de los puntos en torno a la línea de mínimos cuadrados 
# debe ser aproximadamente constante.
#   Observando los gráficos 1 y 3 ("Resicuals vs Fitted" 
#   y "Scale-Location" respectivamente) se visualizan
#   patrones en la organización de los residuos, por ende, no hay 
#   variabilidad constante en los puntos.

# 4. Las observaciones deben ser independientes entre sí. Esto significa 
# que no se puede usar regresión lineal con series de tiempo.
#   Los datos al no ser una serie de tiempo, sino que solo una recolección
#   de distintas botellas de vino. Por consecuencia, se concluye que
#   las observaciones son independientes entre sí.


# ############### Modelo de regresión lineal múltiple. ########################

# 1. Distribución de residuos con distribución cercana a la normal.
#   Dado a que en el gráfico "Normal Q-Q" se ven datos atípicos, se puede
#   deducir que la distibución de los residuos no sigue una distribución
#   normal.

# 2. La variabilidad de los residuos debe ser aproximadamente constante.
#   Debido a que en los gráficos 1 y 3 presentan que algunos grupos de
#   residuos siguen algún patrón. Se deduce que estos no tienen una 
#   variabilidad constante.

# 3. Los residuos son independientes entre sí.
#   Para comprobar esta condición se optó por realizar un gráfico de 
#   residuos en base a su orden de observación (literalmente es el orden
#   en el cual se presentan).

residuos <- resid(modelo_RLM)
plot(residuos ~ seq_along(residuos), ylab = "Residuos", 
     xlab = "Orden de las observaciones", 
     main = "Gráfico de Residuos vs. Orden de las Observaciones")

#   Al no presentarse un patrón visible se puede dar por cumplida esta
#   condición.

# 4. Cada variable se relaciona linealmente con la respuesta.
#   Observando el gráfico Residual vs Fitted, se puede disernir que hay grupos
#   de valores de residuos que siguen un patron de rectas, por ende, se puede
#   concluir que las variables no se relacionan linealmente con la respuesta.

################################################################################
############################# Pregunta 9 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 9 - Grupo 1 ########################\n")

# Evaluar el poder predictivo del modelo en datos no utilizados para construirlo
# (o utilizando validación cruzada).

















