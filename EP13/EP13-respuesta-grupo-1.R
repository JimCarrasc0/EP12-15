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

variables_predictoras_seleccionadas <- sample(variables_predictoras, 4)

modelo_RLM <- lm(calidad ~ .,
                 data = muestra[, c("calidad",
                                    variables_predictoras_seleccionadas)])
print(summary(modelo_RLM))

plot(modelo_RLM)

################################################################################
############################# Pregunta 8 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 8 - Grupo 1 ########################\n")

# Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con
# las condiciones que deben cumplir.

# Debido a que los métodos de regresión lineal fueron creados usando la
# función lm() se requiere de verificar las condiciones de las regresiones
# lineales creadas en base a esta función.
# Para lo anterior, se utilizarán los gráficos generados por la función lm()
# para evaluar la naturaleza de los residuos generados por la regresión lineal
# tanto simple como múltiple.

# ############### Modelo de regresión lineal simple. ##########################

# 1. Los datos deben representar una relación lineal.
#   Si se analiza el primer gráfico "Residuals vs Fitted", podemos notar
#   que ciertos grupos de los datos de la muestra presentan un patrón
#   de rectas descendentes a lo largo de la recta 0. Por consecuencia,
#   se puede concluir que los datos no representan una relación lineal.

# 2. La distribución de los residuos debe ser cercana a la normal.
#   Analizando el segundo gráfico "Normal Q-Q" podemos suponer que los
#   residuos siguen aproximadamente una distribución normal. No obstante,
#   la existencia de valores atípicos hace que esta conclusión no sea del
#   todo correcta.

# 3. La variabilidad de los puntos en torno a la línea de mínimos cuadrados 
# debe ser aproximadamente constante.
#   Observando los gráficos "Residuals vs Fitted" y "Scale-Location" 
#   se puede vislumbrar la misma situación que el punto 1, es decir, 
#   hay conjuntos de puntos que parecen seguir un patrón. Por lo anterior,
#   se deducir que la variabilidad de los residuos no es constante.

# 4. Las observaciones deben ser independientes entre sí. Esto significa 
# que no se puede usar regresión lineal con series de tiempo.
#   Considerando que las botellas seleccionadas son distintas entre sí y 
#   que el encargado del estudio se empeñó en no obtener una población
#   que tenga observaciones correlacionadas. Se supone que se cumple esta
#   condición.


# ############### Modelo de regresión lineal múltiple. ########################

# 1. Distribución de residuos con distribución cercana a la normal.
#   Dado a que en el gráfico "Normal Q-Q" se ven datos atípicos, se puede
#   deducir que la distribución de los residuos no sigue una distribución
#   normal.

# 2. La variabilidad de los residuos debe ser aproximadamente constante.
#   Debido a que en los gráficos "Residuals vs Fitted" y "Scale-Location" 
#   presentan que algunos grupos de residuos siguen algún patrón. Se deduce 
#   que estos no tienen una variabilidad constante.

# 3. Los residuos son independientes entre sí.
#   Para comprobar esta condición se optó por realizar un gráfico de 
#   residuos en base a su orden de observación y verificar que existe algún
#   patrón en el cual se presenten los residuos.

residuos <- resid(modelo_RLM)
plot(residuos ~ seq_along(residuos), ylab = "Residuos", 
     xlab = "Orden de las observaciones", 
     main = "Gráfico de Residuos vs. Orden de las Observaciones")

#   Al no visibilizarse un patrón se puede dar por cumplida esta
#   condición, es decir, los residuos son independientes entre sí.

# 4. Cada variable se relaciona linealmente con la respuesta.
#   Observando el gráfico "Residual vs Fitted", se puede disernir que hay grupos
#   de valores de residuos que siguen un patrón de rectas, por ende, se puede
#   concluir que las variables no se relacionan linealmente con la respuesta.



# Ante lo mencionado con anterioridad se concluye que los métodos de regresión
# lineal utilizados no son pertinentes, por tal razón se deben de realizar 
# modificaciones que permitan generar nuevos modelos predictores.
# Se tienen muchas alternativas para este propósito, pueden llegar desde hacer
# cambios menores, transformaciones de datos, hasta cambiar de modelo.
# Por propósito de simplificar este script se opta por solo mostrar la
# alternativa utilizada.


# ##################################### RLS.
# Después de muchas pruebas y cambios realizados se concluye lo siguiente:
#   1- Realizar transformaciones a la variable predictora de interés no genera
#     un cambio significativo en el modelo. Incluso, puede llegar a empeorarlo.

modelo_RLS <- lm(calidad ~ log(alcohol),
                 data = muestra)
print(summary(modelo_RLS))

# Se grafica el modelo
plot(modelo_RLS)

modelo_RLS <- lm(calidad ~ sqrt(alcohol),
                 data = muestra)
print(summary(modelo_RLS))

# Se grafica el modelo
plot(modelo_RLS)

modelo_RLS <- lm(calidad ~ 1/alcohol,
                 data = muestra)
print(summary(modelo_RLS))

# Se grafica el modelo
plot(modelo_RLS)

# 2- Cambiar de variable predictora no es una opción debido a que independiente
# el alcohol es quien mejor correlación tiene, e incluso algunos cambios de
# predictor generan modelos que no cumplen ninguna condición.
# Un ejemplo de esto es con el PH.
R <- cor(muestra[["ph"]], muestra[["calidad"]])
cat("Correlación: ", R)

modelo_RLS <- lm(calidad ~ ph,
                 data = muestra)
print(summary(modelo_RLS))

# Se grafica el modelo
plot(modelo_RLS)

# 3- Realizando un análisis gráfico de varias variables predictoras
# con la calidad se llegó a la siguiente conclusión.
ggplot(muestra, aes(x = alcohol, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Alcohol",
       y = "Calidad")

ggplot(muestra, aes(x = acidez.fija, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Acidez fija",
       y = "Calidad")

ggplot(muestra, aes(x = acidez.volatil, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Acidez volatil",
       y = "Calidad")

ggplot(muestra, aes(x = acido.citrico, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Acido citrico",
       y = "Calidad")

ggplot(muestra, aes(x = azucar.residual, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Azucar residual",
       y = "Calidad")

ggplot(muestra, aes(x = cloruros, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Cloruros",
       y = "Calidad")

ggplot(muestra, aes(x = dioxido.azufre.libre, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Dioxido de azufre libre",
       y = "Calidad")

ggplot(muestra, aes(x = dioxido.azufre.total, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Dioxido de azufre total",
       y = "Calidad")

ggplot(muestra, aes(x = densidad, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Densidad",
       y = "Calidad")

ggplot(muestra, aes(x = ph, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "PH",
       y = "Calidad")

ggplot(muestra, aes(x = sulfatos, y = calidad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión Lineal",
       x = "Sulfatos",
       y = "Calidad")

# Independiente de los cambios posibles a realizar, es imposible arreglar el
# modelo lineal para que se ajuste a una recta debido a la naturaleza de la
# variable calidad. Es decir, la naturaleza discreta de la variable calidad
# hace que existan comportamiento de bandas entre esta y el resto de las variables,
# logrando así que un modelo simple no pueda adaptarse a un modelo escalonado.

# La forma de poder mejorar esta situación es realizando un modelo no lineal,
# algo que se aleja de los alcances de esta actividad.


# ####################################### RLM
# Haciendo una transformación a las variables sucede lo mismo del caso
# anterior.
muestraA <- muestra
muestraA <- muestraA %>% mutate(alcohol = log(alcohol))
muestraA <- muestraA %>% mutate(cloruros = log(cloruros))
muestraA <- muestraA %>% mutate(acido.citrico = log(acido.citrico))
muestraA <- muestraA[is.finite(muestraA$acido.citrico), ]
muestraA <- muestraA %>% mutate(sulfatos = log(sulfatos))
muestraA <- muestraA %>% mutate(acidez.volatil = log(acidez.volatil))
muestraA <- muestraA %>% mutate(dioxido.azufre.total = log(dioxido.azufre.total))
muestraA <- muestraA %>% mutate(dioxido.azufre.libre = log(dioxido.azufre.libre))
modelo_RLM_A <- lm(calidad ~ .,
                 data = muestraA[, c("calidad", variables_predictoras)])
print(summary(modelo_RLM_A))

plot(modelo_RLM_A)

muestraA <- muestra
muestraA <- muestraA %>% mutate(alcohol = sqrt(alcohol))
muestraA <- muestraA %>% mutate(cloruros = sqrt(cloruros))
muestraA <- muestraA %>% mutate(acido.citrico = sqrt(acido.citrico))
muestraA <- muestraA[is.finite(muestraA$acido.citrico), ]
muestraA <- muestraA %>% mutate(sulfatos = sqrt(sulfatos))
muestraA <- muestraA %>% mutate(acidez.volatil = sqrt(acidez.volatil))
muestraA <- muestraA %>% mutate(dioxido.azufre.total = sqrt(dioxido.azufre.total))
muestraA <- muestraA %>% mutate(dioxido.azufre.libre = sqrt(dioxido.azufre.libre))
modelo_RLM_A <- lm(calidad ~ .,
                   data = muestraA[, c("calidad", variables_predictoras)])
print(summary(modelo_RLM_A))

plot(modelo_RLM_A)


# Añadir más variables no mejoró el modelo, incluso agregando todas este
# termina empeorando. La razón de esto se debe a que en si ya en el conjunto
# usado anteriormente se tienen las variables con mayor correlación.
variables_predictoras_A <- names(muestra)[3:13]
modelo_RLM_A <- lm(calidad ~ .,
                   data = muestra[, c("calidad", variables_predictoras)])
print(summary(modelo_RLM_A))

plot(modelo_RLM_A)

# Al final se extiende la situación vista en RLS. Que debido a la naturaleza
# de calidad, no se puede traducir de forma certera una relación escalonada
# a una línea recta.

################################################################################
############################# Pregunta 9 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 9 - Grupo 1 ########################\n")

# Evaluar el poder predictivo del modelo en datos no utilizados para construirlo
# (o utilizando validación cruzada).
# Crear conjuntos de entrenamiento y prueba
set.seed(125)  # Establecer semilla
n <- nrow(datos)
n_entrenamiento <- floor(0.7 * n)
muestra <- sample.int (n = n , size = n_entrenamiento, replace = FALSE)  
# 70% para entrenamiento
datos_entrenamiento <- datos[muestra, ]
datos_prueba <- datos[-muestra, ]

# Ajustar el modelo con el conjunto de entrenamiento
modelo <- lm(calidad ~ ., data = datos_entrenamiento)
print(summary(modelo))

# Calcular error cuadrado promedio para el conjunto de entrenamiento
mse_entrenamiento <- mean(modelo$residuals ** 2)
cat("Error cuadrático medio para el conjunto de entrenamiento:", mse_entrenamiento, "\n")

# Realizar predicciones para el conjunto de prueba
predicciones <- predict(modelo, datos_prueba)

# Calcular el error cuadrado promedio para el conjunto de prueba
mse_prueba <- mean((datos_prueba$calidad - predicciones)^2)
cat("Error cuadrático medio para el conjunto de prueba:", mse_prueba, "\n")



# Utilizando una semilla aleatoria de 125, se puede observar que al ser los errores
# cuadráticos medios para los conjuntos de entrenamiento y prueba iguales 
#a 0.5419986 y 0.5249593
# respectivamente, este modelo sí podría ser generalizable, ya que ambos valores
#son muy parecidos.
















