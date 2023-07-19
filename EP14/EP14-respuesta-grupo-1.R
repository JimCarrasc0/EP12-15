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

if (!require(caret)) {
  install.packages("caret",
                   dependencies = TRUE)
  require(caret)
}

if (!require(pROC)) {
  install.packages("pROC",
                   dependencies = TRUE)
  require(pROC)
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
modelo_rlogm <- glm(clase_num ~ ., data = muestra_final[,c("clase_num", variables_predictoras_seleccionadas)])
print(summary(modelo_rlogm))
plot(modelo_rlogm)


################################################################################
############################# Pregunta 8 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 8 - Grupo 1 ########################\n")

# Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de
# ajuste y son generalizables) y “arreglarlos” en caso de que tengan algún
# problema.

# Para esta sección los modelos serán clasificados durante 4 secciones, siendo
# la siguientes. 
#     * Confiabilidad de clasificadores.
#     * Bondad de ajuste.
#     * Condiciones.
#     * Generalización del modelo.
# La razón por la cual no se quiere quedarse solo con condiciones es para 
# evaluar que tan valioso es en términos de confianza el modelo construido.
# Tanto el simple como el múltiple.

cat("############################## Regresión logística simple.")

# Predicciones con datos de entrenamiento.
predicciones <- predict(modelo_rlogistico, 
                        newdata = conjunto_entrenamiento, 
                        type = "response")

################## Confiabilidad de clasificadores #############

# Buscamos verificar que tan confiables son las predicciones del modelo
# respecto a los valores reales dentro de lo que es el conjunto de 
# entrenamiento.

# Obtener la matriz de confusión.
umbral <- 0.5 # Umbral para clasificar como 0 o 1
predicciones_clasificadas <- ifelse(predicciones > umbral, 1, 0)
matriz_confusion <- table(conjunto_entrenamiento$clase_num, 
                          predicciones_clasificadas)

# Imprimir matriz de confusión.
print(matriz_confusion)

# A simpe vista se puede observar que en términos de confiabilidad el modelo
# es bastante funcional, aunque para tener una mayor seguridad se opta por
# también utilizar las métricas derivadas de esta matriz.

# Obtener los valores de la matriz de confusión
VP <- matriz_confusion[2, 2]  # Verdaderos positivos
VN <- matriz_confusion[1, 1]  # Verdaderos negativos
FP <- matriz_confusion[1, 2]  # Falsos positivos
FN <- matriz_confusion[2, 1]  # Falsos negativos

# Métricas de rendimiento
precision <- (VP + VN) / (VP + VN + FP + FN)  # Precisión (Accuracy)
sensibilidad <- VP / (VP + FN)                # Sensibilidad (Recall)
especificidad <- VN / (VN + FP)               # Especificidad
ppv <- VP / (VP + FP)                         # Valor predictivo positivo (Precision)
npv <- VN / (VN + FN)                         # Valor predictivo negativo

# Imprimir las métricas
cat("Precisión (Accuracy):", precision, "\n")
cat("Sensibilidad (Recall):", sensibilidad, "\n")
cat("Especificidad:", especificidad, "\n")
cat("Valor predictivo positivo (Precision):", ppv, "\n")
cat("Valor predictivo negativo:", npv, "\n")

# Observando las métricas correspondientes se puede entender que el modelo
# es altamente confiable al momento de predecir los datos, por ende, este
# punto se puede dar como valido.

########################## Bondad de ajuste ####################

# Si utilizamos cálculos para la curva ROC.
roc_curve <- roc(conjunto_entrenamiento$clase_num, predicciones)
auc_score <- auc(roc_curve)
plot(roc_curve, main = "Curva ROC RligS trainning")

# Podemos observar un fuerte alejamiento de la curva frente a la diagonal, lo cual
# es un fuerte indicador de una buena bondad de ajuste, es decir, el modelo
# se ajusta bien a los datos observados.

############################ Condiciones #######################

# 1. Debe existir una relación lineal entre los predictores y 
# la respuesta transformada.

# Calcular las probabilidades logísticas estimadas por el modelo.
predicciones_logit <- predict(modelo_rlogistico, type = "link")

# Graficar la probabilidad logística vs. densidad.
plot(conjunto_entrenamiento$clase_num, predicciones_logit, xlab = "clase_num", 
     ylab = "Probabilidad Logística",
     main = "Probabilidad Logística vs. clase_num", col = "blue", pch = 16)
abline(lm(predicciones_logit ~ conjunto_entrenamiento$densidad), col = "red")

# Se puede observar que en la recta generada, las probabilidades y los
# datos de la densidad tienen un comportamiento lineal, lo cual, permite
# concluir que hay una relación lineal entre los predictores y la respuesta
# transformada.

# Sin embargo, esto debe ser tomado con pinzas, ya que se puede observar
# que el modelo tiende a poseer una relación lineal más fuerte con la clase 1.
# Lo que permite visualizar problemas como un posible sobreajuste, sesgos o
# perdida de generalización.

# 2. Los residuos deben ser independientes entre sí.

# Para probar esta condición debemos de verificar si los modelos poseen un
# patron al azar en el primer gráfico generado por glm().

# Graficar los residuos vs. los valores ajustados.
plot(modelo_rlogistico, wich = 1)

# Como se puede visualizar un patrón claro en el gráfico se puede concluir
# que el modelo no captura una independencia entre los residuos.

############################ Generalización #######################

# Se realiza las predicciones con el conjunto de prueba.
predicciones_probabilidades <- predict(modelo_rlogistico, 
                                       newdata = conjunto_evaluacion, 
                                       type = "response")

# Se genera matriz de confusión de estas nuevas predicciones.
predicciones_clasificacion <- ifelse(predicciones_probabilidades >= 0.5, 1, 0)
matriz_confusion <- table(conjunto_evaluacion$clase_num, predicciones_clasificacion)
print(matriz_confusion)

# Se calculan valores derivados de esta matriz.
precision <- (matriz_confusion[1, 1] + matriz_confusion[2, 2]) / sum(matriz_confusion)
sensibilidad <- matriz_confusion[2, 2] / (matriz_confusion[2, 1] + matriz_confusion[2, 2])
especificidad <- matriz_confusion[1, 1] / (matriz_confusion[1, 1] + matriz_confusion[1, 2])

# Impresión de valores.
cat("Precisión (Accuracy):", precision, "\n")
cat("Sensibilidad (Recall):", sensibilidad, "\n")
cat("Especificidad:", especificidad, "\n")

# Generar CurvaROC.
roc_curva <- roc(conjunto_evaluacion$clase_num, predicciones_probabilidades)
auc_roc <- auc(roc_curva)

# Graficar la curva ROC
plot(roc_curva, main = "Curva ROC RlogS Test")


# Como se puede observar el modelo sigue manteniendo buen rendimiento
# con los datos de prueba, por ende, se puede concluir que el modelo es
# generalizable. Además se puede ver que dentro de un intervalo, la curva
# tiende a la derecha, por lo que puede inferir que tiene este modelo
# la capacidad de evitar detectar falsos negativos.

# Aunque no se cumpla la segunda condición (independencia), 
# se observa que el modelo tiene un excelente desempeño prediciendo los
# valores de densidad, por ende, no se considera necesario hacer un cambio.
# La razón de esto se debe a que se tiene fe en que el modelo múltiple
# pueda capturar mucho mejor la relación buscada.

# Además se puede decir que cambiar un modelo con tan buenas características
# arriesgándose a sesgos (por transformar variables), alta carga computacional y
# perdida de información o violar más supuestos empeorando el modelo no vale
# mucho la pena.

cat("############################## Regresión logística múltiple.")

################## Confiabilidad de clasificadores #############
predicciones_M <- predict(modelo_rlogm, 
                        newdata = conjunto_entrenamiento, 
                        type = "response")

umbral <- 0.5 # Umbral para clasificar como 0 o 1
predicciones_clasificadas_M <- ifelse(predicciones_M > umbral, 1, 0)
matriz_confusion_M <- table(conjunto_entrenamiento$clase_num, 
                          predicciones_clasificadas_M)

# Imprimir matriz de confusión.
print(matriz_confusion_M)

# Obtener los valores de la matriz de confusión
VP_M <- matriz_confusion_M[2, 2]  # Verdaderos positivos
VN_M <- matriz_confusion_M[1, 1]  # Verdaderos negativos
FP_M <- matriz_confusion_M[1, 2]  # Falsos positivos
FN_M <- matriz_confusion_M[2, 1]  # Falsos negativos

# Métricas de rendimiento
precision_M <- (VP_M + VN_M) / (VP_M + VN_M + FP_M + FN_M)  # Precisión (Accuracy)
sensibilidad_M <- VP_M / (VP_M + FN_M)                # Sensibilidad (Recall)
especificidad_M <- VN_M / (VN_M + FP_M)               # Especificidad
ppv_M <- VP_M / (VP_M + FP_M)                         # Valor predictivo positivo (Precision)
npv_M <- VN_M / (VN_M + FN_M)                         # Valor predictivo negativo

# Imprimir las métricas
cat("Precisión (Accuracy):", precision_M, "\n")
cat("Sensibilidad (Recall):", sensibilidad_M, "\n")
cat("Especificidad:", especificidad_M, "\n")
cat("Valor predictivo positivo (Precision):", ppv_M, "\n")
cat("Valor predictivo negativo:", npv_M, "\n")

# En este modelo se puede apreciar una mejora frente al anterior observando
# los valores de las métricas derivadas de la matriz de confusión.

########################## Bondad de ajuste ####################

roc_curve <- roc(conjunto_entrenamiento$clase_num, predicciones_M)
auc_score <- auc(roc_curve)
plot(roc_curve, main = "Curva ROC RLogM Trainning")

# Misma situación del punto anterior.

############################ Condiciones #######################

# 1. Debe existir una relación lineal entre los predictores y 
# la respuesta transformada.

# Calcular las probabilidades logísticas estimadas por el modelo.
predicciones_logit_M <- predict(modelo_rlogm, type = "link")

# Graficar la probabilidad logística vs. densidad.
plot(conjunto_entrenamiento$clase_num, predicciones_logit_M, xlab = "clase_num", 
     ylab = "Probabilidad Logística",
     main = "Probabilidad Logística vs. clase_num", col = "blue", pch = 16)
abline(lm(predicciones_logit_M ~ conjunto_entrenamiento$densidad), col = "red")

# 2. Los residuos deben ser independientes entre sí.

# Para probar esta condición debemos de verificar si los modelos poseen un
# patron al azar en el primer gráfico generado por glm().

# Graficar los residuos vs. los valores ajustados.
plot(modelo_rlogistico, wich = 1)

# Se repite situación del punto anterior.

############################ Generalización #######################

# Se realiza las predicciones con el conjunto de prueba.
predicciones_probabilidades_M <- predict(modelo_rlogm, 
                                       newdata = conjunto_evaluacion, 
                                       type = "response")

# Se genera matriz de confusión de estas nuevas predicciones.
predicciones_clasificacion_M <- ifelse(predicciones_probabilidades_M >= 0.5, 1, 0)
matriz_confusion_M <- table(conjunto_evaluacion$clase_num, 
                            predicciones_clasificacion_M)
print(matriz_confusion_M)

# Se calculan valores derivados de esta matriz.
precision_M <- (matriz_confusion_M[1, 1] + matriz_confusion_M[2, 2]) / sum(matriz_confusion_M)
sensibilidad_M <- matriz_confusion_M[2, 2] / (matriz_confusion_M[2, 1] + matriz_confusion_M[2, 2])
especificidad_M <- matriz_confusion_M[1, 1] / (matriz_confusion_M[1, 1] + matriz_confusion_M[1, 2])

# Impresión de valores.
cat("Precisión (Accuracy):", precision_M, "\n")
cat("Sensibilidad (Recall):", sensibilidad_M, "\n")
cat("Especificidad:", especificidad_M, "\n")

# Generar CurvaROC.
roc_curva <- roc(conjunto_evaluacion$clase_num, predicciones_probabilidades_M)
auc_roc <- auc(roc_curva)

# Graficar la curva ROC
plot(roc_curva, main = "Curva ROC RlogM Test")


############################ Arreglo #######################

# Dentro de lo que cabe en este modelo, se puede observar que potencia,
# en una primera instancia, los aspectos del modelo simple. Sin embargo,
# se sigue con el problema de la revisión anterior de no cumplir independencia.

# Sin embargo, se opta por dejar el modelo en el estado actual, por la mera
# razón de que es extremadamente funcional y confiable para registrar
# lo vinos tinto. También se tiene en consideración en que se requiere de
# usar métodos de exploración de datos para llegar a un mejor modelo
# lo cual incumbe a faltas de ética.


################################################################################
############################# Pregunta 9 - Grupo 1 #############################
################################################################################
cat("######################### Pregunta 9 - Grupo 1 ########################\n")

# Usando herramientas del paquete caret, evaluar el poder predictivo de los
# modelos con los datos de los 40 vinos que no se incluyeron en su construcción
# en términos de sensibilidad y especificidad.

# Primero: Obtener las predicciones de los modelos con los datos de evaluación
predicciones_evaluacion_simple <- predict(modelo_rlogistico, 
                                          newdata = conjunto_evaluacion, 
                                          type = "response")

predicciones_evaluacion_multiple <- predict(modelo_rlogm, 
                                            newdata = conjunto_evaluacion, 
                                            type = "response")

# Segundo: Comparar las predicciones con las clases reales y calcular sensibilidad y especificidad

# Para el modelo de regresión logística simple
predicciones_clasificacion_evaluacion_simple <- ifelse(predicciones_evaluacion_simple >= 0.5, 1, 0)
matriz_confusion_evaluacion_simple <- table(conjunto_evaluacion$clase_num, 
                                            predicciones_clasificacion_evaluacion_simple)

# Calcular sensibilidad y especificidad
sensibilidad_evaluacion_simple <- matriz_confusion_evaluacion_simple[2, 2] / (matriz_confusion_evaluacion_simple[2, 1] + matriz_confusion_evaluacion_simple[2, 2])
especificidad_evaluacion_simple <- matriz_confusion_evaluacion_simple[1, 1] / (matriz_confusion_evaluacion_simple[1, 1] + matriz_confusion_evaluacion_simple[1, 2])

# Para el modelo de regresión logística múltiple
predicciones_clasificacion_evaluacion_multiple <- ifelse(predicciones_evaluacion_multiple >= 0.5, 1, 0)
matriz_confusion_evaluacion_multiple <- table(conjunto_evaluacion$clase_num, 
                                              predicciones_clasificacion_evaluacion_multiple)

# Calcular sensibilidad y especificidad
sensibilidad_evaluacion_multiple <- matriz_confusion_evaluacion_multiple[2, 2] / (matriz_confusion_evaluacion_multiple[2, 1] + matriz_confusion_evaluacion_multiple[2, 2])
especificidad_evaluacion_multiple <- matriz_confusion_evaluacion_multiple[1, 1] / (matriz_confusion_evaluacion_multiple[1, 1] + matriz_confusion_evaluacion_multiple[1, 2])

# Imprimir los resultados
cat("Resultados para el modelo de regresión logística simple:\n")
cat("Sensibilidad:", sensibilidad_evaluacion_simple, "\n")
cat("Especificidad:", especificidad_evaluacion_simple, "\n")

cat("Resultados para el modelo de regresión logística múltiple:\n")
cat("Sensibilidad:", sensibilidad_evaluacion_multiple, "\n")
cat("Especificidad:", especificidad_evaluacion_multiple, "\n")









