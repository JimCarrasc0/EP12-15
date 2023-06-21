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

# Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos
# del RUN (sin considerar el dígito verificador) del integrante de menor edad
# del equipo.

# El integrante de menor edad es                       : Ángel Vilches U.
# El rut del integrante de menor edad es               : 20.996.064-8
# Los últimos cuatro dígitos del RUN son               : 6 0 6 4
# A continuación se define la semilla según lo indicado:
set.seed(6064)

################################################################################
############################# Pregunta 2 - Grupo 1 #############################
################################################################################

# Seleccionar una muestra de 100 vinos.

# A continuación se extrae una muestra de 100 vinos de acuerdo con los datos
# obtenidos (EP13 Datos.csv)

muestra <- sample_n(datos, 100)

################################################################################
############################# Pregunta 3 - Grupo 1 #############################
################################################################################

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

# Seleccionar, entre las variables que no fueron escogidas en el punto anterior,
# una que el equipo considere que podría ser útil para predecir la variable
# calidad, justificando bien esta selección.

# Se opta por seleccionar la variable "pH".
# En este sentido, el pH del vino puede influir en su sabor, equilibrio y
# estabilidad, lo cual puede afectar la percepción de su calidad. Además, 
# el pH puede interactuar con otras características químicas presentes en el
# vino, como la acidez y los sulfatos. Por lo tanto, consideramos que la 
# variable "pH" es relevante para la predicción de la calidad.

variable_seleccionada <- names(muestra)[11]
cat("Variable predictora seleccionada: \n")
print(variable_seleccionada)

################################################################################
############################# Pregunta 5 - Grupo 1 #############################
################################################################################

# Usando el entorno R, construir un modelo de regresión lineal simple con el
# predictor seleccionado en el paso anterior.

modelo_RLS <- lm(calidad ~ ph,
                 data = muestra)
print(summary(modelo_RLS))

# Se grafica el modelo
grafico_modelo_RLS <- ggscatter(datos,
                                x = "ph",
                                y = "calidad",
                                color = "purple",
                                fill = "purple",
                                xlab = "pH",
                                ylab = "Calidad",
                                xticks.by = 1)
grafico_modelo_RLS <- grafico_modelo_RLS + geom_smooth(method = lm,
                                                       se = FALSE,
                                                       colour = "purple")
print(grafico_modelo_RLS)

################################################################################
############################# Pregunta 6 - Grupo 1 #############################
################################################################################

# Agregue la variable seleccionada en el paso 4 al conjunto obtenido en el
# punto 3.

variables_predictoras <- c(variables_predictoras, variable_seleccionada)

# Variables predictoras actualizadas
cat("Variables predictoras seleccionadas actualizadas: \n")
print(variables_predictoras)

################################################################################
############################# Pregunta 7 - Grupo 1 #############################
################################################################################

################################################################################
############################# Pregunta 8 - Grupo 1 #############################
################################################################################

################################################################################
############################# Pregunta 9 - Grupo 1 #############################
################################################################################



















