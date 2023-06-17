# Integrantes - Grupo 1
# Jaime Carrasco Quintrequeo
# Andrés Haussmann Garín
# Benjamín Gonzalo Hurtado
# Ángel Vilches Urrutia

# Librerías
if (!require(ggpubr)) {
  install.packages("ggpubr",
                   dependencies = TRUE)
  require(ggpubr)
}
if (!require(rcompanion)) {
  install.packages("rcompanion",
                   dependencies = TRUE)
  require(rcompanion)
}
if (!require(dplyr)) {
  install.packages("dplyr",
                   dependencies = TRUE)
  require(dplyr)
}
if (!require(WRS2)) {
  install.packages("WRS2",
                   dependencies = TRUE)
  require(WRS2)
}

# Primeramente, se obtienen los datos de la actividad
sem15 <- c(67.0975, 65.20517, 66.46108, 69.68791, 67.25581, 76.00934, 66.03579,
           75.89793, 69.54054, 64.58422, 66.01125)
sem20 <- c(68.53608, 68.70861, 67.6798, 71.75897, 71.24894, 77.59157, 67.70297,
           77.45975, 71.33677, 66.71235, 69.30179)

# Se confecciona el dataframe correspondiente
datos <- data.frame(sem15, sem20)

################################################################################
############################# Pregunta 1 - Grupo 1 #############################
################################################################################

# Tras el éxito obtenido recientemente por un equipo de agrónomos en la mejora
# del cultivo de manzanas de exportación, con la ayuda de algunos modelos
# estadísticos, un colega dedicado a la producción de peras desea estudiar
# algunas características de sus productos. Para ello, ha registrado los pesos
# (en gramos) de las mismas unidades (cada una de un árbol diferente) de la
# variedad Winter Nelly durante las semanas 15 y 20 de crecimiento. Desea saber
# si existe diferencia en el peso de las peras en este periodo de tiempo. ¿Qué
# puede concluir?

# De acuerdo al enunciado, es posible notar que se realiza un contraste entre la
# media de los pesos (gramos) de las mismas peras en un periodo de tiempo
# distinto (en semanas). Bajo este contexto, se logra identificar dos muestras 
# pareadas.

# A continuación, se expresan las hipótesis nula y alternativa con respectiva
# notación matemática:
# H0: En promedio, no existe diferencia en el peso de las peras entre semana 15
#     y semana 20. (p15 - p20 = 0)
# HA: En promedio, existe diferencia en el peso de las peras entre semana 15 y 
#     20. (p15 - p20 != 0)

# Dado que no se hace alusión a un especial cuidado en la prueba, se determina
# establecer un nivel de significancia de 0.05

alfa <- 0.05

# Ahora bien, según el análisis realizado previamente. Se pretende emplear una
# prueba paramétrica, para lo cual se verifican las condiciones asociadas:
# En primer lugar, de acuerdo al enunciado se confirma la independencia de las
# observaciones ("cada una de un árbol diferente").
# En segundo lugar, se validará la distribución normal de las muestras 
# extraídas.

# Shapiro wilk para verificar normalidad
shapiro_sem15 <- shapiro.test(sem15)
shapiro_sem20 <- shapiro.test(sem20)
cat("Shapito Wilk para semana 15: ")
print(shapiro_sem15)
cat("Shapiro Wilk para semana 20: ")
print(shapiro_sem20)

# Se puede observar que, de acuerdo con el p-valor obtenido mediate Shapiro Wilk
# ambas distribuciones no se asemejan a la normal. Dado que,
# p-valor_sem15 < p-valor_sem20 < alfa
# 0.01058       < 0.03166       < 0.05

# Se realiza un gráfico cuantil-cuantil (Q-Q)
qq_sem15 <- ggqqplot(datos,
                     x = "sem15",
                     color = "purple",
                     title = "Gráfico Q-Q semana 15")
qq_sem20 <- ggqqplot(datos,
                     x = "sem20",
                     color = "purple",
                     title = "Gráfico Q-Q semana 20")
print(qq_sem15)
print(qq_sem20)

# Según los gráficos Q-Q generados y los p-valor obtenidos gracias a Shapiro
# Wilk se confirma que ninguna distribución cumple con el requisito de 
# normalidad. Por lo tanto, es preciso emplear transformación de datos.
# Para el presente caso se utilizará: Escalera de potencias de Tukey. Dado que
# permite que una distribución asimétrica se asemeje a la normal, proceso 
# necesario para datos problemáticos.

tukey15 <- transformTukey(sem15)
tukey20 <- transformTukey(sem20)
datos_tukey <- data.frame(tukey15, tukey20)

# En este sentido, se logra observar que tras aplicar Escalera de potencias de 
# Tukey los datos se asemejan a la normal. Pues,
# p-valor_sem20_tukey > p-valor_sem15_tukey > alfa
# 0.4905              > 0.3557              > 0.05
# Asimismo, se realiza un nuevo gráfico Q-Q para visualizar valores atípicos:

qq_sem15_tukey <- ggqqplot(datos_tukey,
                           x = "tukey15",
                           color = "purple",
                           title = "Gráfico Q-Q semana 15 Tukey")

qq_sem20_tukey <- ggqqplot(datos_tukey,
                           x = "tukey20",
                           color = "purple",
                           title = "Gráfico Q-Q semana 20 Tukey")
print(qq_sem15_tukey)
print(qq_sem20_tukey)

# De acuerdo con la transformación de datos realizada, se confirma que los datos
# cumplen con el requisito de normalidad. Por lo tanto, se procede a utilizar
# una prueba paramétrica, específicamente, la prueba T de Student para dos 
# muestras pareadas:

t_student_dos_muestras <- t.test(sapply(tukey15, mean), 
                                 sapply(tukey20, mean),
                                 paired = TRUE,
                                 alternative = "two.sided",
                                 conf.level = 1 - alfa)

print(t_student_dos_muestras)

# De acuerdo con la prueba t de Student realizada, se obtiene un p-valor igual a
# 0.0003878, el cual es menor al nivel de significancia establecido, igual a 
# 0.05. Por lo que, se rechaza la hipótesis nula en favor de la hipótesis 
# alternativa. Así, se concluye, con un 95% de confianza, que en promedio
# existe diferencia en el peso de las peras entre semana 15 y 20.
  
################################################################################
############################# Pregunta 2 - Grupo 1 #############################
################################################################################

# Analice la primera pregunta abordada en el ejercicio práctico 11, con los
# mismos datos, utilizando un método robusto adecuado.

# En este sentido, la pregunta propuesta es:
# ¿En promedio, la edad de hombres y mujeres solteros/as pertenecientes a la
# Región Metropolitana es el mismo?

# A continuación se plantea la hipótesis nula y alternativa con respectiva
# notación matemática:
# H0: La media de la edad de hombres y mujeres soltero/as pertenecientes a la
#     Región Metropolitana es igual. (μA - μB = 0)
# HA: La media de la edad de hombres y mujeres soltero/as pertenecientes a la
#     Región Metropolitana es diferente. (μA - μB != 0)
 
# Se obtienen los datos de acuerdo al conjunto de datos del ejercicio práctico
# anterior: 
datos2 <- read.csv2(file.choose(),
                    stringsAsFactors = TRUE)

# En primer lugar, de acuerdo a la naturaleza independiente entre dos muestras
# (hombres y mujeres solteros/as) con eje en el promedio (media) de la edad de
# estos grupos es preciso afirma que la prueba tradicional (paramétrica) que
# correspondería emplearse en primera instancia responde a la prueba t de 
# Student para dos muestras independientes.

# Dado que no se hace alusión a un especial cuidado en la prueba, se determina
# establecer un nivel de significancia de 0.05

alfa <- 0.05

# Se extrae la muestra según las variables involucradas
muestra <- datos2 %>% filter(ecivil == "Soltero(a)" &
                             region == "Region Metropolitana de Santiago")
muestra <- muestra %>% select(sexo, ecivil, edad)
muestra_hombres <- muestra %>% filter(sexo == "Hombre")
muestra_mujeres <- muestra %>% filter(sexo == "Mujer")

# A continuación, se realiza un gráfico Q-Q buscando visualizar datos atípicos
# en conjunto con la semejanza (o no) a la normalidad

qq_muestra <- ggqqplot(muestra,
                       x = "edad",
                       color = "purple",
                       title = "Gráfico Q-Q muestra")
print(qq_muestra)

# Como se logra visualizar según el gráfico confeccionado, la muestra no sigue
# una distribución normal, pues presenta demasiados valores atípicos. Por lo 
# tanto, se procede a aplicar el método de Yuen para muestras pareadas como 
# método robusto no parámetrico capaz de abordar la presente interrogante.

gamma <- 0.02
# Se extraen muestras aleatorias del mismo tamaño para efectos del estudio y
# el método a realizar
n_igual_muestra_hombres <- sample_n(muestra_hombres, 500)
n_igual_muestra_mujeres <- sample_n(muestra_mujeres, 500)
prueba_yuen <- yuend(x = sapply(n_igual_muestra_hombres[["edad"]], mean),
                     y = sapply(n_igual_muestra_mujeres[["edad"]], mean),
                     tr = gamma)
print(prueba_yuen)

# En este sentido, tras múltiples pruebas se observa que el p-valor obtenido
# es siempre menor al nivel de significación establecido (0.00XXXXX < 0.05).
# Por lo tanto, se rechaza la hipótesis nula en favor de la hipótesis 
# alternativa. Así, se concluye, con un 95% de confianza, que la media de la
# edad de hombres y mujeres soltero/as pertenecientes a la Región Metropolitana
# es diferente.

################################################################################
############################# Pregunta 3 - Grupo 1 #############################
################################################################################

#Pregunta 3.
# Analice la segunda pregunta abordada en el ejercicio práctico 11, con los
# mismos datos, utilizando un método robusto adecuado.

# Pregunta propuesta:
# En promedio, la edad de las personas es igual en las regiones de Tarapacá,
# Valparaíso, La Araucanía y Metropolitana de santiago ¿es el mismo?

# A continuación se plantea la hipótesis nula y alternativa con respectiva
# notación matemática:
# H0: La media de las edades es igual en las regiones de Tarapacá, Valparaiso,
#     La Araucanía y Metropolitana de Santiago. (μA = μB = μC = μD)
# HA: La media de las edades es diferente en las regiones de Tarapacá, 
#     Valparaiso, La Araucanía y Metropolitana de Santiago.
#     (μA != μB != μC != μD)

# En primer lugar, de acuerdo a la naturaleza independiente entre más de dos
# grupos (cuatro específicamente aludiendo a las regiones) con eje en el
# promedio (media) de la edad de hombres y mujeres solteros/as es preciso 
# afirma que la prueba tradicional (paramétrica) que correspondería emplearse en
# primera instancia responde a ANOVA de una vía para muestras independientes.

# Dado que no se hace alusión a un especial cuidado en la prueba, se determina
# establecer un nivel de significancia de 0.05

alfa <- 0.05

# Se seleccionan los datos de interés según la interrogante propuesta
muestra2 <- datos2 %>% select(region, edad)
muestra2 <- muestra2 %>% filter(region == "Region de Tarapaca" |
                                                region == "Region de Valparaiso" |
                                                region == "Region de La Araucania" |
                                                region == "Region Metropolitana de Santiago")
muestra2 <- droplevels(muestra2)
muestra2 <- sample_n(muestra2, 809)

# Se obtiene los datos referentes a la región de Tarapacá de acuerdo a la
# muestra obtenida anteriormente
reg_tarapaca <- muestra2 %>% filter(region == "Region de Tarapaca")

# Se obtiene los datos referentes a la región de Valparaiso de acuerdo a la
# muestra obtenida anteriormente
reg_valparaiso <- muestra2 %>% filter(region == "Region de Valparaiso")


# Se obtiene los datos referentes a la región de La Araucanía de acuerdo a la
# muestra obtenida anteriormente
reg_araucania <- muestra2 %>% filter(region == "Region de La Araucania")

# Se obtiene los datos referentes a la región Metropolitana de Santiago de
# acuerdo a la muestra obtenida anteriormente
reg_santiago <- muestra2 %>% filter(region == 
                                             "Region Metropolitana de Santiago")

# A continuación, se realiza un gráfico Q-Q buscando visualizar datos atípicos
# en conjunto con la semejanza (o no) a la normalidad para cada grupo (región)

qq_muestra2_araucania <- ggqqplot(reg_araucania,
                                  x = "edad",
                                  color = "purple",
                                  title = "Gráfico Q-Q Región de La Araucanía")
print(qq_muestra2_araucania)
qq_muestra2_valparaiso <- ggqqplot(reg_valparaiso,
                                   x = "edad",
                                   color = "purple",
                                   title = "Gráfico Q-Q Región de Valparaiso")
print(qq_muestra2_valparaiso)
qq_muestra2_santiago <- ggqqplot(reg_santiago,
                                 x = "edad",
                                 color = "purple",
                                 title = "Gráfico Q-Q Región Metropolitana de Santiago")
print(qq_muestra2_santiago)
qq_muestra2_tarapaca <- ggqqplot(reg_tarapaca,
                                 x = "edad",
                                 color = "purple",
                                 title = "Gráfico Q-Q Región de Tarapacá")
print(qq_muestra2_tarapaca)

# Como se logra visualizar según los gráficos confeccionados, los grupos no 
# siguen una distribución normal, pues presentan demasiados valores atípicos.
# Por lo tanto, se procede a aplicar un método robusto que permita comparaciones
# de una vía para múltiples grupos independientes. Bajo este contexto, se 
# aplicará la función t1way del paquete WRS2:

gamma <- 0.2
B <- 3639
prueba_t1way <- t1waybt(edad ~ region,
                        data = muestra2,
                        tr = gamma,
                        nboot = B)
print(prueba_t1way)

# Finalmente, se obtiene un p-valor igual a 0.03792, menor que el nivel de 
# significación establecido (0.05) se rechaza la hipótesis nula en favor de la
# hipótesis alternativa. Por lo tanto, se concluye, con un 95% de confianza, que
# la media de las edades es diferente para al menos un grupo en las regiones de
# Tarapacá, Valparaiso, La Araucanía y/o Metropolitana de Santiago.

# En este sentido, dado que se han presentado diferencias entre los grupos 
# evaluados, es preciso realizar un análisis post-hoc para identificar los
# pares que presentan diferencias.

# Aplicamos ahora un procedimiento post-hoc.
post_hoc <- mcppb20(edad ~ region,
                    data = muestra2,
                    tr = gamma,
                    nboot = B)
print(post_hoc)

# En este sentido, se observan diferencias en la Región Metropolitana de 
# Santiago respecto de las regiones de Valparaíso y La Araucanía.