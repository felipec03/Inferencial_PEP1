# EP02
# Vicente Aninat - Rodrigo González - Juan Pablo Loyola

setwd("C:\\Users\\Usuario\\Documents\\Estadística_inferencial\\EP02")

# Importación de los packages correspondientes.
if(!requireNamespace('dplyr', quietly = TRUE)){
  install.packages('dplyr')
}
library(dplyr)

# Lectura del archivo csv
datosOlimpico <- read.csv2("EP02 Datos(1).csv")
#Tipos de variables relevantes:
#Raza: Variable categorica nominal
#Previo: variable numerica continua
#Posterior: variable numerica continua

# Contexto: El Comité Olímpico de una gran potencia del atletismo está estudiando 
# el programa de entrenamiento de varones para la competencia de 100 metros planos, 
# por lo que ha recopilado datos de una muestra aleatoria de atletas.
#
# En base a esto se busca responder a las siguientes preguntas:

# PREGUNTA 1
# El Comité Olímpico cree que el mejor tiempo medio de los atletas de raza 
# oriental antes de ingresar al programa de entrenamiento era 19,4 segundos. 
# ¿Soportan los datos esta afirmación?

# Respuesta: Para poder determinar su la afirmación en cuestión está sostenida por los datos presentados,
# se realizará una prueba de hipótesis usando la prueba estadística de Shapiro-Wilk test y t-test para dos muestras.
# Ya que con Shapiro-Wilk test se analizará si los datos siguen una distribución normal, condición para
# la utilización de t-test. Esta prueba ha sido escogida dado que la cantidad de muestras es menor a 30
# y adicionalmente se desconoce la desviación estándar de la muestra.
#
# Hipótesis nula: El mejor tiempo medio de los atletas de raza oriental antes
# de ingresar al programa de entrenamiento es igual a 19,4 segundos.
# Matemáticamente: μ = 19,4.
#
# Hipótesis alternativa: El mejor tiempo medio de los atletas de raza oriental antes
# de ingresar al programa de entrenamiento no es igual a 19,4 segundos.
# Matemáticamente: μ != 19,4
#
# Valor nulo: 19,4. Se toma este como valor nulo ya que si esta es la media, no hay suficiente
# evidencia para rechazar la hipótesis nula.
#
# Dirección de la prueba: Bilateral. Dado que la media puede ser tanto mayor como menor.

datosOriental <- filter(datosOlimpico, Raza == "Oriental") # Filtrado de los datos segun raza

SpTestOriental <- shapiro.test(datosOriental$Previo) # Shapiro-Wilk test
print(SpTestOriental)

tTestOriental <- t.test(datosOriental$Previo, alternative = "two.sided", mu = 19.4, conf.level = 0.95) # t-test
print(tTestOriental)

# Conclusión: Dado que los datos siguen una distribución normal sustentado por el Shapiro-Wilk test, y el
# p-value obtenido del t-test es 0.11, mayor al nivel de significancia de 0.05, no se ha encontrado
# suficiente evidencia para rechazar la hipótesis nula, por ende, El mejor tiempo medio de los atletas de }
# raza oriental antes de ingresar al programa de entrenamiento es igual a 19,4 segundos.


# PREGUNTA 2
# ¿Sugieren los datos que la mejor marca de los atletas de raza negra
# se reduce en promedio 1,3 segundos tras el entrenamiento?
#
# Respuesta: En esta pregunta se debe comparar la diferencia de los tiempos de los atletas
# por lo que se trabajara con la media de la diferencia de los 
# resultados de los atletas, ademas no tenemos desviacion estandar ni 30 datos
# por lo tanto se usara el se utilizara la prueba t de student para muestras
# pareadas.
#
# Hipótesis nula: El promedio de la diferencia entre los tiempos de los atletas ed raza negra es -1.3
# Matematicamente: μ = -1.3
#
# Hipótesis alternativa: El promedio de la diferencia entre los tiempos de los atletas ed raza negra es menor a 1.3
# Matematicamente: μ < -1.3
#
# Valor nulo: -1.3. Debido a que se pregunta si los tiempos de los atletas de raza negra disminuyen en 
# promedio 1.3, es decir, que los tiempos cambian en -1.3.
#
# Dirección de la prueba: Unilateral. Por lo anteriormente dicho.

# Filtrado del dataset de los atletas
datosNegra <- filter(datosOlimpico, Raza == "Negra")

# Obtencion de la diferencia de los tiempos de los atletas
diferencia <- datosNegra$Posterior-datosNegra$Previo

# Creacion del dataframe con todos los datos
tabladiferencia <- data.frame(datosNegra,diferencia)

#Para demostrar normalidad se utilizara el test de Wilk Shapiro
Shapirodif<-shapiro.test(tabladiferencia$diferencia)
#Se muestran los resultadosd el tes de Wilk-Shapiro
print(Shapirodif)

# De los resultados obtenidos W = 0.9781, p-value = 0.8027
# se concluye que si efectivamente sigue una distribucion normal

# Continuando, se aplica la prueba de t de student, con un nivel de confianza del 95%
tTestdiferencia<-t.test(tabladiferencia$diferencia, alternative ="less", mu=-1.3,  conf.level = 0.95)
# Muestra de los resultados
print(tTestdiferencia)

# Conclusion: Debido a que se obtiene un p-value =0.001138 > α=0.05, se tiene suficiente evidencia
# para rechazar la hipotesis nula, concluyendo que, efectivamente, en promedio el
# tiempo de los atletas de raza negra disminuyo en mas de 1.3 segundos despues
# del entrenamiento.

# Se incluye ejecucion adicional de la prueba t de student, trabajando dirtectamente
# en ambas columnas, sin juntarlas de antemano, dando los mismos resultados
tTestdiferencia2<-t.test(x= datosNegra$Posterior, y=datosNegra$Previo, paired= TRUE, alternative ="less", mu=-1.3,  conf.level = 0.95)
print(tTestdiferencia2)


# PREGUNTA 3
# ¿Es posible afirmar que, en promedio, los atletas de raza negra superaban a los de raza oriental 
# por menos de 5,2 segundos antes del entrenamiento?
#
# Respuesta: En este caso se busca comparar las medias de dos poblaciones no relacionadas, los atletas negros y los atletas orientales.
# Debido a esto, se realizará un t test para muestras independientes.
# Se elige el t test ya que se quieren comparar medias, y no tenemos la desviación estándar de las poblaciones,
# por lo que se elige este por sobre el Z test.
# Además, se usará un alfa de 0,5.
#
# Hipótesis nula: En promedio, los atletas de raza negra superan en 5,2s a los atletas de raza oriental.
# Matemáticamente: muN - muO = 5.2
#
# Hipótesis alternativa: En promedio, los atletas de raza negra superan en menos de 5,2s a los atletas de raza oriental. 
# Matemáticamente: muN - muO < 5.2
#
# Valor nulo: 5,2. Se toma este valor dado que en caso de que sea el valor de la media, no hay suficiente evidencia
# para rechazar la hipótesis alternativa.
#
# Dirección de la prueba: Unilateral. Dado que se buscan valores menores al valor nulo para verificar las hipótesis.

# Primero, se realiza el test de Shapiro-Wilk, para asegurarse de que ambas muestras sigan una distribución cercana a la normal
SpTestNegro <- shapiro.test(datosNegra$Previo)

print(SpTestNegro) #W = 0.97301, p-value = 0.6631
print(SpTestOriental) #W = 0.98354, p-value = 0.932
#Para ambas muestras no se puede rechazar la hipótesis nula de que vengan de una distribución normal

# Ahora se realiza el t test con muestras no pareadas
prueba3 <- t.test(x = datosNegra$Previo, y=datosOriental$Previo, paired=FALSE, alternative="less", mu=5.2, conf.level = 0.95)
print(prueba3) # p-value < 2.2e-16 << alfa

# Conclusión: Como se obtuvo un p-value tan pequeño, no se cuenta con la suficiente evidencia para aceptar la hipótesis nula 
# con un nivel de confianza del 95%.
# Se puede decir entonces con un 95% de confianza que los atletas negros superaban a los atletas orientales por menos de 5,2 segundos
# antes del entrenamiento.


# 4) ¿Será cierto que hay menos atletas de raza oriental que redujeron sus 
# mejores marcas en al menos 5,2 segundos que atletas de raza blanca que lo 
# hicieron en al menos 2,8 segundos?

# Respuesta: Dado que se que busca hacer una comparación de proporciones y a pesar de que no se tienen los
# datos suficientes para aplicar la condición de éxito-fracaso, se utilizará el método de Wilson.
# Para esto se obtiene la cantidad de éxitos para cada una de las razas, además del tamaño de la
# muestra total y la concatenación de las tablas resultantes para utilizar el método de Wilson.

# Hipótesis nula: La proporción de atletas de raza oriental que redujeron sus 
# mejores marcas en al menos 5,2 segundos es igual a la proporción de atletas 
# de raza blanca que lo hicieron en al menos 2,8 segundos?
# Matemático: μOR = μBL.

# Hipótesis alternativa: La proporción de atletas de raza oriental que redujeron sus 
# mejores marcas en al menos 5,2 segundos es menor a la proporción de atletas 
# de raza blanca que lo hicieron en al menos 2,8 segundos?
# Matemático: μOR < μBL.

# Valor nulo: 0. Vale destacar que la prueba de Wilson no acepta ningun otro valor que no sea 0.

# Dirección de la prueba: Unilateral. Dado que se buscan valores menores al valor nulo para verificar las hipótesis.

datosOriental <- filter(datosOlimpico, Raza == "Oriental")

# Obtencion de la diferencia de los tiempos de los atletas
diferenciaOriental <- datosOriental$Posterior-datosOriental$Previo

# Creacion del dataframe con todos los datos
tabladiferenciaOriental <- data.frame(datosOriental,diferenciaOriental)

# Extracción de los exitos en la raza oriental
totalOriental<-nrow(tabladiferenciaOriental)
exitosOriental<- nrow(filter(tabladiferenciaOriental,diferenciaOriental<=-5.2))

datosBlanca <- filter(datosOlimpico, Raza == "Blanca")

# Obtencion de la diferencia de los tiempos de los atletas
diferenciaBlanca <- datosBlanca$Posterior-datosBlanca$Previo

# Creacion del dataframe con todos los datos
tabladiferenciaBlanca <- data.frame(datosBlanca,diferenciaBlanca)

# Extracción de los exitos en la raza blanca
totalBlanca<-nrow(tabladiferenciaBlanca)
exitosBlanca<- nrow(filter(tabladiferenciaBlanca,diferenciaBlanca<=-2.8))

# Concatenación de las tablas
totales<-c(totalOriental,totalBlanca)
exitos<-c(exitosOriental,exitosBlanca)

# Prueba de Wilson
wilson<- prop.test(exitos, n=totales, alternative = "less", conf.level = 0.95)
print(wilson)

# Conclusión: Debido al valor de 0.0007551 del p-value, se cuenta con la suficiente evidencia
# como para rechazar con seguridad la hipótesis nula. En otras palabras, la proporción de 
# atletas de raza oriental que redujeron sus mejores marcas en al menos 5,2 segundos es menor 
# a la proporción de atletas de raza blanca que lo hicieron en al menos 2,8 segundos.

