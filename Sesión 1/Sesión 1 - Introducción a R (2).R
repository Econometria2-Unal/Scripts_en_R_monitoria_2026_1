#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2024 - 02
#                            Econometría II | Monitoría 
#
#                                     Sesión 1:
#                                 Introducción a R
#                                  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Es importante limpiar nuestro entorno y dispositivos antes de iniciar o correr
# un código nuevo.

rm(list = ls())
dev.off()

#_____________________________________________________________________________________#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Tabla de Contenidos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. Números
#  1.1. Operaciones aritméticas básicas
#  1.2. Colecciones básicas
#  1.3. Funciones de variable númerica útiles para el curso
# 2. Carácteres y operadores lógicos
#  2.1. Carácteres
#  2.2. Operadores lógicos
# 3. Funciones
# 4. Estructuras de control
#  4.1. Condicionales
#  4.2. Bucles
# 5. Paquetes útiles para la econometría 
# 6. Estadística descriptiva (Análisis exploratorio de datos)
#  6.1. Medidas de tendencia central
#  6.2. Análisis exploratorio de datos gráfico
#  6.3. Manejo de datos con Dplyr
#  6.4. Paquete tidyr
#  6.5. Gráficos con ggplot2


#~~~~~~~~~~~~~~~~~~#
#### 1. Números ####
#~~~~~~~~~~~~~~~~~~#

1
3.4
1023

x = 33
x

class(x)


##### 1.1. Operaciones aritméticas básicas ####


#~~ SUMA ~~#

2 + 2 

suma = 2 + 2 
class(suma)

a = 10
b = 15 
c = 5.5

a+b 

suma_1 = a+b; suma_1
(suma_2 = a+c)
suma_3 = b+c
suma_3

class(suma_1);class(suma_2);class(suma_3) # Con ";" podremos ejecutar más de una
                                          # instrucción en una línea

#~~ RESTA ~~#

4 - 2

resta = 3 - 4
class(resta)

resta_1 = a-b
resta_2 = a-c

resta_1;resta_2

class(resta_1)

#~~ MULTIPLICACIÓN ~~#

2*3

multipliacion = 4*4
class(multipliacion)

#~~ DIVISIÓN ~~#

6/2

division = a/b; division
class(division)

#~~ POTENCIA ~~#

3**2

potencia = a**c
potencia

#~~ RAÍZ CUADRADA ~~#

sqrt(81)
sqrt(a)

# Otra forma en la programación para usar una raíz cuadrada es elevar a la 1/2

4**(1/2)

a = 81
b = 0.5

raiz = a**b
raiz




##### 1.2. Colecciones básicas #####

#~~ VECTORES ~~#

numeros = c(0,1,2,3,4,5,6,7,8,9)
numeros

class(numeros)

# Algunas funciones para crear vectores

# Vectores secuencias 

secuencia_simple = 0:10
secuencia_simple

secuencia_en_2 = seq(0,10, by = 2)
secuencia_en_2

secuencia_repetida = rep(5,10)
secuencia_repetida

# También podemos seleccionar elemntos de un vector usando "[]"

vector = c(1,6,8,4,2,3)

vector[1]

# Recordemos que los arrays en R comienzan desde el elemento "1", a diferencia 
# de programas como Python donde inician desde el elemento "0".

# Si usamos el "-n" obtendremos todos los elementos de la lista menos el n-ésimo
# elemento del vector

vector[-1]

# También podremos aplicar operadores a los vectores

sqrt(vector)


#~~ LISTAS ~~#

lista= list(2,3,5,7,11)
lista

# ¿Y cuál es la diferencia?

class(lista)

lista_1 = list(1,"Hola", lista, vector)
lista_1

# También podremos traer elementos

lista_1[2]

lista_1[5]

# Con "$" podemos traer un elemento 

lista_1$vector

# Para solucionar el problema de "NULL" tenemos que asignar un nombre a los elementos
# de la lista

lista_2 = list(uno = 1, str = "Hola", lista_0 = lista, vector_0 = vector)

lista_2

lista_2$uno
lista_2$lista_0

# Las listas tienen algunas limitaciones, una de ellas es que no podremos aplicar
# funciones de variables vectoriales.

sqrt(lista_2)


#~~ MATRICES ~~#

matriz = matrix(1:10, nrow = 5, ncol = 2)
matriz
class(matriz)

matriz_1 = matrix(1:10, nrow = 5, byrow = TRUE)
matriz_1

matriz_prueba = matrix(c(4,12,9,11), nrow = 2)
matriz_prueba


# Diagonal
diag(matriz_prueba)

# Determinante
det(matriz_prueba)

# Transpuesta
t(matriz_prueba)

# Dimensión 
dim(matriz_prueba)

# Inversa
solve(matriz_prueba)


# Para multiplicar matrices no se usa "*" si no "%*%"

matriz_prueba_trans = t(matriz_prueba)

matriz_prueba_trans %*% matriz_prueba


##### 1.3. Funciones de variable numérica útiles para el curso ####

vector_prueba = c(5,3,13,46,12,45,6,13,5)

# Especificamente, serán MUY útiles las siguientes dos funciones:

diff(vector_prueba)
log(vector_prueba)

diff(log(vector_prueba))

# El operador de diferencia elimina el primer elemento

length(vector_prueba)
length(diff(vector_prueba))




#### 2. Carácteres y operadores lógicos ####

##### 2.1. Carácteres ####

saludo = "Hola"
saludo
class(saludo)


# paste() permite unir varios strings usando un espacio como separador

nombre = "Pilar"

paste(saludo, nombre)

# Paste permite unir carácteres y variables numéricas

paste(saludo,"mi nombre es", nombre, "y tengo", 23, "años")

# También podemos hacer un vector con carácteres.

nombres = c("Felipe","Luis","Milena")
nombres
class(nombres)


##### 2.2. Operadores lógicos ####

# En R, se emplea lógica con las condiciones "FALSE" y "TRUE". Estas condiciones 
# lógicas cobraran más sentido con la implenmentación de las funciones usadas en el 
# curso.

class(TRUE)
class(FALSE)

a = 1
b = 2
c = 3

# > Mayor que

a > b
c > a

# < Menor que

b<c
b<a

# == Igual a

a==b
a==a

# != Distinto a 

a!=b
b!=b

#### 3. Funciones ####

# R permite definir y usar funciones, por ejemplo la función suma.

suma = function(A,B)
{ return(A+B)
}


suma(10,15)
suma(-1,30)


# Generemos una función para una n-ésima raíz

n_raiz = function(radicando, orden = 2)
  { 
  resultado = radicando**(1/orden)
  
  return(resultado)
  }

n_raiz(4)

n_raiz(64,3)


# Elevemos la dificultad, creemos una función que hallé las raices de una cuadrática

cuadratica = function(a,b,c){
  numerador_cuadratica = b^2-4*a*c
  
  x1 = (-b-sqrt(numerador_cuadratica)/2*a)
  x2 = (-b+sqrt(numerador_cuadratica)/2*a)
  
  return(c(x1,x2))
}


cuadratica(4,8,2)

# El único problema es que no tolera raices complejas.


#### 4. Estructuras de flujo ####

##### 4.1. Condicionales ####

#~~ If - else - else if ~~#
 
# Los condicionales van muy de la mano con los operadores lógicos.

nota_econometria_II = NULL  # Inserte la nota que se sacará (con confianza ;) )

if (nota_econometria_II >= 3.0 & nota_econometria_II <= 5.0){
  
  print("Usted ha aprobado satisfactoriamente el curso de Econometría II")
  
} else if (nota_econometria_II >= 0 & nota_econometria_II < 3.0){
  
  print("Mijo..., se rajó")
  
} else if (nota_econometria_II)  {
  
  print("???")
}

##### 4.2. Bucles ####

# Los bucles usaran una iteración de elementos y aplicará algún procedimiento.
# Los bucles básicos son el "For" y "While".


#~~ Bucle For ~~#

# La sentencias "for" permite ejecutar un comando repetidas veces según el vector
# seleccionado, por ejemplo:

for(i in c(1,2,3,4,5,6,7,8)){
  
  print(i^2-10)
  
}

# También podemos usar un bucle con condicionales adentro.

# Nota: Cuando usemos el comando "print" es recomendable usar "quote = FALSE" 
# para ignorar las comillas del texto.

for(n in -10:10){
  
  if(n >= 0){
    
    print(paste(n, "es un número positivo"), quote = FALSE)
    
  } else{
    
    print(paste(n, "es un número negativo"), quote = FALSE)
  }
}

# También podemos generar bucles anidados, por ejemplo las tablas de multiplicar.

numeros_1 = 1:10
numeros_2 = 1:10

for(x in numeros_1){
  
  for(y in numeros_2){
    
    print(paste(x,"X",y,"=", x*y), quote = "FALSE")
  }
  
  print(" ", quote = FALSE)
}


#~~ Bucle While ~~#

# El bucle While ejecutará un comando mientras se cumpla determinada condición.

i = 0

while (i < 5){
  
  print(i)
  
  i = i + 1    # Condición de actualización
}

#### 5. Paquetes útiles para la econometría ####

# A lo largo del curso se usaran paquetes diseñados para el manejo de datos como 
# series de tiempo, corte transversal y data panel. Además varios paquetes traeran 
# funciones que se verán el curso de econometría II, como pruebas de raíz unitaria,
# gráficos de FAC y FACP, test de hipótesis clave, etc. Algunos de los paquetes 
# importantes son:  

# readxl:     Para leer archivos excel
# tidyverse:  Conjunto de paquetes (incluye dplyr y ggplot2)
# tseries:    Para manipular series de tiempo
# ggplot2:    Poderosisimo motor gráfico para datos.
# ggfortify:  Extensión de ggplot2 que permite graficar tseries
# seasonal:   Para desestacionalizar series de tiempo
# urca:       Para las pruebas de raíz unitaria
# lmtest:     Para pruebas de significancia individual 
# forecast:   Para generar las ACF, ACFP y generar pronósticos
# plm:        Para la manipulación de datos panel
# dplyr:      Para modificar Data.Frames
# vars:       Para usar modelos VAR
# VAR.etp:    Para intervalos de confianza por Bootstrapping
# seasonal:   Para desestacionalizar series. 
# tsDyn:      Para hallar los coeficientes del modelo VECM
# haven:      Para leer archivos de STATA


#~~ Instalación de los paquetes ~~#

# Existen multiples formas de instalar y utilizar un paquete. Para esta sesión 
# usaremos los siguientes:

# Forma 1 de instalar e importar paquetes:

  # install.packages(- "Paquete" -)
  # library( - Paquete -)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tidyr")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)


#### 6. Estadística descriptiva (Análisis exploratorio de datos) ####

# Para esta sección usaremos los paquetes previamente instalados.

# En primer lugar traigamos una base de datos.

?mtcars # Base de datos sobre características de vehiculos antigüos

##### 6.1. Medidas de tendencia central #### 

# Observar las bases de datos

class(mtcars)

View(mtcars)    # Ver el Data Frame
glimpse(mtcars) # Ver un "resumen" en la consola


# Medidas de tendencia aplicables a una variable (Usando como referencia los 
# "horse-powers" - Caballos de fuerza).

# Media 
mean(mtcars$hp)

# Varianza
var(mtcars$hp)

# Desviación estándar
sd(mtcars$hp)

# Rango
range(mtcars$hp)

# Cuantiles 
quantile(mtcars$hp, probs = 0.25)
quantile(mtcars$hp, probs = 0.89)

# Sumatoria 
sum(mtcars$hp)

# R no presenta una función predeterminada para la moda.

# Para ver todas estas funciones reunidas, podemos usar "summary". Una buena forma
# de analizar la estadística descriptiva.

summary(mtcars)

# Podemos ver la covarianza y correlación de las variables

cov(mtcars)

cor(mtcars)

##### 6.2. Análisis exploratorio de datos gráfico ####

# De cierta forma, los gráficos son mucho más útiles para entender el comportamiento
# de los datos. En esta subsección se podrán encontrar gráficos descriptivos como
# histogramas, diagramas, gráficos de dispersión, etc.

#~~ Histogramas ~~#

hist(mtcars$mpg)

# Podemos agregar títulos y colores también

hist(mtcars$mpg, main = "Histograma de las millas por galón",
     col = "lightblue", ylab = "Frecuencia", xlab = "Millas por galón")



#~~ Boxplots (Cajas y bigotes) ~~#

boxplot(mtcars$hp)

# Boxplot múltiple 

boxplot(mtcars)

# Hagamos un gráfico más bonito

boxplot(mtcars$hp, main = "Boxplot de los caballos de fuerza",
     col = "lightgreen", ylab = "Número de caballos")

boxplot(mtcars$hp, mtcars$disp, 
        main = "Boxplot de HP y Disp",
        col = c("salmon", "orange"))


#~~ Gráficos de dispersión ~~#

plot(mtcars$hp,mtcars$disp)

plot(mtcars$hp, mtcars$disp, 
      main = "Diagrama de dispersión",
      col = "blue", xlab = "", ylab ="")


# Podremos ver más de una dispersión al mismo tiempo

dispersiones = mtcars[1:3]

plot(dispersiones)

#~~ Grafíco de cuantiles por normalidad ~~#

# Posteriormente será usado para ver pruebas Jarque-Bera de normalidad.

qqnorm(mtcars$hp)
qqnorm(mtcars$disp)



##### 6.3. Manejo de datos con Dplyr ####


# Dplyr ofrece una de las funciones más poderosas para el manejo de datos. 
# La función pipe definida como "%>%" permite realizar secuencias de operaciones 
# de manipulación y transformación de datos en una forma legible y eficiente. 
# En resumen, la función pipe nos permitirá acceder a las filas y columnas de un 
# Data.Frame con el fin de seleccionar, filtrar, reordenar, eliminar y muchas 
# funciones más al gusto del analista de datos (Pueden generar un pipe de forma
# rápida usando (Ctrl + Shift + M).

# Veamos algunas funciones con la base de datos que ya hemos empleado. A estas 
# funciones le denominamos verbos.

#~~ Mtcars~~#

# Supongamos que somos un ciudadano interesado en carrocería de los 80's  y queremos 
# comprar un nuevo vehiculo. Sin embargo, tenemos un par de condiciones... 
# Necesitamos un vehiculo que sea amable con nuestro bolsillo, es decir, que 
# gastemos la menor cantidad de gasolina (el mayor mpg posible). También queremos
# que nuestro carro tenga más de 4 cilindros porque necesitamos una combustión 
# alta. Además queremos potencia en nuestro vehiculo, por lo que necesitaremos 
# una cantidad significativa de caballos de fuerza. Para nosotros será suficiente
# más de 120 hp.


#~~ Seleccionar ~~#

# "%>% select()" Nos permitirá seleccionar solamente un conjunto de variables 
# especificas, para este caso, seleccionaremos las variables numéricas.

mtcars_selected = mtcars %>%  select(mpg, cyl, hp)
mtcars_selected

#~~ Ordenar __#

# Necesitamos ordenar nuestros datos depende a la duración de la gasolina de los 
# vehiculos. Es decir, ordenaremos de forma descendente los datos con respecto al 
# millaje por galón. Para ello organizamos nuestros datos con "%>% arrange(desc())":

mtcars_selected %>% arrange(desc(mpg))

#~~ Filtrar ~~# 

# Podemos notar que el Toyota Corolla tiene el mayor millaje, ajustemos las 
# variables ahora con el fin de que tengan más de 4 cilindros, para ello usamos: 
# "%>% filter()".

mtcars_selected_c4 = mtcars_selected %>% filter(cyl > 4)
mtcars_selected_c4

# Ahora filtramos por caballos de fuerza

mtcars_selected_c4_hp = mtcars_selected_c4 %>% filter(hp > 120)
mtcars_selected_c4_hp

# Veamos de mayor a menor el mpg

mtcars_selected_c4_hp %>% arrange(desc(mpg))

# El Ferrari Dino, es la mejor opción.

#~~ Mutar ~~#

manip1 = mtcars %>%  mutate(
  mult = mpg * cyl,
  sqr_mpg = mpg^2)

manip1

# Veamos un ejemplo analogo con los condados de EE.UU

#~~ Counties ~~#

# Importamos la base de datos con la función read_rds(). Una manera de seleccionar
# la carpeta directamente 

counties = read_rds(file.choose())

# Visualización de la base de datos

View(counties)
glimpse(counties)

# Seleccionamos ciertas columnas de interés

counties_selected = counties %>%
  select(state, county, population, 
         private_work, public_work, self_employed)

# Ordenar de manera descendente el trabajo público

counties_selected %>%
  arrange(desc(public_work))

# Vamos a seleccionar otras variables 

counties_selected2 = counties %>%
  select(state, county, population)

# Filtramos por los condados con una población superior a un millón

counties_selected2 %>%
  filter(population > 1000000)

# Filtramos por los condados en el estado de California con una población 
# superior a un millón

counties_selected2 %>%
  filter(state == "California",
         population > 1000000)

# Resultado: hay 9 condados en California con una población superior a un millón


# Seleccionamos de nuevo unas columnas en específico

counties_selected3 = counties %>%
  select(state, county, population, men, women)

# Calculamos la proporción de mujeres como una fracción entre el número de 
# mujeres /población

counties_selected3 %>%
  mutate(proportion_women = women / population)

# A manera de resumen de los 4 verbos más importantes: Select, mutate, filter y 
# arrange

counties %>%
  
  select(state, county, population, men, women) %>% # Seleccionamos 5 columnas
  
  mutate(proportion_women = women / population) %>% # Añadimos la proporción de mujeres
  
  filter(population >= 10000) %>%  # Filtramos por una población de al menos 
                                   # 10k habitantes
  
  arrange(desc(proportion_women)) # Ordemanos de manera descendente 
                                  # por la proporción de mujeres


# Notemos que en el ejemplo anterior logramos generar una filtración de los datos
# en una sola instrucción. Es otra manera útil de hacerlo.


#====> Otros 2 verbos importantes: Summarize y group_by

# De nuevo, seleccionamos las variables que necesitamos

counties_selected4 = counties %>%
  select(state, county, population, income, unemployment,land_area)

# Agrupamos para encontrar la población mín., máx. desempleo y el ingreso promedio

counties_selected4 %>%
  summarize(min_population = min(population),
            max_unemployment = max(unemployment),
            average_income = mean(income))

# ¿A cuáles condados corresponden los 2 primeros valores?

counties_selected4 %>% 
  filter(population == "85")

# filter(unemployment == "29.4")

#~~ Sumarize y Group_by ~~#

# Agrupamos por (group by) estado para encontrar el área total y la población

data_grap1 = counties_selected4 %>%
  group_by(state) %>%
  summarize(total_area = sum(land_area),
            total_population = sum(population)) %>% 
  
# Si queremos saber la densidad (en cantidades de personas por metro^2)
  
mutate(density = total_population  / total_area) %>%
arrange(desc(density)); data_grap1

##### 6.4. Paquete tidyr ####

# El paquete "tidyr" en R es utilizado para manipular y reorganizar datos en un 
# formato "tidy", donde cada variable tiene su propia columna y cada observación 
# tiene su propia fila. Sus funciones principales incluyen "gather" para combinar
# múltiples columnas en una sola, "spread" para separar una columna en múltiples,
# y "pivot_longer" y "pivot_wider" para transformar datos entre formatos largo y 
# ancho. Estas funciones ayudan a la limpieza y transformación de datos para un
# análisis más eficiente y claro.


# Usemos la base de datos iris.

# iris: base de datos sobre 3 tipos diferentes de flores 

view(iris)
glimpse(iris)

# Formato Long

iris_longer = pivot_longer(iris, cols = 1:4, 
                           names_to = "Plant_characteristics",
                           values_to = "Value")

glimpse(iris_longer)

# table2: número de casos de tuberculosis en algunos paises

view(table2)
glimpse(table2)

# Formato wider

table2_wider = pivot_wider(table2, names_from = type,values_from = count)

glimpse(table2_wider)

# Como pueden observar, el paquete tidyr 
# Efectivamente permite modificar las dimensiones de las bases de datos

##### 6.5. Gráficos con ggplot2 ####


# Gráfico de dispersión con ggplot2: Relación entre caballos de fuerza y millaje
# por galón con selección de grupos.

grafica = mtcars %>% 
  
  ggplot(aes(x = mpg, y = hp)) +
  geom_point(aes(color = factor(cyl))) +
  geom_smooth(method='lm', formula= y~x, se = F)+
  labs(title = "Gráfico de dispersión", x = "Millas por Galón (Gasto)",
       y = "Caballos de fuerza")+
  geom_abline(slope = 1, intercept = 1)
  
grafica

# Histogramas con ggplot2

ggplot(data, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Histograma de Consumo de Combustible", 
       x = "Consumo (mpg)", y = "Frecuencia")

# Combinación de Boxplots

ggplot(mtcars, aes(x = factor(cyl), y = hp)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplots de Potencia (hp) y Desplazamiento (disp) por Cilindros",
       x = "Cilindros", y = "Valor") +
  geom_boxplot(aes(x = factor(cyl), y = disp), fill = "purple",
               color = "black", alpha = 0.7) +
  facet_grid(. ~ cyl, scales = "free", space = "free_x") +
  theme_minimal()


# Recomendamos revisar la documentación de ggplot2, tidyr y dplyr para generar 
# gráficos más interesantes y mejor manipulación de datos.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          FIN DEL CÓDIGO                           #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
