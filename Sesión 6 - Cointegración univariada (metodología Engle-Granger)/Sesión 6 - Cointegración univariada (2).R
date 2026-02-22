#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2023 - 02
#                            Econometría II | Monitoría 
#
#                                     Sesión 6   
#                             Cointegración Univariada 
#                                  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Limpieza de entorno

rm(list = ls())
dev.off()

#_____________________________________________________________________________________#

#~~~~~~~~~~~~~~~~~~~~~#
# Tabla de contenidos #
#~~~~~~~~~~~~~~~~~~~~~#
#
#  Instalación de paquetes
#  Ejemplo de una regresion espuria con series simuladas
#  Ejemplo de regresión espuria: divorcios en el estado de Maine vs consumo per 
#  capita de margarina
#  A.Expectations hypothesis: Hipótesis de expectativas de la estructura temporal 
#     de la tasa de interés 
#  Paso 1: Uso de la prueba ADF y KPSS sobre las series
#   1.1 ADF y KPSS sobre la tasa de interés de los T-Bills a 3 meses 
#   1.2 ADF y KPSS sobre la tasa de interés de los T-Bonds a 10 años
#  Paso 2: Estimación de los residuales de la regresión en niveles
#   2.1 Estimación por MCO
#  Paso 3: Regresión con variables cointegradas 
#   3.1 Modelo de Minimos Cuadrados Ordinarios Dinámicos
#   B.PPA
#
#_____________________________________________________________________________________#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Instalación de paquetes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Paquetes de proposito general

library(pacman)

pacman::p_load(
tidyverse, # Conjunto de paquetes fundamental para manejo de datos enR
broom, # Paquete para analizar modelos en R
readxl # Para importar archivos excel en R
)
# Paquetes de series de tiempo 
library(pacman)

pacman::p_load(
dynlm, # Paquete para realizar MCOD (DOLS) y modelos de corrección de error en R
urca, # Paquete para realizar pruebas de raíz unitaria y cointegración en R
car, # Para usar la función qqPlot
aTSA,
forecast,
tseries,
rlang,
stargazer
)


#En economía, nos interesa poder analizar las relaciones económicas entre diferentes
#variables económicas, y en particular, en macroeconomía nos interesa estudiar
#las relaciones que se pueden establecer entre variables agregadas. 

#La metodología de cointegración, nos permite analizar relaciones de largo plazo
#entre dos o más variables analizando la tendencia o comportamiento común entre éstas.
#No obstante, hay que tener muy presente que un aparente movimiento común 
#no garantiza, que exista una relación de largo plazo entre las series.Esto, 
#debido a que se puede presentar una regresión espuria.

# Ejemplo de una regresión espuria con series simuladas -------

# A modo de ilustración, se va a simular dos caminadas aleatorias independientes 
# y se realizará una regresión por MCO entre ellas. 

# Defino la semilla 
set.seed(12345)

# Número de observaciones
n = 10000

# Se inicializan las dos series de tiempo con un vector de ceros 
y = rep(0, n)
y
x = rep(0, n)
x
# Se construye un vector de errores normales de 10000 observaciones
# para cada una de las series
ey = rnorm(n)
ey
ex = rnorm(n)
ex

# Ojo: Notar que los dos errores son independientes entre sí, por lo que 
#      las series (caminatas aleatorias) también lo son

# Se construyen las dos caminatas aleatorias:
# Nota: Como las series son caminatas aleatorias el coeficiente AR de éstas 
#       es igual a 1

for (i in 2:n) {
  y[i] = y[i-1] + ey[i]
  x[i] = x[i-1] + ex[i]
}
x
y

# Se realiza la regresión por MCO : y_t = alpha_1 * x_t + e_t 
sp_regression = lm(formula = y ~ x); stargazer(sp_regression, type="text")

xts = ts(x)
yts = ts(y)


x11()
ts.plot(xts,yts, col = c("lightblue", "coral2"),
        main = "Comparación serie X y Y", sub = "Series Suimuladas")

legend("topright", legend = c("X","Y"), col = c("lightblue","coral2"),
       lty = c(1,1))


# Como se observa, el coeficiente de la variable x es significativo a un nivel de
# significancia de 0,1% y arroja un R^2 de 0.19 cuando este debería ser cero 

#### Ejemplo de regresión espuria: divorcios en el estado de Maine vs consumo per capita de margarina ----

# Ahora, vamos a usar dos series que se suponen independientes los divorcios en 
# el estado de Maine, en Inglaterra y el consumo per cápita de margarina en EE.UU.

# Subimos la base de datos:
ej_spou_regre = read_xlsx(file.choose())
View(ej_spou_regre)

#Convertirmos las variables en series de tiempo
div_rate_maine = ts(ej_spou_regre$Div_rate_Maine, start = c(2000), end = c(2009),
                    frequency = 1)

pc_cons_margarine = ts(ej_spou_regre$Pc_cons_margarine, start = c(2000), end = c(2009),
                       frequency = 1)

# Se grafican las series en un mismo plano:
autoplot(cbind(div_rate_maine, pc_cons_margarine), facets = F, 
         main="Divorce Rate in Maine vs Consumo per cápita de margarina en EE.UU",
         xlab="", ylab="", size=1.5)

#Aunque se observa que en el caso del consumo per cápita de margarina se obtiene 
#una senda positiva por encima de la tasas de divorcio sus movimientos se asimilan 
#entre sí. 


# Generamos el modelo de regresión de tipo: y_t = x_t
sp_regression_ej = lm(formula = pc_cons_margarine ~ div_rate_maine)
stargazer(sp_regression_ej, type="text")

# Como se observa, aunque no hay ninguna relación entre las variables, obtenemos un 
# R^2 muy cercano a 1. De igual forma, los coeficientes de la variable dependiente
# rezagada son significativos.

#Comparamos los resultados de ambas regresiones: 
stargazer(sp_regression, sp_regression_ej, type = "text")


# A. Expectations hypothesis: Hipótesis de expectativas de la estructura temporal de la tasa de interés  ----

# En economía financiera, existe la concepción de que las tasas de interés de corto
# y de largo plazo de los bonos de un mismo país tienden a tener un movimiento común.
# Un bono, se puede entender como ese instrumento/vehículo de financiación que utilizan 
#los países para conseguir financiación extranjera.

# El análisis se centra en la explicación de la famosos Yield Curve de dichos bonos
# y en explicar su dinámica así como su pendiente.Existen diferentes teorías que 
#intentan explicar este comportamiento común de las tasa de interés de dichos bonos. 

# Para este ejemplo veremos la relacion entre la tasa de interés 
# de corto plazo (Treasury bills a 3 meses) y la tasa de interés de largo plazo (Treasury
# bonds a 10 años) de los Estados Unidos. En caso de estar cointegradas las series, 
# se esperaría que el spread entre estas dos tasas, que es básicamente la diferencia
# entre las dos tasas, sea una serie estacionaria.  En algunos contextos, se utiliza 
#la Treasury bills a 3 meses como la tasa libre de riesgo mientras que en otros se 
#emplea la Treasury bonds a 10 años como la tasa libre de riesgo

# Importación de la base de datos 


# Series históricas de las tasas de interés de los bonos del Tesoro de los EE.UU.
# A 3 meses (TB3MS), a un año (GS1) y a 10 años (GS10)

USMacroSWQ <- read_xlsx(file.choose(),sheet = 1,col_types = c("text", rep("numeric", 9)))
View(USMacroSWQ)

# Formato de la columna fecha (date)
USMacroSWQ$...1 <- as.yearqtr(USMacroSWQ$...1, format = "%Y:0%q")
View(USMacroSWQ)

# Ajustamos nombres de las columnas
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI",
                          "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")

# Visualización de la base de datos
glimpse(USMacroSWQ)
view(USMacroSWQ)

# Tasa de interés 3-months Treasury bills
TB3MS = ts(USMacroSWQ$TB3MS, start = c(1957,1), end = c(2013,4), frequency = 4)
# Tasa de interés 10-years Treasury bonds
TB10YS = ts(USMacroSWQ$GS10,start = c(1957,1), end = c(2013,4), frequency = 4)


# Se analiza el comportamiento gráfico de las dos tasas
x11()
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)),
     plot.type = "single",
     lty = c(1, 1),
     lwd = 2,
     xlab = "Año",
     ylab = "Porcentaje anual",
     ylim = c(-5, 17),
     main = "Tasas de interés", col = c("coral2","purple3"))

legend("bottomleft", legend = c("TB 3 Meses", "TB 10 años"), 
       col =  c("coral2","purple3"), lty = c(1,1) )


# Ahora se grafica el spread de las dos tasas en la misma gráfica.
lines(as.zoo(TB10YS-TB3MS),
      col = "steelblue",
      lwd = 2,
      xlab = "Año",
      ylab = "Porcentaje anual")

# Del anterior gráfico es posible concluir que las series posiblemente están 
# cointegradas, en tanto:las series en nivel aparentemente son I(1) (aparentan 
# tener una tendencia estocástica común), y finalmente, el spread entre ellas parece 
# ser estacionario (la combinación lineal entre ambas parece eliminar la tendencia estocástica)


###
# Metodología Engle & Granger para la hipótesis de expectativas: 
###

# Paso 1: Uso de la prueba ADF y KPSS sobre las series ----

# 1.1 ADF y KPSS sobre la tasa de interés de los T-Bills a 3 meses ----


# Hay dos tipos de pruebas importantes que se emplea comúnente en la literatura para 
# inverstigar si una serie tiene o no tiene raíz unitaria: 
## 1. La familia de las pruebas tipo ADF donde la hipótesis nula es no estacionariedad
## 2. La familia de pruebas donde la hipótesis nula es estacionariedad. La más famosas 
##    de este tipo de prueba es la prueba KPSS

# Primero vamos a hacer una prueba de ADF con tendencia e intercepto. 


adf1 = ur.df(TB3MS, lags=4, type = "trend"); plot(adf1)
summary(adf1)

#El test nos indica que el termino de tendencia no es significativo, luego pasaremos 
# a evaluar la serie con el test solo con deriva.

adf2 = ur.df(TB3MS, lags=4, type = "drift"); plot(adf2)
summary(adf2)

#Los resultados indican que la serie tiene al menos una raíz unirtaria. El phi1,
#por su parte, indica que la deriva de la serie no es significativa, por tanto, 
#debe hacerse una prueba sin deriva.

adf3 = ur.df(TB3MS,lags=4, type = "none"); plot(adf3)
summary(adf3)

#Claramente se evidencia que la serie tiene al menos una raíz unitaria, en tanto 
#no se rechaza la hipótesis nula. Noten la importancia de determinar si la serie 
#tiene términos detemrminísticos, pues el valor calculado en cada especificación 
#de la prueba cambió de forma importante.


# Hacemos la prueba KPSS cuya hipótesis nula es que la serie es estacionaria, la 
#cual es totalmente contraria a la ADF. Se toman 4 rezagos porque fueron los mismos 
#utilizados en las pruebas ADF

kp1 = ur.kpss(TB3MS, use.lag = 4,
              type = "mu") #el mu nos indica que es una prueba sin términos determinísticos.
summary(kp1)

}

# Claramente rechazamos la hipótesis nula de estacionariedad, así que la serie de la tasa
# de interés de los treasury bills a 3 meses no es estacionaria. 

# 1.2 ADF y KPSS sobre la tasa de interés de los T-Bonds a 10 años ----

# Primero vamos a hacer una prueba de ADF con tendencia e intercepto. Los valores 
# críticos de la prueba son sensibles a la inclusión de términos determinísticos, 
#de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

adf4 = ur.df(TB10YS,lags=1, type = "trend"); plot(adf4)
summary(adf4)

#El test nos indica que el termino de tendencia no es significativo, luego pasaremos 
# a evaluar la serie con el test solo con deriva.


adf5 = ur.df(TB10YS, lags=1, type = "drift"); plot(adf5)
summary(adf5)

#Los resultados indican que la serie tiene al menos una raíz unirtaria. El phi1,
#por su parte, indica que la deriva de la serie no es significativa, por tanto, 
#debe hacerse una prueba sin deriva.

adf6 =  ur.df(TB10YS, lags=1, type = "none"); plot(adf6)
summary(adf6)

# Claramente se evidencia que la serie tiene al menos una raíz unitaria, en tanto 
# no se rechaza la hipótesis nula. 


# Hacemos la prueba KPSS cuya hipótesis nula es que la serie es estacionaria, la cual 
# es totalmente contraria a la ADF. 

kp2 = ur.kpss(TB10YS, use.lag = 1,
              type = "mu") #el mu nos indica que es una prueba sin términos determinísticos.
summary(kp2)
plot(kp2)

# Claramente rechazamos la hipótesis nula de estacionariedad, así que las serie 
#tiene al menos una raíz unitaria. 


#Hacemos la prueba ADF sobre las series diferenciadas para confirmar que son I(1)
adf8 = ur.df(diff(TB3MS), lag=2, type = "none");plot(adf8); summary(adf8) #Estacionaria. 
adf7 = ur.df(diff(TB10YS), lag=1, type = "none");plot(adf7); summary(adf7) #Estacionaria

# Paso 2: Estimación de los residuales de la regresión en niveles ----

# La teoría económica sugiere que el vector de cointegración es -1. Para corroborar 
#lo anterior empíricamente, se empieza encontrando el valor del vector de cointegración. 

# El comando dynlm: Me permite estimar estimar modelos lineales dinámicos
#                   Incluyendo modelos dinámicos de series de tiempo. 

# Nota: 
## TB3MS: Tasa de interés de los T-bills de EE.UU. a 3 meses
## TB10YS: Tasa de interés de los Treasury bones de los EE.UU. a 10 años 

# 2.1 Estimación por MCO: ----

# Test de cointegración de Engle-Granger: 
# Pasos: 
## 1. Estimar por MCO o por MCOD (DOLS) el parámetro de cointegración 
##    entre las dos series que se creen que están cointegradas
## 2. Recuperar los residuales de la regresión anterior
## 3. Efectuar un test ADF sobre dichos residuales para saber si son o no son 
##    estacionarios. En caso de ser estacionarios, según el test de engle-granger
##    las series están cointegradas y en caso de no ser estacionarios, entonces
##    según este mismo test las series no están cointegradas.
## 4. Emplear los valores críticos modificados para el test de cointegración de 
##    Engle-Granger.

# Si TB3MS y TB10YS son series I(1) y además están cointegradas, se puede estimar
#una regresión en niveles:  

# El estimador de MCO de la regresión en niveles es consistente cuando yt y zt 
#están cointegradas

FS_EGADF <- dynlm(TB10YS ~ TB3MS) 
summary(FS_EGADF)


# Parámetro/vector de cointegración calculado por MCO: 

coint_param_MCO = tidy_FS_EGADF$estimate[2]; coint_param_MCO

# Se obtienen los residuales de la regresión

z_hat <- residuals(FS_EGADF)

# La idea, es verificar si los residuales de la regresión son estacionarios, 
# es decir, si son I(0), porque de serlo se sabe que la serie de residuales 
# seria estacionaria y por tanto las series estarian cointegradas

# Graficamos los residuales y analizamos su ACF y PACF

x11()
plot.ts(z_hat)


x11()
lags = 18
par(mfrow=c(1,2))
acf(z_hat,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales') 
pacf(z_hat,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales')

# Hacemos la prueba ADF sin términos determinísticos.
adf9 = ur.df(z_hat,lags=1,type = "none"); plot(adf9); summary(adf9) 

#Los residuales son estacionarios: hay cointegración. 

# No obstante, la prueba de ur.df ofrece valores críticos que no corresponden a 
#los valores que se requieren para la prueba de cointegración a pesar de que las 
#mecánica sea lo misma (Estimo por MCO la regresión entre TB10YS ~ TB3MS, cálculo 
#los resdiduales de dicha regresión y realizo una prueba de ADF pero utilizando los 
#valores críticos correctos (test de Engle-Granger)

# Lo anterior, se debe a que los residuales fueron estimados (no son las verdaderas 
#innovaciones porque no son conocidas) debido a que  el vector de cointegración 
#(el theta) en el caso univariado tuvo que ser estimado en la primer etapa.

# También es posible ejecutar la prueba de cointegración de engle-granger de manera 
# automática de una vez .El comando coint.test del paquete aTSA permite realizar 
#los dos pasos de la prueba de cointegración 

coint.test(TB10YS, TB3MS)

# El test Type1: no trend, muestra que se rechaza la hipótesis nula 
# de no cointegración a favor de la hipótesis alternativa de que las series 
# están cointegradas 

# Nota: A veces el test de cointegración de engle-granger puede no dar cointegración 
#       en una dirección de las variables, es decir en x ~ y, pero en la otra dirección 
#       sí puede dar cointegración y ~ x, o viceversa.

# numero de datos a la hora de realizar el test de cointegracion 
length(TB10YS)

# Sea el valor critico de la prueba al 5% de significancia igual -3.34. Luego se rechaza 
#la hipotesis nula de no cointegracion y se concluye de  que hay cointegracion entre 
# TB10YS y TB3MS. Revisar de igual forma a la hora de hacer el taller el numero de 
# observaciones 

# Paso 3: Regresión con variables cointegradas ----

# 3.1 Modelo de Minimos Cuadrados Ordinarios Dinámicos ----

# Ahora bien, en la práctica es mucho más usual realizar un modelo de Mínimos cuadrados 
# dinámicos (MCOD) o DOLS (Dynamic Ordinary Least Squares), dado que a pesar de que 
# cuando las series están cointegradas MCO es una metodología consistente, se tiene 
# que MCO tiene una distribución no normal por lo que la inferencia estadística no es válida. 

# Asumiremos que hay un máximo de 6 rezagos, por lo tanto, miraremos cuál tiene 
#el menor AIC.

#Ojo con los signos! -1 denota un adelanto en la serie (un paso adelante) y 1 denota 
#un rezago (un paso atrás tal y como se ve rezagos/lags en clase)

# Tener en cuenta que para un modelo dinámico es necesario incluir tanto rezagos como adelantos
# Para que la inferencia sea válida 

FS_MCOD1 = dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-1:1)); summary(FS_MCOD1)
FS_MCOD2 = dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-2:2)); summary(FS_MCOD2)
FS_MCOD3 = dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-3:3)); summary(FS_MCOD3)
FS_MCOD4 = dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-4:4)); summary(FS_MCOD4)
FS_MCOD5 = dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-5:5)); summary(FS_MCOD5)
FS_MCOD6 = dynlm(TB10YS ~ TB3MS + L(d(TB3MS),-6:6)); summary(FS_MCOD6)


# Se selecciona el modelo que minimice el AIC
cbind(AIC(FS_MCOD1), AIC(FS_MCOD2), AIC(FS_MCOD3), AIC(FS_MCOD4), AIC(FS_MCOD5), AIC(FS_MCOD6))

# Se elige el modelo con 6 rezagos y 6 adelantos, en tanto tuvo el menor AIC
summary(FS_MCOD6)

# También es posible encontrar los resultados de la regresión utilizando
# el comando tidy() del paquete broom
tidy_FS_MCOD6 = tidy(FS_MCOD6)

# Parámetro/vector de cointegración calculado por MCO: 
coint_param_MCOD = tidy_FS_MCOD6$estimate[2]; coint_param_MCOD

# Observen que ahora la estimación del vector/parámetro de cointegración 
# dió 0.95737 lo cuál es bastante cercano al valor teórico de 1.

# Se calculan los residuales del modelo FS_MCOD6: 
residuals_FS_MCOD6 = residuals(FS_MCOD6)

# Graficamos el spread que dice la teoría económica resepcto al 
# spread "empírico" que obtuvimos:

### TB10YS-TB3MS: Es un spread teórico, en la medida de que asume un vector
###               de cointegración de 1

### residuals_FS_MCOD6: Es el "spread" que se observaría si se asumiera el vector
###                      de cointegración igual a 0.95737

x11()
plot(merge(as.zoo(TB10YS-TB3MS), as.zoo(residuals_FS_MCOD6)),
     plot.type = "single",
     lty = c(2, 1),
     lwd = 2,
     ylim = c(-5, 5),
     xlab = "Año",
     main = "Diferencias")

legend("bottomright",
       legend = c("Spread","MCOD"),
       col = c(2,1),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1))

lags = 18
par(mfrow=c(1,2))
acf(residuals_FS_MCOD6, lag.max=lags, plot=T , lwd=2 , xlab='', main='ACF de los residuales') 
pacf(residuals_FS_MCOD6, lag.max=lags, plot=T, lwd=2, xlab='', main='PACF de los residuales')

#Hacemos la prueba ADF sobre los residuales del modelo MCOD
adf10 = ur.df(residuals(FS_MCOD6), lag=1, type = "none"); plot(adf10)
summary(adf10) #Residuales estacionarios,

# NOTA: Casi siempre los residuales en un modelo MCOD presentará autocorrelación 
#       debido a que no se incluyen rezagos  de la variable dependiente, de  manera 
#       que en esos casos es recomendable utilizar errores robustos tipo HAC.

# 3.2 Estimación del modelo de corrección de errores ---- 

# MODELO DE CORRECIÓN DE ERRORES (VEC): Utilizaremos los betas estimados por 
# Introduciremos 3 rezagos de cada variable por supuesto. La 
# inclusión del intercepto en el modelo de largo plazo no afecta el resultado.

# En modelo de corrección de errores se estiman dos ecuaciones, cada una utilizando 
# como variable dependiente una serie diferente de las dos series que están cointegradas
# (una serie diferente por cada ecuación diferente)

# 3.2.1 Modelo de corrección de errores usando el parámetro de cointegración estimado por MCO ---- 
# Parámetro/vector de cointegración que se obtuvó empleando MCO
parametro_cointegración_MCO = coint_param_MCOD

VEC_EQ1 <- dynlm(d(TB10YS) ~  L(TB10YS - parametro_cointegración_MCO * TB3MS,1) + L(d(TB3MS), 0:3) + L(d(TB10YS), 1:3));summary(VEC_EQ1) #El parámetro de velocidad de ajuste alpha tiene el signo esperado
stargazer(VEC_EQ1, type = "text")
VEC_EQ2 <- dynlm(d(TB3MS) ~  L(TB10YS - parametro_cointegración_MCO * TB3MS,1)+ L(d(TB3MS), 1:3) + L(d(TB10YS), 0:3));summary(VEC_EQ2) #El parámetro de velocidad de ajuste alpha tiene el signo esperado
stargazer(VEC_EQ2, type = "text")
# 3.2.2 Modelo de corrección de errores usando el parámetro de cointegración estimado por MCOD ---- 

# Parámetro/vector de cointegración que se obtuvó empleando MCOD
parametro_cointegración_MCOD = coint_param_MCOD

VEC_EQ3 <- dynlm(d(TB10YS) ~  L(TB10YS - parametro_cointegración_MCOD * TB3MS, 1) + L(d(TB3MS), 0:3) + L(d(TB10YS), 1:3));summary(VEC_EQ3) 

#El parámetro de velocidad de ajuste alpha tiene el signo esperado

stargazer(VEC_EQ3, type = "text")

VEC_EQ4 <- dynlm(d(TB3MS) ~  L(TB10YS - parametro_cointegración_MCOD * TB3MS, 1)+ L(d(TB3MS), 1:3) + L(d(TB10YS), 0:3));summary(VEC_EQ4) 

#El parámetro de velocidad de ajuste alpha tiene el signo esperado

stargazer(VEC_EQ4, type = "text")

# PASO 4: Validación de supuestos del modelo de corrección de errores ----

# En este caso, vamos a hacer la validación de supuestos del modelo de corrección
# de errores donde reemplazamos como coeficiente de cointegración el obtenido por MCOD, 
# es decir, parametro_cointegración_MCOD

# Cálculo de los residuales del modelo: 
residuales_VEC_EQ3 = residuals(VEC_EQ3)
residuales_VEC_EQ4 = residuals(VEC_EQ4)

#Analizamos que los residuales sean estacionarios: 

# Ecuación VEC_EQ3
adf_VEC_EQ3 = ur.df(residuales_VEC_EQ3, lag=1, type = "none"); plot(adf_VEC_EQ3)  
summary(adf_VEC_EQ3) # Residuales estacionarios. 

# Ecuación VEC_EQ4
adf_VEC_EQ4 = ur.df(residuales_VEC_EQ4, lag=1, type = "none"); plot(adf_VEC_EQ4)  
summary(adf_VEC_EQ4) #Residuales estacionarios.

# Los valores criticos para usar son los mismos que se usan para 
# la prueba de cointegracion de Engle-Granger y están en el manual suplementario del 
#Enders (Tabla C). No obstante, dicho test no es para probar que los residuales son 
#ruido blanco sino que son  estacionarios en media, lo cual es una consecuencia de 
# que haya cointegración. 

# Test para probar que los residuales son ruido blanco
# Selección del número de lags para la prueba
length(residuales_VEC_EQ3)/4 # aproximdamente 60

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuales_VEC_EQ3,lag=60, type = c("Box-Pierce")) #No rechazo H0, se cumple el supuesto. 
Box.test(residuales_VEC_EQ3,type='Box-Pierce',lag=20) #No rechazo H0, se cumple el supuesto. 
Box.test(residuales_VEC_EQ3,type='Box-Pierce',lag=30) #No rechazo H0,  se cumple el supuesto.

Box.test(residuales_VEC_EQ4,lag=60, type = c("Ljung-Box")) #NO rechazo H0, se cumple el supuesto. 
Box.test(residuales_VEC_EQ4,type='Ljung-Box',lag=20) #NO rechazo H0, se cumple el supuesto. 
Box.test(residuales_VEC_EQ4,type='Ljung-Box',lag=30) #NO rechazo H0,  se cumple el supuesto.

# En tanto los residuales no están autocorrelacionados en ninguna ecuación, deberían 
#validar los demás supuestos.

# A diferencia de MCOD, la inclusión de rezgos de la variable dependiente en la 
# ecaución, hace que el modelo de corrección de errores sí capture las estructuras 
# de correlación de carácter autorregresivo que allí no se incorporan que pueden 
# estar presentes en la serie cosa que no puede hace bien el modelo MCOD en la 
# medida que solo tiene incorporados rezagos de la variable independiente 


# 2. PPA  ----

#Ahora haremos un ejericio del libro de Enders para probar la PPA R=N*(P*/P)->  r= e + log(P*) - log(P)


#JAPANCPI is the Japanese price level and JAPANEX is the bilateral Japanese/U.S. exchange rate. 
#The starting date for all variables is January 1974 while the availability of the 
#variables is such that most end near the end of 2012. The price indices have been 
#normalized to equal 100 in January 1973

PPA <- read_excel(file.choose())
attach(PPA)
View(PPA)

e = ts(log(JAPANEX), start=c(1974,1), frequency=12)
lp.j = ts(log(JAPANCPI), start=c(1974,1), frequency=12)  
lp.u = ts(log(USCPI), start=c(1974,1), frequency=12)

#METODOLOGÍA ENGLE & GRANGER.


#Paso 1: Aplicaremos la prueba ADF y KPSS sobre cada una de las series.

## Prueba sobre el logaritmo de la tasa de cambio nominal

#Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores 
#críticos de la prueba son sensibles a la inclusión de términos determinísticos, 
#de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

adf11= ur.df(e, lags=9, type = "trend")
summary(adf11)
plot(adf11)

#El tau nos dice que la serie tiene al menos una raíz unitaria, mientras el phi3 
#nos dice que la tendencia no es significativa. Por lo tanto, vamos a mirar 
#únicamente la prueba con intercepto.

adf12= ur.df(e, lags=9, type = "drift")
summary(adf12)
plot(adf12)

#Los resultados indican que la serie tiene al menos una raíz unitaria. El phi2, 
#por su parte, indica que la deriva de la serie no es significativa, por tanto, 
#debe hacerse una prueba sin términos determinísticos.

adf13=ur.df(e, lags=9, type = "none")
summary(adf13)
plot(adf13)

#Claramente se evidencia que la serie tiene al menos una raíz unitaria, en tanto 
#no se rechaza la hipótesis nula. 

#Hacemos la prueba KPSS cuya hipótesis nula es que la serie es estacionaria, la 
#cual es totalmente contraria a la ADF. 

kp3= ur.kpss(e, use.lag = 9, type = "mu") 

#el mu nos indica que es una prueba sin términos deterministicos.
summary(kp3)
plot(kp3)

#Claramente rechazamos la hipótesis nula de estacionariedad, así que las serie no 
#es estacionaria. 

# Prueba sobre el logaritmo del nivel internacional de precios

# Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores 
# críticos de la prueba son sensibles a la inclusión de términos determinísticos, 
# de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

adf14= ur.df(lp.j, lags=8, type = "trend")
summary(adf14)
plot(adf14)

#La serie según esta prueba es estacionaria en torno a una tendencia determinística. 

#Hacemos la prueba KPSS cuya hipótesis nula es que la serie es estacionaria.

kp4=ur.kpss(lp.j, use.lag = 8, 
            type = "tau") #el tau nos indica que es una prueba con términos determinísticos.
summary(kp4)
plot(kp4)

#Claramente rechazamos la hipótesis nula de estacionariedad, así que las serie no 
# es estacionaria. Nos inclinamos por esta prueba al evaluar la gráfica de la serie 
# y sus residuales.


# Prueba sobre el logaritmo del nivel nacional de precios

#Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores 
#críticos de la prueba son sensibles a la inclusión de términos determinísticos,
# de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

adf15 = ur.df(lp.u,lags=7, type = "trend")
summary(adf15)
plot(adf15)

#La serie según esta prueba no es estacionaria en torno a una tendencia determinística. 

#Hacemos la prueba KPSS cuya hipóesis nula es que la serie es estacionaria.

kp5=ur.kpss(lp.u, use.lag = 7, 
            type = "tau") #el tau nos indica que es una prueba con términos determinísticos.
summary(kp5)
plot(kp5)

#Claramente rechazamos la hipótesis nula de estacionariedad, así que las serie no es 
#estacionaria. 


#Graficamos
x11()
autoplot(e, col="red", main = "logaritmo de la tasa de cambio nominal",
         xlab = "Fecha", ylab="", lwd=2)
autoplot(lp.j, col="red", main = "logaritmo del indice de precios de Japón",
         xlab = "Fecha", ylab="", lwd=2)
autoplot(lp.u, col="red", main = "logaritmo del indice de precios de USA",
         xlab = "Fecha", ylab="", lwd=2)



#Paso 2: Estimación de los residuales de la regresión en niveles.

#La teoría económica sugiere que el tipo de cambio real bajo PPA debe ser uno, 
#así que al sacar logaritmos nos queda e = log(p)-log(p*) 

R.1 <- dynlm(e ~ lp.j + lp.u) 
summary(R.1)

#Obtenemos los residuales
res <- residuals(R.1)

#Graficamos los residuales y analizamos su ACF y PACF
plot.ts(res)

lags=48
par(mfrow=c(1,2))
acf(res,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales') 
pacf(res,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales')

#Haremos la prueba ADF sin términos determinísticos.
adf16=ur.df(res, lags=12,type = "none") #Los residuales son estacionarios: hay cointegración. 
summary(adf16)
plot(adf16)


# Usando la funcion coef.test del paquete aTSA se obtiene: 

coint.test(e, cbind(lp.j, lp.u),nlag = 12)


#Paso 3: Modelo de Corrección de errores.
#Para este ejemplo solo estimaremos la primera ecuación. 
### Introduciremos 5 rezagos de cada variable por supuesto. 


#Sin incluir la constante en el término de corrección de error. 
VEC_EQ1 <- dynlm(d(e) ~  L(e-0.10420*lp.j -0.76823*lp.u ) + L(d(lp.j), 0:5) + L(d(lp.u), 0:5) + L(d(e), 1:5));summary(VEC_EQ1) #El parámetro de velocidad de ajuste alpha tiene el signo esperado
stargazer(VEC_EQ1, type = "text")
#Incluyendo la constante en el término de corrección de error.
VEC_EQ1.cons <- dynlm(d(e) ~  L(e-9.97459 -0.10420*lp.j  -0.76823*lp.u ) + L(d(lp.j), 0:5) + L(d(lp.u), 0:5) + L(d(e), 1:5));summary(VEC_EQ1.cons) #El parámetro de velocidad de ajuste alpha tiene el signo esperado

stargazer(VEC_EQ1.cons, type = "text")


#PASO 4: VALIDACIÓN

#Analizamos que los residuales sean estacionarios.
summary(ur.df(residuals(VEC_EQ1), lags=3, type = "none")) #Residuales estacionarios.
summary(ur.kpss(residuals(VEC_EQ1), type="mu", use.lag = 3)) #Residuales estacionarios.

#Los valores críticos para usar son los mismos que se usan para la prueba de 
#cointegracion y están en el Enders.No obstante, dicho test no es para probar que 
#los residuales son ruido blanco sino que son estacionarios en media, lo cual es 
#una consecuencia de que haya cointegración. 

# Test Box-Pierce para autocorrelaci?n en los residuales
Box.test(residuals(VEC_EQ1),lag=117,
         type = c("Box-Pierce")) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),
         type='Box-Pierce',lag=20) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),
         type='Box-Pierce',lag=30) # rechazo H0,no  se cumple el supuesto.

Box.test(residuals(VEC_EQ1),lag=117,
         type = c("Ljung-Box")) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),lag=60,
         type = c("Ljung-Box")) #rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),
         type='Box-Pierce',lag=20) # rechazo H0, no se cumple el supuesto. 
Box.test(residuals(VEC_EQ1),
         type='Box-Pierce',lag=30) #rechazo H0,  no se cumple el supuesto. 

#En tanto no se satisface el supuesto de no autocorrelación, tenemos que volver 
#a la etapa de estimación e incluir más rezagos. Si fuese un modelo de MCOD, es 
#natural que no se satisfaga el supuesto, por lo cual se utilizaría errores tipo HAC. 