#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2023 - 02
#                            Econometría II | Monitoría 
#
#                                     Sesión 4:
#
#      Metodología Box-Jenkins para la identificación, estimación y pronóstico de
#                           series de tiempo univariadas
#                                  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Limpiamos el entorono 

rm(list = ls())
dev.off()

#_____________________________________________________________________________________#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Tabla de Contenidos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Instalación e importación de paquetes 
# 1. Primer paso: Identificación 
#  1.1. Análisis gráfico
#  1.2. Prueba Dickey Filler aumentada
#  1.3. Transformación para volver estacionaria las series
#  1.4. Métodos para idenficar modelo ARIMA por criterios de información
#   1.4.1. Método manual
#   1.4.2. Función ARIMA de Fable
#   1.4.3. Función auto.arima() de Forecast
# 2. Segundo paso: Estimación
# 3. Tercer paso: Validación de supuestos
#  3.1. No autocorrelación serial de los errores
#  3.2. Homocedasticidad de los residuales
#  3.3. Normalidad en los residuos
# 4. Cuarto paso: Pronóstico
#  4.1. Pronósticos futuros
#  4.2. Ajuste vs Real
#____________________________________________________________________________________#

# Importante: Verificar que las series esten DESESTACIONALIZADAS. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  Instalación de Paquetes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Descargamos e importamos los paquetes que vayamos a usar con el paquete "pacman"

library(pacman)

# Pacman contiene una función denominada "p_load" que permite al usuario descargar
# un paquete e importarlo si no lo tiene, y si el usuario tiene descargado el 
# paquete, Pacman lo importa automáticamente. Veamoslo. 

pacman::p_load(
  
  forecast,   # Para hacer pronósticos con modelos arima
  lmtest,     # Significancia individual de los coeficientes ARIMA
  urca,       # Prueba de raíz unitaria
  tseries,    # Para estimar modelos de series de tiempo y hacer pruebas de supuestos
  stargazer,  # Para presentar resultados más estéticos
  psych,      # Para hacer estadísticas descriptiva
  seasonal,   # Para desestacionalizar series
  aTSA,       # Para hacer la prueba de efectos ARCH
  astsa,      # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA
  xts,        # Para utilizar objetos xts 
  tidyverse,  # Conjunto de paquetes (incluye dplyr y ggplot2)
  readxl,     # Para leer archivos excel 
  car,        # Para usar la función qqPlot
  mFilter,    # Para aplicar el Filtro Hodrick-Prescott
  quantmod,    
  
  # Paquetes del tidyverts
  
  fable,      # Forma moderna de hacer pronóstiocs en R (se recomienda su uso)  
  tsibble,    # Para poder emplear objetos de series de tiempo tsibble
  feasts      # Provee una colección de herramientas para el análisis de datos de series de tiempo 
)


# En este script modelaremos las siguientes series: 
#   - Índice de Producción Industrial (IPI)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                         METODOLOGÍA BOX-JENKINS                              #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 1. Primer paso: Identificación ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~ CARGAR DATOS ~~~#

# Vamos a trabajar con el industrial production index de los EE.UU. el cual es 
# un indicador económico que mide la producción real de los Estados Unidos en 
# términos de manufactura, minería, electicidad y gas. 


# Fuente: https://fred.stlouisfed.org/series/INDPRO#0 
# (Tomados de la FRED de la Reserva Federa de St. Louis)


#~~~~ Industrial Production Index (indprod) ~~~~#

# Ojo: Al ser un índice tiene que tener un año base en el cuál el valor del índice sea 100
       # (para el ejemplo el año base fue 2012, es decir Index 2012=100)

       # Valores mayores a 100 indican que para ese año corriente la producción 
       # manufacturera fue mayor que en el año base.
       # Valores menores a 100 indican que para ese año corriente la producción 
       # manufacturera fue menor que en el año base.


# Se cargan las series de tiempo

base_fred = read_excel(file.choose())

# Visualización de la base de datos

glimpse(base_fred)
View(base_fred)

#~~~ TRANSFORMACIÓN A DATOS TS y XTS ~~~#

# Convertir la serie en un objeto tipo ts

indprod = ts(base_fred$INDPRO, start = 1960, frequency = 4) 
 
#    Se coloca 4 para indiciar que la frecuencia es trimestral


# Visualización de los serie de tiempo

glimpse(indprod)  
view(indprod)

# Crear un objeto tipo xts

indprod_xts = xts(base_fred$INDPRO, 
                  order.by = base_fred$observation_date)

# Ver la clase de cada uno de los ejemplos de series de tiempo

class(indprod)
class(indprod_xts)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.1. Análisis gráfico ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~ GRÁFICAS DE LAS SERIE ~~~#

# Para el objeto ts, usamos la función "plot.ts()" para graficar.

x11() # Usamos x11() para generar una ventana completa de plots. 

plot.ts(indprod, xlab="Time",ylab="IPI",
        main="IPI trimestral de Estados Unidos",
        sub = "1960-2012 (Index 2012 = 100)"
        ,lty=1, lwd=1, col="blue")

# Con ggplot2 usamos "autoplot()"

autoplot(indprod, col="turquoise4", 
         xlab = "Fecha", ylab="", lwd=1)+ 
         theme_light()+
         ggtitle("IPI trimestre de Estados Unidos",
                 subtitle = "1960-2012 (Index 2012 = 100)")


# Gráfica mucho más linda e informativa usando xts

plot(indprod_xts, main = "IPI trimestre de Estados Unidos",
     sub = "1960-2010 (Index 2012 = 100)",
     ylab  = "IPI")

#~~~ APLICACIÓN DEL FILTRO HODRICK-PRESCOTT ~~~#

#El filtro Hodrick-Prescot puede ser una herramienta útil para analizar la estacionaridad
#de la serie. Este filtro separa la parte tendencial de la cíclica en la serie; con una 
#serie estacionaria deberiamos tener una tendencia sin pendiente y relativamente fija a
#una constate, y una varianza que se muestre estable en el componente cíclico.

hpf = hpfilter(base_fred$INDPRO, freq = 1600) #Aplicar el filtro
#Componentes de la prueba
names(hpf)
#La parte ciclica de la serie de tiempo
hpf$cycle
ciclo = ts(hpf$cycle, start = 1960, frequency = 4)
#La parte tendencial de la serie de tiempo
hpf$trend
trend = ts(hpf$trend, start = 1960, frequency = 4)
#El valor de lambda correspondiente a la periodicidad de la ts
hpf$lambda
#La serie original
base_fred$INDPRO

#Grafico con los componentes ciclico y tendencial de la serie 
x11()
#Viendo el componente tendencial Claramente la tendencia no es plana y no es independiente  
#del tiempo
plot.ts(trend)
#Viendo el componente cíclico no se observa un rango que denote la estabilidad de la varianza  
plot.ts(ciclo)
plot(hpf)


#~~~ GRÁFICOS DE LAS FAC Y FACP ~~~#

# Versión base R

lags=24

x11()
par(mfrow=c(1,2))

acf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del IPI de USA') 
pacf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del IPI de USA')

# Versíón ggplot:

ggAcf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del IPI de USA") + theme_light()
ggPacf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del IPI de USA") + theme_light()

# Versión paquete astsa (sirve tanto para objetos ts como para objetos xts)

acf2(indprod)

# Como podemos ver, la FAC tiene un decaimento lento a cero,lo que nos da indicios 
# de que el proceso no es estacionario. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.2. Prueba Dickey Fuller aumnetada (ADF) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# La ADF es una prueba estándar muy utilizada para saber si una serie de tiempo 
# tiene al menos una raíz unitaria o no. 

# Dado que aún no han visto pruebas de raíz unitaria se colocará el resultado 
# de la prueba por completitud  y en scripts posteriores se les indicará como 
# es la mecánica de la prueba y como interpretar sus resultados.

# Prueba con trend

adf.trend_indprod= ur.df(indprod, type="trend", lags = 5)
plot(adf.trend_indprod)
summary(adf.trend_indprod)

# Prueba con drift

adf.drift_indprod= ur.df(indprod, type="drift", lags = 5)
plot(adf.drift_indprod)
summary(adf.drift_indprod) 

# Conclusión: 

# Los resultados de la prueba indican que la serie debería tener término de 
# deriva (drift) Y que la serie no es estacionaria, por lo que hay que 
# diferenciarla para eliminar raíces unitarias.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.3. Transformación para volver estacionaria la serie #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#  Aplicar diff() : 

d.indprod= diff(indprod) # Serie diferenciada

#  Aplicar log(): 

l.indprod=log(indprod) # Serie que se le aplica solo el logaritmo 


# Aplicar diff(log(serie_original)): 

dl.indprod= diff(log(indprod))*100   # Diferencia de logaritmos de la serie 
                                     # (tasa de crecimiento)

# IMPORTANTE: Primero se aplica log y luego diff si van a usar ambos. 

# Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su 
# valor en logaritmos.

x11()
par(mfrow=c(2,2))

plot.ts(indprod, xlab="",ylab="", 
        main="IPI en nivel para Estados Unidos",lty=1, lwd=2, col="lightblue")
plot.ts(l.indprod, xlab="",ylab="", 
        main="IPI en logaritmo para Estados Unidos",lty=1, lwd=2, col="black")
plot.ts(d.indprod, xlab="",ylab="", 
        main="Variación del IPI para Estados Unidos",lty=1, lwd=2, col="orange")
plot.ts(dl.indprod, xlab="",ylab="",
        main="Tasa de crecimiento del IPI para Estados Unidos",lty=1, 
        lwd=2, col="lightgreen")

#Aplicando el filtro a las tres transformaciónes para apreciar el efecto de caca una 
#sobre la serie orignial.:

hpf_l = hpfilter(l.indprod, freq = 1600)
hpf_d = hpfilter(d.indprod, freq = 1600)
hpf_dl = hpfilter(dl.indprod, freq = 1600)

#Efecto del logaritmo sobre la serie
x11()
plot(hpf)
x11()
plot(hpf_l)
#Efecto de la diferencia sobre la serie
x11()
plot(hpf_d)
x11()
plot(hpf_d)
#Efecto de la diferencia del logaritmo sobre la serie
x11()
plot(hpf_dl)
x11()
plot(hpf_dl)

# Vamos a elegir la tasa de crecimiento del IPI, es decir "dl.indprod".

# Ahora hacemos la ACF y la PACF, para la tasa de crecimiento del indprod donde 
# evidenciamos un proceso débilmente dependiente. 

x11()
lags=30

par(mfrow=c(1,2))

acf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',
    main='ACF de la tasa de crecimiento del IPI') 
pacf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',
     main='PACF de la tasa de crecimiento del IPI')


# Se observa en la ACF la típica caída "geométrica" que uno esperaría observar 
# de una serie estacionaria, lo cual es un indicativo de que la diferenciación 
# si eliminó la raíz unitaria. 


ADF.dl.indprod <- ur.df(dl.indprod, type="none", selectlags = "AIC")
summary(ADF.dl.indprod) 

# Rechazamos H0, así que la tasa de crecimiento del IPI es estacionaria en 
# sentido débil.

# Vamos a analizar las estadísticas descriptivas de la serie en primera diferencia. 

describe(dl.indprod) # Parece que la media es distina a cero. Intercepto
describe(l.indprod)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.4. Métodos para idenficar modelo ARIMA por criterios de información ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#________________________________#
###### 1.4.1. Método manual ######


# Método manual para identificar el ARIMA usando criterios de información 

# Ahora vamos a ver lo que muestran los criterios AIC y BIC

AR.m <- 6 #Supondremos que el rezago autorregresivo máximo es 6 (pmax)
MA.m <- 6 #Supondremos que el rezago de promedio móvil máximo es 6. (qmax)


#~~~ FUNCIÓN PARA SELECCIONAR ARIMA CON EL MENOR CRITERIO DE INFORMACIÓN ~~~#

# Función que me permite crear un data frame para distintos modelos ARIMA(p,d,q)
# en donde cada fila del df me da el orden del modelo y los AIC y BIC 
# correspondientes a dicho modelo.

# Utilizar method = "ML" dentro de arima(), de lo contrario les arrojará error.


arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool_trend, metodo){
  
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool_trend, 
                    method = metodo)
      df[index,] = c(p, d, q, AIC(fitp), BIC(fitp))
      index = index + 1
    }
  }  
  return(df)
}

#~~~ FUNCIÓN PARA SELECCIONAR ARIMA POR MENOR AIC ~~~#

arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}

#~~~ FUNCIÓN PARA SELECCIONAR ARIMA POR MENOR BIC ~~~#


arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}

# Llamo la función arma_selection_df para construir un data frame con todos los 
# posibles modelos ARIMA(p, d, q).

# Para nuestro caso D = 0 ya que ya hemos diferenciado.

# Usaremos la función que hemos creado denominada arma_seleccion_df para escoger 
# el ARIMA a usar, con los máximos rezagos que hemos fijado (p = 6, q = 6).

mod_d0_indprod = arma_seleccion_df(dl.indprod, AR.m, MA.m, d = 0, TRUE, "ML")

# Veamos los criterios.

View(mod_d0_indprod)

# Selecciono el mejor modelo según menor valor de los criterios AIC y BIC.

min_aic_dlindprod = arma_min_AIC(mod_d0_indprod)
min_aic_dlindprod # ARIMA (2,0,4)

min_bic_dlindprod = arma_min_BIC(mod_d0_indprod)
min_bic_dlindprod # ARIMA (2,0,4)

# Ambos criterios de información sugieren un ARIMA (2,0,4)

#________________________________________________#
###### 1.4.2. Función ARIMA del paquete fable ####

# Para usar el paquete Fable, los objetos tienen que ser "tsibble".

dl.indprod_tsibble = as_tsibble(dl.indprod)
l.indprod_tsibble = as_tsibble(l.indprod)


class(dl.indprod_tsibble) 
glimpse(dl.indprod_tsibble)
view(dl.indprod_tsibble)

# tsibble es un tipo de data.frame diseñado para trabajar con una o más series de 
# tiempo.


# Grafica la serie de tiempo y su FAC y FACP con gg_tsdisplay()

gg_tsdisplay(dl.indprod_tsibble, value, plot_type = "partial")

# Grafica FAC Y FACP de la primera diferencia.

gg_tsdisplay(dl.indprod_tsibble, difference(value), plot_type='partial')


# USamos la forma "search" de la identificación ARIMA y la función pivot_longer
# de tidyr para organizar por el método.

# Criterio AIC ~~

dl.indprod_fit_aic = dl.indprod_tsibble %>%
  model(stepwise = ARIMA(value, ic = "aic"),
        search = ARIMA(value, ic = "aic", stepwise=FALSE))

dl.indprod_fit_aic %>% pivot_longer(everything(), names_to = "Model name",
                                   values_to = "Orders")

# Recomienda un ARIMA(4,0,2) (Es decir, un modelo ARIMA sin parte estacional) 


# Criterio BIC ~~

dl.indprod_fit_bic = dl.indprod_tsibble %>%
  model(stepwise = ARIMA(value, ic = "bic"),
        search = ARIMA(value, ic = "bic", stepwise=FALSE))

dl.indprod_fit_bic %>% pivot_longer(everything(), names_to = "Model name",
                                   values_to = "Orders")


# Recomienda un ARIMA(1,1,0)(1,0,2)[4] (Es decir, un modelo SARIMA que modela 
# la parte estacional, lo que es erroneo ya que la serie esta desestacionlizada)

# Ignoraremos esta última sugerencia.

#________________________________________________#
###### 1.4.3. Función auto.arima de Forecast #####

# Método automático para identificar el ARIMA usando criterios de información 
# Usando la función auto.arima() del paquete forecast. Se recomienda usar los dos
# métodos anteriores, auto.arima() esta sujeto a imprecisiones. 

auto.arima(dl.indprod, method = "ML")   

# Sugiere un ARIMA(1,0,0) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 2. Segundo paso: Estimación ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Una vez se ha identificado la serie, se procede a estimar el modelo ARIMA. 
# Debe tomarse en cuenta que incluir muy pocos rezagos puede llevar a que los 
# residuales del modelo no se comporten como un Ruido Blanco y que incluir 
# muchos parámetros puede llevar a que nuestro modelo no sea parsimonioso; se 
# pierdan muchos grados de libertad y se pierda eficiencia.

# Se puede estimar un modelo arima con 3 funciones distintas: 

  # arima: función del paquete stats que es la más usual 
  # Arima: función del paquete forecast 
  # sarima: función del paquete astsa que permite estimar modelos sarima.


# Nota: En este curso siempre estimaremos por el método máxima verosimilitud.

# Vamos a analizar los modelos que nos sugirió cada método de identificación 
# priorizando parsimonia.

# ARIMA(1,0,0);     ARIMA(2,0,4);        ARIMA(4,0,2)
# auto.arima()      método manual       ARIMA de fable


#~~ arima de stats ~~#

arima_1.0.0_dlindprod = arima(dl.indprod, order = c(1,0,0), 
                             include.mean = T, method = "ML")

summary(arima_1.0.0_dlindprod) # modelamiento ARIMA(1,0,0)


arima_2.0.4_dlindprod = arima(dl.indprod, order = c(2,0,4), 
                             include.mean = T, method = "ML")

summary(arima_2.0.4_dlindprod) # modelamiento ARIMA(2,0,4)

 
arima_4.0.2_dlindprod = arima(dl.indprod, order = c(4,0,2), 
                             include.mean = T, method = "ML")

summary(arima_4.0.2_dlindprod) # modelamiento ARIMA(4,0,2)


# Stargazer.

# Con stargazer podremos ver una tabla para comparar los coeficientes de los modelos

stargazer(arima_1.0.0_dlindprod, arima_2.0.4_dlindprod, arima_4.0.2_dlindprod,
          column.labels=c("ARIMA(1,0,0)", "ARIMA(2,0,4)","ARIMA(4,0,2)"),
          keep.stat=c("n","rsq"), 
          type = "text", style = "aer")

# Función que también permite sacar una salida para latex

stargazer(arima_1.0.0_dlindprod, arima_2.0.4_dlindprod, arima_4.0.2_dlindprod,
          column.labels=c("ARIMA(1,0,0)", "ARIMA(2,0,4)","ARIMA(4,0,2)"),
          keep.stat=c("n","rsq"), 
          type = "latex", style = "aer")


#~~ ARIMA de Fable ~~#

arima_1.0.0_dlindprod_fable  = as_tsibble(dl.indprod) %>%
  model(arima100 = ARIMA(value ~ pdq(1,0,0) + PDQ(0, 0, 0)))

glimpse(arima_1.0.0_dlindprod_fable) # modelo ARIMA(1,0,0): 


arima_2.0.4_dlindprod_fable  = as_tsibble(dl.indprod) %>%
  model(arima204 = ARIMA(value ~ 1 + pdq(2,0,4) + PDQ(0, 0, 0)))

glimpse(arima_2.0.4_dlindprod_fable) # modelo ARIMA(2,0,4)


arima_4.0.2_dlindprod_fable  = as_tsibble(dl.indprod) %>%
  model(arima402 = ARIMA(value ~ pdq(4,0,2) + PDQ(0, 0, 0)))

glimpse(arima_4.0.2_dlindprod_fable) # modelo ARIMA(4,1,2)

# SARIMA 

# Para estimar un modelo SARIMA (i.e. un modelo ARIMA en donde también se modela 
# la componente de estacionalidad el proceso), no se especifica las componentes
# PDQ y se deja que el software las escoga). Por ejemplo:

sarima_1.1.0_lindprod_fable  = as_tsibble(l.indprod) %>%
  model(arima110 = ARIMA(value ~ pdq(1,1,0)))

glimpse(sarima_1.1.0_lindprod_fable)

# Nuevamente, esto no es necesario.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 3. Tercer paso: Validación de supuestos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Es importante verificar los supuestos de nuestro modelo ARIMA. se debe ver que
# los residuales estimados se comporten como un ruido blanco. Es decir, que la 
# media de los residuales sea cero, la varianza constante y la covarianza sea cero.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 3.1. No autocorrelación de los errores ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Vamos a realizar el análisis de residuales para cada modelo: 

#--> ARIMA(1,0,0)

# Residuales del modelo

res_arima_1.0.0_dlindprod = residuals(arima_1.0.0_dlindprod)

par(mfrow=c(1,2))

acf(res_arima_1.0.0_dlindprod,lag.max=24,plot=T,lwd=1,xlab='',
    main='ACF residuales (1,0,0)') 
pacf(res_arima_1.0.0_dlindprod,lag.max=24,plot=T,lwd=1,xlab='',
     main='ACF al cuadrado residuales (1,0,0)')

# Estas gráficas parecen sugerir la violación del supuesto.

# Pruebas formales para la verificación de la No-autocorrelación serial: 

# Generalmente la prueba se hace sobre un 1/4 de la muestra, 
# pero también la haremos para otros rezagos. 

lags.test = length(dl.indprod)/4;lags.test

#~~ BOX-PIERCE TEST ~~# 

Box.test(res_arima_1.0.0_dlindprod, lag=lags.test, type = c("Box-Pierce")) #Rechazo H0, no se cumple el supuesto. 
Box.test(res_arima_1.0.0_dlindprod, lag=20, type='Box-Pierce') # No rechazo H0, se cumple el supuesto para 20 rezagos. 


#~~ LJUNG-BOX ~~#

Box.test(res_arima_1.0.0_dlindprod, lag=lags.test, type = c("Ljung-Box")) #Rechazo H0, no se cumple el supuesto.
Box.test(res_arima_1.0.0_dlindprod , lag=20, type='Ljung-Box') #Rechazo H0, no se cumple el supuesto.


# No se cumple el supuesto de no-autocorrelación serial, el cual es considerado
# como el más importante (Se cumple solo para un caso, pero descartamos la prueba
# ya que se viola para varios test).

# DESCARTAMOS el modelo ARIMA(1,1,0)

#--> ARIMA(2,0,4)

# Argumento gráfico

res_arima_2.0.4_dlindprod = residuals(arima_2.0.4_dlindprod)

par(mfrow=c(1,2))

acf(res_arima_2.0.4_dlindprod,lag.max=24,plot=T,lwd=1,xlab='',
    main='ACF residuales (2,0,4)') 
pacf(res_arima_2.0.4_dlindprod,lag.max=24,plot=T,lwd=1,xlab='',
     main='ACF al cuadrado residuales (2,0,4)')

# Pruebas formales:

#~~ BOX-PIERCE TEST ~~# 

Box.test(res_arima_2.0.4_dlindprod, lag=lags.test, type = c("Box-Pierce")) # No rechazo H0, se cumple el supuesto. 
Box.test(res_arima_2.0.4_dlindprod, lag=20, type='Box-Pierce') # No rechazo H0, se cumple el supuesto.


#~~ LJUNG-BOX ~~#

Box.test(res_arima_2.0.4_dlindprod, lag=lags.test, type = c("Ljung-Box")) # No rechazo H0, se cumple el supuesto.
Box.test(res_arima_2.0.4_dlindprod , lag=20, type='Ljung-Box') # No rechazo H0, se cumple el supuesto.

# CUMPLE EL SUPUESTO! Pasa a la siguiente fase, si se llama. 

#--> ARIMA(4,0,2)

# Argumento gráfico

res_arima_4.0.2_dlindprod = residuals(arima_4.0.2_dlindprod)

par(mfrow=c(1,2))

acf(res_arima_4.0.2_dlindprod,lag.max=24,plot=T,lwd=1,xlab='',
    main='ACF residuales (4,0,2)') 
pacf(res_arima_4.0.2_dlindprod,lag.max=24,plot=T,lwd=1,xlab='',
     main='ACF al cuadrado residuales (4,0,2)')

# Pruebas formales:

#~~ BOX-PIERCE TEST ~~# 

Box.test(res_arima_4.0.2_dlindprod, lag=lags.test, type = c("Box-Pierce")) # No rechazo H0, se cumple el supuesto. 
Box.test(res_arima_4.0.2_dlindprod, lag=20, type='Box-Pierce') # No rechazo H0, se cumple el supuesto.


#~~ LJUNG-BOX ~~#

Box.test(res_arima_4.0.2_dlindprod, lag=lags.test, type = c("Ljung-Box")) # No rechazo H0, se cumple el supuesto.
Box.test(res_arima_4.0.2_dlindprod , lag=20, type='Ljung-Box') # No rechazo H0, se cumple el supuesto.

# CUMPLE EL SUPUESTO! Pasa a la siguiente fase, tenemos dos finalistas.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 3.2. Homocedasticidad de los residuales ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# La prueba ARCH nos dice si los residuales son heterocedasticos H0.

# Hay dos formas de hacer la prueba: Un test Pormenteau y un Test tipo 
# multiplicadores de Lagrange.


#--> ARIMA(2,0,4)

# Prueba formal:

arch_dlindprod_arima_2.0.4 = arch.test(arima_2.0.4_dlindprod, output=TRUE)

# No Rechazo H0 en Portmanteau pero en Lagrange si, así que decidiremos con un
# argumento gráfico. 

# Vamos a graficar la ACF y PACF de residuales al cuadrado del modelo ARIMA(2,0,4)

# Argumento gráfico.

par(mfrow=c(1,2))
acf(res_arima_2.0.4_dlindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF residuales al cuadrado') 
pacf(res_arima_2.0.4_dlindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF residuales al cuadrado')

# Con esto concluimos que existe heterocedasticidad, no se cumple el supuesto.


#--> ARIMA(4,0,2)

# Prueba formal:

arch_dlindprod_arima_4.0.2 = arch.test(arima_4.0.2_dlindprod, output=TRUE)

# No Rechazo H0 en Portmanteau pero en Lagrange si, así tomaremos una decisión a 
# partir del argumento gráfico. 

par(mfrow=c(1,2))
acf(res_arima_4.0.2_dlindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF residuales al cuadrado') 
pacf(res_arima_4.0.2_dlindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF residuales al cuadrado')
par(mfrow=c(1,1))

# Esto indica heterocedasticidad, tampoco se cumple el supuesto.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 3.3. Normalidad en los residuales ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Veremos si los residuales se comportan de manera normal (distribución normal)
# para ello usaremos un argumento gráfico como el QQ-plot y la prueba de normalidad
# Jarque-Bera.


#--> ARIMA(2,0,4)

# Argumento gráfico: QQ-PLOT

qqPlot(res_arima_2.0.4_dlindprod)

# Colas pesadas, no se cumple el supuesto.

# Prueba formal: Jarque-Bera Test

jarque.bera.test(res_arima_2.0.4_lindprod) # Se rechaza H0, no hay normalidad. 

# No se cumple el supuesto de normalidad.


# Conclusiones: 

# - Se descarta el ARIMA(1,0,0) al violar el supuesto de mayor importancia.

# - ARIMA(2,0,4) y ARIMA(4,0,2) no cumplen los supuestos de homocedasticidad y 
#   normalidad pero esto no es tan perjudicial como violar el supuesto de no
#   autocorrelacción serial, por lo que seguimos trabajando con estos modelos.

# EXTRA: Verificación de raices fuera del circulo unitario. (Estacionariedad)

gg_arma(arima_1.0.0_dlindprod_fable)
gg_arma(arima_2.0.4_dlindprod_fable)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 4. Cuarto paso: Pronóstico ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Haremos los pronósticos con ARIMA(2,0,4) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 4.1. Pronósticos futuros ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Generamos una estimación nueva con los datos sin diferenciar pero con logaritmo
# para recibir un pronostico en la variable nivel.

# La única diferencia es que la diferenciación no es manual si no la hace el comando
# ARIMA. Por lo que:

# l.indprod(2,1,4) ===== dl.indprof(2,0,4)

# ARIMA(2,1,4) sobre la variable base en logaritmo

arima_2.1.4_lindprod_fable  = as_tsibble(l.indprod) %>%
  model(arima214 = ARIMA(value ~ pdq(2,1,4) + PDQ(0, 0, 0)))

arima_2.1.4_lindprod = arima(l.indprod, order = c(2,1,4), 
                              include.mean = T, method = "ML")


# Fable realiza el proceso de boostraping automaticamente con boostrap = TRUE.
  
forecast_arima_2.1.4_lindprod = arima_2.1.4_lindprod_fable %>% 
forecast(h = 10, bootstrap = TRUE, times = 10000) 

forecast_arima_2.1.4_lindprod

# Predicciones a NIVEL, aplicamos euler a la predicción en logaritmo.

exp(forecast_arima_2.1.4_lindprod$.mean)

#~~ Pronóstico por intervalos al 95% ~~#

pronosticos_2.1.4_lindprod = forecast_arima_2.1.4_lindprod %>% 
  hilo(level = c(95))

View(pronosticos_2.1.4_lindprod)

# Pronósticos a nivel

exp(pronosticos_2.1.4_lindprod$.mean)
exp(pronosticos_2.1.4_lindprod$`95%`)

#~~ GRÁFICOS DEL PRONÓSTICO ~~#

#--> Gráfico de la variación del IPI

pronostico = forecast(arima_2.1.4_lindprod, level = c(95), h = 10)

autoplot(pronostico, color = 'firebrick1',
         predict.linetype = 'dashed', conf.int = FALSE, ylab = "Log IPI")+
  ggtitle("Pronóstico del crecimiento del IPI", subtitle = "(2012 Q4 - 2015 Q1)")

#--> Gráfico en Niveles

pronostico_nivel = predict(arima_2.1.4_lindprod, n.ahead = 10)
pronostico_nivel

# Pronosticar 10 trimestres futuros

prediccion_nivel_10Q = exp(pronostico_nivel$pred)

# Añadir margen de error

intervalo_sup = prediccion_nivel_10Q + qnorm(0.95, 0, 1) * exp(pronostico_nivel$se)
intervalo_inf = prediccion_nivel_10Q - qnorm(0.95, 0, 1) * exp(pronostico_nivel$se)

ts.plot(indprod, prediccion_nivel_10Q, intervalo_sup, intervalo_inf,
        col=c('black', 'blue', "orange", "orange"),lty = c(1,2,2,2), 
        main = "Pronóstico del IPI")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 4.2. Ajuste vs Real ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Vamos a ver el ajuste dentro de la muestra del modelo diferenciado manualmente. 

# Para la series dl.indprod
fit_0 = dl.indprod - residuals(arima_2.0.4_dlindprod)


plot.ts(dl.indprod,type="l",
        main="Tasa de crecimiento ajustada VS Tasa de crecimeinto observada",
        lwd=1, ylab = "Diff Log IPI")
points(fit_0,col="salmon",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","salmon"),lty=1,lwd=2)

# Como pueden observar el ajuste en muestra es bastante bueno, lo que muestra 
# la capacidad y potencia de los modelos ARIMA.


# Gráfico de la volatilidad - Clusters de volatilidad.

plot(diff(log(indprod_xts)),
     main = "Tasa de crecimiento IPI trimestre de Estados Unidos 
     (1960-2012) (Index 2012 = 100)", ylab  = "Tasa de crecimiento IPI")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                   FIN DEL CÓDIGO                   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#