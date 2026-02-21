#=====================================================================================#
#
#                           UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2024 - 02
#                            Econometría II | Monitoría 
#
#                                     Sesión 3:
#           Metodología Box Jenkins Primera Parte: Identificación y Estimación
#
#=====================================================================================#

# Analizaremos las siguientes series de tiempo para ver una aplicacíon real:

#       I). Índice de producción industrial 
#      II). Spread tasa de bonos USA y T- Bills
#     III). Tasa de desempleo mensual EE.UU (1959-2012) 


# Carga y descarga de paquetes

# El proceso se realizará con el comando "pacman", el cual carga los paquetes 
# ya instalados y descarga (para posteriormente cargar) los no instalados

install.packages("pacman")

pacman :: p_load(
  forecast,   # Para hacer pronóstico con los modelos ARIMA
  lmtest,     # Obtener la significancia individual de los coeficientes ARIMA
  urca,       # Prueba de raíz unitaria (estacionariedad)
  tseries,    # Para estimar modelos de series de tiempo y pruebas de supuestos 
  readxl,     # Para leer archivos de excel
  stargazer,  # Presentación de resultados y tablas más estéticos
  psych,      # Para hacer estadística descriptiva 
  seasonal,   # Para desestacionalizar las series de tiempo
  aTSA,       # Para hacer prueba de efectos ARCH
  astsa,      # Para estimar, validar y pronosticar en modelos ARIMA/SARIMA
  xts,        # Para utilizar objetos de tipo xts
  tidyverse,  # Conjunto de paquetes (incluye dplyr y ggplot2)
  readxl,     # Para leer archivo excel
  car         # Función qqplot
)

# Limpiar el Environment
rm(list = ls()) 


# Nota: En R uno puede trabajar series de tiempo con 3 objetos:

# ts: Intervalos de tiempo fijos, objeto estándar de series de tiempo en R)
# zoo: Diferentes frecuencias de tiempo
# xts: xts significa "eXtensible time series".  xts es una subclase de zoo.

# El objeto xts otorga una flexibilidaad y modelamiento preferible a los demás.


#
#---- I). Industrial Production Index (indprod) USA ----
#
# El índice de producción industrial mide la producción real en términos de 
# manufactura, minería, electricidad y gas.

# Al ser un índice real tiene un año base (para este caso específico es 2012).
# El índice se normaliza de forma tal que el año base tiene un valor de 100.

# Valores > 100: producción manufacturera fue MAYOR que 2012.
# Valores < 100: producción manufacturera fue MENOR que 2012.



# Fuente: https://fred.stlouisfed.org/series/INDPRO#0 (FRED, Reserva Federal de St. Louis)

#---- I.I) IDENTIFICACIÓN  ----

# Cargamos el archivo (INDPRO)
base_fred <- read_excel(file.choose())  

# Convertir la serie en un objeto tipo ts
indprod = ts(base_fred$INDPRO, start = 1960, frequency = 4) # 4 observaciones/año

# Visualización de los datos
View(indprod)  

# Consideremos también al objeto xts
indprod_xts = xts(base_fred$INDPRO, 
                  order.by = base_fred$observation_date)

# Visualización xts
View(indprod_xts)

# Ver la clase de cada uno de los ejemplos de series de tiempo
class(indprod)
class(indprod_xts)


# ---- Gráficas -----

# Para el objeto ts
x11()   
plot.ts(indprod, xlab="",ylab="", 
        main="IPI trimestral de Estados Unidos (1960-2012)",lty=1, lwd=2, col="salmon")


# Usando ggplot2
autoplot(indprod, col="green", main = "IPI trimestre de Estados Unidos (1960-2012)",
         xlab = "Fecha", ylab="IPI", lwd=1)


#  Para el objeto xts
x11()
plot(indprod_xts, main = "IPI trimestre de Estados Unidos (1960-2012)",
         ylab  = "IPI")

# Podemos notar las diferencias en gráficos de cada tipo de objeto

# ACF y PACF de la serie en nivel.

lags=24
par(mfrow=c(1,2))
acf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PCE de USA') 
pacf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PCE de USA')

# Versión ggplot:
x11()
ggAcf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA")
ggPacf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")

# Versión paquete astsa (sirve tanto para objetos ts como para objetos xts)
acf2(indprod)

# Es posible ver características de procesos no estacionarios en las FAC y FACP


# ¿ Qué hacemos :/ ?, tenemos tres opciones:
# 1. Diferenciación (media no constante): eliminar la tendencia estocástica.

d.indprod= diff(indprod) 

# 2. Logaritmo Natural: para estabilizar la varianza de la serie

l.indprod=log(indprod) 

# 3. Todas las anteriores ( Media y Varianza no constante ): Tasa de crecimiento

dl.indprod= diff(log(indprod))*100

# Grafiquemos cada transformación y comparemos con la serie en nivel

x11()
par(mfrow=c(2,2))
plot.ts(indprod, xlab="",ylab="",main="Indprod en nivel EE.UU 1959-2019",lty=1, lwd=2, col="blue")
plot.ts(l.indprod, xlab="",ylab="", main="Indprod en logaritmo EE.UU 1959-2019",lty=1, lwd=2, col="black")
plot.ts(d.indprod, xlab="",ylab="", main="Variación Indprod EE.UU 1959-2019",lty=1, lwd=2, col="red")
plot.ts(dl.indprod, xlab="",ylab="", main="Tasa de crecimiento del Indprod EE.UU 1959-2019",lty=1, lwd=2, col="darkgreen")

# La tasa de crecimiento es nuestra ganadora !!!

# FAC y FACP con indicios de dependencia débil
lags=30
par(mfrow=c(1,2))
acf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la tasa de crecimiento del PIB') 
pacf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la tasa de crecimiento del PIB')

# Ggplot
ggAcf(dl.indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA")
ggPacf(dl.indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")

# Paquete Astsa version
acf2(dl.indprod)

# ---- Criterios de información -----

# 1. Método manual

# Establecemos unos rezagos máximos (parsimoneidad)
AR.m <- 6 
MA.m <- 6 

# Esta función selecciona el modelo ARIMA con el menor criterio de información

# ¿Qué hace? realiza mediante permutación distintos modelos ARMA y sintetiza
# el calculo de los criterios de información en un Data Frame. (SOLO ML)

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

# 2. Creo la función arma_min_AIC para seleccionar el mejor modelo según AIC.
arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}

# 3. Creo la función arma_min_BIC para seleccionar el mejor modelo según BIC
arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}



# Sin embargo, como la función trabaja con el comando arima(), podemos hacer la 
# diferenciación a través del comando o manualmente.

# Analicemos la serie en logaritmo (necesita d=1 para que la función la diferencie)

mod_d1_indprod = arma_seleccion_df(l.indprod, AR.m, MA.m, d = 1, TRUE, "ML") 
view(mod_d1_indprod)

# Mejor modelo según AIC y BIC

min_aic_lindprod = arma_min_AIC(mod_d1_indprod); min_aic_lindprod #ARIMA (3,1,5)
min_bic_lindprod = arma_min_BIC(mod_d1_indprod); min_bic_lindprod #ARIMA (1,1,0)

#Recuerden que en la metodología BIC prima la parsimonia

# Ahora, trabajo con la serie diferenciada manualmente (d = 0)

mod_d0_indprod = arma_seleccion_df(dl.indprod, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod_d0_indprod)

# Mejor modelo según criterios de información cuando d=0
min_aic_dlindprod = arma_min_AIC(mod_d0_indprod); min_aic_dlindprod #ARIMA (2,0,4)
min_bic_dlindprod = arma_min_BIC(mod_d0_indprod); min_bic_dlindprod #ARIMA (2,0,4)


# 2. Método automático (auto.arima) 

# Nota: La función auto.arima calcula un modelo SARIMA de forma tal que modela 
# el componente estacional de la serie.


# Ver Enders Capítulo 2 - Sección 11: Seasonality

auto.arima(l.indprod, method = "ML") #NO SE USA


# auto.arima() como método complementario, no absoluto. 

# Ojo: Para seleccionar un modelo es importante usar TODAS las herramientas de 
# las que dispongamos: Gráfico serie, FAC, FACP y Criterios de información


# Seleccionamos el modelo usando el BIC, aunque AIC tiene aplicaciones importantes

# Un exceso de rezagos puede ser perjudicial, pero incluir muy pocos pueden causar
# que los errores del modelo no se comporte como un Ruido Blanco

#---- I.II) ESTIMACIÓN ----

# Existen 3 métodos de estimación para la función arima:

## ML: Máxima verosimilitud (más preciso y la mejor opción para bases pequeñas)
## CSS: (más veloz generalmente, usado en bases de datos grandisimas).
## CSS-ML: Una combinación de ambas.

# Siendo CSS  (Coherent Source Separation) 

# Sin embargo ML puede no converger y CSS puede no hacer estimaciones lo 
# suficientemente precisas y arrojar error. 


# La estimación de un modelo arima se puede realizar 3 funciones distintas: 

## arima: Paquete stats, es la más usual emplear y predicción con forecast
## Arima: Paquete forecast que es básicamente un wrapper de la función arima.
## sarima: Paquete astsa

# Tanto la función Arima como la función sarima están construidas sobre la 
# función arima(), por lo que es posible modelar estacionalidad en cada alternativa

# Estimamos mediante Maxima verosimilitud no incurrir en imprecisiones

arima_1.1.0_lindprod = arima(l.indprod, order = c(1,1,0), include.mean = T, 
                             method = "ML"); summary(arima_1.1.0_lindprod) # ARIMA (1,1,0)

arima_2.0.4_dlindprod = arima(dl.indprod, order = c(2,0,4), include.mean = T,
                              method = "ML"); summary(arima_2.0.4_dlindprod) # ARIMA (2,0,4)

# Sintetizamos los resultados
stargazer(arima_1.1.0_lindprod, arima_2.0.4_dlindprod, type = "text")


#---- II). Spread between 5 year Treasury bonds rate and 3 month Trills (spread) ----


# La tasa de interés de los bonos del tesoro norteamericano a 3 meses es comúnmente 
# usada como la tasa libre de riesgo de los mercados financieros, por lo que 
# ese spread modelado se podría ver como el exceso de retorno de los bonos de 5 años del tesoro.

# Fuente: http://time-series.net/data_sets (Página oficial de Walter Enders)

#---- II.I)  IDENTIFICACIÓN ----

# Se cargan las series de tiempo
quarterly <- read_excel(file.choose()) # quarterly.7775706

# Haciendo uso del paquete dplyr, vamos crear el spread de las tasas
quarterly = quarterly %>% 
  mutate(spread = r5 - Tbill) 

quarterly$DATE = as.yearqtr(quarterly$DATE,format = "%YQ%q")

# Tratemos la base como un objeto xts
spread = xts(quarterly$spread, order.by = quarterly$DATE) 

# Gráfica base de datos 
x11()
plot(spread, main = "Spread entre los bonos a 5 años y Tbills a 3 meses Tesoro de los EE.UU.", 
     ylab  = "spread")
abline(h = mean(spread), col = "green")

# ACF y PACF
lags=24
ggAcf(spread,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del spread")
ggPacf(spread,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del spread")
acf2(spread)

#La FAC nos da indicios de que la serie puede ser estacionaria. 

# Seleccionar el modelo (d = 0 porque no hay que diferenciar la serie dado que es estacionaria)
AR.m = 8; MA.m = 8
df_criterios_info_spread = arma_seleccion_df(spread, AR.m, MA.m, d = 0, TRUE, "ML")
min_aic_spread = arma_min_AIC(df_criterios_info_spread); min_aic_spread # selecciona el min AIC / ARMA(5,5)
min_bic_spread = arma_min_BIC(df_criterios_info_spread); min_bic_spread # selecciona el min BIC / ARMA(1,1)

# Nota: El modelo con el menor BIC es un ARIMA(1,0,1), sin embargo veamos un vistazo
# rápido a algunas pruebas de supuestos


# Estimamos y guardamos el modelo
model_101= arima(spread, order = c(1,0,1), include.mean = T, method= "ML");summary(model_101)

# Pruebas de autocorrelación serial (Ljung- Box y Box-Pierce)

# Se realiza para un cuarto de la muestra y más rezagos

lags_test= length(spread)/4

#Ljung-Box
Box.test(residuals(model_101), lag= lags_test, type= "Ljung-Box")#No rechazo
Box.test(residuals(model_101), lag= 30, type= "Ljung-Box")#Rechazo

#Box-Pierce

Box.test(residuals(model_101), lag= lags_test, type= "Box-Pierce")#No rechazo
Box.test(residuals(model_101), lag= 30, type= "Box-Pierce")#Rechazo

# Como este es el supuesto más importante, debemos ser estrictos con la prueba.
# En otras sesiones profundizaremos más sobre las demás pruebas ;)

# Selecciono el modelo ARIMA(1,0,2) que es el segundo mejor según el Criterio de 
# información BIC, que es más parsimonioso que el modelo ARIMA(5,0,5) dado por Akaike


# Por   auto.arima
auto.arima(spread, method = "ML")

#---- II.II  ESTIMACIÓN ----

arima_1.0.2_spread = arima(spread,
                       order = c(1,0,2), include.mean = T, 
                       method = "ML"); summary(arima_1.0.2_spread) 

#Imprimimos el resultado

stargazer(arima_1.0.2_spread, type = "text")

# ------ III. Tasa de desempleo --------

# La serie de la Tasa de desempleo mensual para Estados Unidos de 1959 a 2012.

# Fuente: https://fred.stlouisfed.org/series/UNRATE (FRED de la Reserva Federal de St Louis)

#---- III.I IDENTIFICACIÓN  ------

# Se cargan las series de tiempo
base_unem <- read_excel(file.choose()) #UNRATE
view(base_unem)

UNRATE = ts(base_unem$UNRATE, start = c(1959,11), frequency = 12) 

# Crear un objeto tipo xts
UNRATE_xts = xts(base_unem$UNRATE, 
                 order.by = base_unem$observation_date) # Importar como un objeto xts

View(UNRATE_xts)

## Graficamos la serie con la funcion del paquete stats de R
x11()   
plot.ts(UNRATE, xlab="",ylab="", main="Tasa de desempleo mensual EE.UU (1959-2012)",
        lty=1, lwd=2, col="purple")

# Para el objeto xts
x11()
plot(UNRATE_xts, main = "Tasa de desempleo mensual EE.UU (1959-2012)", 
     ylab  = "UNRATE")

#Vamos a graficar la ACF y PACF de la serie en nivel.
lags=24
par(mfrow=c(1,2))
acf(UNRATE,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la Tasa de desempleo') 
pacf(UNRATE,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la Tasa de desempleo')

# Media no estable a través del tiempo a pesar de tener una varianza constante

# Diferenciemos
d.UNRATE = diff(UNRATE) 

# Vamos a graficar ahora el nivel (serie original) y la variación (serie diferenciada) de la tasa de desempleo.

x11()
par(mfrow=c(2,1))
plot.ts(UNRATE, xlab="",ylab="", main="Tasa de desempleo en nivel 1959-2012",
        lty=1, lwd=2, col="blue")
plot.ts(d.UNRATE, xlab="",ylab="", main="Variación de la Tasa de desempleo 1959-2012",
        lty=1,lwd=2, col="red")


# Igualmente graficamos la ACF y la PACF para la variación de la tasa de desempleo

x11()
lags=30
par(mfrow=c(1,2))
acf(d.UNRATE,lag.max=lags,plot=T,lwd=2,xlab="",main="ACF de la variación de la Tasa de desempleo")
pacf(d.UNRATE,lag.max=lags,plot=T,lwd=2,xlab="",main="PACF de la variación de la Tasa de desempleo")


# Analicemos los criterios de información (Ambos casos)
AR.m = 6; MA.m = 6
mod_d1_UNRATE = arma_seleccion_df(UNRATE, AR.m, MA.m, d = 1, TRUE, "ML")
view(mod_d1_UNRATE)

# Minimizamos 
min_aic_UNRATE = arma_min_AIC(mod_d1_UNRATE); min_aic_UNRATE #ARIMA (6,1,6)
min_bic_UNRATE = arma_min_BIC(mod_d1_UNRATE); min_bic_UNRATE #ARIMA (1,1,2)

# Serie diferenciada (con d=0):

mod_d0_UNRATE = arma_seleccion_df(d.UNRATE, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod_d0_UNRATE)
# Selecciono el mejor modelo según criterios de información cuando d=1
min_aic_d.UNRATE = arma_min_AIC(mod_d0_UNRATE); min_aic_d.UNRATE #ARIMA (6,0,6)
min_bic_d.UNRATE = arma_min_BIC(mod_d0_UNRATE); min_bic_d.UNRATE #ARIMA (1,0,2)

# Este es un claro ejemplo en el cual modelamos el mejor modelo según AIC, ya que
# en los correlogramas vemos una dependencia de muchos periodos anteriores. 

# Nos curamos en salud para no cumplir con el supuesto de autocorrelación serial


# Por auto.arima
auto.arima(UNRATE, method = "ML")

#---- III.II).  ESTIMACIÓN  ----

arima_6.1.6_UNRATE = arima(UNRATE,
                           order = c(6,1,6), include.mean = T, 
                           method = "ML"); summary(arima_6.1.6_UNRATE) 

arima_6.0.6_UNRATE = arima(d.UNRATE,
                           order = c(6,0,6), include.mean = T, 
                           method = "ML"); summary(arima_6.0.6_UNRATE) 
#Imprimimos el resultado
stargazer(arima_6.0.6_UNRATE, arima_6.1.6_UNRATE, type = "text")






