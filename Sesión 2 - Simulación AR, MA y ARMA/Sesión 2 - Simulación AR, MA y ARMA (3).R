#=====================================================================================#
#                           UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2024 - 02
#                            Econometría II | Monitoría 
#
#                                     Sesión 2:
#                              Simulación AR, MA y ARMA
#=====================================================================================#

#Limpiamos el environment.
remove(list = ls())

#El paquete pacman nos permite cargar diferentes paquetes al mismo tiempo. 
install.packages("pacman")
library(pacman)

pacman::p_load(
  
  plm,        # Para usar modelos con Data Panel
  gplots,     # Para usar herramientas de graficación 
  stargazer,  # Para generar tablas comparativas en los modelos
  haven,      # Para importar datos
  sandwich,   # Para estimar errores robustos
  lmtest,     # Para realizar inferencia estadística
  tseries,    # Para manipular series de tiempo
  woolridge,  # Para usar los datasets del libro de Woolridge
  tidyverse,  # Paquete que incluye ggplot2 y dplyr
  car,         # Para verificar normalidad con gráficos de distribución cuantil.
  forecast,    # Para hacer pronósticos con modelos arima 
  lmtest,      # Significancia individual de los coeficientes ARIMA 
  urca,        # Prueba de raíz unitaria 
  tseries,     # Estimación modelos de series de tiempo y verificación de supuestos.
  readxl,      # Para leer archivos de excel 
  stargazer,   # Para presentar resultados/salidas de R de manera más estética
  psych,        # Para hacer estadísticas descriptivas 
  seasonal,    # Para desestacionalizar series 
  aTSA,        # Para hacer la prueba de efectos ARCH 
  astsa,       # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA 
  xts,         # Para utilizar objetos xts  
  tidyverse,   # Conjunto de paquetes (incluye dplyr y ggplot2) 
  car          # Para usar la función qqPlot
  )


#---- Comentarios sobre el manejo de series de tiempo en R -----

# En R uno puede trabajar series de tiempo con 3 tipos de objetos:
### ts  
### zoo
### xts 

# Número de observaciones para todas las series

obs =  1000 

# Existe un soporte teórico que justifica el uso de un amplio número de observaciones
# Reducción del error muestral y mayor precisión de los estimadores (betas consistente)

#---- Características de las simulaciones ---- 

# Se empleará el comando arima.sim, paquete nativo de R.

# En la práctica, es muy común que los errores de varias series de tiempo no sean
# normales y por el contrario tengan una distribución diferente a la normal. 
# En caso tal de que los errores no distribuyan de manera normal, va a ser necesario
# emplear métodos de simulación (como bootstrapping). 


# La simulación por bootstrapping la pueden ver ya sea en el libro de series de 
# tiempo Enders o en el libro de probabilidad de´ DeGroot

#---- Sobre el análisis gráfico de estacionariedad ---- 

# Una gráfica de una serie de tiempo puede dar indicios de si el proceso es estacionario 

# Si la gráfica muestra un movimiento alrededor de una media de largo plazo y además
# la varianza de la serie paraece ser constante, a indicios de que la serie tiene un 
# comportamiento estacionario. 

# No obstante, todo análisis gráfico de una serie tiene que ser siempre complementado 
# por análisis de FAC y FACP y por pruebas formales (Unit Root Test)

#---- Simulación de un proceso de ruido blanco ----

set.seed(1)

yt_rb = ts(arima.sim(model= list(order = c(0,0,0)), n = obs, 
                     innov=rnorm(n = obs, mean = 0, sd = 1))) 

# Graficación 

x11()
autoplot(yt_rb, xlab="",ylab="", main=" Simulación serie y_t ARIMA (0,0,0) (Ruido Blanco)",
         lty=1, lwd=0.25, col="turquoise4") + theme_light()


#Vamos a hacer la ACF y PACF del PGD ARIMA(0,0,0) (Ruido Blanco)
  x11()
  lags=20
  par(mfrow=c(1,2))
  acf(yt_rb,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,0,0) (Ruido Blanco)') 
  pacf(yt_rb,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,0,0) (Ruido Blanco)')
  
  
# Versíón ggplot:
  x11()
  ggAcf(yt_rb,lag.max=lags,plot=T,lwd=2) + 
    ggtitle('ACF del PGD ARIMA(0,0,0) (Ruido Blanco)') + theme_light()
  
  x11()
  ggPacf(yt_rb,lag.max=lags,plot=T,lwd=2) + 
    ggtitle('PACF del PGD ARIMA(0,0,0) (Ruido Blanco)') + theme_light()

#---- Simulación de un proceso AR(1) estacionario ----

set.seed(8202) # Para que todos veamos lo mismo ;) (Reproducibilidad)

yt1s = ts(arima.sim(model= list(order = c(1,0,0), ar=c(0.6)), n = obs,
                    innov=rnorm(n = obs, mean = 0, sd = 1))) 


class(yt1s)

#Graficamos
x11()
autoplot(yt1s, xlab="",ylab="", main=" Serie y_t1s  PGD ARIMA (1,0,0)",lty=1, lwd=0.4,
         col="turquoise4") + theme_light()

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,0,0)
x11()
lags=20
par(mfrow=c(1,2))
acf(yt1s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,0,0)') 
pacf(yt1s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,0,0)')

#Otro comando para encontrar la acf y la pacf simultáneamente 
x11()
acf2(yt1s, max.lag = lags)

#---- Simulación de un proceso AR(1) no estacionario (ARIMA(1,1,0)) ----

set.seed(29101) 
yt1n = ts(arima.sim(model= list(order = c(1,1,0), ar=c(0.1)), n = obs, 
                    innov=rnorm(n = obs, mean = 0, sd = 1))) 

#Graficamos
x11()
autoplot(yt1n, xlab="",ylab="", main=" Serie y_t  PGD ARIMA (1,1,0)",
         lty=1, lwd=0.4, col="darkblue") + theme_light()

# Se puede ver que el proceso pareciera no tener una media constante a la que converge
# en el largo plazo. Adicionalmente, pareciera ser un proceso altamente persistente.




# Vamos a hacer la ACF y PACF  en donde evidenciamos que con 20 rezagos la correlación 
# persiste: el proceso no es débilmente dependiente y por ende tampoco estacionario.

x11()
lags=20
par(mfrow=c(1,2))
acf(yt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,0)') 
pacf(yt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,0)')

# Puede observarse que el proceso tiene una caída lineal, indicio fuerte de que
# no es un proceso estacionario.

# No olviden reafirmar o complementar dichas conclusiones con pruebas formales !!


# ¿ Y ahora que hacemos :/ ?

# Diferenciar la serie es la metodología que usualmente se emplea cuando una serie
# presenta tendencias estocásticas.

# La presencia de tendencias estocásticas en una series hace que la serie no sea 
# estacionaria, aspecto que profundizarán conforme avance la materia.

diffyt1n <- diff(yt1n)
x11()
autoplot(diffyt1n, main = "Diferenciación de una serie I(1)", col="turquoise4", 
         lty=1, lwd=0.25) + theme_light()


# comparacion graficas de la serie con diferencia y sin diferencia

x11()
par(mfrow = c(1, 2))  # 1 fila, 2 columnas

# Primera gráfica
plot(diffyt1n, main = "Diferenciación de una serie I(1)", 
     col = "turquoise4", lty = 1, lwd = 0.25, type = "l")  # `type = "l"` para líneas
grid()  # Agregar cuadrícula

# Segunda gráfica
plot(yt1n, main = "Serie y_t PGD ARIMA (1,1,0)", 
     col = "darkblue", lty = 1, lwd = 0.4, type = "l")  # `type = "l"` para líneas
grid()  # Agregar cuadrícula
# Tiene un comportamiento que sugiere estacionariedad


# Para corroborar lo anterior, vemos como cambian la ACF y PACF 

x11()
lags=20
par(mfrow=c(1,2))
acf(diffyt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,0)') 
pacf(diffyt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,0)')

# Claramente, se observa que la ACF ahora cae de manera rápida y "geométrica" 
# lo que es un indicativo de que la primera diferencia de la serie es estacionaria 


#---- Simulación de un proceso AR(2) estacionario ----
#ar=c() coeficiente  autoregresivo

set.seed(2929) 
yt2s = ts(arima.sim(model= list(order = c(2,0,0), ar=c(0.6,0.3)), n = obs, 
                    innov=rnorm(n = obs, mean = 0, sd = 1))) 

#Graficamos

x11()
autoplot(yt2s, xlab="",ylab="", main=" Serie y_t PGD ARIMA (2,0,0)",
         lty=1, lwd=0.4, col="green4") + theme_light()

# Aunque se puede notar la estacionariedad, podemos ver que presenta una mayor 
# persistencia que el AR(1). Comparemos con éste mismo.


x11()
par(mfrow = c(1, 2)) 

plot(yt1s, xlab="",ylab="", main=" Serie y_t  PGD ARIMA (1,0,0)",lty=1, lwd=0.4,
         col="turquoise4") + theme_light()
plot(yt2s, xlab="",ylab="", main=" Serie y_t PGD ARIMA (2,0,0)",
         lty=1, lwd=0.4, col="green4") + theme_light()


#Vamos a hacer la ACF y PACF del PGD ARIMA(2,0,0)
x11()
lags=20
par(mfrow=c(1,2))
acf(yt2s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(2,0,0)') 
pacf(yt2s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(2,0,0)')

# Se puede ver la caída "geométrica" de la ACF característica de las series estacionarias. 
# No obstante, se muestra que dicha caída es más lenta a comparación del proceso
# AR(1) estacionario.

#---- Simulación de un proceso AR(2) no estacionario (ARIMA(2,1,0)) ----

set.seed(0202) 
yt2n = ts(arima.sim(model= list(order = c(2,1,0), ar=c(0.6,0.3)), n = obs, 
                    innov=rnorm(n = obs, mean = 0, sd = 1)))

#Graficamos
x11()
autoplot(yt2n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (2,1,0)",lty=1, lwd=0.9, col="green4") + theme_light()

# Podemos ver todos los sintomas de que es un proceso no estacionario.

#Vamos a hacer la ACF y PACF donde confirmamos la persistencia del proceso
x11()
lags=20
par(mfrow=c(1,2))
acf(yt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(2,1,0)') 
pacf(yt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(2,1,0)')
        
# Nuevamente, se observa esa caída lenta "lineal" en la ACF de la serie no estacionaria 

# Vamos a diferenciar la serie y ver que pasa con la serie diferenciada 

diffyt2n <- diff(yt2n)
x11()
autoplot(diffyt2n, main = "Diferenciación de una serie I(1)", col="turquoise4") +
  theme_light()

# Pareciera gráficamente que la primera diferencia de la series original es estacionaria

# Vemos como cambian la ACF y PACF de la serie diferenciada
        
x11()
lags=20
par(mfrow=c(1,2))
acf(diffyt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(2,1,0)') 
pacf(diffyt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(2,1,0)')

#Nuevamente vemos que la serie diferenciada SÍ es estacionaria


#---- Simulación de un MA(1)  ----
#ma=c() coeficiente  del modelo de media movil

set.seed(2020) 
yt3s = ts(arima.sim(model= list(order = c(0,0,1), ma=c(0.7)), n = obs, 
                    innov=rnorm(n = obs, mean = 0, sd = 1))) 
#Graficamos

x11()
autoplot(yt3s, xlab="",ylab="", main=" Serie y_t  ARIMA (0,0,1)",lty=1, lwd=0.9,
         col="gray") + theme_light()

# Claramente, la gráfica muestra un proceso que parece ser estacionario 

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,0,1)

x11()
lags=20
par(mfrow=c(1,2))
acf(yt3s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,0,1)') 
pacf(yt3s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,0,1)')


#---- Simulación de un MA(2) estacionario ----

set.seed(10181) 
yt4s = ts(arima.sim(model= list(order = c(0,0,2), ma=c(0.7,0.25)), n = obs, 
                    innov=rnorm(n = obs, mean = 0, sd = 1))) 

#Graficamos
x11()
autoplot(yt4s, xlab="",ylab="", main=" Serie y_t ARIMA (0,0,2)",
         lty=1, lwd=0.18, col="turquoise4") + theme_light()

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,0,2)
lags=20
x11()
par(mfrow=c(1,2))
acf(yt4s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,0,2)') 
pacf(yt4s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,0,2)')


#---- Simulación de un ARMA(1,1) estacionario ----

# La parsimonia es una razón relevante para estimar modelos con ambos componentes
# ya que el componente MA recoge la dependencia que habría con un número alto de 
# rezagos.

set.seed(220422) 
yt5s = ts(arima.sim(model= list(order = c(1,0,1), ar=c(0.7),ma=c(0.3)), n = 1000,
                    innov=rnorm(n = obs, mean = 0, sd = 1))) 

#Graficamos
x11()
autoplot(yt5s, xlab="",ylab="", main=" Serie y_t PGD ARIMA (1,0,1)",
         lty=1, lwd=0.7, col="orange") + theme_light()

# El proceso ARMA(1,1) gráficamente parece ser estacionario 

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,0,1)
x11()
lags=20
par(mfrow=c(1,2))
acf(yt5s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,0,1)') 
pacf(yt5s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,0,1)')

# Para un proceso ARMA(p,q) es más complejo el análisis de ACF y PACF. 

# Ésto se debe a que a diferencia de una AR puro o un MA puro puede que las ACF 
# y la PAFC del ARMA(p,q) no se corten abruptamente como si pasa en los procesos "puros".

#---- Simulación de un ARMA(1,1) no estacionario (ARIMA(1,1,1))----

set.seed(81711) 
yt5n = ts(arima.sim(model= list(order = c(1,1,1),ar=c(0.6), ma=c(0.7)), n = obs,
                    innov=rnorm(n = obs, mean = 0, sd = 1))) 

#Graficamos

x11()
autoplot(yt5n, xlab="",ylab="", main=" Serie y_t PGD ARIMA (1,1,1)",lty=1, lwd=0.7,
         col="red2") + theme_light()

# Nuevamente, la gráfica de la serie parece mostrar que esta no se estabailiza 
# alrededor de una media de largo plazo. Adicionalmente, se ve persistencia en el 
# proceso.


#Vamos a hacer la ACF y PACF del PGD ARIMA(1,1,1)
x11()
lags=20
par(mfrow=c(1,2))
acf(yt5n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,1)') 
pacf(yt5n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,1)')

# Diferenciemos

dif_yt5n <- diff(yt5n)

x11()
autoplot(dif_yt5n, xlab="",ylab="", main=" Diferencia de una serie y_t PGD ARIMA (1,1,1)",
         lty=1, lwd=0.7, col="red2") + theme_light()

# Analicemos sus gráficos de correlación

x11()
par(mfrow=c(1,2))
acf(dif_yt5n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,1)') 
pacf(dif_yt5n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,1)')


#===================================================================#
#                          FIN DEL CÓDIGO                           #
#===================================================================#
