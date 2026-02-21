#=====================================================================================#
#                          UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2024 - 02
#                            Econometría II | Monitoría 
#
#                                     Sesión 5:
#                 Prueba Dickey - Fuller : Conceptos y aplicaciones
#=====================================================================================#

#~~ ¡Feliz año nuevo! 🎄 ~~# 

#===========================#
#### Tabla de Contenidos ####
#===========================#

#   Instalación e importación de paquetes

# 1. Simulaciones de Montecarlo
#  1.1. Establecimiento de valores y funciones
#  1.2. Simulación con tendencia y deriva
#  1.3. Simulación con deriva
#  1.4. Simulación prueba sencilla
#  1.5. Contraste

# 2. Aplicación ADF - Log Consumo UK
# 2.1 Introducción
# 2.2 Carga y gráficos 
# 2.3 Prueba Dickey Fuller
# 2.4 Primera diferencia

# 3. Aplicación ADF - GDP real USA
# 3.1 Carga y gráficos 
# 3.2 Prueba Dickey Fuller
# 3.3 Primera diferencia


# Limpiar el Environment y reiniciar dispositivos
rm(list = ls()) 
dev.off()


# Importación de paquetes
install.packages("pacman")

pacman :: p_load(
  forecast,   # Realiza predicción y autoplot
  lmtest,     # Significancia individual Coeficientes ARIMA
  tseries,    # Para estimar modelos de series de tiempo y pruebas de supuestos 
  readxl,     # Para leer archivos de excel
  stargazer,  # Presentación de resultados y tablas más estéticos
  tidyverse,  # Conjunto de paquetes (incluye dplyr y ggplot2)
  dynlm,      # Paquete que me permite realizar MCOD
  urca        # Prueba de raíz unitaria
)

### I. Simulaciones de Montecarlo

# Las simulaciones que vamos a realizar tienen como objetivo observar las 
# distribuciones de los parámetros de interés y los t-estadísticos de la prueba.

# I.I Establecimiento de valores y funciones

set.seed(1)   
n = 100        # Número de observaciones por serie
N = 10000      # Número de series simuladas

# Términos complementarios

trend = 1:n # tendencia deterministica lineal
drift = 0.5 # deriva

# Funciones

# Esta función genera un DataFrame que guarda la información necesaria de cada
# simulación.

simulacion_DF = function(n, N, tipo_prueba, trend = 1:n, drift = 0.1){
  
  # Se crea un Data Frame vacio 
  sim_df = data.frame(parametro_interes = double(), DF_statistic = double())
  
  # Tendencia y deriva
  if(tipo_prueba == "drift+trend"){
    
    # Se realiza la iteración de cada serie simulada
    for(i in 1:N){
      
      #1. Serie de Tiempo simulada
      yt = arima.sim(model= list(order = c(0,1,0)), n = n-1, innov = rnorm(n, mean = 0, sd =1)) 
      + drift + trend
      
      # 2. Estadístico de la regresión auxiliar Dickey Fuller
      summa = summary(dynlm(diff(yt, 1) ~ L(yt, 1) + trend(yt)))
      
      # Extraemos el coeficiente y el t estadístico para agregarlos al DF
      sim_df[i,] = c(summa$coef[2,1], summa$coef[2,3])
    }
    # Establecemos lo mismo para la prueba únicamente con deriva y sencilla
    
    # Deriva
  }else if(tipo_prueba == "drift"){
    for(i in 1:N){
      yt = arima.sim(model= list(order = c(0,1,0)), n = n, innov = rnorm(n, mean = 0, sd =1)) + drift 
      summa = summary(dynlm(diff(yt, 1) ~ L(yt, 1))) 
      sim_df[i,] = c(summa$coef[2,1], summa$coef[2,3])
    }
    
    # Sencilla
  }else if(tipo_prueba == "none"){
    for(i in 1:N){
      yt = arima.sim(model= list(order = c(0,1,0)), n = n, innov = rnorm(n, mean = 0, sd =1))
      summa = summary(dynlm(diff(yt, 1) ~ L(yt, 1) - 1))
      sim_df[i,] = c(summa$coef[1,1], summa$coef[1,3])
    }
  }
  return(sim_df)
} 


# Ahora creamos una función que grafica los histogramas según cada parámetro.

histogram_rho = function(df, titulo, x_lab, color_llenado, num_bins = 30){
  
  
  histog = df %>%
    ggplot(aes(x = parametro_interes)) +
    geom_histogram(color = "black", fill = color_llenado, bins = num_bins) + 
    theme_light() +
    ggtitle(titulo) +
    ylab("Frencuencia") +
    xlab(x_lab)
  return(histog)
  
}

# Histograma para el estadístico de DF
histogram_DF = function(df, titulo, x_lab, color_llenado, num_bins = 30){
  
  
  histog = df %>%
    ggplot(aes(x = DF_statistic)) +
    geom_histogram(color = "black", fill = color_llenado, bins = num_bins, aes(y = ..density..)) + 
    geom_density() + 
    theme_light() +
    ggtitle(titulo) +
    ylab("Densidad") +
    xlab(x_lab)
  return(histog)
  
}

# I.II Simulación con trend y deriva

# Simulación de monte carlo para una estadístico DF con trend y drift
df_trend_drift = simulacion_DF(n = n, N = N, tipo_prueba = "drift+trend", 
                               trend = trend, drift = drift)

# Histograma para el parámetro de interés en la prueba de DF (el que acompaña y_t-1)
histogram_rho_trend_drift = histogram_rho(df_trend_drift,
                                          "Rho estimado Dickey-Fuller con drift y trend", 
                                        "Valores rho", 
                                        "red1"); histogram_rho_trend_drift

# Histograma del estadístico-t de DF con trend y drift
hist_DF_trend_drift = histogram_DF(df_trend_drift, 
                            "Estadístico Dickey-Fuller con drift y trend",
                            "Valores estadístico de DF", 
                            "turquoise1"); hist_DF_trend_drift

# Valores críticos estadístico-t de DF con trend y drift
critical_trend_drift = round(quantile(df_trend_drift$DF_statistic, c(0.1, 0.05, 0.01)), 2); critical_trend_drift



# I.III Simulación con deriva

# Simulación de monte carlo para una estadístico DF con drift
df_drift = simulacion_DF(n = n, N = N, tipo_prueba = "drift", trend = trend, drift = drift)

# Histograma para el parámetro de interés en la prueba de DF (el que acompaña y_{t-1})
histogram_rho_drift = histogram_rho(df_drift,
                                        "Rho estimado Dickey-Fuller solo con drift", 
                                        "Valores rho", "purple1"); histogram_rho_drift

# Histograma del estadístico-t de DF solo con deriva
hist_DF_drift = histogram_DF(df_drift, 
                            "Estadístico Dickey-Fuller solo con drift",
                            "Valores estadístico de DF", "orange1"); hist_DF_drift

# Valores críticos estadístico-t de DF solo con drift
critical_drift = round(quantile(df_drift$DF_statistic, c(0.1, 0.05, 0.01)), 2); critical_drift



# I.III Simulación sencilla (No trend No drift)

# Simulación de monte carlo para una estadístico DF sin términos determinísticos 
df_none = simulacion_DF(n = n, N = N, tipo_prueba = "none", trend = trend, drift = drift)

# Histograma para el parámetro de interés en la prueba de DF 
histogram_rho_none = histogram_rho(df_none,
                                        "Rho estimado prueba Dickey-Fuller", 
                                        "Valores rho", "pink2"); histogram_rho_none

# Histograma del estadístico-t de DF sin trend y sin deriva
hist_DF_none = histogram_DF(df_none, 
                            "Estadístico Dickey-Fuller sencilla",
                            "Valores estadístico de DF", "yellow1"); hist_DF_none

# Valores críticos estadístico-t de DF solo con drift
critical_none = round(quantile(df_none$DF_statistic, c(0.1, 0.05, 0.01)), 2); critical_none



# I.IV Contraste

# Construcción de un data frame con todos los valores críticos para las diferentes
# especificaciones de la prueba de DF.

critical_df = data.frame(sign_10 = double(), sign_5 = double(), sign_1 = double())
critical_df[1,] = critical_trend_drift
critical_df[2,] = critical_drift
critical_df[3,] = critical_none
names(critical_df)[1] = "10%"; names(critical_df)[2] = "5%"; names(critical_df)[3] = "1%"


# Visualicemoslo
critical_df

# Vamos a recoger en un Data Frame todas las distribuciones 
df_todos = data.frame(DF_none = df_none$DF_statistic, DF_drift = df_drift$DF_statistic, 
                      DF_trend_drift = df_trend_drift$DF_statistic, Normal_estándar = rnorm(10000)) %>% 
            pivot_longer(cols = 1:4, names_to = "Distribuciones", values_to = "Valores_distribuciones") 
#Nota: Pivot Longer es una función del paquete Tidyverse. Ancho a largo en DF


# Finalmente graficamos
x11()
density_comparacion = df_todos %>% 
  ggplot(aes(x = Valores_distribuciones, color = Distribuciones)) +
  geom_density() + 
  theme_light() +
  ggtitle("Comparación de funciones de densidad estadísticos de DF vs Normal") +
  ylab("Densidades") +
  xlab("Valores diferentes distribuciones DF y distribución normal"); density_comparacion 
  
  

### II. Aplicación ADF - Log Consumo UK
# II.I Introducción

#Para conocer el orden de integración de una serie se siguen los siguientes pasos:
  
#1. Graficar la serie
#2. Mirar FAC y FACP
#3. Conducir la prueba secuencial de Dickey Fuller
#4. Si no hay estacionariedad, diferenciar la serie y repetir el procedimiento.

# Nota: Prueba KPSS y Zivot-Andrews como pruebas complementarias y/o alternativas.

# Los valores críticos corresponden a 100 observaciones de muestra. Para mejor
# calibración observar Tabla A del apéndice de tablas estadística del manual 
# suplementario del libro de Enders.

#II.II Carga y gráficos del Data Set

data(Raotbl3) # Paquete Urca
glimpse(Raotbl3)

# Como profesionales, primero convertimos a un objeto ts
lc = ts(Raotbl3$lc, start = c(1966, 4), end = c(1991, 2), frequency = 4)

# Graficamos la serie
x11()
autoplot(lc, col="blue", main = "Consumo real del Reino Unido (A precios de 1985)",
         xlab = "Fecha", ylab="", lwd=0.8) + theme_light()

# Graficamos los correlogramas

lags = 24
x11()
ggAcf(lc,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF consumo real UK") + theme_light()
ggPacf(lc,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF consumo real UK")+ theme_light()

# II.III Prueba Dickey Fuller

#tau3 nos dice si hay presencia de raiz unitaria 
#phi2 nos dice si hay deriva significativa
#phi3 nos dice si hay tendencia significativa

adf.trend = ur.df(lc, type = "trend", lags =3); x11(); plot(adf.trend)
summary(adf.trend)

adf.drift = ur.df(lc, type = "drift", lags = 3); x11(); plot(adf.drift)
summary(adf.drift)

adf.none = ur.df(lc, type = "none", lags = 3); x11() ; plot(adf.none)
summary(adf.none)

# Sin deriva ni tendencía, y no hay estacionariedad.

summary(ur.kpss(lc)) # Prueba KPSS complementaria

# Debemos diferenciarla :0

# II.IV Primera Diferencia

diff.lc = diff(lc)

# Graficamos
x11()
autoplot(diff.lc, col="deepskyblue4",
         main = "Tasa de crecimiento del consumo real del Reino Unido",
         xlab = "Fecha", ylab="", lwd=0.8) + theme_light()

# Realizamos los correlogramas

x11()
ggAcf(diff.lc,lag.max=lags,plot=T,lwd=2) + 
  ggtitle("ACF tasa de crecimiento consumo real UK") + theme_light()
ggPacf(diff.lc,lag.max=lags,plot=T,lwd=2) + 
  ggtitle("PACF tasa de crecimiento consumo real UK")+ theme_light()

# Realicemos las respectivas pruebas formales


adf.diff.none = ur.df(diff.lc, type = "none", lags = 1);x11();plot(adf.diff.none) 
summary(adf.diff.none)


# Comprobamos con una segunda prueba formal

summary(ur.kpss(diff.lc))



## III. Aplicación de Prueba Dickey Fuller Aumentada - GDP real USA

# III.I Gráficos y carga

gdp <- read_excel(file.choose())
glimpse(gdp)

# Graficamos la serie

real_gdp = ts(gdp$RGDP,start = c(1957,1), end = c(2007,4), frequency = 4)
x11()
autoplot(real_gdp, col="hotpink3", main = "PIB Real USA", xlab = "Fecha", ylab="",
         lwd=0.8) + theme_light()

# Visualicemos los correlogramas.

x11()
ggAcf(real_gdp,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA") 
+ theme_light()
ggPacf(real_gdp,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")
+ theme_light()

# III.II Prueba Dickey Fuller

adf.trend = ur.df(real_gdp, type = "trend", lags = 1); x11(); plot(adf.trend)
summary(adf.trend)

# ¿Será que diferenciamos?

# III.III Primera diferencia

# Para mayor interpretabilidad y estabilización de varianza aplicamos log natural

dl.real_gdp = diff(log(real_gdp))
x11()
autoplot(dl.real_gdp, col="red", main = "tasa de crecimiento del PIB real USA",
         xlab = "Fecha", ylab="", lwd=0.8) + theme_light()

# Observemos sus correlogramas

x11()
ggAcf(dl.real_gdp,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF tasa de crecimiento PIB real USA") + theme_light()
ggPacf(dl.real_gdp,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF tasa de crecimiento PIB real USA")+ theme_light() 

# Aunque hay estacionariedad aparente, veamos una prueba formal
# Se mantiene la deriva para la serie transformada

adf.diff.drift = ur.df(dl.real_gdp,type="drift",lags=1);x11();plot(adf.diff.drift)
summary(adf.diff.drift)

# BONO ! : Para el que analice los resultados de esta prueba Dickey Fuller