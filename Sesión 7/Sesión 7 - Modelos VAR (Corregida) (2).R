#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2023 - 02
#                            Econometría II | Monitoría 
#
#                                     Sesión 7:
#                        Modelos de vectores autorregresivos 
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
# 1. Creación de serie simulada 
# 2. Metodologpia Box-Jenkins para series multivariadas
#  2.1. Identificación
#  2.2. Estimación
#  2.3. Validación de supuestos
#  2.4. Pronóstico y funciones Impulso respuesta 
# 3. Ejemplo de Enders: VAR con 3 variables
#
#_____________________________________________________________________________________#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Instalación de paquetes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(pacman)

pacman::p_load(
  vars,         # Para usar modelos VAR
  urca,         # Para realizar pruebas de raíz unitaria
  tidyverse,    # Paquete que incluye ggplot2 y dplyr
  ggfortify,    # Para graficar series de tiempo
  gridExtra,    # Para concatenar gráficas en un solo plot
  tidyr,        # Para ordenar y depurar datos 
  readxl        # Para leer archivos xlsx
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 1. Creación de serie simulada ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Fijamos la semilla para que siempre dé el mismo resultado

set.seed(82901) 

# Determinamos un tamaño de muestra de 300 observaciones

T = 300 

# y_t es una matriz de 2 variables (una variable por columna), En tanto hay dos 
# variables, habrá 2 residuales.

y_t <- cbind(rep(0, T),rep(0, T))
u_t = cbind(rnorm(T), rnorm(T))

# Definimos la matriz de coeficientes autorregresivos.

A_1 = cbind(c(0.3, 0.5), c(0.2, 0.6))

A_0 = rbind(0.5,0.5)

# Función que permite simular un VAR(1) 

sim = function(y_t, A_1, u_t, T){
  for (i in 2:T) {
    y_t[i,] = A_0 + A_1 %*% y_t[i-1,] + u_t[i,] 
  }  
  return(y_t)
}

# La simulación lo que busca es modelar las variables a partir de la fórmula del 
# VAR en forma reducida

y_t = sim(y_t, A_1, u_t, T) # La función sim lo que busca es llenar la matriz 
                            # de ceros y_t con valores

# Convertimos la serie en un objeto ts

y_t = ts(y_t, start=c(1900,1), frequency=4)


#~~~~~~~~~~~~~~~~~~~~~~#
# Gráficas de la serie #
#~~~~~~~~~~~~~~~~~~~~~~#

y1 = autoplot(y_t[,1], size=1,ts.colour="lightblue", 
              xlab="",ylab="", main="Variable y_1")
y2 = autoplot(y_t[,2], size=1,ts.colour="royalblue", 
              xlab="",ylab="", main="Variable y_2")

x11();grid.arrange(y1,y2,ncol=2)

# Recuerden que los modelos VAR requieren de series estacionarias. 

adf1= ur.df(y_t[,1], lags=3,selectlags = "AIC",type="none")
summary(adf1) # Rechazo H0, la serie es I(0)

adf2= ur.df(y_t[,2], lags=3,selectlags = "AIC",type="none")
summary(adf2) # Rechazo H0, la serie es I(0)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 2. Metodología Box-Jenkins para series multivariadas ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###### 2.1. Identificación ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Ya tenemos nuestra serie multivariada, veamos que rezago nos recomienda la 
# función VARselect() 

# Selección de rezagos para un VAR con tendencia e intercepto.
VARselect(y_t, lag.max=6,type = "both", season = NULL)

# Selección de rezagos para un VAR con sólo intercepto.
VARselect(y_t, lag.max=6,type = "const", season = NULL)

# Selección de rezagos para un VAR sin términos determinísticos.
VARselect(y_t, lag.max=6,type = "none", season = NULL)

# Todos los criterios en cada prueba, recomiendan un VAR(1)

#~~~~~~~~~~~~~~~~~~~~~~~~~#
###### 2.2. Estimación ####
#~~~~~~~~~~~~~~~~~~~~~~~~~#

# Para seleccionar el VAR(1), verificamos si tiene intercepto y deriva

# VAR con tendencia e intercepto
V.tr = VAR(y_t, p=1, type="both", season=NULL)
summary(V.tr) # La tendencia no es significativa, analizamos const

# VAR con intercepto.
V.dr= VAR(y_t, p=1, type="const", season=NULL) 
summary(V.dr) # El intercepto es significativo en una ecuación, veamos none

# VAR sin términos determinísticos.
V.no = VAR(y_t, p=1, type="none", season=NULL)  
summary(V.no)

# Elegimos el modelo con constante, pues se ha visto que tienen constante signi-
# ficativa.

# Raíces del proceso. Deben ser menores a |1| para que sea estacionario. 

roots(V.dr) #El proceso es estable.

# Ahora analizamos cada uno de los coeficientes estimados. 

#Coeficientes:

Bcoef(V.dr) # Podemos ver todos los coeficientes con Bcoef


Acoef(V.dr) # Los valores teóricos eran: a11=0.3; a12=0.2; a21=0.5; a22=0.6. 
            # Las estimaciones son cercanas.

# Matriz de varianzas y covarianzas de los residuales

Sigma.est = summary(V.dr)$covres
Sigma.est 

# Análisis en conjunto
summary(V.dr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###### 2.3. Validación de supuestos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ No autocorrelación serial ~~#

# PT.asymptotic es para muestra grande y "PT.adjusted" para muestra pequeña.
P.75=serial.test(V.dr, lags.pt = 75, type = "PT.asymptotic");P.75 # No rechazo
P.30=serial.test(V.dr, lags.pt = 30, type = "PT.asymptotic");P.30 # No rechazo
P.20=serial.test(V.dr, lags.pt = 20, type = "PT.asymptotic");P.20 # No rechazo
P.10=serial.test(V.dr, lags.pt = 10, type = "PT.asymptotic");P.10 # No rechazo


# Graficamos los residuales para 20 pasos_adelantes: se grafican los residuales, 
# su distribución, la ACF y PACF de los residuales y a ACF y PACF de los 
# residuales al cuadrado (proxy para heterocedasticidad)

x11()
plot(P.20, names = "Series.1") # Los residuales de la primera serie se comportan bien
plot(P.20, names = "Series.2") # Los residuales de la segunda serie se comportan bien

#~~ Homocedasticidad ~~#

# Test tipo ARCH multivariado
arch.test(V.dr, lags.multi = 24, multivariate.only = TRUE) # No rechazo
arch.test(V.dr, lags.multi = 12, multivariate.only = TRUE) # No rechazo

#~~ Normalidad ~~#

# Jarque-Bera para series multivariadas.  
normality.test(V.dr) #No rechazo, se cumple el supuesto.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###### 2.4. Pronóstico y funciones Impulso respuesta ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~#
# Pronóstico #
#~~~~~~~~~~~~#

x11()
predict(V.dr, n.ahead = 12,ci=0.95) 
autoplot(predict(V.dr, n.ahead = 12,ci=0.95)) 

# Versión Fanchart
fanchart(predict(V.dr, n.ahead = 12), colors = c("blue","lightblue"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Funciones de impulso respuesta #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Definimos el número pasos adelante

pasos_adelante = 0:18

# Función que me permite calcular y graficar las funciones impulso respuesta 

# (A cada función impulso respuesta le asigno una gráfica)

impulso_respuesta = function(var, impulso, respuesta, pasos_adelante, ortog, 
                             int_conf, titulo){
  
  "Función diseñada por German Camilo Rodriguez" 
  
 "Calcula las funciones impulso respuesta ortogonalizadas y no ortogonalizadas 
  y devuelve una grafíca IRF o OIRF dependiendo la especificación"
  
  # Cáclulo de la función impulso respuesta
  total_pasos_futuros = length(pasos_adelante) - 1
  IRF = irf(var, impulse=impulso, response=respuesta, n.ahead = total_pasos_futuros, 
            ortho=ortog, ci = int_conf)
  IRF_data_frame = data.frame(IRF$irf,IRF$Lower,IRF$Upper, pasos_adelante)
  # Gráfica de la función impulso respuesta
  graph = IRF_data_frame %>% 
    ggplot(aes(x=IRF_data_frame[,4], y=IRF_data_frame[,1], ymin=IRF_data_frame[,2], 
               ymax=IRF_data_frame[,3] )) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(titulo)+
    ylab("")+
    xlab("Pasos adelante") +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))    
  return(graph)
}

# IRF de las variables del sistema ante distintos choques exógenos.

y1.y1 = impulso_respuesta(V.dr, "Series.1", "Series.1", pasos_adelante, ortog = F,
                          int_conf = 0.95, titulo = "Impulso de y1 - respuesta de y1")
y1.y2 = impulso_respuesta(V.dr, "Series.1", "Series.2", pasos_adelante, ortog = F,
                          int_conf = 0.95, titulo = "Impulso de y1 - respuesta de y2")
y2.y1 = impulso_respuesta(V.dr, "Series.2", "Series.1", pasos_adelante, ortog = F,
                          int_conf = 0.95, titulo = "Impulso de y2 - respuesta de y1")
y2.y2 = impulso_respuesta(V.dr, "Series.2", "Series.2", pasos_adelante, ortog = F, 
                          int_conf = 0.95, titulo = "Impulso de y2 - respuesta de y2")

x11()
grid.arrange(y1.y1,y1.y2,y2.y1,y2.y2,ncol=2)

# IRF Ortogonalizadas. 

y1.y1. = impulso_respuesta(V.dr, "Series.1", "Series.1", pasos_adelante, ortog = T,
                           int_conf = 0.95, titulo = "Impulso ortogonal de y1 - respuesta de y1")
y1.y2. = impulso_respuesta(V.dr, "Series.1", "Series.2", pasos_adelante, ortog = T,
                           int_conf = 0.95, titulo = "Impulso ortogonal de y1 - respuesta de y2")
y2.y1. = impulso_respuesta(V.dr, "Series.2", "Series.1", pasos_adelante, ortog = T,
                           int_conf = 0.95, titulo = "Impulso ortogonal de y2 - respuesta de y1")
y2.y2. = impulso_respuesta(V.dr, "Series.2", "Series.2", pasos_adelante, ortog = T,
                           int_conf = 0.95, titulo = "Impulso ortogonal de y2 - respuesta de y2")

x11()
grid.arrange(y1.y1.,y1.y2.,y2.y1.,y2.y2.,ncol=2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Descomposición de varianza del error de pronóstico #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

# Aquí veremos la proporción de la varianza de error de pronóstico de cada variable
# explicada por las variables dentro del sistema

x11()
fevd(V.dr, n.ahead = 18)
plot(fevd(V.dr, n.ahead = 18),col=c("orange3", "firebrick4"))
 
# VAR(1) --> VMA(8^T)

# Representación donde se obtienen las IRF sencillas.


Phi(V.dr, nstep=10) # Esta función nos calcula la matriz de coeficientes 
                   # n pasos adelante

# Coeficientes de las IRF ortogonales

Psi(V.dr, nstep=10)  

#~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 3. Ejemplo Enders ####
#~~~~~~~~~~~~~~~~~~~~~~~~~#

# Tenemos series de frecuencia trimestral desde 1960 Q1 - 2012 Q4 
# para el Índice de Producción Industrial, El índice de precios al consumidor y 
# la tasa de desempleo de Estados Unidos


# Definimos la base de datos y las variables en nivel
Base = read_excel(file.choose())
            
IPI = ts(Base$IPI, start=c(1960,1), frequency=4)
CPI= ts(Base$CPI, start=c(1960,1),  frequency=4)
UNEM = ts(Base$Unem, start=c(1960,1), frequency=4)

# Ahora definimos la tasa de inflación y la tasa de crecimiento del IPI

dl.IPI = diff(log(IPI))
dl.CPI = diff(log(CPI))

# Graficamos las series:

x11()
autoplot(dl.IPI, size=1, colour="lightblue", xlab="Año", ylab="Porcentaje", 
         main="Tasa de crecimeinto IPI")+labs(subtitle = "USA (1960-2012)")
autoplot(CPI, size=1, colour="sienna1", xlab="Año",  ylab="Porcentaje",
         main="Variación de CPI")+ labs(subtitle = "USA (1960-2012)")
autoplot(UNEM, size=1, colour="mediumpurple2", xlab="Año", ylab="Porcentaje",
         main="Tasa de desempleo")+ labs(subtitle = "USA (1960-2012)")

# Ahora hacemos las pruebas de raíz unitaria

#Tasa de crecimiento IPI
adf3= ur.df(IPI, lags=6, selectlags="AIC", type="none") 
summary(adf3) # La serie es estacionaria y con intercepto

#Variación del CPI
adf4= ur.df(CPI, lags=6, selectlags="AIC", type="trend")
summary(adf4) # La serie es estacionaria y con intercepto.

#Tasa de desempleo
adf5= ur.df(UNEM, lags=6, selectlags="AIC", type="drift")
summary(adf5) # La serie es estacionaria y con intercepto.

# Todas las variables son estacionarias.

#Tasa de crecimiento IPI
adf3= ur.df(dl.IPI, lags=6, selectlags="AIC", type="none") 
summary(adf3) # La serie es estacionaria y con intercepto
 
#Variación del CPI
adf4= ur.df(dl.CPI, lags=6, selectlags="AIC", type="trend")
summary(adf4) # La serie es estacionaria y con intercepto.

#Tasa de desempleo
adf5= ur.df(UNEM, lags=6, selectlags="AIC", type="drift")
summary(adf5) # La serie es estacionaria y con intercepto.

# Todas las variables son estacionarias después de las respectivas 
# transformaciones.

# Definimos una matriz con las variables, con el siguiente ordenamiento
Unem = UNEM[1:211]
Y = cbind(dl.IPI,Unem,dl.CPI)

#~~~~~~~~~~~~~~~~~~~~~#
# 3.1. Identificación #
#~~~~~~~~~~~~~~~~~~~~~#

#Selección de rezagos para un VAR con deriva y tendencia
VARselect(Y, lag.max=6,type = "trend", season = NULL) # Elegimos 3 rezagos

#Selección de rezagos para un VAR con sólo intercepto.
VARselect(Y, lag.max=6,type = "const", season = NULL) # Elegimos 3 rezagos

#Selección de rezagos para un VAR sin términos determinísticos.
VARselect(Y, lag.max=6,type = "none", season = NULL) # Elegimos 5 rezagos

#~~~~~~~~~~~~~~~~~#
# 3.2. Estimación #
#~~~~~~~~~~~~~~~~~#

# VAR con Tendencia e intercepto.
V.tr.1= VAR(Y, p=3, type="both", season=NULL) 
summary(V.tr.1) # Tendencia significativa en dl.IPI ¿Tiene esto sentido?
 
# VAR con sólo intercepto.
V.dr.1= VAR(Y, p=3, type="const", season=NULL) 
summary(V.dr.1) # El intercepto es significativo en el desempleo ¿Es lógico?

# VAR con sólo intercepto.
V.no.1= VAR(Y, p=3, type="none", season=NULL) 
summary(V.no.1)

# Por Parsimonia y por la presencia de constantes en la serie, escogemos el 
# modelo con 3 rezagos de const. No escogemos el de tendencia porque la gráfica
# no es convincente y por las conclusiones de la Dickey Fuller.

# Elegimos el VAR con constante

# Raíces del proceso
roots(V.dr.1) # El proceso es estable.

#Ahora analizamos cada uno de los coeficientes estimados. 

#Coeficientes:
Acoef(V.dr.1) #Presenta los resultados de la matriz A_1, A_2 y A_3

#Matriz de varianzas y covarianzas de los residuales
Sigma.e = summary(V.dr.1)$covres
Sigma.e


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3.3. Validación de supuestos #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Ahora vamos a realizar la validación de supuestos. 

#~~ No autocorrelación serial ~~#

P.75.1=serial.test(V.dr.1, lags.pt = 50, type = "PT.asymptotic");P.75.1 #No rechazo
P.30.1=serial.test(V.dr.1, lags.pt = 30, type = "PT.asymptotic");P.30.1 #No rechazo
P.20.1=serial.test(V.dr.1, lags.pt = 20, type = "PT.asymptotic");P.20.1 #No rechazo
 
# Comportamiento de los residuales

x11()
plot(P.20.1, names = "dl.IPI") #Relativamente Bien comportados, salvo por normalidad
plot(P.20.1, names = "Unem") #Relativamente Bien comportados.
plot(P.20.1, names= "dl.CPI") #Relativamente Bien comportados, salvo por normalidad.

#~~ Homocedasticidad ~~# 

arch.test(V.dr.1, lags.multi = 24, multivariate.only = TRUE) # rechazo H0
arch.test(V.dr.1, lags.multi = 12, multivariate.only = TRUE) # rechazo H0 

#~~ Normalidad ~~# 

normality.test(V.dr.1) #rechazo, no se cumple el supuesto. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3.4. Pronóstico y funciones de impulso respuesta #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~#
# Pronóstico #
#~~~~~~~~~~~~#

# Se requiere bootstrapping 
 
x11()
predict(V.dr.1, n.ahead = 12)
autoplot(predict(V.dr.1, n.ahead = 12), main = "Pronóstico de las variables")+
  labs(subtitle = "(2013 T2 - 2016 T2)")

fanchart(predict(V.dr.1, n.ahead = 12))
 
# Necesitamos estimar los intervalos de confianza por Bootstrapping

library(VAR.etp)

For.Boot= VAR.BPR(Y, 3, 12, nboot = 1000, type = "const", alpha = 0.95)
For.Boot

help(VAR.etp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Gráfica de pronóstico puntual con bootsrapping #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

boots = For.Boot$Forecast
boots

boots.forecast = ts(boots, start = c(2013,2), freq = 4)
autoplot(boots.forecast, main = "Pronóstico con Boostrapping")+
  labs(subtitle = "(2013 T2 - 2016 T2)") # ?????

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Funciones de impulso respuesta #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Definimos el número pasos adelante
pasos_adelantes = 0:24

# IRF de las variables ante distintos choques ortogonales
# impulso respuesta: 1. calcula las IRF y 2. graficarlas

Y1.Y1. = impulso_respuesta(V.dr.1, "dl.IPI",
                           "dl.IPI", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. del crecimiento del IPI - res. del crecimiento del IPI")
Y1.Y2. = impulso_respuesta(V.dr.1, "dl.IPI",
                           "Unem", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. del crecimiento del IPI - resp. del desempleo")
Y1.Y3. = impulso_respuesta(V.dr.1, "dl.IPI",
                           "dl.CPI", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. del crecimiento del IPI - resp. de la inflación")
Y2.Y1. = impulso_respuesta(V.dr.1, "Unem",
                           "dl.IPI", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. tasa de desempleo - resp. del crecimiento del IPI")
Y2.Y2. = impulso_respuesta(V.dr.1, "Unem",
                           "Unem", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de desempleo - resp. tasa de desempleo")
Y2.Y3. = impulso_respuesta(V.dr.1, "Unem",
                           "dl.CPI", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de desempleo - resp. de la inflación")
Y3.Y1. = impulso_respuesta(V.dr.1, "dl.CPI",
                           "dl.IPI", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de inflación - resp. del crecimiento del IPI")
Y3.Y2. = impulso_respuesta(V.dr.1, "dl.CPI",
                           "Unem", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de inflación - resp. del desempleo")
Y3.Y3. = impulso_respuesta(V.dr.1, "dl.CPI",
                           "dl.CPI", pasos_adelantes, ortog = T, int_conf = 0.95, 
                           titulo = "Imp. de la tasa de inflación - resp. de la inflación")

# Las IRF ORTOGONALES son:

x11()
grid.arrange(Y1.Y1., Y1.Y2., Y1.Y3., Y2.Y1., Y2.Y2., Y2.Y3., Y3.Y1., Y3.Y2., Y3.Y3., ncol=3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Descomposición de varianza del error de pronóstico #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Vamos a ver la proporción de la varianza que es explicada por las variables del
# sistema

x11()
fevd(V.dr.1, n.ahead = 24)
plot(fevd(V.dr.1, n.ahead = 24),col=c("magenta4", "cyan3", "slateblue3"))

Phi(V.dr.1,nstep=10) # Matriz de coeficientes para 10 pasos adelante

Psi(V.dr.1,nstep=10) # Matriz de coeficientes de las IRF ortogonales 10 pasos
                     # adelante. 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                    FIN DEL CÓDIGO                                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
 