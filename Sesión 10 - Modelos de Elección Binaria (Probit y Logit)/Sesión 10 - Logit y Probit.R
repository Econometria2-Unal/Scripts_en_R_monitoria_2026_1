#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2023 - 02
#                            Econometría II | Monitoría 
#
#                                    Sesión 10:
#                                  Logit y Probit 
#                                  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Limpieza de entorno

rm(list = ls())
dev.off()

#__________________________________________________________________________________#

#~~~~~~~~~~~~~~~~~~~~~#
# Tabla de contenidos #
#~~~~~~~~~~~~~~~~~~~~~#

# 1. Datos 
#  1.1. Importación de datos
#  1.2. Análisis exploratorio
# 2. Modelos 
#  2.1. Modelo de probabilidad lineal
#  2.2. Modelo Probit
#  2.3. Modelo Logit 
# 3. Comparación de los modelos
#  3.1. Desempeño de modelos
# 4. Interpretación de los modelos 
# 5. Ejemplo: Participación Laboral
#  5.1. Estimación de modelos
#  5.2. Predicciones
#  5.3. Efectos marginales
# 6. Algunas funciones de interés

#__________________________________________________________________________________#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Instalación de Paquetes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(pacman)

pacman::p_load(
        
        tidyverse,   # Paquete que incluye ggplot2 y dplyr
        AER,         # Paquete que contiene la base de datos
        gridExtra,   # Para combinar gráficos
        stargazer,   # Para comparar resultados de modelos
        mfx,         # Para interpretar coeficientes de Logit y Probit
        broom,       # Para calcular predicciones 
        DescTools    # Para evaluar el desempeño de los modelos
        
        )


#~~~~~~~~~~~~~~~~#
#### 1. Datos #### 
#~~~~~~~~~~~~~~~~#

# Solicitudes de hipótecas presentadas 
# Boston - USA | (1990)


# Queremos responder la pregunta:

# ¿Que carácterísticas hacen que una solicitud de hipóteca sea menos propensa
#  a ser rechazada?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.1. Importación de datos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

data(HMDA)    # Llamamos los datos del paquete AER
datos = HMDA  # Le asignamos un nombre para trabajar

View(datos)
class(datos)

help("HMDA") # Vemos que significa cada variable

# Resumen de datos

summary(datos)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.2. Análisis exploratorio ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Variables dicotómicas ~~#

# deny = ¿Se rechazó la hipoteca?  ~~ VARIABLE DEPENDIENTE ~~

# high = ¿Se graduó de Bachiller?
# self = ¿Trabaja por su propia cuenta?
# single = ¿Está solter@?

deny_bar = ggplot(datos, aes(x = deny),)+
  geom_bar(colour = "black", fill = c("navy","coral2"))+
  labs(x = " ", y = "Frecuencia", 
       title = "Hipoteca Denegada", subtitle = "(Gráfico de frecuencias)")


high_bar = ggplot(datos, aes(x = hschool))+
  geom_bar(colour = "black", fill = c("red","blue"))+
  labs(x = " ", y = "Frecuencia", 
       title = "Diploma de bachillerato", subtitle = "(Gráfico de frecuencias)")


self_bar = ggplot(datos, aes(x = selfemp))+
  geom_bar(colour = "black", fill = c("lightblue3","orange"))+
  labs(x = " ", y = "Frecuencia", 
       title = "Auto-Empleado", subtitle = "(Gráfico de frecuencias)")


single_bar = ggplot(datos, aes(x = single))+
  geom_bar(colour = "black", fill = c("cadetblue4","brown2"))+
  labs(x = " ", y = "Frecuencia", 
       title = "Soltero", subtitle = "(Gráfico de frecuencias)")

# Gráficos juntos
x11()
grid.arrange(deny_bar,high_bar,self_bar,single_bar)
 

#~~ Variables categóricas ~~#

# chist = Historial crediticio. 1 Muy bueno - 5 Muy deficiente
# mhist = Historia crediticio hipotecario. 1 Muy Bueno - 4 Deficiente


chist_bar = ggplot(datos, aes(x = chist )) +
  geom_bar(fill = "cadetblue3", color = "black") +
  labs(x = "", y = "Frecuencia",
       title = "Diagrama de barras de calificación crediticia general")

mhist_bar = ggplot(datos, aes(x = mhist )) +
  geom_bar(fill = "palegreen3", color = "black") +
  labs(x = "", y = "Frecuencia",
       title = "Diagrama de barras de calificación crediticia hipotecaria")

# Gráficos juntos
x11()
grid.arrange(chist_bar,mhist_bar)


#~~ Variables continuas ~~#

# lvrat = Monto del préstamo/Valor tasado de la propiedad
# pirat = Pagos mensuales de deuda/Ingreso mensual 

pi_dens = ggplot(datos, aes(x = pirat)) +
  geom_density(fill = "orange1", color = "darkorange2") +
  labs(x = "Pagos mensuales de deuda sobre ingreso mensual ", 
       y = " ", title = "Densidad de P/I")+
  coord_cartesian(xlim = c(0, 1.2))

lv_dens = ggplot(datos, aes(x = lvrat)) +
  geom_density(fill = "darkorchid2", color = "darkorchid4") +
  labs(x = "Monto de préstamos sobre valor tasado de la propiedad ", 
       y = " ", title = "Densidad de L/V")+
  coord_cartesian(xlim = c(0, 1.5))

# Gráficos juntos
x11()
grid.arrange(pi_dens, lv_dens, ncol = 2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 2. Estimación de Modelos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Convertimos en valores numéricos los factores 
datos_num = datos %>% mutate(across(where(is.factor), as.numeric)-1)
View(datos_num)


# Fijamos la base de datos en el entorno
attach(datos_num)


# Convertimos las variables factor en variables dicotómicas.


X = cbind(pirat,hirat,lvrat,chist,single,hschool,selfemp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.1. Modelo de probabilidad lineal ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

mpl = glm(deny ~ X, family = gaussian, data = datos_num)
summary(mpl)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.2. Modelo Probit ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

probit = glm(deny ~ X, family = binomial(link = "probit"))
summary(probit)


#~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.3. Modelo Logit ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

logit = glm(deny ~ X, family = binomial(link = "logit"))
summary(logit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 3. Comparación Modelos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Comparación de coeficientes ~~#

stargazer(mpl, probit, logit, type = "text") 
stargazer(mpl, probit, logit, type = "latex") # Salida LaTex

# Los coeficientes de logit y probit no son interpretables.
# Lo único en lo que podemos inferir es el signo y la significancia


#~~~ Gráfica univariada de probabilidad ~~~#

# Generamos un modelo que compare la probabilidad solamente con variable pirat

probit_deny = glm(deny ~ pirat, 
                 family = binomial(link = "probit"), 
                 data = datos_num)

logit_deny = glm(deny ~ pirat, 
                 family = binomial(link = "logit"), 
                 data = datos_num)

mpl_deny = lm(deny~pirat, data = datos_num)


plot(x = hirat, y = deny,
     main = "Probabilidad de rechazar una hipoteca",
     xlab = "P/I ratio",
     ylab = " ",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.85)

# Agregamos líneas horizontales
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Hipoteca denegada")
text(2.5, -0.1, cex= 0.8, "Hipoteca aprobada")

# Agregamos las líneas de estimación
x = seq(0, 3, 0.01)

y_mpl = predict(mpl_deny, list(pirat = x), type = "response")
y_probit = predict(probit_deny, list(pirat = x), type = "response")
y_logit = predict(logit_deny, list(pirat = x), type = "response")

lines(x, y_mpl, lwd = 1.25, col = "black")
lines(x, y_probit, lwd = 1.5, col = "springgreen2")
lines(x, y_logit, lwd = 1.5, col = "royalblue2", lty = 2)

# Generamos una leyenda para guiarnos
legend("topleft", horiz = TRUE,
       legend = c("M.P.L.","Probit", "Logit"),
       col = c("black","springgreen2","royalblue2"), 
       lty = c(1,1, 2))

# Esta es solo una ilustración de los modelos gráficamente, para que vean que 
# pueden haber pequeños cambios entre logit y probit.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 3.1. Desempeño del modelo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Pseudo R2 de McFadden ~~#

# Para evaluar el desempeño usamos el Pseudo R2 de McFadden. El valor de este 
# PseudoR2 es subjetivo a veces. Un PseudoR2 bajo no significa que el modelo no 
# sea "bueno" o "malo".

PseudoR2(logit, which = "McFadden")
PseudoR2(probit, which = "McFadden")

#~~ Criterios de información ~~#

AIC(logit,probit)
BIC(logit,probit)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 4. Interpretación de modelos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Para interpretar los modelos, buscamos conocer el efecto marginal de cada 
# regresora. Veíamos que para los Logit y Probit esto era un problema. En esta 
# sección lo podremos solucionar.

#~~ Modelo de probabilidad lineal ~~#

# Los coeficientes se interpretan directamente
coef(mpl)


# Para el caso de Logit y Probit usamos el paquete mfx. Con este, seremos 
# capaces de calcular los efectos marginales directamente. 

#~~ Modelo Logit ~~#

coef_marg_logit = logitmfx(deny ~ X, data = datos_num)
coef_marg_logit    # Efectos marginales

#~~ Modelo Probit ~~#

coef_marg_probit = probitmfx(deny ~ X, data = datos_num)
coef_marg_probit   # Efectos marginales




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 5. Ejemplo: Pariticipación laboral ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Datos 

data = read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta")
help(mroz)
attach(data)

# ¿Qué condiciones permiten que una mujer casada en Estados Unidos se encuentre
#  en el mercado laboral? (1975)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 5.1. Estimación de modelos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Modelo de Pobrabilidad Lineal ~~#

MPL = lm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,
       data = data); summary(MPL)

coeftest(MPL,vcov=hccm) # Estimación con errores Robustos

#~~ Modelo Logit ~~#

LOGIT = glm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,
            family = binomial(link = logit),data = data); summary(LOGIT)

coeftest(LOGIT,vcov=hccm)

#~~ Modelo Probit ~~#

PROBIT = glm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6,
             family=binomial(link=probit),data=data); summary(PROBIT)

coeftest(PROBIT,vcov=hccm)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 5.2. Predicción de los modelos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Creamos dos nuevas mujeres para la muestra, sus características estan
# contenidas en el DataFrame.

#~~ Predicción MPL ~~#

MPL.pred = data.frame(nwifeinc=c(100,0),educ=c(5,17),
                      exper=c(0,30),expersq=c(0,900),
                      age=c(20,52),kidslt6=c(2,0),kidsge6=c(0,0))

augment(MPL,newdata = MPL.pred, type.predict="response") # Predicción Caso nuevo

#~~ Predicción LOGIT ~~#

LOGIT.pred = data.frame(nwifeinc=c(100,0),educ=c(5,17),
                      exper=c(0,30),expersq=c(0,900),
                      age=c(20,52),kidslt6=c(2,0),kidsge6=c(0,0))

augment(LOGIT,newdata = LOGIT.pred, type.predict="response") 


#==> ODD RATIOS para Modelos Logit

# El cociente de odds ratio mide cuánto es más probable que se dé la alternativa 
# de éxito al agregar una unidad de un regresor (manteniendo los demás constantes) 
# frente al caso base de no agregar la unidad adicional al regresor.

z = augment(LOGIT,newdata = data, type.predict="response") # Creamos que incluye
                                                           # toda la data
odds=exp(z$.fitted);odds

c.odds=exp(coefficients(LOGIT));c.odds  

log.odds= coefficients(LOGIT);log.odds 
stargazer(c.odds, log.odds, type="text")



#~~ Predicción PROBIT ~~#

PROBIT.pred = data.frame(nwifeinc=c(100,0),educ=c(5,17),
                      exper=c(0,30),expersq=c(0,900),
                      age=c(20,52),kidslt6=c(2,0),kidsge6=c(0,0))

augment(PROBIT,newdata = PROBIT.pred, type.predict="response") 



# A forma de ejemplo, de esta forma podemos ver el DataFrame con todos los 
# contenidos. Usamos el modelo Probit pero se puede usar cualquiera. Solo 
# hace falta cambiar los argumentos.

# Sobre la muestra nueva

predict_muestra = augment(LOGIT,newdata = PROBIT.pred,
                          type.predict = "response")  # Predicción con PROBIT
View(predict_muestra)

# Sobre la muestra total 

predict_total = augment(PROBIT,newdata = data,  # Se ajusta la data   
                        type.predict = "response")
View(predict_total)
probabilidades = data.frame(predict_total$.fitted)
View(probabilidades)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 6. Algunas funciones de interés ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Para visualizar los datos

tidy_mpl = tidy(MPL); tidy_mpl 
augment_mpl = augment(MPL); View(augment_mpl)  
glance_mpl = glance(MPL); View(glance_mpl) 

# Otra librería distinta a mfx para estimar Efectos Marginales

library(margins)

margins(LOGIT, type = "response") 
margins(PROBIT, type = "response") 

# Usando la media como referencia

margins(LOGIT, type = "response",
        data.frame(nwifeinc = mean(nwifeinc), educ = mean(educ), 
                   age = mean(age), exper = mean(exper), 
                   kidslt6 = mean(kidslt6),kidsge6 = mean(kidsge6))) 

margins(PROBIT, type = "response", 
        data.frame(nwifeinc= mean(nwifeinc), educ = mean(educ), 
                  age = mean(age), exper = mean(exper), 
                  kidslt6 = mean(kidslt6), kidsge6 = mean(kidsge6))) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                    FIN DEL CÓDIGO                                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#