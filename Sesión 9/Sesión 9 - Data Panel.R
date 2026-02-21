#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           UNIVERSIDAD NACIONAL DE COLOMBIA
#                   Facultad de Ciencias Económicas | 2024 - 01
#                            Econometría II | Monitoría 
#
#                                     Sesión 9:
#                                    Data Panel 
#                                  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Limpieza de entorno

rm(list = ls())
dev.off()

#_____________________________________________________________________________________#

# Tabla de contenidos
#
# 1. Ejemplo Woolridge
#  1.1. Conversión a Data Panel
#  1.2. Estimación por Pooled y EA
#  1.3. Estimación por FD y EF  
#  1.4. Selección de Modelo 
#  1.5. Validación de supuestos
# 2. Ejemplo Jtrain
#  2.1. Conversión a Data Panel
#  2.2. Estimación por Pooled
#  2.3. Estimación por EA, EF y FD
#  2.4. Resultados de modelos
#  2.5. Ejercicio para ensayar
#_____________________________________________________________________________________#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Instalación de paquetes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

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
  car         # Para verificar normalidad con gráficos de distribución cuantil.
  
)

# Nota: Foreign es una biblioteca que es muy freuentemente usada para importar bases 
#       de datos de stata (.dta es el archivo de base de datos de stata)
#       No obstante, dicho paquete genera inconvenientes a la hora de importar cierto
#       bases de datos provenientes de ciertas versiones de stata
#       El paquete haven es mucho más general que el paquete foreign y no genera 
#       inconvenientes a la hora de importar bases de datos tipo .dta. Por tanto, 
#       al igual que en el presente script, se recomienda usar la función read_dta a la
#       hora de trabajar bases de datos de stata. 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 1. Ejemplo Woolridge ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# En en libro de Wooldridge capítulo 14.4, encontramos un ejercicio
# ¿Ha cambiado la educación a lo largo del tiempo?

# Base de datos:
Education=read_dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta")  

# Manera de visualizar toda la base de datos desde R
View(Education)

# Manera compacta de ver una base de datos
glimpse(Education)

#Gráficas interesantes para ver el comportamiento PROMEDIO de las variables
#En este caso, no diferenciamos por cada uno de los años, por lo que estamos 
#graficando todos los individuos durante todo el intervalo de tiempo.

#Primer tipo de gráfico: gráfico de dispersión y línea de tendencia
#Este tipo de gráfico es propicio para variables numéricas 

#Cruzamos el lwage y los años de educación
ggplot(Education, aes(x = educ, y = lwage)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(title = "Relación entre años de educación y log(salarios)", 
       x = "Años de educación", y = "Log(salarios)") +
  theme_bw()

#Cruzamos el lwage y los años de experiencia
ggplot(Education, aes(x = exper, y = lwage)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(title = "Relación entre experiencia y log(salarios)", 
       x = "Años de experiencia", y = "Log(salarios)") +
  theme_minimal()

#Segundo tipo de gráfico: gráfico de cajás 
#Este tipo de gráfico es propicio para variables dummies 

#Cruzamos el lwage y black
ggplot(Education, aes(x = factor(black), y = lwage)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribución de log(salarios) por raza (black)", 
       x = "Black", y = "Log(salarios)") +
  theme_minimal()

#Cruzamos el lwage e hisp
ggplot(Education, aes(x = factor(hisp), y = lwage)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribución de log(salarios) por etnicidad (hisp)", 
       x = "Hisp", y = "Log(salarios)") +
  theme_minimal()

#Cruzamos el lwage y married
ggplot(Education, aes(x = factor(married), y = lwage)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribución de log(salarios) por estado civil (married)", 
       x = "Married", y = "Log(salarios)") +
  theme_minimal()

#Otra forma de ver la distribución de la población
plotmeans(lwage ~ married, main="Distribución de la población", 
          data=panel.Education)

#Cruzamos el lwage y union
ggplot(Education, aes(x = factor(union), y = lwage)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribución de log(salarios) por pertenencia a sindicato (union)", 
       x = "Union", y = "Log(salarios)") +
  theme_minimal()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.1. Conversión a Data Panel ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Tratar la base de datos como un panel de datos
panel.Education = pdata.frame(Education, index = c("nr","year"))
help("pdata.frame")

# Estructura del codigo:
# Nombre <- pdata.frame(Base_datos, index=c("Var_indivuo","Var_tiempo"))
#-------------------------------------------------------------------------#

#Para conocer las dimensiones del panel
pdim(panel.Education)

#Para determinar si las variables cambian a lo largo del tiempo
pvar(panel.Education)

#Definición de las variables
exper2 = panel.Education$expersq
panel.Education$yr = factor(panel.Education$year)# Una Dummy para cada 
                                                 # año de la muestra

### ¡PASO PREVIO A LA ESTIMACIÓN DEL MODELO! ###

#Calculamos el promedio de las variables de interés por cada uno de los años
mean_year <- panel.Education %>%
  group_by(year) %>%
  summarise(across(c(educ, black, hisp, exper, married, union), 
                   \(x) mean(x, na.rm = TRUE)))

mean_year

#Cruzamos la educación y el lwage por cada uno de los años
panel_data2 <- as.data.frame(panel.Education)

#Graficamos
ggplot(panel_data2, aes(x = educ, y = lwage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(title = "Relación entre años de educación y log(salarios) diferenciado por año", 
       x = "Años de educación", y = "Log(salarios)") +
  theme_minimal() +
  facet_wrap(~ year)

#Podemos hacer lo mismo para exper (tareita), pero antes...
#Vamos a comprobar si existen rendimientos decrecientes de la exper sobre lwage 

#Primero, ajustamos la regresión cuadrática
modelo_mincer <- lm(lwage ~ exper + I(exper^2), data = panel_data2)
summary(modelo_mincer)

#Graficamos para verlo más claro
ggplot(panel_data2, aes(x = exper, y = lwage)) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), color = "darkred", se = FALSE) +
  labs(title = "Relación entre la experiencia y log(salarios) con curva cuadrática", 
       x = "Años de experiencia", y = "Log(salarios)") +
  theme_minimal()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.2. Estimación por Pooled y efectos aleatorios ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
class(Education)


panel.Education = pdata.frame(Education, 
                              index = c("nr","year"))


# Mínimos cuadrados combinados.

pooled = plm(lwage~educ+black+hisp+exper+exper2+married+union+yr,
             data=panel.Education, model="pooling")

summary(pooled)

# Modelo de efectos aleatorios. 
random = plm(lwage~educ+black+hisp+exper+exper2+married+union+yr, 
             data=panel.Education , model="random")
summary(random)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.3. Estimación por First Differences y Efectos fijos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Modelo de primeras diferencias.
FD = plm(lwage~exper2+married+union+yr,
         data=panel.Education, model="fd")
summary(FD)

# Modelo de efectos fijos.
fixed = plm(lwage~exper2+married+union+yr,
            data=panel.Education, model="within")
summary(fixed)

### Propuesta de 2 modelos adicionales más:

#Pooled con individual & time effects: dummy para cada periodo de tiempo y una
#dummy por cada individuo
Ind.Plus.Time = lm(lwage~educ+black+hisp+exper+exper2+married+union+yr+factor(nr),data=panel.Education)

#Dummy por cada individuo,
Binary = lm(lwage~educ+black+hisp+exper+exper2+married+union+factor(nr),data=panel.Education)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.4. Selección de Modelo #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Presentación de resultados.
stargazer(pooled,random,fixed,FD,Ind.Plus.Time,Binary, type="text",           
          column.labels=c("OLS","RE","FE", "PD","Ind+Time", "Binary"),keep.stat=c("n","rsq"), 
          omit = c("nr","yr1981", "yr1982", "yr1983", "yr1984", "yr1985", "yr1986", "yr1987"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Test de Hausman (EF vs EA) #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ¿Es mejor Efectos fijos o Efectos Aleatorios?

phtest(fixed, random) # Ho: Los modelos son equivalentes estadisticamente,
                      # por eficiencia elijo EA. 
                      # H1: EF

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Test de Primeras diferencias de Wooldridge (EF vs PD) #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ¿Efectos fijos o Primeras diferencias? Depende de la autocorrelación.

pwfdtest(lwage~exper2+married+union+yr,
         data=panel.Education,h0= "fe") # H0 = corr(Uit,Uit-1) = 0
pwfdtest(lwage~exper2+married+union+yr, 
         data=panel.Education,h0= "fd") # H0 = errores diferenciados
                                        # no correlacionados

# La prueba no es concluyente, tanto los errores diferenciados como 
# sin diferenciar tienen correlación serial. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Test de Multiplicadores de Lagrange de Breusch-Pagan (EA vs Pooled) #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

plmtest(pooled,type = "bp")  # Ho: Mejor Pooled porque var(ai) = 0
                             # H1: Se prefiere EA 

# Prueba Breush-Pagan (Ho: Homocedasticidaad) 
bptest(pooled)

# ¿El efecto es individual, temporal, o ambos?

plmtest(pooled,"time","bp")        # Ho:Efectos temporales no significativos 
plmtest(pooled,"individual","bp")  # Ho:Efectos indivuduales no significativos
plmtest(pooled,"twoways","bp")     # Ho:Efectos temporales e individuales 
                                   #    no significativos


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.5. Validación de supuestos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Prueba de heterocedasticidad: Usamos bptest()
bptest(pooled);bptest(random);bptest(fixed); bptest(FD)

# Test Breusch-Godfrey para autocorrelación de orden p
bgtest(pooled);bgtest(random);bgtest(fixed);bgtest(FD)
?bgtest

# Breusch-Pagan prueba para dependencia transversal en paneles.
pcdtest(pooled,test = "lm")
pcdtest(random,test = "lm")
pcdtest(fixed,test = "lm")
pcdtest(FD,test = "lm")      

# Correción de correlación serial para EF.
# MCOV=vcovHC.plm(Fixed, method=c("arellano")) 
MCOV1 = vcovHC(fixed, method="arellano")

# Coeftest(Fixed,MCOV)
coeftest(fixed,MCOV1)
help("vcovHC.plm")
         # Arellano : Soluciona heterosedasticidad y autocorrelacion serial 
         # White1 - Whit 2 : Soluciona heteroscedaticidad. 

# Análisis de Normalidad.
hist(residuals(fixed))

qqnorm(residuals(fixed))
qqline(residuals(fixed))

Residuales=residuals(fixed)
qqPlot(Residuales)
# Es deseable que se ajusten a la linea de tendencia para cumplir normalidad 

jarque.bera.test(residuals(fixed))  

#~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 2. Ejemplo Jtrain ####
#~~~~~~~~~~~~~~~~~~~~~~~~~#

# En este ejercicio se utiliza la base de datos JTRAIN.RAW para determinar el 
# efecto del subsidio a la capacitación laboral en las horas de capacitación por 
# empleado. 

# El modelo  básico para los tres años es: 
# hrsemp~ Bo + S1d88 + S2d89+ B1grant + B2grant_1 + B3lemploy + ai + uit

# Base de datos
jtrain = read_dta("http://fmwww.bc.edu/ec-p/data/wooldridge/jtrain.dta")  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.1. Conversión a Data Panel ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

panel.jtrain = pdata.frame(jtrain, index = c("fcode","year"))

#Analizamos las caracteristicas principales.

View(jtrain)
pdim(panel.jtrain) 
pvar(panel.jtrain)

# Creamos la dummy para cada uno de los años
years = factor(panel.jtrain$year)  



# Modelo MCO Pooled
Pool.jtrain = plm(hrsemp~grant + grant_1 + lemploy,
                  data=panel.jtrain, model="pooling", effect = "twoways")

#(MCO Combinados + Time effect + Individual effects)
Pool.E1 =plm(hrsemp~ grant + grant_1 + lemploy + factor(year)+factor(fcode),
            data=panel.jtrain, model="pooling")

Pool.E2= lm(hrsemp~ grant + grant_1 + lemploy + factor(year)+factor(fcode), 
             data = panel.jtrain)

stargazer(Pool.jtrain, Pool.E1, Pool.E2, 
          keep = c("grant", "gran_1", "lemploy","constant"),type = "text", 
          style = "AER")

# Al estimar MCOC incluyendo  efectos de tiempo e individuales se puede 
# realizar con el comando plm y lm y los resultados son equivalentes.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.3. Estimaciones por Efectos Aleatorios, Efectos fijos y PD ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

EF.jtrain = plm(hrsemp~years+ grant + grant_1 + lemploy,
                    data=panel.jtrain, model="within") 

PD.jtrain = plm(hrsemp~years+ grant + grant_1 + lemploy,
                data=panel.jtrain, model="fd")

EA.jtrain = plm(hrsemp~years+ grant + grant_1 + lemploy,
                data=panel.jtrain, model="random")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.4. Resultados de Modelos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Resultados
summary(EF.jtrain); summary(PD.jtrain); summary(EA.jtrain); summary(Pool.jtrain)

# Tabla en los resultados
stargazer(EF.jtrain,PD.jtrain,EA.jtrain, Pool.jtrain, type="text", 
          column.labels=c("Efectos Fijos", "Primeras Diferencias", 
                          "Efectos Aleatorios"),keep.stat=c("n","rsq"))

# Tabla de Latex
stargazer(EF.jtrain,PD.jtrain,EA.jtrain, Pool.jtrain, type="latex", 
          column.labels=c("Efectos Fijos", "Primeras Diferencias", 
          "Efectos Aleatorios"),keep.stat=c("n","rsq"))
 
# Un modelo de variables binarias se realiza con el comando lm y lo unico que hay 
# que incluir dentro de las variables explicativas, son dummies relacionadas a 
# los individuo. Recuerde que incluir o no la constante afecta el número de dummies
# que se incluiran en el modelo para evitar multicolinealidad. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2.5. Ejercicio para ensayar ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# QUEDA COMO EJERCICIO REALIZAR LAS PRUEBAS RESPECTIVAS PARA COMPARAR LOS MODELOS

# COMO LA RESPECTIVA VALIDACI?N DE SUPUESTOS Y SU RESPECTIVA CORRECCIÓN, CUANDO 
# SEA NECESARIO.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                    FIN DEL CÓDIGO                                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#