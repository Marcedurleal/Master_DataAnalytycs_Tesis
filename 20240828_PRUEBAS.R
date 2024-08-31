library(tseries)
library(forecast)
library(car)
library(urca)
library(TSstudio)
library(highcharter)
library(ggfortify)
library(rio)
library(ggplot2)
library(readxl)
library(lmtest)
library(gridExtra)
library(writexl)
library(dplyr)
library(openxlsx)
library(rio)

############## 1)DATA

data=rio::import("https://github.com/Marcedurleal/Master_DataAnalytycs_Tesis/raw/main/data_normalizada_x%20marca.xlsx")
data


marca <- data[data$Marca_Anonimizada == "Marca107", ]


##### 2)FORMATO SERIE DE TIEMPO ##########################
ts_marca <- ts(marca$Prevalece, frequency = 12)

########## 3 Gráficas ACF, PACF ##################################

#gráfica normal
plot(ts_marca)
#gráfica con movimiento
ts_plot(ts_marca, color = "red",slider=T)
hchart(ts_marca)

######### GRAFICAS SERIE NORMAL ##############################
par(mfrow=c(2,3))
plot(ts_marca)
forecast::Acf(ts_marca,lag.max=36)
forecast::Pacf(ts_marca,lag.max=36)

###############GRAFICAS SERIE DIFERENCIADA ##############################
plot(diff(ts_marca)) #diferencia
abline(h=2*sqrt(var(diff(ts_marca))),col="red")
abline(h=-2*sqrt(var(diff(ts_marca))),col="red")
acf(diff(ts_marca),lag.max = 36)
pacf(diff(ts_marca),lag.max=36)

######### 4) Pruebas de raíz unitaria ########### Estacionariedad ##########


#Prueba Dickey Fuller y Phillips-Perrron

############### PRUEBA DICKEY FULLER#####################################

#HO:= ts_marca es noe stacionaria
#Ha:= ts_marca es estacionaria
### nivel de significancia alpha=0.05

### SIN DIFERENCIA

Prueba_Dicker <- adf.test(ts_marca);Prueba_Dicker

####CON DIFERENCIA

Prueba_Dicker_diff <- adf.test(diff(ts_marca));Prueba_Dicker_diff


#P-VALUE=0.49218
#se rechaza Ho si valor p<0.05
#conclusion: ts_marca es no estacioanria

############### PRUEBA phillips perron #####################################
### SIN DIFERENCIA

Prueba_Phillips <- pp.test(ts_marca);Prueba_Phillips
#p-value=0.01
#conclusion ts_marca es estacionaria

### Pruebas de estacionariedad serie diferenciada
#p-value = 0.01
#conclusion ts_marca es estacionaria
### CON DIFERENCIA

Prueba_Phillips_diff <- pp.test(diff(ts_marca));Prueba_Phillips_diff
#p-value = 0.01
#conclusion ts_marca es estacionaria

#############################################################################

#5) Funcion Autocorrelación

ts_cor(diff(ts_marca),lag.max = 36)



##### 6) ESTIMACION MODELOS ARIMA ###############################################


### 6.1. OPCION ARIMA SEASONAL

modelo_1_1 <- arima(ts_marca, order = c(4,1,4),list(order = c(0,0,0),period=12));summary(modelo_1_1)

### 6.2. OPCION ARIMA FIXED

modelo_1_2=arima(ts_marca,order=c(4,1,4),
             fixed=c(NA,NA,NA,NA,NA,NA,NA,NA)); summary(modelo_1_2)


###### 7) BIC ###################################################################

BIC_11 <- BIC(modelo_1_1);BIC_11
BIC_12 <- BIC(modelo_1_2);BIC_12


########## 8) Significancia de los coeficientes PARA  FIXED #######################

sqrt(diag(modelo_1_2$var.coef))

tt=modelo_1_2$coef[which(modelo_1_2$coef!=0)]/sqrt(diag(modelo_1_2$var.coef))
1-pt(abs(tt),modelo_1_2$nobs-1)


##### SE IDENTIFICARON SIGNIFICATIVOS 5 COEFICIENTES, SE REFUJERON 5 DE LOS 10 (5AR Y 5 MA)

modelo_1_2=arima(ts_marca,order=c(4,1,4),
                 fixed=c(NA,NA,NA,NA,NA,NA,NA,NA)); summary(modelo_1_2)

BIC_12 <- BIC(modelo_1_2);BIC_12
#AIC=-1311
#BIC=-1292.556

#### 9) RESIDUALES ###############################################


et=residuals(modelo_1_1)
y.fit=fitted(modelo_1_1)

par(mfrow=c(3,2))
plot(y.fit, col="red")
lines(ts_marca)
plot(scale(et))
abline(h=2*sqrt(var(scale(et))),col="red")
abline(h=-2*sqrt(var(scale(et))),col="red")
acf(et,lag.max = 40)
pacf(et,lag.max=40)

qqPlot(et)
acf(abs(et))

###### 10) Test de Ljung-Box ######################################
#Ho: r1=r2=-....=rlag=0 (No hay correlación seria en los residuales)
#Ha: al menos uno es diferente de cero

#Objetivo no rechazar Ho
tsdiag(modelo_1_1)

Box.test(et,lag=9,type="Ljung-Box")

### Prueba de normalidad jarque Bera
#Ho: Los residuales se ajustan a una distribución normal
#Ha: los residuales no son normales

###### 11) Test de jarque bera ######################################


jarque.bera.test(et)

#p-value = 0.0004656
#Conclusion los residuales no son normales

####prueba de aleatoriedad de los residuales
##Ho: Los residuales son aleatorios
##Ha:Los residuales no son aleatorios

###### 12 Test RUN ################################################

### Objetivo No rechazar Ho (que los residuales no tengan patrones)

runs.test(as.factor(sign(et)),alternative = "two.sided")

#p-value = 0.0005469
#Los residuales no son aleatorios

#### R2

R2 <-  1 - sum(et^2) / sum((ts_marca - mean(ts_marca))^2)
R2
