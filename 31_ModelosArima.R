library(ggplot2)
library(forecast)
library(tseries)
library(readxl)
library(urca)
library(lmtest)
library(gridExtra)
library(writexl)
library(dplyr)
library(openxlsx)
library(rio)

# Leer datos
data=rio::import("https://github.com/Marcedurleal/Master_DataAnalytycs_Tesis/raw/main/data_normalizada_x%20marca.xlsx")
data

# Función para calcular el valor p de Phillips-Perron usando regresión auxiliar
calcular_pvalue_phillips <- function(ts_data) {
  pp_test <- pp.test(diff(ts_data))  # Usar la serie original para la prueba
  return(pp_test$p.value)  # Retorna el valor p de la prueba Phillips-Perron
}

# Función para calcular el p-valor de la prueba Dickey-Fuller
calcular_pvalue_dickey <- function(serie) {
  adf_test <- adf.test(diff(serie), alternative = "stationary")  # Usar la serie diferenciada
  return(adf_test$p.value)
}

# Evaluación de estacionariedad: Si cualquiera de las pruebas pasa (p < 0.05), se considera "Bueno"
evaluar_resultado_estacionariedad <- function(pp_p_value, df_p_value) {
  if ((!is.na(pp_p_value) && pp_p_value < 0.05) || (!is.na(df_p_value) && df_p_value < 0.05)) {
    return("Bueno")
  } else {
    return("Malo")
  }
}

# Evaluar métricas para cada modelo
evaluar_metricas <- function(metricas) {
  resultados <- list()
  resultados$MAPE <- ifelse(!is.na(metricas$MAPE) && metricas$MAPE < 20, "Bueno", "Malo")
  resultados$R2 <- ifelse(!is.na(metricas$R2) && metricas$R2 > 0.67, "Bueno", "Malo")
  resultados$Ljung_Box <- ifelse(!is.na(metricas$Ljung_Box_p) && metricas$Ljung_Box_p > 0.05, "Bueno", "Malo")
  resultados$Jarque_Bera <- ifelse(!is.na(metricas$Jarque_Bera_p) && metricas$Jarque_Bera_p > 0.05, "Bueno", "Malo")
  resultados$Runs <- ifelse(!is.na(metricas$Runs_p) && metricas$Runs_p > 0.05, "Bueno", "Malo")
  resultados$Phillips_Perron <- ifelse(!is.na(metricas$Phillips_Perron_p) && metricas$Phillips_Perron_p < 0.05, "Bueno", "Malo")
  resultados$Dickey_Fuller <- ifelse(!is.na(metricas$Dickey_Fuller_p) && metricas$Dickey_Fuller_p < 0.05, "Bueno", "Malo")
  return(resultados)
}

# Inicializar los DataFrames para acumular los resultados
resultados_acumulados_Arima111 <- data.frame()
predicciones_acumuladas_Arima111 <- data.frame()

# Inicializar listas de marcas buenas y malas
marcas_buenas_Arima111 <- c()
marcas_malas_Arima111 <- c()

# Lista de marcas a procesar
marcas <- unique(data$Marca_Anonimizada)

# Bucle para procesar cada marca
for (marca in marcas) {
  data_marca <- data[data$Marca_Anonimizada == marca, ]
  
  if (nrow(data_marca) == 0) next  # Salta a la siguiente marca si no hay datos
  
  ts_data <- ts(data_marca$Prevalece, frequency = 12)
  promedio_valor <- mean(ts_data, na.rm = TRUE)
  
  # Evaluar estacionariedad
  pp_p_value <- calcular_pvalue_phillips(ts_data)
  df_p_value <- calcular_pvalue_dickey(ts_data)
  evaluacion_estacionariedad <- evaluar_resultado_estacionariedad(pp_p_value, df_p_value)
  
  # Si la serie no es estacionaria, agregar a marcas malas y continuar
  if (evaluacion_estacionariedad == "Malo") {
    marcas_malas_Arima111 <- c(marcas_malas_Arima111, marca)
    next
  }
  
  # Ajustar modelo ARIMA manual
  modelo_arima_manual <- tryCatch({
    arima(ts_data, order = c(1,1,1),fixed=c(NA,NA))
  }, error = function(e) NULL)
  
  # Si el modelo no se ajusta, agregar a marcas malas y continuar
  if (is.null(modelo_arima_manual)) {
    marcas_malas_Arima111 <- c(marcas_malas_Arima111, marca)
    next
  }
  
  # Calcular residuos y métricas para el modelo manual
  residuals_manual <- residuals(modelo_arima_manual)
  metricas_manual <- list(
    MAPE = mean(abs(residuals_manual / ts_data) * 100, na.rm = TRUE),
    MASE = mean(abs(residuals_manual), na.rm = TRUE) / mean(abs(diff(ts_data)), na.rm = TRUE),
    R2 = 1 - sum(residuals_manual^2, na.rm = TRUE) / sum((ts_data - mean(ts_data))^2, na.rm = TRUE),
    AIC = AIC(modelo_arima_manual),
    BIC = BIC(modelo_arima_manual),
    Jarque_Bera_p = jarque.bera.test(residuals_manual)$p.value,
    Ljung_Box_p = Box.test(residuals_manual, lag = 9, type = "Ljung-Box")$p.value,
    Runs_p = runs.test(as.factor(sign(residuals_manual)), alternative = "two.sided")$p.value,
    Phillips_Perron_p = pp_p_value,
    Dickey_Fuller_p = df_p_value
  )
  
  # Evaluar métricas
  evaluacion_metricas <- evaluar_metricas(metricas_manual)
  
  # Condiciones para determinar si la marca es apta
  es_apto <- (
    evaluacion_metricas$Ljung_Box == "Bueno" &
      evaluacion_metricas$Dickey_Fuller == "Bueno" &
      evaluacion_metricas$R2 == "Bueno" &
      evaluacion_metricas$MAPE == "Bueno"&
      evaluacion_metricas$Runs == "Bueno"
  )
  
  # Clasificar la marca en buenas o malas
  if (es_apto) {
    marcas_buenas_Arima111 <- c(marcas_buenas_Arima111, marca)
  } else {
    marcas_malas_Arima111 <- c(marcas_malas_Arima111, marca)
  }
  
  # Compilar resultados para la marca actual
  resultados <- data.frame(
    Marca = marca,
    MAPE_manual = metricas_manual$MAPE,
    Evaluacion_MAPE_manual = evaluar_metricas(metricas_manual)$MAPE,
    R2_manual = metricas_manual$R2,
    Evaluacion_R2_manual = evaluar_metricas(metricas_manual)$R2,
    AIC_manual = metricas_manual$AIC,
    BIC_manual = metricas_manual$BIC,
    Jarque_Bera_p_manual = metricas_manual$Jarque_Bera_p,
    Evaluacion_Jarque_Bera_manual = evaluar_metricas(metricas_manual)$Jarque_Bera,
    Ljung_Box_p_manual = metricas_manual$Ljung_Box_p,
    Evaluacion_Ljung_Box_manual = evaluar_metricas(metricas_manual)$Ljung_Box,
    Runs_p_manual = metricas_manual$Runs_p,
    Evaluacion_Runs_manual = evaluar_metricas(metricas_manual)$Runs,
    Phillips_Perron_p_manual = metricas_manual$Phillips_Perron_p,
    Evaluacion_Phillips_Perron_manual = evaluar_metricas(metricas_manual)$Phillips_Perron,
    Dickey_Fuller_p_manual = metricas_manual$Dickey_Fuller_p,
    Evaluacion_Dickey_Fuller_manual = evaluar_metricas(metricas_manual)$Dickey_Fuller,
    Num_metricas_malas = sum(unlist(evaluacion_metricas) == "Malo"),
    Apto = ifelse(es_apto, "Apto", "No apto"),
    Evaluacion_Estacionariedad = evaluacion_estacionariedad
  )
  
  resultados_acumulados_Arima111 <- rbind(resultados_acumulados_Arima111, resultados)
  
  # Realizar predicciones para los próximos 12 períodos
  prediccion_Arima111 <- forecast(modelo_arima_manual, h = 12)
  
  # Ajustar nuevo_reconteo para que coincida con el número total de filas en la comparación
  periodos <- length(ts_data)
  forecast_horizon <- length(prediccion_Arima111$mean)
  nuevo_reconteo <- 1:(periodos + forecast_horizon)
  
  # Crear un DataFrame que combine los valores reales y los valores pronosticados
  comparacion <- data.frame(
    Marca = rep(marca, periodos + forecast_horizon),  # Incluir la marca en las predicciones
    Periodo = nuevo_reconteo,
    Prediccion = c(rep(NA, periodos), prediccion_Arima111$mean),
    Prevalece_Real = c(ts_data, rep(NA, forecast_horizon)),
    Lower_CI = c(rep(NA, periodos), prediccion_Arima111$lower[, 2]),  # Límite inferior del 95%
    Upper_CI = c(rep(NA, periodos), prediccion_Arima111$upper[, 2])   # Límite superior del 95%
  )
  
  # Generar gráfico de comparación
  p_comparacion <- ggplot(comparacion, aes(x = Periodo)) +
    geom_line(aes(y = Prevalece_Real), color = "#415D82", size = 1, linetype = "solid", na.rm = TRUE) +
    geom_line(aes(y = Prediccion), color = "#2F917E", size = 1, linetype = "dashed", na.rm = TRUE) +
    geom_point(aes(y = Prediccion), color = "#2F917E", size = 2, na.rm = TRUE) +
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "#2F917E", alpha = 0.2, na.rm = TRUE) +
    ggtitle(paste("Comparación de Valores Reales y Predicciones -", marca, "(Modelo Manual)")) +
    theme_minimal(base_size = 15, base_family = "Arial") +
    labs(x = "Período", y = "Ventas (Miles de Millones)") +
    scale_y_continuous(labels = scales::number_format(scale = 1e-9, suffix = "B")) +  # Escala a miles de millones con sufijo "B"
    theme(axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12))
  
  # Mostrar la gráfica
  print(p_comparacion)
  
  # Guardar la gráfica como archivo PNG
  ggsave(filename = paste0("C:/Users/marce/Downloads/grafica_Arima111", marca, ".png"), plot = p_comparacion, width = 10, height = 6)
  
  # Acumular las predicciones en un único DataFrame
  predicciones_acumuladas_Arima111 <- rbind(predicciones_acumuladas_Arima111, comparacion)
}

# Crear un workbook de Excel y guardar los resultados
wb <- createWorkbook()
addWorksheet(wb, "Metricas")
writeData(wb, "Metricas", resultados_acumulados_Arima111)
addWorksheet(wb, "Predicciones")
writeData(wb, "Predicciones", predicciones_acumuladas_Arima111)
saveWorkbook(wb, "C:/Users/marce/Downloads/ModeloArima111.xlsx", overwrite = TRUE)

print("Resultados y predicciones acumulados guardados en el archivo Excel.")

# Imprimir marcas buenas y malas
print(paste("Marcas Buenas:", paste(marcas_buenas_Arima111, collapse = ", ")))
print(paste("Marcas Malas:", paste(marcas_malas_Arima111, collapse = ", ")))

# Contar y mostrar la cantidad de métricas malas para cada marca
resultados_acumulados_Arima111$Num_metricas_malas <- rowSums(resultados_acumulados_Arima111[, grep("Evaluacion_", colnames(resultados_acumulados_Arima111))] == "Malo")
print("Resumen de métricas malas por marca:")
print(resultados_acumulados_Arima111[, c("Marca", "Num_metricas_malas", "Apto")])

marcas_buenas_Arima111
marcas_malas_Arima111

predicciones_acumuladas_Arima111

# Filtrar el dataframe predicciones buenas
prediccion_marcas_buenas_Arima111 <- predicciones_acumuladas_Arima111[predicciones_acumuladas_Arima111$Marca %in% marcas_buenas_Arima111, ]
prediccion_marcas_buenas_Arima111

# Filtrar el dataframe resultados metricas buenas

Resultados_marcas_buenas_Arima111 <- resultados_acumulados_Arima111[resultados_acumulados_Arima111$Marca %in% marcas_buenas_Arima111, ]
Resultados_marcas_buenas_Arima111

##############################################################################################################
######################## 2) ARIMA 414  #######################################################################
##############################################################################################################

# Inicializar los DataFrames para acumular los resultados
resultados_acumulados_Arima414 <- data.frame()
predicciones_acumuladas_Arima414 <- data.frame()

# Inicializar listas de marcas buenas y malas
marcas_buenas_Arima414 <- c()
marcas_malas_Arima414 <- c()

# Lista de marcas a procesar
marcas <- marcas_malas_Arima111


# Bucle para procesar cada marca
for (marca in marcas) {
  data_marca <- data[data$Marca_Anonimizada == marca, ]
  
  if (nrow(data_marca) == 0) next  # Salta a la siguiente marca si no hay datos
  
  ts_data <- ts(data_marca$Prevalece, frequency = 12)
  promedio_valor <- mean(ts_data, na.rm = TRUE)
  
  # Evaluar estacionariedad
  pp_p_value <- calcular_pvalue_phillips(ts_data)
  df_p_value <- calcular_pvalue_dickey(ts_data)
  evaluacion_estacionariedad <- evaluar_resultado_estacionariedad(pp_p_value, df_p_value)
  
  # Si la serie no es estacionaria, agregar a marcas malas y continuar
  if (evaluacion_estacionariedad == "Malo") {
    marcas_malas_Arima414 <- c(marcas_malas_Arima414, marca)
    next
  }
  
  # Ajustar modelo ARIMA manual
  modelo_arima_manual <- tryCatch({
    arima(ts_data, order = c(4,1,4),fixed=c(NA,NA,NA,NA,NA,NA,NA,NA))
  }, error = function(e) NULL)
  
  # Si el modelo no se ajusta, agregar a marcas malas y continuar
  if (is.null(modelo_arima_manual)) {
    marcas_malas_Arima414 <- c(marcas_malas_Arima414, marca)
    next
  }
  
  # Calcular residuos y métricas para el modelo manual
  residuals_manual <- residuals(modelo_arima_manual)
  metricas_manual <- list(
    MAPE = mean(abs(residuals_manual / ts_data) * 100, na.rm = TRUE),
    MASE = mean(abs(residuals_manual), na.rm = TRUE) / mean(abs(diff(ts_data)), na.rm = TRUE),
    R2 = 1 - sum(residuals_manual^2, na.rm = TRUE) / sum((ts_data - mean(ts_data))^2, na.rm = TRUE),
    AIC = AIC(modelo_arima_manual),
    BIC = BIC(modelo_arima_manual),
    Jarque_Bera_p = jarque.bera.test(residuals_manual)$p.value,
    Ljung_Box_p = Box.test(residuals_manual, lag = 9, type = "Ljung-Box")$p.value,
    Runs_p = runs.test(as.factor(sign(residuals_manual)), alternative = "two.sided")$p.value,
    Phillips_Perron_p = pp_p_value,
    Dickey_Fuller_p = df_p_value
  )
  
  # Evaluar métricas
  evaluacion_metricas <- evaluar_metricas(metricas_manual)
  
  # Condiciones para determinar si la marca es apta
  es_apto <- (
    evaluacion_metricas$Ljung_Box == "Bueno" &
      evaluacion_metricas$Dickey_Fuller == "Bueno" &
      evaluacion_metricas$R2 == "Bueno" &
      evaluacion_metricas$MAPE == "Bueno" &
      evaluacion_metricas$Runs=="Bueno"
  )
  
  # Clasificar la marca en buenas o malas
  if (es_apto) {
    marcas_buenas_Arima414 <- c(marcas_buenas_Arima414, marca)
  } else {
    marcas_malas_Arima414 <- c(marcas_malas_Arima414, marca)
  }
  
  # Compilar resultados para la marca actual
  resultados <- data.frame(
    Marca = marca,
    MAPE_manual = metricas_manual$MAPE,
    Evaluacion_MAPE_manual = evaluar_metricas(metricas_manual)$MAPE,
    R2_manual = metricas_manual$R2,
    Evaluacion_R2_manual = evaluar_metricas(metricas_manual)$R2,
    AIC_manual = metricas_manual$AIC,
    BIC_manual = metricas_manual$BIC,
    Jarque_Bera_p_manual = metricas_manual$Jarque_Bera_p,
    Evaluacion_Jarque_Bera_manual = evaluar_metricas(metricas_manual)$Jarque_Bera,
    Ljung_Box_p_manual = metricas_manual$Ljung_Box_p,
    Evaluacion_Ljung_Box_manual = evaluar_metricas(metricas_manual)$Ljung_Box,
    Runs_p_manual = metricas_manual$Runs_p,
    Evaluacion_Runs_manual = evaluar_metricas(metricas_manual)$Runs,
    Phillips_Perron_p_manual = metricas_manual$Phillips_Perron_p,
    Evaluacion_Phillips_Perron_manual = evaluar_metricas(metricas_manual)$Phillips_Perron,
    Dickey_Fuller_p_manual = metricas_manual$Dickey_Fuller_p,
    Evaluacion_Dickey_Fuller_manual = evaluar_metricas(metricas_manual)$Dickey_Fuller,
    Num_metricas_malas = sum(unlist(evaluacion_metricas) == "Malo"),
    Apto = ifelse(es_apto, "Apto", "No apto"),
    Evaluacion_Estacionariedad = evaluacion_estacionariedad
  )
  
  resultados_acumulados_Arima414 <- rbind(resultados_acumulados_Arima414, resultados)
  
  # Realizar predicciones para los próximos 12 períodos
  prediccion_Arima414 <- forecast(modelo_arima_manual, h = 12)
  
  # Ajustar nuevo_reconteo para que coincida con el número total de filas en la comparación
  periodos <- length(ts_data)
  forecast_horizon <- length(prediccion_Arima414$mean)
  nuevo_reconteo <- 1:(periodos + forecast_horizon)
  
  # Crear un DataFrame que combine los valores reales y los valores pronosticados
  comparacion <- data.frame(
    Marca = rep(marca, periodos + forecast_horizon),  # Incluir la marca en las predicciones
    Periodo = nuevo_reconteo,
    Prediccion = c(rep(NA, periodos), prediccion_Arima414$mean),
    Prevalece_Real = c(ts_data, rep(NA, forecast_horizon)),
    Lower_CI = c(rep(NA, periodos), prediccion_Arima414$lower[, 2]),  # Límite inferior del 95%
    Upper_CI = c(rep(NA, periodos), prediccion_Arima414$upper[, 2])   # Límite superior del 95%
  )
  
  # Generar gráfico de comparación
  p_comparacion <- ggplot(comparacion, aes(x = Periodo)) +
    geom_line(aes(y = Prevalece_Real), color = "#415D82", size = 1, linetype = "solid", na.rm = TRUE) +
    geom_line(aes(y = Prediccion), color = "#2F917E", size = 1, linetype = "dashed", na.rm = TRUE) +
    geom_point(aes(y = Prediccion), color = "#2F917E", size = 2, na.rm = TRUE) +
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "#2F917E", alpha = 0.2, na.rm = TRUE) +
    ggtitle(paste("Comparación de Valores Reales y Predicciones -", marca, "(Modelo Manual)")) +
    theme_minimal(base_size = 15, base_family = "Arial") +
    labs(x = "Período", y = "Ventas (Miles de Millones)") +
    scale_y_continuous(labels = scales::number_format(scale = 1e-9, suffix = "B")) +  # Escala a miles de millones con sufijo "B"
    theme(axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12))
  
  # Mostrar la gráfica
  print(p_comparacion)
  
  # Guardar la gráfica como archivo PNG
  ggsave(filename = paste0("C:/Users/marce/Downloads/grafica_Arima414", marca, ".png"), plot = p_comparacion, width = 10, height = 6)
  
  # Acumular las predicciones en un único DataFrame
  predicciones_acumuladas_Arima414 <- rbind(predicciones_acumuladas_Arima414, comparacion)
}

# Crear un workbook de Excel y guardar los resultados
wb <- createWorkbook()
addWorksheet(wb, "Metricas")
writeData(wb, "Metricas", resultados_acumulados_Arima414)
addWorksheet(wb, "Predicciones")
writeData(wb, "Predicciones", predicciones_acumuladas_Arima414)
saveWorkbook(wb, "C:/Users/marce/Downloads/ModeloArima414.xlsx", overwrite = TRUE)

print("Resultados y predicciones acumulados guardados en el archivo Excel.")

# Imprimir marcas buenas y malas
print(paste("Marcas Buenas:", paste(marcas_buenas_Arima414, collapse = ", ")))
print(paste("Marcas Malas:", paste(marcas_malas_Arima414, collapse = ", ")))

# Contar y mostrar la cantidad de métricas malas para cada marca
resultados_acumulados_Arima414$Num_metricas_malas <- rowSums(resultados_acumulados_Arima414[, grep("Evaluacion_", colnames(resultados_acumulados_Arima414))] == "Malo")
print("Resumen de métricas malas por marca:")
print(resultados_acumulados_Arima414[, c("Marca", "Num_metricas_malas", "Apto")])

marcas_buenas_Arima414
marcas_malas_Arima414

predicciones_acumuladas_Arima414

# Filtrar el dataframe predicciones buenas
prediccion_marcas_buenas_Arima414 <- predicciones_acumuladas_Arima414[predicciones_acumuladas_Arima414$Marca %in% marcas_buenas_Arima414, ]
prediccion_marcas_buenas_Arima414

# Filtrar el dataframe resultados metricas buenas

Resultados_marcas_buenas_Arima414 <- resultados_acumulados_Arima414[resultados_acumulados_Arima414$Marca %in% marcas_buenas_Arima414, ]
Resultados_marcas_buenas_Arima414


##############################################################################################################
######################## 3) SARIMA 111_110  #######################################################################
##############################################################################################################

# Inicializar los DataFrames para acumular los resultados
resultados_acumulados_Sarima111_110 <- data.frame()
predicciones_acumuladas_Sarima111_110 <- data.frame()

# Inicializar listas de marcas buenas y malas
marcas_buenas_Sarima111_110 <- c()
marcas_malas_Sarima111_110 <- c()

# Lista de marcas a procesar
marcas <- marcas_malas_Arima414


# Bucle para procesar cada marca
for (marca in marcas) {
  data_marca <- data[data$Marca_Anonimizada == marca, ]
  
  if (nrow(data_marca) == 0) next  # Salta a la siguiente marca si no hay datos
  
  ts_data <- ts(data_marca$Prevalece, frequency = 12)
  promedio_valor <- mean(ts_data, na.rm = TRUE)
  
  # Evaluar estacionariedad
  pp_p_value <- calcular_pvalue_phillips(ts_data)
  df_p_value <- calcular_pvalue_dickey(ts_data)
  evaluacion_estacionariedad <- evaluar_resultado_estacionariedad(pp_p_value, df_p_value)
  
  # Si la serie no es estacionaria, agregar a marcas malas y continuar
  if (evaluacion_estacionariedad == "Malo") {
    marcas_malas_Sarima111_110 <- c(marcas_malas_Sarima111_110, marca)
    next
  }
  
  # Ajustar modelo ARIMA manual
  modelo_arima_manual <- tryCatch({
    arima(ts_data, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12))
  }, error = function(e) NULL)
  
  # Si el modelo no se ajusta, agregar a marcas malas y continuar
  if (is.null(modelo_arima_manual)) {
    marcas_malas_Sarima111_110 <- c(marcas_malas_Sarima111_110, marca)
    next
  }
  
  # Calcular residuos y métricas para el modelo manual
  residuals_manual <- residuals(modelo_arima_manual)
  metricas_manual <- list(
    MAPE = mean(abs(residuals_manual / ts_data) * 100, na.rm = TRUE),
    MASE = mean(abs(residuals_manual), na.rm = TRUE) / mean(abs(diff(ts_data)), na.rm = TRUE),
    R2 = 1 - sum(residuals_manual^2, na.rm = TRUE) / sum((ts_data - mean(ts_data))^2, na.rm = TRUE),
    AIC = AIC(modelo_arima_manual),
    BIC = BIC(modelo_arima_manual),
    Jarque_Bera_p = jarque.bera.test(residuals_manual)$p.value,
    Ljung_Box_p = Box.test(residuals_manual, lag = 9, type = "Ljung-Box")$p.value,
    Runs_p = runs.test(as.factor(sign(residuals_manual)), alternative = "two.sided")$p.value,
    Phillips_Perron_p = pp_p_value,
    Dickey_Fuller_p = df_p_value
  )
  
  # Evaluar métricas
  evaluacion_metricas <- evaluar_metricas(metricas_manual)
  
  # Condiciones para determinar si la marca es apta
  es_apto <- (
    evaluacion_metricas$Ljung_Box == "Bueno" &
      evaluacion_metricas$Dickey_Fuller == "Bueno" &
      evaluacion_metricas$R2 == "Bueno" &
      evaluacion_metricas$MAPE == "Bueno" &
      evaluacion_metricas$Runs=="Bueno"
  )
  
  # Clasificar la marca en buenas o malas
  if (es_apto) {
    marcas_buenas_Sarima111_110 <- c(marcas_buenas_Sarima111_110, marca)
  } else {
    marcas_malas_Sarima111_110 <- c(marcas_malas_Sarima111_110, marca)
  }
  
  # Compilar resultados para la marca actual
  resultados <- data.frame(
    Marca = marca,
    MAPE_manual = metricas_manual$MAPE,
    Evaluacion_MAPE_manual = evaluar_metricas(metricas_manual)$MAPE,
    R2_manual = metricas_manual$R2,
    Evaluacion_R2_manual = evaluar_metricas(metricas_manual)$R2,
    AIC_manual = metricas_manual$AIC,
    BIC_manual = metricas_manual$BIC,
    Jarque_Bera_p_manual = metricas_manual$Jarque_Bera_p,
    Evaluacion_Jarque_Bera_manual = evaluar_metricas(metricas_manual)$Jarque_Bera,
    Ljung_Box_p_manual = metricas_manual$Ljung_Box_p,
    Evaluacion_Ljung_Box_manual = evaluar_metricas(metricas_manual)$Ljung_Box,
    Runs_p_manual = metricas_manual$Runs_p,
    Evaluacion_Runs_manual = evaluar_metricas(metricas_manual)$Runs,
    Phillips_Perron_p_manual = metricas_manual$Phillips_Perron_p,
    Evaluacion_Phillips_Perron_manual = evaluar_metricas(metricas_manual)$Phillips_Perron,
    Dickey_Fuller_p_manual = metricas_manual$Dickey_Fuller_p,
    Evaluacion_Dickey_Fuller_manual = evaluar_metricas(metricas_manual)$Dickey_Fuller,
    Num_metricas_malas = sum(unlist(evaluacion_metricas) == "Malo"),
    Apto = ifelse(es_apto, "Apto", "No apto"),
    Evaluacion_Estacionariedad = evaluacion_estacionariedad
  )
  
  resultados_acumulados_Sarima111_110 <- rbind(resultados_acumulados_Sarima111_110, resultados)
  
  # Realizar predicciones para los próximos 12 períodos
  prediccion_Sarima111_110 <- forecast(modelo_arima_manual, h = 12)
  
  # Ajustar nuevo_reconteo para que coincida con el número total de filas en la comparación
  periodos <- length(ts_data)
  forecast_horizon <- length(prediccion_Sarima111_110$mean)
  nuevo_reconteo <- 1:(periodos + forecast_horizon)
  
  # Crear un DataFrame que combine los valores reales y los valores pronosticados
  comparacion <- data.frame(
    Marca = rep(marca, periodos + forecast_horizon),  # Incluir la marca en las predicciones
    Periodo = nuevo_reconteo,
    Prediccion = c(rep(NA, periodos), prediccion_Sarima111_110$mean),
    Prevalece_Real = c(ts_data, rep(NA, forecast_horizon)),
    Lower_CI = c(rep(NA, periodos), prediccion_Sarima111_110$lower[, 2]),  # Límite inferior del 95%
    Upper_CI = c(rep(NA, periodos), prediccion_Sarima111_110$upper[, 2])   # Límite superior del 95%
  )
  
  # Generar gráfico de comparación
  p_comparacion <- ggplot(comparacion, aes(x = Periodo)) +
    geom_line(aes(y = Prevalece_Real), color = "#415D82", size = 1, linetype = "solid", na.rm = TRUE) +
    geom_line(aes(y = Prediccion), color = "#2F917E", size = 1, linetype = "dashed", na.rm = TRUE) +
    geom_point(aes(y = Prediccion), color = "#2F917E", size = 2, na.rm = TRUE) +
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "#2F917E", alpha = 0.2, na.rm = TRUE) +
    ggtitle(paste("Comparación de Valores Reales y Predicciones -", marca, "(Modelo Manual)")) +
    theme_minimal(base_size = 15, base_family = "Arial") +
    labs(x = "Período", y = "Ventas (Miles de Millones)") +
    scale_y_continuous(labels = scales::number_format(scale = 1e-9, suffix = "B")) +  # Escala a miles de millones con sufijo "B"
    theme(axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12))
  
  # Mostrar la gráfica
  print(p_comparacion)
  
  # Guardar la gráfica como archivo PNG
  ggsave(filename = paste0("C:/Users/marce/Downloads/grafica_Sarima111_110", marca, ".png"), plot = p_comparacion, width = 10, height = 6)
  
  # Acumular las predicciones en un único DataFrame
  predicciones_acumuladas_Sarima111_110 <- rbind(predicciones_acumuladas_Sarima111_110, comparacion)
}

# Crear un workbook de Excel y guardar los resultados
wb <- createWorkbook()
addWorksheet(wb, "Metricas")
writeData(wb, "Metricas", resultados_acumulados_Sarima111_110)
addWorksheet(wb, "Predicciones")
writeData(wb, "Predicciones", predicciones_acumuladas_Sarima111_110)
saveWorkbook(wb, "C:/Users/marce/Downloads/ModeloSarima111_110.xlsx", overwrite = TRUE)

print("Resultados y predicciones acumulados guardados en el archivo Excel.")

# Imprimir marcas buenas y malas
print(paste("Marcas Buenas:", paste(marcas_buenas_Sarima111_110, collapse = ", ")))
print(paste("Marcas Malas:", paste(marcas_malas_Sarima111_110, collapse = ", ")))

# Contar y mostrar la cantidad de métricas malas para cada marca
resultados_acumulados_Sarima111_110$Num_metricas_malas <- rowSums(resultados_acumulados_Sarima111_110[, grep("Evaluacion_", colnames(resultados_acumulados_Sarima111_110))] == "Malo")
print("Resumen de métricas malas por marca:")
print(resultados_acumulados_Sarima111_110[, c("Marca", "Num_metricas_malas", "Apto")])

marcas_buenas_Sarima111_110
marcas_malas_Sarima111_110

##############################################################################################################
######################## 4) SARIMA 111_111  #######################################################################
##############################################################################################################


# Inicializar los DataFrames para acumular los resultados
resultados_acumulados_Sarima111_111 <- data.frame()
predicciones_acumuladas_Sarima111_111 <- data.frame()

# Inicializar listas de marcas buenas y malas
marcas_buenas_Sarima111_111 <- c()
marcas_malas_Sarima111_111 <- c()

# Lista de marcas a procesar
marcas <- marcas_malas_Sarima111_110

marcas_malas_Sarima111_110

# Bucle para procesar cada marca
for (marca in marcas) {
  data_marca <- data[data$Marca_Anonimizada == marca, ]
  
  if (nrow(data_marca) == 0) next  # Salta a la siguiente marca si no hay datos
  
  ts_data <- ts(data_marca$Prevalece, frequency = 12)
  promedio_valor <- mean(ts_data, na.rm = TRUE)
  
  # Evaluar estacionariedad
  pp_p_value <- calcular_pvalue_phillips(ts_data)
  df_p_value <- calcular_pvalue_dickey(ts_data)
  evaluacion_estacionariedad <- evaluar_resultado_estacionariedad(pp_p_value, df_p_value)
  
  # Si la serie no es estacionaria, agregar a marcas malas y continuar
  if (evaluacion_estacionariedad == "Malo") {
    marcas_malas_Sarima111_111 <- c(marcas_malas_Sarima111_111, marca)
    next
  }
  
  # Ajustar modelo ARIMA manual
  modelo_arima_manual <- tryCatch({
    arima(ts_data, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))
  }, error = function(e) NULL)
  
  # Si el modelo no se ajusta, agregar a marcas malas y continuar
  if (is.null(modelo_arima_manual)) {
    marcas_malas_Sarima111_111 <- c(marcas_malas_Sarima111_111, marca)
    next
  }
  
  # Calcular residuos y métricas para el modelo manual
  residuals_manual <- residuals(modelo_arima_manual)
  metricas_manual <- list(
    MAPE = mean(abs(residuals_manual / ts_data) * 100, na.rm = TRUE),
    MASE = mean(abs(residuals_manual), na.rm = TRUE) / mean(abs(diff(ts_data)), na.rm = TRUE),
    R2 = 1 - sum(residuals_manual^2, na.rm = TRUE) / sum((ts_data - mean(ts_data))^2, na.rm = TRUE),
    AIC = AIC(modelo_arima_manual),
    BIC = BIC(modelo_arima_manual),
    Jarque_Bera_p = jarque.bera.test(residuals_manual)$p.value,
    Ljung_Box_p = Box.test(residuals_manual, lag = 9, type = "Ljung-Box")$p.value,
    Runs_p = runs.test(as.factor(sign(residuals_manual)), alternative = "two.sided")$p.value,
    Phillips_Perron_p = pp_p_value,
    Dickey_Fuller_p = df_p_value
  )
  
  # Evaluar métricas
  evaluacion_metricas <- evaluar_metricas(metricas_manual)
  
  # Condiciones para determinar si la marca es apta
  es_apto <- (
    evaluacion_metricas$Ljung_Box == "Bueno" &
      evaluacion_metricas$Dickey_Fuller == "Bueno" &
      evaluacion_metricas$R2 == "Bueno" &
      evaluacion_metricas$MAPE == "Bueno" &
      evaluacion_metricas$Runs=="Bueno"
  )
  
  # Clasificar la marca en buenas o malas
  if (es_apto) {
    marcas_buenas_Sarima111_111 <- c(marcas_buenas_Sarima111_111, marca)
  } else {
    marcas_malas_Sarima111_111 <- c(marcas_malas_Sarima111_111, marca)
  }
  
  # Compilar resultados para la marca actual
  resultados <- data.frame(
    Marca = marca,
    MAPE_manual = metricas_manual$MAPE,
    Evaluacion_MAPE_manual = evaluar_metricas(metricas_manual)$MAPE,
    R2_manual = metricas_manual$R2,
    Evaluacion_R2_manual = evaluar_metricas(metricas_manual)$R2,
    AIC_manual = metricas_manual$AIC,
    BIC_manual = metricas_manual$BIC,
    Jarque_Bera_p_manual = metricas_manual$Jarque_Bera_p,
    Evaluacion_Jarque_Bera_manual = evaluar_metricas(metricas_manual)$Jarque_Bera,
    Ljung_Box_p_manual = metricas_manual$Ljung_Box_p,
    Evaluacion_Ljung_Box_manual = evaluar_metricas(metricas_manual)$Ljung_Box,
    Runs_p_manual = metricas_manual$Runs_p,
    Evaluacion_Runs_manual = evaluar_metricas(metricas_manual)$Runs,
    Phillips_Perron_p_manual = metricas_manual$Phillips_Perron_p,
    Evaluacion_Phillips_Perron_manual = evaluar_metricas(metricas_manual)$Phillips_Perron,
    Dickey_Fuller_p_manual = metricas_manual$Dickey_Fuller_p,
    Evaluacion_Dickey_Fuller_manual = evaluar_metricas(metricas_manual)$Dickey_Fuller,
    Num_metricas_malas = sum(unlist(evaluacion_metricas) == "Malo"),
    Apto = ifelse(es_apto, "Apto", "No apto"),
    Evaluacion_Estacionariedad = evaluacion_estacionariedad
  )
  
  resultados_acumulados_Sarima111_111 <- rbind(resultados_acumulados_Sarima111_111, resultados)
  
  # Realizar predicciones para los próximos 12 períodos
  prediccion_Sarima111_111 <- forecast(modelo_arima_manual, h = 12)
  
  # Ajustar nuevo_reconteo para que coincida con el número total de filas en la comparación
  periodos <- length(ts_data)
  forecast_horizon <- length(prediccion_Sarima111_111$mean)
  nuevo_reconteo <- 1:(periodos + forecast_horizon)
  
  # Crear un DataFrame que combine los valores reales y los valores pronosticados
  comparacion <- data.frame(
    Marca = rep(marca, periodos + forecast_horizon),  # Incluir la marca en las predicciones
    Periodo = nuevo_reconteo,
    Prediccion = c(rep(NA, periodos), prediccion_Sarima111_111$mean),
    Prevalece_Real = c(ts_data, rep(NA, forecast_horizon)),
    Lower_CI = c(rep(NA, periodos), prediccion_Sarima111_111$lower[, 2]),  # Límite inferior del 95%
    Upper_CI = c(rep(NA, periodos), prediccion_Sarima111_111$upper[, 2])   # Límite superior del 95%
  )
  
  # Generar gráfico de comparación
  p_comparacion <- ggplot(comparacion, aes(x = Periodo)) +
    geom_line(aes(y = Prevalece_Real), color = "#415D82", size = 1, linetype = "solid", na.rm = TRUE) +
    geom_line(aes(y = Prediccion), color = "#2F917E", size = 1, linetype = "dashed", na.rm = TRUE) +
    geom_point(aes(y = Prediccion), color = "#2F917E", size = 2, na.rm = TRUE) +
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "#2F917E", alpha = 0.2, na.rm = TRUE) +
    ggtitle(paste("Comparación de Valores Reales y Predicciones -", marca, "(Modelo Manual)")) +
    theme_minimal(base_size = 15, base_family = "Arial") +
    labs(x = "Período", y = "Ventas (Miles de Millones)") +
    scale_y_continuous(labels = scales::number_format(scale = 1e-9, suffix = "B")) +  # Escala a miles de millones con sufijo "B"
    theme(axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12))
  
  # Mostrar la gráfica
  print(p_comparacion)
  
  # Guardar la gráfica como archivo PNG
  ggsave(filename = paste0("C:/Users/marce/Downloads/grafica_Sarima111_111", marca, ".png"), plot = p_comparacion, width = 10, height = 6)
  
  # Acumular las predicciones en un único DataFrame
  predicciones_acumuladas_Sarima111_111 <- rbind(predicciones_acumuladas_Sarima111_111, comparacion)
}

# Crear un workbook de Excel y guardar los resultados
wb <- createWorkbook()
addWorksheet(wb, "Metricas")
writeData(wb, "Metricas", resultados_acumulados_Sarima111_111)
addWorksheet(wb, "Predicciones")
writeData(wb, "Predicciones", predicciones_acumuladas_Sarima111_111)
saveWorkbook(wb, "C:/Users/marce/Downloads/ModeloSarima111_111.xlsx", overwrite = TRUE)

print("Resultados y predicciones acumulados guardados en el archivo Excel.")

# Imprimir marcas buenas y malas
print(paste("Marcas Buenas:", paste(marcas_buenas_Sarima111_111, collapse = ", ")))
print(paste("Marcas Malas:", paste(marcas_malas_Sarima111_111, collapse = ", ")))

# Contar y mostrar la cantidad de métricas malas para cada marca
resultados_acumulados_Sarima111_111$Num_metricas_malas <- rowSums(resultados_acumulados_Sarima111_111[, grep("Evaluacion_", colnames(resultados_acumulados_Sarima111_111))] == "Malo")
print("Resumen de métricas malas por marca:")
print(resultados_acumulados_Sarima111_111[, c("Marca", "Num_metricas_malas", "Apto")])

marcas_buenas_Sarima111_111
marcas_malas_Sarima111_111


##############################################################################################################
######################## 5) SARIMA 010_110  #######################################################################
##############################################################################################################


# Inicializar los DataFrames para acumular los resultados
resultados_acumulados_Sarima010_110 <- data.frame()
predicciones_acumuladas_Sarima010_110 <- data.frame()

# Inicializar listas de marcas buenas y malas
marcas_buenas_Sarima010_110 <- c()
marcas_malas_Sarima010_110 <- c()

# Lista de marcas a procesar
marcas <- marcas_malas_Sarima111_111


# Bucle para procesar cada marca
for (marca in marcas) {
  data_marca <- data[data$Marca_Anonimizada == marca, ]
  
  if (nrow(data_marca) == 0) next  # Salta a la siguiente marca si no hay datos
  
  ts_data <- ts(data_marca$Prevalece, frequency = 12)
  promedio_valor <- mean(ts_data, na.rm = TRUE)
  
  # Evaluar estacionariedad
  pp_p_value <- calcular_pvalue_phillips(ts_data)
  df_p_value <- calcular_pvalue_dickey(ts_data)
  evaluacion_estacionariedad <- evaluar_resultado_estacionariedad(pp_p_value, df_p_value)
  
  # Si la serie no es estacionaria, agregar a marcas malas y continuar
  if (evaluacion_estacionariedad == "Malo") {
    marcas_malas_Sarima010_110 <- c(marcas_malas_Sarima010_110, marca)
    next
  }
  
  # Ajustar modelo ARIMA manual
  modelo_arima_manual <- tryCatch({
    arima(ts_data, order = c(0, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
  }, error = function(e) NULL)
  
  # Si el modelo no se ajusta, agregar a marcas malas y continuar
  if (is.null(modelo_arima_manual)) {
    marcas_malas_Sarima010_110 <- c(marcas_malas_Sarima010_110, marca)
    next
  }
  
  # Calcular residuos y métricas para el modelo manual
  residuals_manual <- residuals(modelo_arima_manual)
  metricas_manual <- list(
    MAPE = mean(abs(residuals_manual / ts_data) * 100, na.rm = TRUE),
    MASE = mean(abs(residuals_manual), na.rm = TRUE) / mean(abs(diff(ts_data)), na.rm = TRUE),
    R2 = 1 - sum(residuals_manual^2, na.rm = TRUE) / sum((ts_data - mean(ts_data))^2, na.rm = TRUE),
    AIC = AIC(modelo_arima_manual),
    BIC = BIC(modelo_arima_manual),
    Jarque_Bera_p = jarque.bera.test(residuals_manual)$p.value,
    Ljung_Box_p = Box.test(residuals_manual, lag = 9, type = "Ljung-Box")$p.value,
    Runs_p = runs.test(as.factor(sign(residuals_manual)), alternative = "two.sided")$p.value,
    Phillips_Perron_p = pp_p_value,
    Dickey_Fuller_p = df_p_value
  )
  
  # Evaluar métricas
  evaluacion_metricas <- evaluar_metricas(metricas_manual)
  
  # Condiciones para determinar si la marca es apta
  es_apto <- (
    evaluacion_metricas$Ljung_Box == "Bueno" &
      evaluacion_metricas$Dickey_Fuller == "Bueno" &
      evaluacion_metricas$R2 == "Bueno" &
      evaluacion_metricas$MAPE == "Bueno" &
      evaluacion_metricas$Runs=="Bueno"
  )
  
  # Clasificar la marca en buenas o malas
  if (es_apto) {
    marcas_buenas_Sarima010_110 <- c(marcas_buenas_Sarima010_110, marca)
  } else {
    marcas_malas_Sarima010_110 <- c(marcas_malas_Sarima010_110, marca)
  }
  
  # Compilar resultados para la marca actual
  resultados <- data.frame(
    Marca = marca,
    MAPE_manual = metricas_manual$MAPE,
    Evaluacion_MAPE_manual = evaluar_metricas(metricas_manual)$MAPE,
    R2_manual = metricas_manual$R2,
    Evaluacion_R2_manual = evaluar_metricas(metricas_manual)$R2,
    AIC_manual = metricas_manual$AIC,
    BIC_manual = metricas_manual$BIC,
    Jarque_Bera_p_manual = metricas_manual$Jarque_Bera_p,
    Evaluacion_Jarque_Bera_manual = evaluar_metricas(metricas_manual)$Jarque_Bera,
    Ljung_Box_p_manual = metricas_manual$Ljung_Box_p,
    Evaluacion_Ljung_Box_manual = evaluar_metricas(metricas_manual)$Ljung_Box,
    Runs_p_manual = metricas_manual$Runs_p,
    Evaluacion_Runs_manual = evaluar_metricas(metricas_manual)$Runs,
    Phillips_Perron_p_manual = metricas_manual$Phillips_Perron_p,
    Evaluacion_Phillips_Perron_manual = evaluar_metricas(metricas_manual)$Phillips_Perron,
    Dickey_Fuller_p_manual = metricas_manual$Dickey_Fuller_p,
    Evaluacion_Dickey_Fuller_manual = evaluar_metricas(metricas_manual)$Dickey_Fuller,
    Num_metricas_malas = sum(unlist(evaluacion_metricas) == "Malo"),
    Apto = ifelse(es_apto, "Apto", "No apto"),
    Evaluacion_Estacionariedad = evaluacion_estacionariedad
  )
  
  resultados_acumulados_Sarima010_110 <- rbind(resultados_acumulados_Sarima010_110, resultados)
  
  # Realizar predicciones para los próximos 12 períodos
  prediccion_Sarima010_110 <- forecast(modelo_arima_manual, h = 12)
  
  # Ajustar nuevo_reconteo para que coincida con el número total de filas en la comparación
  periodos <- length(ts_data)
  forecast_horizon <- length(prediccion_Sarima010_110$mean)
  nuevo_reconteo <- 1:(periodos + forecast_horizon)
  
  # Crear un DataFrame que combine los valores reales y los valores pronosticados
  comparacion <- data.frame(
    Marca = rep(marca, periodos + forecast_horizon),  # Incluir la marca en las predicciones
    Periodo = nuevo_reconteo,
    Prediccion = c(rep(NA, periodos), prediccion_Sarima010_110$mean),
    Prevalece_Real = c(ts_data, rep(NA, forecast_horizon)),
    Lower_CI = c(rep(NA, periodos), prediccion_Sarima010_110$lower[, 2]),  # Límite inferior del 95%
    Upper_CI = c(rep(NA, periodos), prediccion_Sarima010_110$upper[, 2])   # Límite superior del 95%
  )
  
  # Generar gráfico de comparación
  p_comparacion <- ggplot(comparacion, aes(x = Periodo)) +
    geom_line(aes(y = Prevalece_Real), color = "#415D82", size = 1, linetype = "solid", na.rm = TRUE) +
    geom_line(aes(y = Prediccion), color = "#2F917E", size = 1, linetype = "dashed", na.rm = TRUE) +
    geom_point(aes(y = Prediccion), color = "#2F917E", size = 2, na.rm = TRUE) +
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "#2F917E", alpha = 0.2, na.rm = TRUE) +
    ggtitle(paste("Comparación de Valores Reales y Predicciones -", marca, "(Modelo Manual)")) +
    theme_minimal(base_size = 15, base_family = "Arial") +
    labs(x = "Período", y = "Ventas (Miles de Millones)") +
    scale_y_continuous(labels = scales::number_format(scale = 1e-9, suffix = "B")) +  # Escala a miles de millones con sufijo "B"
    theme(axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12))
  
  # Mostrar la gráfica
  print(p_comparacion)
  
  # Guardar la gráfica como archivo PNG
  ggsave(filename = paste0("C:/Users/marce/Downloads/grafica_Sarima010_110", marca, ".png"), plot = p_comparacion, width = 10, height = 6)
  
  # Acumular las predicciones en un único DataFrame
  predicciones_acumuladas_Sarima010_110 <- rbind(predicciones_acumuladas_Sarima010_110, comparacion)
}

# Crear un workbook de Excel y guardar los resultados
wb <- createWorkbook()
addWorksheet(wb, "Metricas")
writeData(wb, "Metricas", resultados_acumulados_Sarima010_110)
addWorksheet(wb, "Predicciones")
writeData(wb, "Predicciones", predicciones_acumuladas_Sarima010_110)
saveWorkbook(wb, "C:/Users/marce/Downloads/ModeloSarima010_110.xlsx", overwrite = TRUE)

print("Resultados y predicciones acumulados guardados en el archivo Excel.")

# Imprimir marcas buenas y malas
print(paste("Marcas Buenas:", paste(marcas_buenas_Sarima010_110, collapse = ", ")))
print(paste("Marcas Malas:", paste(marcas_malas_Sarima010_110, collapse = ", ")))

# Contar y mostrar la cantidad de métricas malas para cada marca
resultados_acumulados_Sarima010_110$Num_metricas_malas <- rowSums(resultados_acumulados_Sarima010_110[, grep("Evaluacion_", colnames(resultados_acumulados_Sarima010_110))] == "Malo")
print("Resumen de métricas malas por marca:")
print(resultados_acumulados_Sarima010_110[, c("Marca", "Num_metricas_malas", "Apto")])

marcas_buenas_Sarima010_110
marcas_malas_Sarima010_110



