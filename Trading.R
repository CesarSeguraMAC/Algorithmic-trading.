library(randtests)
library(readxl)
library(ggplot2)
library(dplyr)
library(MASS)
library(car)
library('rvest')

##INICIO
ruta_excel <- "E:\\Programas de R\\Finanzas.xlsx"




Fecha <- read_excel(ruta_excel, sheet = "Hoja1", range = "A1:A52")
Carso <- read_excel(ruta_excel, sheet = "Hoja1", range = "B1:B52")
Cemex <- read_excel(ruta_excel, sheet = "Hoja1", range = "C1:C52")
Banorte <- read_excel(ruta_excel, sheet = "Hoja1", range = "D1:D52")
Bimbo <- read_excel(ruta_excel, sheet = "Hoja1", range = "E1:E52")
Elektra <- read_excel(ruta_excel, sheet = "Hoja1", range = "F1:F52")
IPC <- read_excel(ruta_excel, sheet = "Hoja1", range = "G1:G52")
Tickect <- read_excel(ruta_excel, sheet = "Hoja1", range = "B1:G1", col_names = FALSE)
Tickects <- as.vector(as.matrix(Tickect))


print(Tickects)

datos <- data.frame(
  Fecha = as.Date(Fecha[[1]], format = "%m/%d/%Y"),  # Asegurarse de que "Fecha" sea tipo Date
  Carso = as.numeric(Carso[[1]]),
  Cemex = as.numeric(Cemex[[1]]),
  Banorte = as.numeric(Banorte[[1]]),
  Bimbo = as.numeric(Bimbo[[1]]),
  Elektra = as.numeric(Elektra[[1]]),
  IPC = as.numeric(IPC[[1]])
)

#MODELO

library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(quantmod)

# Verificar los datos de IPC para la serie temporal
IPC_xts <- xts(datos$IPC, order.by = datos$Fecha)  # Convertir IPC en serie xts
plot(IPC_xts, main = "IPC - Serie Temporal", col = "blue", type = "l")

# Diferenciar la serie para analizar estacionariedad
IPC_diff <- diff(IPC_xts) %>% na.omit()
par(mfrow = c(1, 2))
Acf(IPC_diff, main = "ACF para IPC Diferenciado")
Pacf(IPC_diff, main = "PACF para IPC Diferenciado")

# Prueba de estacionariedad (ADF Test)
adf_result <- adf.test(IPC_xts, alternative = "stationary")
print(adf_result)  # Hipótesis nula: No estacionariedad

# Ajustar modelos ARIMA automáticamente
fitA <- auto.arima(IPC_xts, seasonal = FALSE)
print(fitA)

# Visualizar residuales del modelo
tsdisplay(residuals(fitA), lag.max = 40, main = "(ARIMA Automático) Residuos")

#Ajustar modelos ARIMA manualmente
fitB <- arima(IPC_xts, order = c(1, 2, 4))
tsdisplay(residuals(fitB), lag.max = 40, main = "(1,2,4) Modelo Residuos")

fitC <- arima(IPC_xts, order = c(5, 1, 4))
tsdisplay(residuals(fitC), lag.max = 40, main = "(5,1,4) Modelo Residuos")

fitD <- arima(IPC_xts, order = c(1, 1, 1))
tsdisplay(residuals(fitD), lag.max = 40, main = "(1,1,1) Modelo Residuos")

# Plots de predicciones para cada modelo
par(mfrow = c(2, 2))
term <- 30  # Períodos para predicción

fcast1 <- forecast(fitA, h = term)
plot(fcast1, main = "Pronóstico ARIMA Automático")

fcast2 <- forecast(fitB, h = term)
plot(fcast2, main = "Pronóstico ARIMA (1,2,4)")

fcast3 <- forecast(fitC, h = term)
plot(fcast3, main = "Pronóstico ARIMA (5,1,4)")

fcast4 <- forecast(fitD, h = term)
plot(fcast4, main = "Pronóstico ARIMA (1,1,1)")

# Métricas de precisión
print("Precisión de los modelos:")
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)



##Clustering

library(quantmod)
library(dtwclust)
library(factoextra)

# Definir tickers de la variable `tickects`
tickects <- c("CEMEXCPO.MX", "GFNORTEO.MX", "BIMBOA.MX")

# Obtener datos ajustados de los tickers
getSymbols(tickects, from = "2020-01-01", adjusted = TRUE)

# Crear un objeto combinado con los datos ajustados
datos_comb <- do.call(
  cbind,
  lapply(tickects, function(t) Ad(get(t)))
)

# Verificar si hay valores NA y eliminarlos
anyNA(datos_comb) # TRUE si hay NAs
datos_limpios <- na.omit(datos_comb)

# Normalizar los datos
datos_normalizados <- scale(datos_limpios)

# Transponer para usar en clustering
datos_transpuestos <- t(datos_normalizados)
rownames(datos_transpuestos) <- tickects

# Clustering con 2 clusters (el número adecuado para los datos actuales)
dtw_cluster2 <- tsclust(
  datos_transpuestos,
  type = "partitional",
  k = 2, # Número de clusters
  distance = "dtw_basic",
  centroid = "pam",
  seed = 1234,
  trace = TRUE,
  args = tsclust_args(dist = list(window.size = 5))
)

# Graficar los centroides del clustering con k=2
plot(dtw_cluster2, type = "centroids")

# Información del clustering
print(dtw_cluster2@clusinfo)

# Método Silhouette para determinar el número óptimo de clusters
fviz_nbclust(datos_transpuestos, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Clustering jerárquico
res_hc <- hclust(dist(datos_transpuestos))

# Graficar dendrograma
dev.new(); fviz_dist(dist(datos_transpuestos))
dev.new(); fviz_dend(res_hc)
dev.new(); fviz_dend(res_hc, rect = TRUE, cex = 0.5)

# Dendrograma con colores y 2 clusters
res <- hcut(datos_transpuestos, k = 2)
dev.new(); fviz_dend(res, rect = TRUE, cex = 0.5, k_colors = c("red", "blue"))



#Chartismo

# Cargar la librería quantmod
# Instalar e importar el paquete de Alpha Vantage

library(alphavantager)

# Establecer tu clave API de Alpha Vantage
av_api_key("tu_clave_API")

# Obtener los datos del oro (XAU/USD)
gold_data <- av_get(symbol = "XAUUSD", av_fun = "TIME_SERIES_DAILY")

# Ver los primeros datos
head(gold_data)


###Analisis

library(ggplot2)

# Graficar el precio de cierre
ggplot(gold_data, aes(x = timestamp, y = close)) +
  geom_line(color = "blue") +
  labs(title = "Evolución del precio del oro", x = "Fecha", y = "Precio de cierre (USD)") +
  theme_minimal()




###Analisis2

# Graficar la serie de precios
chartSeries(gold_data, theme = chartTheme("white"))

# Agregar indicadores técnicos
addMACD()        # MACD
addBBands()      # Bollinger Bands
addRSI()         # Relative Strength Index (RSI)





