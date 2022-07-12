
# === LIBRERÍAS === (((
sprintf(" --- LIBRERIAS --- ")
# suppressPackageStartupMessages(library(rio))
suppressPackageStartupMessages(library(ggplot2))
# suppressPackageStartupMessages(library(lmtest))
# )))

# === DATOS === (((
long_prediccion = 1800 # longitud en días de adelanto en la predicción
datos = read.csv('data.csv', header = TRUE, sep = ",")
datos$x = as.numeric(as.Date(datos$start_date))
datos$y = datos$salary
# str(datos)
# )))

# === ANALISIS DE REGRESION POLINOMIAL === (((
grafo = ggplot(data = NULL, aes(x = datos$x,y = datos$y)) +
 	geom_point() +
 	ggtitle("Estimacion polinomica") +
 	labs(x = "Dia",y = "Ventas") +
	xlim(min(datos$x) , max(datos$x) + long_prediccion) +
	geom_smooth(method = "lm"  , fullrange = TRUE,  formula = y ~ x, se = FALSE, col = 'blue') +
	geom_smooth(method = "gam" , fullrange = TRUE,  formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, col = 'green') +
	geom_smooth(method = "gam" , fullrange = TRUE,  formula = y ~ poly(x, 3), se = FALSE, col = 'red')
ggsave("2_MODELOS.pdf")
# )))

# === COEFFICIENTES LINEAL === (((
sprintf(" --- LINEAL --- ")
lm(y ~ x,data = datos)$coefficients
# )))

# === COEFFICIENTES CUADRATICA === (((
sprintf(" --- CUADRATICA --- ")
lm(y ~ poly(x, 2),data = datos)$coefficients
# )))

# === COEFFICIENTES CUBICA === (((
sprintf(" --- CUBICA --- ")
lm(y ~ poly(x, 3),data = datos)$coefficients
# )))
