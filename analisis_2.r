
# === LIBRERÍAS === (((
sprintf(" --- LIBRERIAS --- ")
# suppressPackageStartupMessages(library(rio))
suppressPackageStartupMessages(library(ggplot2))
# suppressPackageStartupMessages(library(lmtest))
pdf("1_MODELOS.pdf")
# )))

# === DATOS === (((
sprintf(" --- DATOS --- ")
datos = read.csv('data.csv', header = TRUE, sep = ",")
datos$x = as.numeric(as.Date(datos$start_date))
datos$y = datos$salary
# str(datos)
# )))

# === ANALISIS DE REGRESION POLINOMIAL === (((
sprintf(" --- REGRESION POLINOMIAL --- ")
grafo = ggplot(data = NULL, aes(x = datos$x,y = datos$y)) +
 	geom_point() +
 	ggtitle("Estimacion polinomica") +
 	labs(x = "Dia",y = "Ventas") +
	xlim(datos$x[1] , max(datos$x)+ 180 ) +
	geom_smooth(method = "lm"  , fullrange = TRUE,  formula = y ~ x, se = FALSE, col = 'blue') +
	geom_smooth(method = "gam" , fullrange = TRUE,  formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, col = 'green') +
	geom_smooth(method = "gam" , fullrange = TRUE,  formula = y ~ poly(x, 3), se = FALSE, col = 'red')
ggsave("1_estimacion.pdf")
# )))
