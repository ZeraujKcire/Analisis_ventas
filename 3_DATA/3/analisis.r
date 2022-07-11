
# === DATOS === (((
long_prediccion = 1800 # cantidad_en_dias_de_predicción
v_datos = read.csv('data.csv', header = TRUE, sep = ",")
v_datos$x = as.numeric(as.Date(v_datos$start_date))
v_datos$y = v_datos$salary
attach(v_datos)
datos = data.frame(x = seq(min(v_datos$x) , max(v_datos$x) + long_prediccion, length.out = 100))
pdf("1_MODELOS.pdf")
plot(y ~ x, data = v_datos, xlim=c(min(v_datos$x) , max(v_datos$x) + long_prediccion))
# )))

# === ANALISIS DE REGRESION LINEAL === (((
sprintf(" --- REGRESION LINEAL --- ")
modelo = lm(y ~ x)
pred = predict(modelo, newdata = datos)
with(datos, lines(x = x, y = pred, col = "blue"))
modelo$coefficients
sprintf("Error:")
sum(abs(y - fitted(modelo)) )
# )))

# === ANALISIS DE REGRESION CUARATICA === (((
sprintf(" --- REGRESION CUADRATICA --- ")
modelo = lm(y ~ poly(x, 2))
pred = predict(modelo, newdata = datos)
with(datos, lines(x = x, y = pred, col = "green"))
modelo$coefficients
sprintf("Error:")
sum(abs(y - fitted(modelo)) )
# )))

# === ANALISIS DE REGRESION CUBICA === (((
sprintf(" --- REGRESION CUBICA --- ")
modelo = lm(y ~ poly(x, 3))
pred = predict(modelo, newdata = datos)
with(datos, lines(x = x, y = pred, col = "red"))
modelo$coefficients
sprintf("Error:")
sum(abs(y - fitted(modelo)) )
# )))

