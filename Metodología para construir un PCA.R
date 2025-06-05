#series temporales
activos_totales_de_bancos <- ts(series_trimestrales_nuevas$`Activos totales de bancos`, start = c(2006), frequency = 1)

sector_financiero <- ts(series_trimestrales_nuevas$`Sector financiero`, start = c(2006), frequency = 1)

cartera_de_los_bancos <- ts(series_trimestrales_nuevas$`Cartera de crédito de bns`, start = c(2006), frequency = 1)

cartera_de_los_tp <- ts(series_trimestrales_nuevas$`cartera-Tarjetas de crédito`, start = c(2006), frequency = 1)

pib_ind <- ts(series_trimestrales_nuevas$`Pib industrial`, start = c(2006), frequency = 1)


#actvos


denton_resultado_activos <- td(activos_totales_de_bancos ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_activos <- predict(denton_resultado_activos)
print(serie_trimestral_activos)

#cartera bns

denton_resultado_cartera_bns <- td(cartera_de_los_bancos~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_cartera_bns <- predict(denton_resultado_cartera_bns)
print(serie_trimestral_cartera_bns)

#cartera tp

denton_resultado_cartera_tp <- td(cartera_de_los_tp~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_cartera_tp <- predict(denton_resultado_cartera_tp)
print(serie_trimestral_cartera_tp)


#Pib ind

denton_resultado_pib_ind <- td(pib_ind~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_pib_ind <- predict(denton_resultado_pib_ind)
print(serie_trimestral_pib_ind)



#objetos
activos <- as.numeric(serie_trimestral_activos)
cartera_bns <- as.numeric(serie_trimestral_cartera_bns)
cartera_tp <- as.numeric(serie_trimestral_cartera_tp)
pib_industrial <- as.numeric(serie_trimestral_pib_ind)

#periodo
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(activos_totales_de_bancos))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(cartera_de_los_bancos))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(cartera_credito_ts))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(pib_industrial))



fechas
base_para_pca <- data.frame(
  Activos_bancos= activos,
  cartera_bancos = cartera_bns,
  cartera_credito_personal = cartera_tp,
  pib_industrial,
  indice_financiarizacion,
  FBKK = fbk_pca)

print(base_para_pca)




#pca
# Escalar los datos (muy importante si las variables están en diferentes unidades)
base_escalada <- scale(base_para_pca)

# Realizar el PCA
pca_resultado <- prcomp(base_escalada, center = TRUE, scale. = TRUE)

# Ver resumen de los componentes principales
summary(pca_resultado)

# Ver la importancia de cada componente
pca_resultado$sdev^2 / sum(pca_resultado$sdev^2)

# Cargar las "loadings" (correlaciones entre las variables originales y los componentes)
pca_resultado$rotation

# Coordenadas de tus observaciones en los nuevos ejes (componentes principales)
pca_resultado$x



screeplot(pca_resultado, type = "lines", main = "Scree Plot")
biplot(pca_resultado, scale = 0)

#pca principal
# El primer componente
indice_financiarizacion <- pca_resultado$x[, 1]

# Agregarlo a tu data si quieres
base_con_indice <- cbind(base_para_pca, Indice_Financiarizacion = indice_financiarizacion)
print(base_con_indice)


# Suponiendo que tienes una columna llamada "Fecha"
plot(Fecha, indice_financiarizacion, type = "l", col = "blue",
     main = "Índice de Financiarización (PC1)",
     xlab = "Tiempo", ylab = "Componente Principal 1")

ts.plot(pib_industrial,indice_financiarizacion)

plot(pib_industrial, type = "l", col = "red")
plot(indice_financiarizacion)


#resultados
install.packages("writexl")
library(writexl)
write_xlsx(base_para_pca, "series_trimestrales_nuevas_taka.xlsx")
getwd()



#modelo de regresi[on lineal
modelo_simple <- lm(pib_industrial~ Indice_Financiarizacion, data = base_con_indice)
summary(modelo_simple)base_con_indice

modelo_log <- lm(lpib~indice_financiarizacion, FBKK, data = series_trimestrales_nuevas_taka)
summary(modelo_log)

#supuestos del modelo
shapiro.test(residuals(modelo_simple))
library(lmtest)
dwtest(modelo_log)
library(lmtest)
bptest(modelo_simple)


#Modelo corregido
# Instala los paquetes si no los tienes
install.packages("sandwich")
install.packages("lmtest")

# Carga los paquetes

library(sandwich)

coeftest(modelo_simple, vcov = NeweyWest(modelo_simple))

# Aplica el estimador Newey-West (HAC)

coeftest(modelo_simple, vcov = NeweyWest(modelo_simple))
s



par(mfrow = c(2, 2))
plot(modelo_simple)
modelo_simple
summary(modelo_simple)

base_con_indice
View(base)


fbk_pca  <- ts(df_trimestral$FBK, start = c(2006), frequency = 1)
View(base_para_pca)


modelo_simple <- lm(pib_industrial~indice_financiarizacion, data = base_para_pca)
modelo_simple
summary(modelo_simple)
