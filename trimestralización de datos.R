
#establecer secuencia
m1_nuevo <- ts(base$M1, start = c(2006), frequency = 1)

m2_nuevo <- ts(base$M2, start = c(2006), frequency = 1)

m3_nuevo <- ts(base$M3, start = c(2006), frequency = 1)

cartera_nuevo <- ts(base$Cartera_de_credito, start = c(2006), frequency = 1)

pib_nuevo <- ts(base$PIB, start = c(2006), frequency = 1)

fbkf_a <- ts(base$fbkf, start = c(2006), frequency = 1)

indix <- ts(base$FBKF, start = c(2006), frequency = 1)

M1_nuevo <- ts(base$M_1, start = c(2006), frequency = 1)

M3_nuevo <- ts(base$M_3, start = c(2006), frequency = 1)
#vista de las variabless
View(m1_nuevo)
print(m1_nuevo)
print(m2_nuevo)
print(m3_nuevo)
print(cartera_nuevo)
print(pib_nuevo)
print(fbkf_a)
print(indix)


#precede a promediar el metodo
install.packages("tempdisagg")
library(tempdisagg)

#M1
denton_resultado_m1 <- td(m1_nuevo ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_m1 <- predict(denton_resultado_m1)
print(serie_trimestral_m1)

#M2

denton_resultado_m2 <- td(m2_nuevo ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_m2 <- predict(denton_resultado_m2)
print(serie_trimestral_m2)

#M3

denton_resultado_m3 <- td(m3_nuevo ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_m3 <- predict(denton_resultado_m3)
print(serie_trimestral_m3)

#CARTERA

denton_resultado_cartera <- td(cartera_nuevo ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_cartera <- predict(denton_resultado_cartera)
print(serie_trimestral_cartera)

#PIB

denton_resultado_pib <- td(pib_nuevo ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_pib <- predict(denton_resultado_pib)
print(serie_trimestral_pib)

#formacion 

denton_resultado_fbkf <- td(fbkf_a ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_fbkf <- predict(denton_resultado_fbkf)
print(serie_trimestral_fbkf)

#fbk
denton_resultado_FBK <- td(indix ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_FBK<- predict(denton_resultado_FBK)
print(serie_trimestral_FBK)


#m1nuevo
denton_resultado_M1 <- td(M1_nuevo ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_M1<- predict(denton_resultado_M1)
print(serie_trimestral_M1)

#m3nuevo
denton_resultado_M3 <- td(M3_nuevo ~ 1, to = "quarterly", method = "denton-cholette")
serie_trimestral_M3<- predict(denton_resultado_M3)
print(serie_trimestral_M3)






# Convertir a vectores si aún no lo hiciste
m1_trim <- as.numeric(serie_trimestral_m1)
m2_trim <- as.numeric(serie_trimestral_m2)
m3_trim <- as.numeric(serie_trimestral_m3)
cartera_trim <- as.numeric(serie_trimestral_cartera)
fbkf_trim <- as.numeric(serie_trimestral_fbkf)
pib_trim <- as.numeric(serie_trimestral_pib)
fbk_trim <- as.numeric(serie_trimestral_FBK)
M1_trim <- as.numeric(serie_trimestral_M1)
M3_trim <- as.numeric(serie_trimestral_M3)



# Crear fechas trimestrales (ajusta la fecha inicial según tus datos)
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(m1_trim))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(m2_trim))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(m3_trim))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(cartera_trim))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(fbkf_trim))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(pib_trim))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(fbk_trim))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(M1_trim))
fechas <- seq(from = as.Date("2006-01-01"), by = "3 months", length.out = length(M3_trim))


# Unir todo en un data frame
df_trimestral <- data.frame(
  Fecha = fechas,
  M1 = m1_trim,
  M2 = m2_trim,
  M3 = m3_trim,
  Cartera = cartera_trim,
  FBKF = fbkf_trim,
  PIB = pib_trim,
  FBK = fbk_trim,
  index = indice_
)

print(df_trimestral)
View(df_trimestral)

#segunda base
df_trimestral_nueva <- data.frame(
  Fecha = fechas,
  M1 = M1_trim,
  M3 = M3_trim,
  Cartera = cartera_trim,
  PIB = pib_trim,
  FBK = fbk_trim,
  indice_prueba
)




#Creación del indice
indice_de_financiarización <- (df_trimestral_$M3-df_trimestral$M1)/df_trimestral$FBKF
print(indice_de_financiarización)

indice_ <- (df_trimestral$M3-df_trimestral$M1)/df_trimestral$FBK
print(indice_)

#TERCER INDICE

indice_prueba<- (df_trimestral_nueva$M3-df_trimestral_nueva$M1)/df_trimestral_nueva$FBK
print(indice_prueba)

#dirección de variables
plot(df_trimestral$Gradoprofundidad, type = "l")
plot(df_trimestral$PIB,type = "l")
plot(df_trimestral$Fecha, df_trimestral$M1, type = "l")


#Test de estacionaridad
install.packages("writexl")
library(writexl)
write_xlsx(df_trimestral, "series_trimestrales.xlsx")
getwd()

install.packages("writexl")
library(writexl)
write_xlsx(df_trimestral_nueva, "series_trimestrales_nuevas.xlsx")
getwd()

print(df_trimestral_nueva)
