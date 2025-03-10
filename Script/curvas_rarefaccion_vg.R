#CURVAS DE RAREFACCION segun VEGAN

library(tidyverse)
abundancias <- read_csv("datos_crudos/abundancias.csv")

#Cargar librerías necesarias
library(vegan)

# Datos de abundancia para cada bolsa
luna_vg <- abundancias[1, 2:12]
corazon_vg <- abundancias[2, 2:12]
naranjas_vg <- abundancias[3, 2:12]
verdes_vg <- abundancias[4, 2:12]
alfa_vg <- abundancias[5, 2:12]
vr_vg <- abundancias[6, 2:12]
happy_face_vg <- abundancias[7, 2:12]

# Generar y guarda las curvas de rarefacción 

#Luna
png("resultados/curva_Luna_vg.png")
rarecurve(luna_vg, main = "Curva de Rarefacción Luna", col = "darkblue")
dev.off()

# Corazón
png("resultados/curva_Corazon_vg.png")
rarecurve(corazon_vg, main = "Curva de Rarefacción Corazón", col = "red")
dev.off()

# Naranjas
png("resultados/curva_Naranjas_vg.png")
rarecurve(naranjas_vg, main = "Curva de Rarefacción Naranjas", col = "orange")
dev.off()

# Verdes
png("resultados/curva_Verdes_vg.png")
rarecurve(verdes_vg, main = "Curva de Rarefacción Verdes", col = "green")
dev.off()

# Alfa
png("resultados/curva_Alfa_vg.png")
rarecurve(alfa_vg, main = "Curva de Rarefacción Alfa", col = "purple")
dev.off()

# VR
png("resultados/curva_VR_vg.png")
rarecurve(vr_vg, main = "Curva de Rarefacción VR", col = "gray")
dev.off()

# Happy Face
png("resultados/curva_Happy_Face_vg.png")
rarecurve(happy_face_vg, main = "Curva de Rarefacción Happy Face", col = "yellow")
dev.off()



#Grafica combinada
png("resultados/todas_curvas_rf_bg.png")

# Asignar colores diferentes a cada curva
colores <- c("darkblue", "red", "orange", "green", "purple", "gray", "yellow")
sitios <- c("Luna", "Corazón", "Naranjas", "Verdes", "Alfa", "VR", "Happy Face")

# Graficar todas las curvas combinadas, con diferentes colores
rarecurve(abundancias[, 2:12], col = colores, xlab = "Fichas extraídas", 
          ylab = "Número de especies", main = "Curvas de Rarefacción Combinadas")
#Añadir leyenda de identificacion
legend("topright", legend = sitios, col = colores, lty = 1, pch = 1, title = "Sitios")
dev.off ()


