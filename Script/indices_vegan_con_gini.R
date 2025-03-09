
library(tidyverse)
abundancias <- read_csv("01_datos_crudos/abundancias.csv")
library(vegan)

# Calcular índices con la librería vegan

# BOLSA 1 --> LUNA
luna_vg <- abundancias[1, 2:12]  # Seleccionar las abundancias de la bolsa Luna
shannon_luna <- diversity(luna_vg, index = "shannon")
pielou_luna <- shannon_luna / log(length(luna_vg))  # Pielou es la diversidad de Shannon normalizada
simpson_luna <- diversity(luna_vg, index = "simpson")
simpson_inv_luna <- diversity(luna_vg, index = "invsimpson")  # Simpson inverso
gini_luna <- 1 - sum((prop.table(luna_vg))^2)  # Cálculo del índice de Gini
chao1_luna <- estimateR(luna_vg)["Chao1"]

# BOLSA 2 --> CORAZON
corazon_vg <- abundancias[2, 2:12]
shannon_corazon <- diversity(corazon_vg, index = "shannon")
pielou_corazon <- shannon_corazon / log(length(corazon_vg))
simpson_corazon <- diversity(corazon_vg, index = "simpson")
simpson_inv_corazon <- diversity(corazon_vg, index = "invsimpson")
gini_corazon <- 1 - sum((prop.table(corazon_vg))^2)  # Cálculo del índice de Gini
chao1_corazon <- estimateR(corazon_vg)["Chao1"]

# BOLSA 3 --> NARANJAS
naranjas_vg <- abundancias[3, 2:12]
shannon_naranjas <- diversity(naranjas_vg, index = "shannon")
pielou_naranjas <- shannon_naranjas / log(length(naranjas_vg))
simpson_naranjas <- diversity(naranjas_vg, index = "simpson")
simpson_inv_naranjas <- diversity(naranjas_vg, index = "invsimpson")
gini_naranjas <- 1 - sum((prop.table(naranjas_vg))^2)  # Cálculo del índice de Gini
chao1_naranjas <- estimateR(naranjas_vg)["Chao1"]

# BOLSA 4 --> VERDES
verdes_vg <- abundancias[4, 2:12]
shannon_verdes <- diversity(verdes_vg, index = "shannon")
pielou_verdes <- shannon_verdes / log(length(verdes_vg))
simpson_verdes <- diversity(verdes_vg, index = "simpson")
simpson_inv_verdes <- diversity(verdes_vg, index = "invsimpson")
gini_verdes <- 1 - sum((prop.table(verdes_vg))^2)  # Cálculo del índice de Gini
chao1_verdes <- estimateR(verdes_vg)["Chao1"]

# BOLSA 5 --> ALFA
alfa_vg <- abundancias[5, 2:12]
shannon_alfa <- diversity(alfa_vg, index = "shannon")
pielou_alfa <- shannon_alfa / log(length(alfa_vg))
simpson_alfa <- diversity(alfa_vg, index = "simpson")
simpson_inv_alfa <- diversity(alfa_vg, index = "invsimpson")
gini_alfa <- 1 - sum((prop.table(alfa_vg))^2)  # Cálculo del índice de Gini
chao1_alfa <- estimateR(alfa_vg)["Chao1"]

# BOLSA 6 --> VR
vr_vg <- abundancias[6, 2:12]
shannon_vr <- diversity(vr_vg, index = "shannon")
pielou_vr <- shannon_vr / log(length(vr_vg))
simpson_vr <- diversity(vr_vg, index = "simpson")
simpson_inv_vr <- diversity(vr_vg, index = "invsimpson")
gini_vr <- 1 - sum((prop.table(vr_vg))^2)  # Cálculo del índice de Gini
chao1_vr <- estimateR(vr_vg)["Chao1"]

# BOLSA 7 --> HAPPY_FACE
happy_face_vg <- abundancias[7, 2:12]
shannon_happy_face <- diversity(happy_face_vg, index = "shannon")
pielou_happy_face <- shannon_happy_face / log(length(happy_face_vg))
simpson_happy_face <- diversity(happy_face_vg, index = "simpson")
simpson_inv_happy_face <- diversity(happy_face_vg, index = "invsimpson")
gini_happy_face <- 1 - sum((prop.table(happy_face_vg))^2)  # Cálculo del índice de Gini
chao1_happy_face <- estimateR(happy_face_vg)["Chao1"]

# Crear un data frame con los resultados
resultados_vg_2 <- data.frame(
  Bolsa = c("Luna", "Corazon", "Naranjas", "Verdes", "Alfa", "VR", "Happy_Face"),
  Shannon = c(shannon_luna, shannon_corazon, shannon_naranjas, shannon_verdes, shannon_alfa, shannon_vr, shannon_happy_face),
  Pielou = c(pielou_luna, pielou_corazon, pielou_naranjas, pielou_verdes, pielou_alfa, pielou_vr, pielou_happy_face),
  Simpson = c(simpson_luna, simpson_corazon, simpson_naranjas, simpson_verdes, simpson_alfa, simpson_vr, simpson_happy_face),
  Simpson_Inverso = c(simpson_inv_luna, simpson_inv_corazon, simpson_inv_naranjas, simpson_inv_verdes, simpson_inv_alfa, simpson_inv_vr, simpson_inv_happy_face),
  Gini = c(gini_luna, gini_corazon, gini_naranjas, gini_verdes, gini_alfa, gini_vr, gini_happy_face),
  Chao1 = c(chao1_luna, chao1_corazon, chao1_naranjas, chao1_verdes, chao1_alfa, chao1_vr, chao1_happy_face)
)

# Ver el data frame con los resultados
# Imprimir el data frame con los resultados
print(resultados_vg_2)


write_csv(resultados_vg_2, "resultados/resultados_indices_vg_2.csv")
library(readr)
resultados_indices_vg_2 <- read_csv("resultados/resultados_indices_vg_2.csv")
View(resultados_indices_vg_2)
