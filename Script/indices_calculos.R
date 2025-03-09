# CREAR FUNCIONES
#Indice de Shannon
shannon <- function(abundancias) {
  proba <- abundancias / sum(abundancias)
  proba <- proba[proba > 0]  # Eliminar ceros antes de calcular el logaritmo
  return(-sum(proba * log(proba)))
}

#Indice de Simpson
simpson <- function(abundancias) {
  prob <- abundancias / sum(abundancias)
  #Condicion de que si solo hay una especie, Simpson debe ser 1
  if (length(prob) == 1) {
    return(1)
  }
  return(1 - sum(prob^2))  
}

#Indice de Pilou
pilou <- function(abundancias) {
  proba <- abundancias / sum(abundancias)
  proba <- proba[proba > 0]  # Eliminar valores 0
  return(-sum(proba * log(proba))) 
}

# CARGAR DATOS
library(tidyverse)
abundancias <- read_csv("01_datos_crudos/abundancias.csv")

# BOLSA 1 --> LUNA
luna <- abundancias[1, 2:12] 
luna

shannon_luna <- shannon(luna)
simpson_luna <- simpson(luna)
pilou_luna <- pilou(luna)

cat("Índice de Shannon:", shannon_luna, "\n")
cat("Índice de Simpson:", simpson_luna, "\n")
cat("Índice de Pilou:", pilou_luna, "\n")

# BOLSA 2 --> CORAZON 
corazon <- abundancias[2, 2:12] 
corazon

shannon_corazon <- shannon(corazon)
simpson_corazon <- simpson(corazon)
pilou_corazon <- pilou(corazon)

cat("Índice de Shannon:", shannon_corazon, "\n")
cat("Índice de Simpson:", simpson_corazon, "\n")
cat("Índice de Pilou:", pilou_corazon, "\n")

# BOLSA 3 --> NARANJAS
naranjas <- abundancias[3, 2:12]  
naranjas

shannon_naranjas <- shannon(naranjas)
simpson_naranjas <- simpson(naranjas)
pilou_naranjas <- pilou(naranjas)

cat("Índice de Shannon:", shannon_naranjas, "\n")
cat("Índice de Simpson:", simpson_naranjas, "\n")
cat("Índice de Pilou:", pilou_naranjas, "\n")

# BOLSA 4 --> VERDES
verdes <- abundancias[4, 2:12] 
verdes

shannon_verdes <- shannon(verdes)
simpson_verdes <- simpson(verdes)
pilou_verdes <- pilou(verdes)

cat("Índice de Shannon para:", shannon_verdes, "\n")
cat("Índice de Simpson:", simpson_verdes, "\n")
cat("Índice de Pilou:", pilou_verdes, "\n")

# BOLSA 5 --> ALFA
alfa <- abundancias[5, 2:12]  
alfa

shannon_alfa <- shannon(alfa)
simpson_alfa <- simpson(alfa)
pilou_alfa <- pilou(alfa)

cat("Índice de Shannon:", shannon_alfa, "\n")
cat("Índice de Simpson", simpson_alfa, "\n")
cat("Índice de Pilou:", pilou_alfa, "\n")

# BOLSA 6 --> VR
vr <- abundancias[6, 2:12]  
vr

shannon_vr <- shannon(vr)
simpson_vr <- simpson(vr)
pilou_vr <- pilou(vr)

cat("Índice de Shannon:", shannon_vr, "\n")
cat("Índice de Simpson", simpson_vr, "\n")
cat("Índice de Pilou:", pilou_vr, "\n")

# BOLSA 7 --> HAPPY_FACE
happy_face <- abundancias[7, 2:12]  
happy_face

shannon_happy_face <- shannon(happy_face)
simpson_happy_face <- simpson(happy_face)
pilou_happy_face <- pilou(happy_face)

cat("Índice de Shannon:", shannon_happy_face, "\n")
cat("Índice de Simpson", simpson_happy_face, "\n")
cat("Índice de Pilou:", pilou_happy_face, "\n")

# Crear un data frame con los resultados
resultados <- data.frame(
  Bolsa = c("Luna", "Corazon", "Naranjas", "Verdes", "Alfa", "VR", "Happy_Face"),
  Shannon = c(shannon_luna, shannon_corazon, shannon_naranjas, shannon_verdes, shannon_alfa, shannon_vr, shannon_happy_face),
  Simpson = c(simpson_luna, simpson_corazon, simpson_naranjas, simpson_verdes, simpson_alfa, simpson_vr, simpson_happy_face),
  Pilou = c(pilou_luna, pilou_corazon, pilou_naranjas, pilou_verdes, pilou_alfa, pilou_vr, pilou_happy_face)
)

print(resultados)

#Guardar y visuaiza indices
write_csv(resultados, "resultados/resultados_indices.csv")
library(readr)
resultados_indices <- read_csv("resultados/resultados_indices.csv")
View(resultados_indices)
