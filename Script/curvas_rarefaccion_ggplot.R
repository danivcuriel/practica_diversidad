##Curvas de rarefaccion con ggplot
library(tidyverse)
rarefaccion_cr <- read_csv("datos_crudos/rarefaccion.csv")

# Calcular el numero acumulado de especies por cada tamano de muestra para todo el dataframe
rarefaccion_cr <- rarefaccion_cr %>%
  group_by(sitio) %>%
  arrange(sitio, fichas_extraidas) %>%
  mutate(numero_especies_acumuladas = cumsum(numero_especies))

# Separar por subsets
luna_r <- subset(rarefaccion_cr, sitio == "luna")
corazon_r <- subset(rarefaccion_cr, sitio == "corazon")
naranjas_r <- subset(rarefaccion_cr, sitio == "todas_naranjas")  
verdes_r <- subset(rarefaccion_cr, sitio == "todas_verdes") 
alfa_r <- subset(rarefaccion_cr, sitio == "alfa") 
vr_r <- subset(rarefaccion_cr, sitio == "vr")
happy_face_r <- subset(rarefaccion_cr, sitio == "happy_face") 

# Asignar colores a cada sitio
colores <- c("luna" = "darkblue", 
             "corazon" = "red", 
             "todas_naranjas" = "orange", 
             "todas_verdes" = "green", 
             "alfa" = "purple",
             "vr" = "gray",
             "happy_face" = "yellow")

# Crear una funcion para graficar cada curva de rarefaccion por separado (usando el acumulado)
gg_rf <- function(data, sitio) {
  ggplot(data, aes(x = fichas_extraidas, y = numero_especies_acumuladas)) +
    geom_line(color = colores[sitio]) +
    geom_point(color = colores[sitio]) +
    labs(title = paste("Curva de rarefaccion de", sitio),
         x = "Fichas extraidas", y = "Numero acumulado de especies") +
    theme_minimal()
}

# Graficar cada sitio
gg_rf(luna_r, "luna")
gg_rf(corazon_r, "corazon")
gg_rf(naranjas_r, "todas_naranjas")
gg_rf(verdes_r, "todas_verdes")
gg_rf(alfa_r, "alfa")
gg_rf(vr_r, "vr")
gg_rf(happy_face_r, "happy_face")

# Graficar todas las curvas combinadas
cr_gg <- ggplot(rarefaccion_cr, aes(x = fichas_extraidas, y = numero_especies_acumuladas, color = sitio)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = colores) +
  labs(title = "Curvas de Rarefaccion", x = "Tamano de la muestra", y = "Numero acumulado de especies") +
  theme_minimal()

# Mostrar grafico combinado
cr_gg

#guardar png de todas las curvas
ggsave("resultados//curvas_combinadas.png", plot = cr_gg, width = 8, height = 6, dpi = 300)
