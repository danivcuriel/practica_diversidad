#DIVERSIDAD BETA

#ÍNDICE JACCARD
jaccard<-function(a,b){
  interseccion=length(which(a==1 & b==1))
  union=length(which(a==1))+length(which(b==1))-interseccion
  return(interseccion/union)
}

presencia<-read.csv("01_datos_crudos/datosjaccard.csv")
presencia

presencia$sitio1->a
presencia$sitio2->b
presencia$sitio3->c
presencia$sitio4->d

jaccard(a,b)
jaccard(a,c)
jaccard(b,c)
jaccard(c,d)
#según yo ya debería quedar bien, voy a estar buscando el de bray curtis por lo mientras


###corregir
# Función corregida para calcular Jaccard
jaccard <- function(a, b) {
  interseccion = length(intersect(a, b))
  union = length(unique(c(a, b)))  # Se usa unique para evitar duplicados
  return(interseccion / union)
}
# Cargar datos
presencia <- read.csv("01_datos_crudos/datosjaccard.csv", row.names = 1)

# Convertir datos en listas de especies presentes por sitio
sitio1 <- rownames(presencia)[which(presencia$sitio1 == 1)]
sitio2 <- rownames(presencia)[which(presencia$sitio2 == 1)]
sitio3 <- rownames(presencia)[which(presencia$sitio3 == 1)]
sitio4 <- rownames(presencia)[which(presencia$sitio4 == 1)]

# Calcular índices de Jaccard correctamente
jaccard(sitio1, sitio2)  
jaccard(sitio1, sitio3)  
jaccard(sitio2, sitio3)  
jaccard(sitio3, sitio4)  

