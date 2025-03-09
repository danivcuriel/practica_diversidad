#DIVERSIDAD BETA

#ÍNDICE JACCARD
jaccard<-function(a,b){
  interseccion=length(intersect(a,b))
  union=length(a)+length(b)-interseccion
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
