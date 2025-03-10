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


