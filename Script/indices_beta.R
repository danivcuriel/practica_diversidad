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
presencia$sitio5->e
presencia$sitio6->f
presencia$sitio7->g
#COMPARACIÓN SITIO A CON LOS DEMÁS 
jaccard(a,b)
jaccard(a,c)
jaccard(a,d)
jaccard(a,e)
jaccard(a,f)
jaccard(a,g)
#COMPARACIÓN SITIO B CON LOS DEMÁS
jaccard(b,c)
jaccard(b,d)
jaccard(b,e)
jaccard(b,f)
jaccard(b,g)
#COMPARACIÓN SITIO C CON LOS DEMÁS
jaccard(c,d)
jaccard(c,e)
jaccard(c,f)
jaccard(c,g)
#COMPARACIÓN SITIO D
jaccard(d,e)
jaccard(d,f)
jaccard(d,g)
#COMPARACIÓN SITIO E
jaccard(e,f)
jaccard(e,g)
#COMPARACIÓN SITIO F
jaccard(f,g)

#No se hizo los índices con las demás porque daba el mismo resultado
#es decir, no se hizo jaccard(b,a) porque da lo mismo que jaccard(a,b)
#es lo mismo que los demás


#ÍNDICE BRAY-CURTIS
braycurtis<-function(a,b){
  mini<-pmin(a,b)
  arri<-sum(mini)*2
  aba<-sum(a)+sum(b)
  return(1-arri/aba)
}




