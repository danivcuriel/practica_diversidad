#DIVERSIDAD BETA
install.packages("vegan")
library(vegan)
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

#MATRIZ DE JACCARD
abund1<-presencia[,-1]
abund1
jaccardmat<-vegdist(abund1, method="jaccard")
jaccardmat


#ÍNDICE BRAY-CURTIS
braycurtis<-function(a,b){
  mini<-pmin(a,b)
  arri<-sum(mini)*2
  aba<-sum(a)+sum(b)
  return(1-arri/aba)
}

#DATOS PARA BRAY-CURTIS
datobray<-read.csv("01_datos_crudos/datosbray.csv")
datobray
#PARA SITIO 1
braycurtis(datobray$sitio1,datobray$sitio2)
braycurtis(datobray$sitio1,datobray$sitio3)
braycurtis(datobray$sitio1,datobray$sitio4)
braycurtis(datobray$sitio1,datobray$sitio5)
braycurtis(datobray$sitio1,datobray$sitio6)
braycurtis(datobray$sitio1,datobray$sitio7)
#PARA SITIO 2
braycurtis(datobray$sitio2,datobray$sitio2)
braycurtis(datobray$sitio2,datobray$sitio3)
braycurtis(datobray$sitio2,datobray$sitio4)
braycurtis(datobray$sitio2,datobray$sitio5)
braycurtis(datobray$sitio2,datobray$sitio6)
braycurtis(datobray$sitio2,datobray$sitio7)
#PARA SITIO 3
braycurtis(datobray$sitio3,datobray$sitio3)
braycurtis(datobray$sitio3,datobray$sitio4)
braycurtis(datobray$sitio3,datobray$sitio5)
braycurtis(datobray$sitio3,datobray$sitio6)
braycurtis(datobray$sitio3,datobray$sitio7)
#PARA SITIO 4
braycurtis(datobray$sitio4,datobray$sitio4)
braycurtis(datobray$sitio4,datobray$sitio5)
braycurtis(datobray$sitio4,datobray$sitio6)
braycurtis(datobray$sitio4,datobray$sitio7)
#PARA SITIO 5
braycurtis(datobray$sitio5,datobray$sitio5)
braycurtis(datobray$sitio5,datobray$sitio6)
braycurtis(datobray$sitio5,datobray$sitio7)
#PARA SITIO 6
braycurtis(datobray$sitio6,datobray$sitio6)
braycurtis(datobray$sitio6,datobray$sitio7)

#MATRIZ DE BRAY
abund<-datobray[,-1]
abund
braycurtismat<-vegdist(abund, method="bray")
print(braycurtismat)

