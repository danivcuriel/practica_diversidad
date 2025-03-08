###índices de diversidad 

shanon<-function(abundancias){
  proba<-abundancias/sum(abundancias)
  return(-sum(proba*log(proba)))
}
#según yo ese ya está bien, volví a tratar con uno de los intentos que hicimos y si salió un número más bajo, pero pueden probarlo xfa

simpson<-function(abundancias){
  prob<-abundancias/sum(abundancias)
  sum(prob*prob)
}
#índice de simpson, si funciona

##Ejercicio Luna
