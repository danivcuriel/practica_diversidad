###Índices de diversidad 
mi_shannon <- function (abundances) {
  prob <- abundances/sum(abundances)  ###que tan equitatitvas estan distribuidas las edades 
  return(-sum(prob+log(prob)))
}

mi_pielou <- function (abundances){
  mi_shannon(abundances)/log(length(abundances))
}


mi_simpson <- function (abundances) {
  prob <- abundances/sum(abundances)
  return(sum(prob^2)) ##probabilidades de que dos individuos tomados de la misma especie 
}





##Ejercicio Luna




