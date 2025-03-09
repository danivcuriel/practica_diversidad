#DIVERSIDAD BETA

#ÍNDICE JACCARD
jaccard<-function(a,b){
  interseccion=length(intersect(a,b))
  union=length(a)+length(b)-interseccion
  return(interseccion/union)
}

