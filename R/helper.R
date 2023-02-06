floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)

ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

invlogit <- plogis

get_m <- function(x){ 
  m <- -0.00009*length(x) -0.0194322*length(x) + 0.624644
  return(m)
}

random_quartet <- function(obj){
  UseMethod("random_quartet")
}

systematic_quartet <- function(obj){
  UseMethod("systematic_quartet")
}
