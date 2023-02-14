floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)

ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

invlogit <- plogis


get_m <- function(ate,x){ 
  #find m for calculating systematic variation quartet. equation from setting m to 0.4 for 10 values, 0.2 for 20 values, 0.03 for 30
  m <- -0.00009*length(x) -0.0194322*length(x) + 0.624644
  if(ate > 0 && m < 0){
    m = 0 - m
  }
  return(m)
}

random_quartet <- function(obj){
  UseMethod("random_quartet")
}

systematic_quartet <- function(obj){
  UseMethod("systematic_quartet")
}