floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)

ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

invlogit <- plogis


#find m parameter for calculating systematic variation quartet plot d
get_m <- function(ate,x){ 
  #equation found by setting m to 0.4 for 10 values, 0.2 for 20 values, 0.03 for 30
  m <- -0.00009*length(x) -0.0194322*length(x) + 0.624644
  if(ate > 0 && m < 0){
    m = 0 - m
  }
  return(m)
}

#finds y_d for random variation quartet, where some proportion p of observations have constant non-zero effect
get_y_d <- function(p,ate,x,yoffset){
  y_d <- rep(0, length(x))
  r <- ceiling(length(x)*p)
  indices <- sample(1:length(x), r, replace=FALSE)
  y_d[indices] <- rep(1, length(indices))
  y_d <- y_d*(ate)/mean(y_d) #works when yoffset = 0
  if(yoffset!=0){ #if yoffset is not 0, need to adjust all values by now adding offset
    y_d[-indices] <- rep(yoffset, length(y_d[-indices]))
    for(k in 1:length(indices)){
      y_d[indices[k]] <- y_d[indices[k]] + yoffset
    }
  }
  return(y_d)
}

#takes a one dimensional vector and yrange and checks whether values in vector violate it
exceeds_bounds <- function(y,yr){
  print(y)
  print(yr)
  val = FALSE
  if(max(y) > yr[2] || min(y) < yr[1]){
    val = TRUE
  }
  return(val)
}

random_quartet <- function(obj){
  UseMethod("random_quartet")
}

systematic_quartet <- function(obj){
  UseMethod("systematic_quartet")
}
