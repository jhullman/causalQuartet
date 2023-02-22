#' Generate y observations for a systematic variation causal quartet
#' @param obj An object of class causal quartet.

systematic_quartet.causal_quartet <- function(obj){ 
  x <- attr(obj, "x")
  yrange <- attr(obj, "yrange")
  yrange_given <- attr(obj, "yrange_given")
  space <- attr(obj, "space")
  ate <- attr(obj, "ate")
  y <- attr(obj, "y")
  yoffset <- attr(obj, "yoffset")

  if(space == "observables"){
    
    #a - linear interaction
    y_a_treat <- y + ate + ate*(x - mean(x))/sd(x)
    
    #b - no effect than increasing
    y_b_treat <- log(1+exp(2*(x-mean(x))))
    y_b_treat <- y + y_b_treat*ate/mean(y_b_treat)
        
    #c - plateau
    y_c_treat <- invlogit(x-mean(x))
    y_c_treat <- y + y_c_treat*ate/mean(y_c_treat)
    
    #if provided yrange violated then its not compatible with the ate
    if( yrange_given && exceeds_bounds(c(y_a_treat,y_b_treat,y_c_treat),yrange)) {
      warning("Input yrange is not compatible with ATE: Overriding provided yrange.")
      yrange_given <- FALSE
    }
    
    #d
    m <- get_m(ate, x)
    y_d_treat <- 1/(1 + exp(m*(x-mean(x))^2))
    y_d_treat <- y_d_treat*ate/mean(y_d_treat)
    y_d_treat <- y + y_d_treat
    
    if(yrange_given){ #check if they gave a yrange that is compatible with ATE
      if(exceeds_bounds(y_d_treat,yrange) ){
        if(ate > 0){ #if its a positive effect, don't exceed yrange max
          while( max( y + (1/(1 + exp(m*(x-mean(x))^2)))*ate/mean(1/(1 + exp(m*(x-mean(x))^2)) ) ) > yrange[2]){
            m <- m*0.5
          }
        }else{ #negative ATE - don't exceed yrange min
          while( min( y + (1/(1 + exp(m*(x-mean(x))^2)))*ate/mean(1/(1 + exp(m*(x-mean(x))^2)) ) ) < yrange[1]){
            m <- m*0.5
          } #end while
        } #end else
        y_d_treat <- 1/(1 + exp(m*(x-mean(x))^2))
        y_d_treat <- y_d_treat*ate/mean(y_d_treat)
        y_d_treat <- y + y_d_treat
      } #end else if
    }else{
      mins <- c(min(y_a_treat),min(y_b_treat),min(y_c_treat),min(y_d_treat),min(y))
      maxs <- c(max(y_a_treat),max(y_b_treat),max(y_c_treat), max(y_d_treat),max(y))
      yrange <- c(min(mins), max(maxs))
    }
   
    df <- data.frame(y_a_treat, y_b_treat, y_c_treat, y_d_treat)
    
  }else{ #latent
    
    #a - linear interaction
    y_a <- ate + ate*(x - mean(x))/sd(x)
    y_a <- y_a + yoffset
    
    #b - no effect than increasing
    y_b <- log(1+exp(2*(x-mean(x))))
    y_b <- y_b*ate/mean(y_b)
    y_b <- y_b + yoffset
  
    #c - plateau
    y_c <- invlogit(x-mean(x))
    y_c <- y_c*ate/mean(y_c)
    y_c <- y_c + yoffset
    
    if( yrange_given && exceeds_bounds(c(y_a,y_b,y_c), yrange) ){
      warning("Input yrange is not compatible with ATE: Overriding provided yrange.")
      yrange_given <- FALSE
    }
    
    #d - intermediate zone
    m <- get_m(ate,x)
    y_d <- 1/(1 + exp(m*(x-mean(x))^2))
    y_d <- y_d*ate/mean(y_d)
    y_d <- y_d + yoffset

    if(yrange_given){ 
      if(exceeds_bounds(y_d,yrange)) {
        if(ate > 0){ #if its a positive effect, don't exceed yrange max
          while( max( (1/(1 + exp(m*(x-mean(x))^2)))*ate/mean(1/(1 + exp(m*(x-mean(x))^2)) ) ) > yrange[2]){
            m <- m*0.5
            y_d <- 1/(1 + exp(m*(x-mean(x))^2))
            y_d <- y_d*ate/mean(y_d)
          }
        }else{ #negative ATE - don't exceed yrange min
          while( min( (1/(1 + exp(m*(x-mean(x))^2)))*ate/mean(1/(1 + exp(m*(x-mean(x))^2)) ) ) < yrange[1]){
            m <- m*0.5
            y_d <- 1/(1 + exp(m*(x-mean(x))^2))
            y_d <- y_d*ate/mean(y_d)
          } #end while
        } #end else
      } #end else if
    }else{
      mins <- c(min(y_a),min(y_b),min(y_c), min(y_d))
      maxs <- c(max(y_a),max(y_b),max(y_c), max(y_d))
      yrange <- c(min(mins), max(maxs))
    } #end yrange given
    
    df <- data.frame(y_a, y_b, y_c, y_d)
  } #end else - latent vs observables 
    
  d <- list(yrange, df[,1], df[,2], df[,3], df[,4])
  names(d) <- c("yr", "y_a", "y_b", "y_c", "y_d")
  return(d)

}
  
