#' Generate y observations for a systematic variation causal quartet
#' @param obj An object of class causal quartet.

systematic_quartet.causal_quartet <- function(obj){ 
  x <- attr(obj, "x")
  yrange <- attr(obj, "yrange")
  yrange_given <- attr(obj, "yrange_given")
  print(paste("yrange", yrange, sep=" "))
  space <- attr(obj, "space")
  ate <- attr(obj, "ate")
  y <- attr(obj, "y")
  yoffset <- attr(obj, "yoffset")
  print(paste("ate", ate, sep=" "))
  
  if(space == "observables"){
    #a - linear interaction
    y_a_treat <- y + ate + ate*(x - mean(x))/sd(x)
    
    #b - no effect than increasing
    y_b_treat <- log(1+exp(2*(x-mean(x))))
    y_b_treat <- y + y_b_treat*ate/mean(y_b_treat)
        
    #c - plateau
    y_c_treat <- invlogit(x-mean(x))
    y_c_treat <- y + y_c_treat*ate/mean(y_c_treat)
    
    #d
    m <- get_m(x)
    y_d_offset <- 1/(1 + exp(0.4*(x-mean(x))^2))
    #print(paste("length y_d", length(y_d), sep=" "))
    y_d_offset <- y_d_offset*ate/mean(y_d_offset)
    y_d_treat <- y + y_d_offset
    #print(paste("length y_d", length(y_d), sep=" "))
    print(paste("y_d_treat", y_d_treat, sep=" "))
  
    df <- data.frame(y_a_treat, y_b_treat, y_c_treat, y_d_treat)
    
    mins <- c(min(y_a_treat),min(y_b_treat),min(y_c_treat),min(y_d_treat),min(y))
    maxs <- c(max(y_a_treat),max(y_b_treat),max(y_c_treat), max(y_d_treat),max(y))
    
  }else{
    #a - linear interaction
    y_a <- ate + ate*(x - mean(x))/sd(x)
    
  
    #b - no effect than increasing
    y_b <- log(1+exp(2*(x-mean(x))))
    y_b <- y_b*ate/mean(y_b)
  
    #c - plateau
    y_c <- invlogit(x-mean(x))
    y_c <- y_c*ate/mean(y_c)
  
    #d - intermediate zone
    #previously: y_d <- 1/(1 + exp(0.4*(x-9.2)^2))
  
    m <- get_m(x)
    y_d <- 1/(1 + exp(0.4*(x-mean(x))^2))
    #print(paste("length y_d", length(y_d), sep=" "))
    y_d <- y_d*ate/mean(y_d)
    #print(paste("length y_d", length(y_d), sep=" "))
    print(paste("y_d", y_d, sep=" "))
    
    df <- data.frame(y_a, y_b, y_c, y_d)
    
    mins <- c(min(y_a),min(y_b),min(y_c), min(y_d))
    maxs <- c(max(y_a),max(y_b),max(y_c), max(y_d))

    
  }
  
  yrange <- c(min(mins), max(maxs))
  d <- list(yrange, df[,1], df[,2], df[,3], df[,4])
  names(d) <- c("yr", "y_a", "y_b", "y_c", "y_d")
  return(d)

}
  
