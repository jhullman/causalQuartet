systematic_causal_quartet <- function(ate, x, y=NULL, yrange=NULL, yoffset=0, obs=FALSE){
    
  xrange <- c(x[1], x[length(x)])
  print(paste("x", x, sep=" "))
  print(paste("xrange", xrange, sep=" "))
  
  if(missing(yrange)) {
      yrange_given <- FALSE
  }else{
    yrange_given <- TRUE
  }
  
  if(obs){ #OBSERVABLES
    
    y_a_treat <- y + ate + ate*(x - mean(x))/sd(x)
  
    y_b_treat <- log(1+exp(2*(x-mean(x))))
    y_b_treat <- y + y_b_treat*ate/mean(y_b_treat)
        
    y_c_treat <- invlogit(x-mean(x))
    y_c_treat <- y + y_c_treat*ate/mean(y_c_treat)
    
    m <- getM(x)
    y_d_offset <- 1/(1 + exp(0.4*(x-mean(x))^2))
    y_d_offset <- y_d_offset*ate/mean(y_d_offset)
    y_d_treat <- y + y_d_offset
  
    q_data <- list(y_a_treat, y_b_treat, y_c_treat, y_d_treat)
    attr(q_data, "space") <- "observables"
    attr(q_data, "y") <- y
    
    mins <- c(min(y_a_treat),min(y_b_treat),min(y_c_treat),min(y_d_treat),min(y))
    maxs <- c(max(y_a_treat),max(y_b_treat),max(y_c_treat), max(y_d_treat),max(y))
  
    
  }else{ #LATENT
    y_a <- ate + ate*(x - mean(x))/sd(x)
  
    y_b <- log(1+exp(2*(x-mean(x))))
    y_b <- y_b*ate/mean(y_b)
  
    y_c <- invlogit(x-mean(x))
    y_c <- y_c*ate/mean(y_c)
  
    m <- getM(x)
    y_d <- 1/(1 + exp(0.4*(x-mean(x))^2))
    y_d <- y_d*ate/mean(y_d)
    
    q_data <- list(y_a, y_b, y_c, y_d)
    attr(q_data, "space") <- "latent"
    
    mins <- c(min(y_a),min(y_b),min(y_c), min(y_d))
    maxs <- c(max(y_a),max(y_b),max(y_c), max(y_d))

  }
  
  yrange <- c(min(mins), max(maxs))
  
  attr(q_data, "variation") <- "systematic"
  attr(q_data, "x") <- x
  attr(q_data, "xrange") <- xrange
  attr(q_data, "yrange") <- yrange
  names(q_data) <- c("a", "b", "c", "d")
  return(q_data)
    
}
