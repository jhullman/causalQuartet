#' Generate y observations for a random variation causal quartet 
#' @param obj An object of class causal quartet.

random_quartet.causal_quartet <- function(obj){ 
  x <- attr(obj, "x")
  yrange <- attr(obj, "yrange")
  yrange_given <- attr(obj, "yrange_given")
  space <- attr(obj, "space")
  ate <- attr(obj, "ate")
  y <- attr(obj, "y")
  yoffset <- attr(obj, "yoffset")
  
  if(space == "observables"){
    
    #a - constant effect
    y_a_treat <- y + ate

    if(yrange_given && exceeds_bounds(y_a_treat,yrange) ){
      warning("Input yrange is not compatible with ATE: Overriding provided yrange.")
      yrange_given <- FALSE
    }
    
    #b, c - low and high random variation
    error <- rnorm(length(x), 0, 2*ate)
    
    if(yrange_given){
      #find the max variance you can have given the provided y range, use 1/5 for low
      y_c_treat <- y + ate + error
      highvar1 <- (yrange[1] - y[which.min(y_c_treat)] - ate)/error[which.min(y_c_treat)]
      highvar2 <-(yrange[2] - y[which.max(y_c_treat)] - ate)/error[which.max(y_c_treat)]
      highvar <- floor_dec( min(highvar1, highvar2), level=2)
      lowvar <- highvar*0.2
      y_b_treat <- y + ate + error*lowvar
      y_c_treat <- y + ate + error*highvar
    
    }else{ #don't have to scale the error down
      y_b_treat <- y + ate + error
      y_c_treat <- y + ate + error*5
    }
    
    #d small proportion non-zero effects
    p_nonzero <- 0.2
    y_d_shift <- get_y_d(p_nonzero,ate,x,0)
    y_d_treat <- y + y_d_shift
    
    if(yrange_given){
      while(p_nonzero < 0.5 && exceeds_bounds(y_d_treat,yrange)){ #while p is less than max allowable 0.5
        p_nonzero <- p_nonzero + 0.1
        y_d_shift <- get_y_d(p_nonzero,ate,x,0)
        y_d_treat <- y+y_d_shift
      } 
      if(exceeds_bounds(y_d_treat,yrange)){ #while stops when p_nonzero==0.5 or when y_d no longer exceeds bounds - see which it was
        warning("Input yrange is not compatible with ATE: Overriding provided yrange.")
        yrange_given <- FALSE
        mins <- c(min(y_a_treat),min(y_b_treat),min(y_c_treat), min(y_d_treat))
        maxs <- c(max(y_a_treat),max(y_b_treat),max(y_c_treat), max(y_d_treat))
        yrange <- c(min(mins), max(maxs))
      }
    }else{
      mins <- c(min(y_a_treat),min(y_b_treat),min(y_c_treat), min(y_d_treat))
      maxs <- c(max(y_a_treat),max(y_b_treat),max(y_c_treat), max(y_d_treat))
      yrange <- c(min(mins), max(maxs))
    } #end if yrange given
    
    df <- data.frame(x, y, y_a_treat, y_b_treat, y_c_treat, y_d_treat) 
    
  }else{ #latent
    #a - constant effect
    y_a <- rep(ate+yoffset, length(x))
    
    if(yrange_given && exceeds_bounds(y_a,yrange)){
      warning("Input yrange is not compatible with ATE: Overriding provided yrange.")
      yrange_given <- FALSE
    }
  
    #b, c - low and high random variation
    error <- lm(rnorm(length(x)) ~ x)$resid
    error <- (error - mean(error))/sd(error)  #standardize for # of sds from mean error

    if(yrange_given){
      #find the max variance you can have given the provided yrange - take 1/5 for low
      y_c <- ate + yoffset + error
      highvar1 <- (yrange[1] - ate - yoffset)/error[which.min(y_c)]
      highvar2 <- (yrange[2] - ate - yoffset)/error[which.max(y_c)]
      highvar <- floor_dec( min(highvar1, highvar2), level=2)
      lowvar <- highvar*0.2 #low var is one fifth of high var
      y_b <- ate + yoffset + error*lowvar
      y_c <- ate + yoffset + error*highvar
      
    }else{
      y_b <- ate + yoffset + error*0.05
      y_c <- ate + yoffset + error*0.25
    }
  
    #d small proportion non-zero effects
    #proportion with non-zero effect - default is 0.2
    p_nonzero <- 0.2
    y_d <- get_y_d(p_nonzero,ate,x,yoffset)
    #if yrange provided 
    if(yrange_given){
      while(p_nonzero < 0.5 && exceeds_bounds(y_d,yrange)){ #while p is less max allowable 0.5
        p_nonzero <- p_nonzero + 0.1
        y_d <- get_y_d(p_nonzero,ate,x,yoffset)
      } 
      if(exceeds_bounds(y_d,yrange)){ #while stops when p_nonzero==0.5 or when y_d no longer exceeds bounds - see which it was
        warning("Input yrange is not compatible with ATE: Overriding provided yrange.")
        yrange_given <- FALSE
        mins <- c(min(y_a),min(y_b),min(y_c), min(y_d))
        maxs <- c(max(y_a),max(y_b),max(y_c), max(y_d))
        yrange <- c(min(mins), max(maxs))
      }
    }else{
      mins <- c(min(y_a),min(y_b),min(y_c), min(y_d))
      maxs <- c(max(y_a),max(y_b),max(y_c), max(y_d))
      yrange <- c(min(mins), max(maxs))
    } #end if yrange given
    
    y <- rep("NA", length(x))
    df <- data.frame(x, y, y_a, y_b, y_c, y_d)
  } #end latent vs observables
  
  d <- list(yrange, df[,1], df[,2], df[,3], df[,4], df[,5], df[,6])
  names(d) <- c("yr", "x", "y", "y_a", "y_b", "y_c", "y_d")
  return(d)
}

