random_quartet.causal_quartet <- function(obj){ 
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
    y_a_treat <- y + ate
    
    error <- rnorm(length(x), 0, 2*ate)
    
    if(yrange_given){
      #find the max variance you can have given the provided y range
      y_c_treat <- y + ate + error
      highvar1 <- (yrange[1] - y[which.min(y_c_treat)] - ate)/error[which.min(y_c_treat)]
      highvar2 <-(yrange[2] - y[which.max(y_c_treat)] - ate)/error[which.max(y_c_treat)]
      highvar <- floor_dec( min(highvar1, highvar2), level=2)
      lowvar <- highvar*0.2
      y_b_treat <- y + ate + error*lowvar
      y_c_treat <- y + ate + error*highvar
    
    }else{
      print("missing y range")  
      y_b_treat <- y + ate + error
      y_c_treat <- y + ate + error*3
      yrange <- c(floor_dec(y_c_treat[which.min(y_c_treat)], 2), ceiling_dec(y_c_treat[which.max(y_c_treat)], 2))
      print(paste("yrange", yrange, sep=" "))
    }
    
    y_d_offset <- rep(0, length(x))
    r <- ceiling(length(x)*0.2)
    indices <- sample(1:length(x), r, replace=FALSE)
    y_d_offset[indices] <- rep(1, length(indices))
    y_d_offset <- y_d_offset*ate/mean(y_d_offset)
    y_d_treat <- y + y_d_offset
    if(max(y_d_treat) > yrange[2]){
      y_d_offset <- rep(0, length(x)) #reinitialize y_d
      r <- floor(length(x)*0.5)
      indices <- sample(1:length(x), r, replace=FALSE)
      y_d_offset[indices] <- rep(1, length(indices))
      y_d_offset <- y_d_offset*ate/mean(y_d_offset)
      y_d_treat <- y + y_d_offset
      if(max(y_d_treat) > yrange[2]){
        if(yrange_given) {
          stop(paste("Upper bound of provided y range is too low:", yrange[2], "<", max(y_d_treat), sep=" "))
        }else{
          yrange[2] <- max(y_d_treat)
        }
      }
    }
    if(min(y_d_treat) < yrange[1]) {
      print("min(y_d_treat) < yrange[1]")
      if(yrange_given) {
        stop(paste("Provided y range is not compatible with ATE: lower bound of provided yrange is too high:", yrange[1], ">", min(y_d_treat), sep=" "))
      }else{
        yrange[1] <- min(y_d_treat)
      }
    }
    
    df <- data.frame(y_a_treat, y_b_treat, y_c_treat, y_d_treat) 
    
  }else{
    y_a <- rep(ate+yoffset, length(x))
  
    error <- lm(rnorm(length(x)) ~ x)$resid
    error <- (error - mean(error))/sd(error)  #standardize for # of sds from mean error

    if(yrange_given){
      #find the max variance you can have given the provided y range
      y_c <- ate + yoffset + error
      highvar1 <- (yrange[1] - ate - yoffset)/error[which.min(y_c)]
      highvar2 <- (yrange[2] - ate - yoffset)/error[which.max(y_c)]
      highvar <- floor_dec( min(highvar1, highvar2), level=2)
      lowvar <- highvar*0.2
      y_b <- ate + yoffset + error*lowvar
      y_c <- ate + yoffset + error*highvar
      
    }else{
      print("missing y range")  
      y_b <- ate + yoffset + error*0.05
      print(paste("y_b", y_b, sep=" "))
      y_c <- ate + yoffset + error*0.25
      yrange <- c(floor_dec(y_c[which.min(y_c)], 2), ceiling_dec(y_c[which.max(y_c)], 2))
      print(paste("yrange", yrange, sep=" "))
      
    }
  
    #d
    y_d <- rep(yoffset, length(x))
    #randomly sample 20% 
    r <- ceiling(length(x)*0.2)
    indices <- sample(1:length(x), r, replace=FALSE)
    y_d[indices] <- rep(1, length(indices))
    y_d <- y_d*(ate)/mean(y_d)
    if(max(y_d) > yrange[2]){
      y_d <- rep(yoffset, length(x)) #reinitialize y_d
      r <- floor(length(x)*0.5)
      indices <- sample(1:length(x), r, replace=FALSE)
      y_d[indices] <- rep(1, length(indices))
      y_d <- y_d*(ate+yoffset)/mean(y_d)
    
      if(max(y_d) > yrange[2]){
        if(yrange_given) {
          stop(paste("Upper bound of provided y range is too low:", yrange[2], "<", max(y_d), sep=" "))
        }else{
           yrange[2] <- max(y_d)
        }
      }
    }
    if(min(y_d) < yrange[1]) {
      if(yrange_given) {
        stop(paste("Provided y range is not compatible with ATE: lower bound of provided yrange is too high:", yrange[1], ">", min(y_d), sep=" "))
      }else{
        yrange[1] <- min(y_d)
        
      }
    }
   df <- data.frame(y_a, y_b, y_c, y_d)
  }
  d <- list(yrange, df[,1], df[,2], df[,3], df[,4])
  names(d) <- c("yr", "y_a", "y_b", "y_c", "y_d")
  print(paste("d$y_a", d$y_a, sep=" "))
  #print()
  return(d)
}

