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
    m <- get_m(ate, x)
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
    
    if( yrange_given && ( ( min(c(y_a,y_b,y_c)) < yrange[1] ) || ( max(c(y_a,y_b,y_c))  > yrange[2] ) ) ) {
      warning("Input yrange is not compatible with ATE: Overriding provided yrange.")
      yrange_given <- FALSE
      mins <- c(min(y_a),min(y_b),min(y_c))
      maxs <- c(max(y_a),max(y_b),max(y_c))
      yrange <- c(min(mins), max(maxs))
    }
    #d - intermediate zone
    #previously: y_d <- 1/(1 + exp(0.4*(x-9.2)^2))
    
    m <- get_m(ate,x)
    y_d <- 1/(1 + exp(m*(x-mean(x))^2))
    y_d <- y_d*ate/mean(y_d)
    
    #print(paste("length y_d", length(y_d), sep=" "))
    print(paste("y_d", y_d, sep=" "))

    if(yrange_given){ 
      if(max(y_d) > yrange[2] || min(y_d) < yrange[1] ){
        print("y_d out of range")
        if(ate > 0){ #if its a positive effect, care about not exceeding yrange max
          while( max( (1/(1 + exp(m*(x-mean(x))^2)))*ate/mean(1/(1 + exp(m*(x-mean(x))^2)) ) ) > yrange[2]){
            print(paste("finding new m, previous:", m, sep=" "))
            m <- m*0.5
            print(paste("new m:", m, sep=" "))
            y_d <- 1/(1 + exp(m*(x-mean(x))^2))
            y_d <- y_d*ate/mean(y_d)
          }
        }else{ #negative ATE - care about not exceeding yrange min
          while( min( (1/(1 + exp(m*(x-mean(x))^2)))*ate/mean(1/(1 + exp(m*(x-mean(x))^2)) ) ) < yrange[1]){
            print(paste("finding new m, previous:", m, sep=" "))
            m <- m*0.5
            print(paste("new m:", m, sep=" "))
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
    #test if y_d acheives the ate
    y_d_realized <- sum(y_d)/length(y_d)
    print(paste("y_d realized", y_d_realized, sep=" "))
  } #end else - latent vs observables 
    
  
  
  d <- list(yrange, df[,1], df[,2], df[,3], df[,4])
  names(d) <- c("yr", "y_a", "y_b", "y_c", "y_d")
  return(d)

}
  
