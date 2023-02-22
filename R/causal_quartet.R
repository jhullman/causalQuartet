#' Generate a causal quartet (constructor function) 
#'
#' @param ate A scalar.
#' @param x A vector.
#' @param y A vector. 
#' @param yrange A vector containing the minimum and maximum x value for the y-axis.
#' @param yoffset A scalar indicating the baseline effect to compare to. Default=0
#' @param obs  Boolean indicating whether quartet should show observables. Default=FALSE
#' @param varType The type of variation (random or systematic) to display. Default="random"
#' @examples
#' ate <- 0.1 
#' x <- c(100:120)
#' r <- causal_quartet(ate,x) #generate a latent causal quartet with random variation
#' 
#' yrange <- c(-0.5, 0.5)
#' r <- causal_quartet(ate,x,"yrange"=yrange) 
#' 
#' y <- seq(0, 1, by=0.05)
#' y <- y + rnorm(n=length(y), sd=0.02)
#' ro <- causal_quartet(ate,x,y,obs="TRUE")
#' 
#' s <- causal_quartet(ate,x,y,obs="TRUE", varType="systematic") 

causal_quartet <- function(ate, x, y=NULL, yrange=NULL, yoffset=0, obs=FALSE, varType="random"){   
#causal_quartet <- function(ate, x, y=NULL, yrange=NULL, yoffset=0, qType="latent", varType="random"){   
    #initialize the object
    q_data <- structure(list(), class = "causal_quartet")  
    attr(q_data, "ate") <- ate
    attr(q_data, "x") <- x
    attr(q_data, "yoffset") <- yoffset
    if(yoffset != 0 && obs==TRUE){
      warning("Provided yoffset cannot be used for an observables plot: Ignoring yoffset.")
    }
    
    #set xrange
    xrange <- c(x[1], x[length(x)]) 
    attr(q_data, "xrange") <- xrange
    
    if(missing(yrange)) {
      attr(q_data, "yrange") <- FALSE
      attr(q_data, "yrange_given") <- FALSE
    }else{
      attr(q_data, "yrange") <- yrange
      attr(q_data, "yrange_given") <- TRUE
    }
    
   # }else if(qType=="latent"){
    #   attr(q_data, "space") <- "latent"
    #}
    
    #if(qType=="obs"){
    if(obs==TRUE){
      attr(q_data, "y") <- y
      attr(q_data, "space") <- "observables"
      
      if(varType=="systematic") {
        attr(q_data, "variation") <- "systematic"
        l <- systematic_quartet(q_data)
      }else{ #random
        attr(q_data, "variation") <- "random"
        l <- random_quartet(q_data)
      }
      
    }else{ #obs==FALSE  
    #}else if(qType=="latent"){
      attr(q_data, "space") <- "latent"

      if(varType=="systematic") {
        attr(q_data, "variation") <- "systematic"
        l <- systematic_quartet(q_data)
      }else{ #random
        attr(q_data, "variation") <- "random"
        l <- random_quartet(q_data)
      }

  #  }else{ #both
  #    attr(q_data, "y") <- y
      
  #    if(varType=="systematic") {
        
  #      attr(q_data, "variation") <- "systematic"
        #first generate latent
  #      attr(q_data, "space") <- "latent"
  #      l <- systematic_quartet(q_data)
  #      attr(q_data, "space") <- "observables"
  #      lo <- systematic_quartet(q_data, l)
  #      attr(q_data, "space") <- "both"
        
  #    }else{ #random
        
  #      attr(q_data, "variation") <- "random"
        #first generate latent
  #      l <- random_quartet(q_data)
  #      attr(q_data, "space") <- "observables"
  #      lo <- random_quartet(q_data, l)
  #      attr(q_data, "space") <- "both"
        
  #    }
      
    }
    
    attr(q_data, "yrange") <- l$yr
    q_data$a <- l$y_a
    q_data$b <- l$y_b
    q_data$c <- l$y_c
    q_data$d <- l$y_d
   
  #  if(qType=="both"){
  #    q_data$a2 <- lo$y_a
  #    q_data$b2 <- lo$y_b
  #    q_data$c2 <- lo$y_c
  #    q_data$d2 <- lo$y_d
  #   }
    
  q_data 
}

