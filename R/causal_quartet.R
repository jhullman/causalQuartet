#' Generate a causal quartet
#'
#' @param ate A scalar
#' @param x A vector
#' @param y A vector 
#' @param yrange A vector containing the minimum and maximum x value for the y-axis. Default=NULL
#' @param yoffset A scalar indicating the baseline effect to compare to. Default=0
#' @param obs  Boolean indicating whether quartet should show observables. Default=FALSE
#' @param vartype The type of variation (random or systematic) to display. Default="random"
#' @param matchlq A causal quartet object a latent quartet, for use in generating a corresponding observables quartet. Default=NULL
#' @returns 
#' A list object of class causal_quartet. The object has the following elements:
#' \itemize{
#' \item{"x"}{ Contains user-provided x observations}
#' \item{"y"}{ Contains user-provided control y observations for an observables quartet}
#' \item{"a"}{ Contains y values for subplot a. If an observables quartet, these are y values for the treatment condition}
#' \item{"b"}{ Contains y values for subplot b. If an observables quartet, these are y values for the treatment condition}
#' \item{"c"}{ Contains y values for subplot c. If an observables quartet, these are y values for the treatment condition}
#' \item{"d"}{ Contains y values for subplot d. If an observables quartet, these are y values for the treatment condition}
#' }
#' The object has the following attributes:
#' \itemize{
#' \item{"ate"}{ The user-provided average treatment effect}
#' \item{"yoffset"}{ The baseline y value indicating no effect in a latent quartet}
#' \item{"xrange"}{ The x-axis range (min and max)}
#' \item{"yrange"}{ The y-axis range (min and max)}
#' \item{"yrange_given"}{ Indicates whether the user specified a y-axis range}
#' \item{"space"}{ Whether the quartet plots latent effects (latent) or hypothetical observables (observables)}
#' \item{"variation"}{ Whether the variation is random or systematic}
#' }
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
#' s <- causal_quartet(ate,x,y,obs="TRUE", vartype="systematic") 
#' @export

causal_quartet <- function(ate, x, y=NULL, yrange=NULL, yoffset=0, obs=FALSE, vartype="random", matchlq=NULL){   
  
    #initialize the object
    q_data <- structure(list(), class = "causal_quartet")  
    attr(q_data, "ate") <- ate
    attr(q_data, "x") <- x
    attr(q_data, "yoffset") <- yoffset
    attr(q_data, "matchlq_given") <- FALSE
    if(yoffset != 0 && obs==TRUE){
      warning("Provided yoffset cannot be used for an observables plot: Ignoring yoffset.")
    }
    
    if(!missing(matchlq)){
      if(obs==FALSE){
        warning("Cannot provide a latent quartet to match unless generating an observables quartet. Ignoring matchlq.")
      }else{
        attr(q_data, "matchlq_given") <- TRUE
        q_data$l_b <- matchlq$b
        q_data$l_c <- matchlq$c
        q_data$l_d <- matchlq$d
      }
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
    
    
    if(obs==TRUE){
      if(missing(y)){
        stop("Missing y argument: Must provide control y observations for observables quartet.")
      }
      attr(q_data, "y") <- y
      attr(q_data, "space") <- "observables"
      
      if(vartype=="systematic") {
        attr(q_data, "variation") <- "systematic"
        l <- systematic_quartet(q_data)
      }else{ #random
        attr(q_data, "variation") <- "random"
        l <- random_quartet(q_data)
      }
      #remove y attribute since y obs now contained in l
      attr(q_data, "y") <- NULL
      #remove the matching latent quartet info in case one was provided
      l$l_b <- NULL
      l$l_c <- NULL
      l$l_d <- NULL
      
    }else{ #obs==FALSE  
      attr(q_data, "space") <- "latent"

      if(vartype=="systematic") {
        attr(q_data, "variation") <- "systematic"
        l <- systematic_quartet(q_data)
      }else{ #random
        attr(q_data, "variation") <- "random"
        l <- random_quartet(q_data)
      }
    }
    
    #remove x attribute since now contained in l
    attr(q_data, "x") <- NULL
    
    attr(q_data, "yrange") <- l$yr
    q_data$x <- l$x
    q_data$y <- l$y
    q_data$a <- l$y_a
    q_data$b <- l$y_b
    q_data$c <- l$y_c
    q_data$d <- l$y_d
   
    
  q_data 
}

