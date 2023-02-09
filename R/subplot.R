#' Plot a latent effect for a causal quartet 
#' 
#' @param x A vector.
#' @param y A vector.
#' @param xr A vector containing the minimum and maximum x value for the x-axis.
#' @param yr A vector containing the minimum and maximum y value for the y-axis.
#' @examples
#' x <- 4:14
#' y <- rep(0.1, length(x))
#' xrange <- c(x[1], x[length(x)])
#' yrange <- c(-0.2, 0.2)
#' quartet_plot_latent(x, y, xrange, yrange, main="(a)")


quartet_plot_latent <- function(x, y, xr, yr, ...){
  cx=1
  if(length(x)>500){
    cx <- 0.3
  }else if(length(x)>200){
    cx <- 0.5
  }
  plot(x, y, ylab="Causal effect", type="n", bty="l", xlim=xr, ylim=yr, ...)
  abline(0,0,col="gray")
  points(x, y, pch=20, col="darkred", cex=cx)
}

#' Plot observables consistent with an effect for a causal quartet 
#' 
#' @param x A vector.
#' @param y_control A vector.
#' @param y_treat  A vector.
#' @param xr A vector containing the minimum and maximum x value for the x-axis.
#' @param yr A vector containing the minimum and maximum y value for the y-axis.
#' @examples
#' x <- 4:14
#' y <- seq(0, 1, by=0.1)
#' y <- y + rnorm(n=length(y), sd=0.02)
#' y2 <- y + 0.1
#' xrange <- c(x[1], x[length(x)])
#' yrange <- c(-0.5, 1.5)
#' quartet_plot_obs(x, y, y_a_treat, xrange, yrange, main="(e)"))

quartet_plot_obs <- function(x, y_control, y_treat, xr, yr, ...){
  cx=1
  if(length(x)>500){
    cx <- 0.3
  }else if(length(x)>200){
    cx <- 0.5
  }
  plot(x, y_control, ylab="Outcome", type="n", bty="l", xlim=xr, ylim=yr, ...)
  points(x, y_control, pch=1, col="#0066CC", cex=cx)
  points(x, y_treat, pch = 4, col="#0066CC", cex=cx*0.75)
}
