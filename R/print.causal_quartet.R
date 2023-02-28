#' Print method for a causal quartet
#'
#' @param obj An object of class causal quartet.
#' @examples
#' ate <- 0.5
#' x <- c(100:120)
#' r <- causal_quartet(ate,x) 
#' r
#' @export


print.causal_quartet <- function(obj) {
  par(mfrow=c(2,2), mar=c(3,3,2,2), mgp=c(1.7,.5,0), tck=-.01)
  xrange <- attr(obj, "xrange")
  yrange <- attr(obj, "yrange")
  x <- attr(obj, "x")
  y <- attr(obj, "y")
  if(attr(obj, "space") == "observables"){
    quartet_plot_obs(x, y, obj$a, xrange, yrange, main="(a)")
    quartet_plot_obs(x, y, obj$b, xrange, yrange, main="(b)")
    quartet_plot_obs(x, y, obj$c, xrange, yrange, main="(c)")
    quartet_plot_obs(x, y, obj$d, xrange, yrange, main="(d)")
  }else{
    quartet_plot_latent(x, obj$a, xrange, yrange, main="(a)")
    quartet_plot_latent(x, obj$b, xrange, yrange, main="(b)")
    quartet_plot_latent(x, obj$c, xrange, yrange, main="(c)")
    quartet_plot_latent(x, obj$d, xrange, yrange, main="(d)")
  }
}
