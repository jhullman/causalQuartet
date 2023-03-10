% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subplot.R
\name{quartet_plot_latent}
\alias{quartet_plot_latent}
\title{Plot a latent effect for a causal quartet}
\usage{
quartet_plot_latent(x, y, xr, yr, ...)
}
\arguments{
\item{x}{A vector.}

\item{y}{A vector.}

\item{xr}{A vector containing the minimum and maximum x value for the x-axis.}

\item{yr}{A vector containing the minimum and maximum y value for the y-axis.}
}
\description{
Plot a latent effect for a causal quartet
}
\examples{
x <- 4:14
y <- rep(0.1, length(x))
xrange <- c(x[1], x[length(x)])
yrange <- c(-0.2, 0.2)
quartet_plot_latent(x, y, xrange, yrange, main="(a)")
}
