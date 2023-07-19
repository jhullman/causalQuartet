ate <- 0.1 
x <- c(100:120)
set.seed(1737)
causal_quartet(ate,x) #generate a latent causal quartet with random variation

yrange <- c(-0.5, 0.5)
#' r <- causal_quartet(ate,x,"yrange"=yrange) 


test_that("Quartet behaviour hasn't changed", {
skip_if_not_installed("vdiffr")

ate <- 0.1 
x <- c(4:14)
set.seed(1737)
  
  
vdiffr::expect_doppelganger(
  "plot-random",
  causal_quartet(ate,x)
)


vdiffr::expect_doppelganger(
  "plot-systematic",
  causal_quartet(ate,x,varType="systematic")
)

vdiffr::expect_doppelganger(
  "plot-custom-yrange",
  causal_quartet(ate,x,varType="systematic",yrange=c(-0.5,1))
)

vdiffr::expect_doppelganger(
  "plot-custom-basey",
  causal_quartet(ate,x,varType="systematic",yoffset=4,yrange=c(2,6))
)

})



test_that("Observables quartet behaviour hasn't changed", {
  skip_if_not_installed("vdiffr")
  
  ate <- 0.21
  x <- c(100:120)
  set.seed(1737)
  y <- seq(0,1,by=0.05)
  y <- y + rnorm(length(y), sd=0.1)
  
  
  vdiffr::expect_doppelganger(
    "plot-random-obs",
    causal_quartet(ate,x,y,obs = TRUE)
  )
  
  vdiffr::expect_doppelganger(
    "plot-systematic-obs",
    causal_quartet(ate,x,y,varType = "systematic", obs = TRUE)
  )
  
})
