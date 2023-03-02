set.seed(27472)
ate <- 0.1 
x <- c(100:120)
yrange <- c(-0.5, 0.5)
y <- seq(0, 1, by=0.05)
y <- y + rnorm(n=length(y), sd=0.02)



test_that("input checks work as expected", {
  expect_warning(
    causal_quartet(ate, x, y, obs = "TRUE", yoffset = 3),
    "Provided yoffset cannot be used for an observables plot: Ignoring yoffset."
  )
  
  expect_error(
    causal_quartet(ate, x, obs = "TRUE"),
    "Missing y argument: Must provide control y observations for observables quartet."
  )
  
  expect_warning(
    causal_quartet(ate,x,yrange = c(0,.05)),
    "Input yrange is not compatible with ATE: Overriding provided yrange."
    )
  
  expect_warning(
    causal_quartet(ate,x, y, obs = "TRUE", yrange = c(0,.05)),
    "Input yrange is not compatible with ATE: Overriding provided yrange."
  )
})
