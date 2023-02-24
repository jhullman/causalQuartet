# causalQuartet: A package for generating causal quartets
The average treatment effect can often be best understood in the context of its variation. <a href="http://www.stat.columbia.edu/~gelman/research/unpublished/causal_quartets.pdf">Causal quartets</a> depict the same average treatment effect under different patterns of heterogeneity. As with the famous correlation quartet of Anscombe (1973), causal quartets dramatize the way in which real-world variation can be more complex than simple numerical summaries.

To cite: Gelman, A., Hullman, J., & Kennedy, L. (2023). Causal quartets: Different ways to attain the same average treatment effect.

![Alt text](figures/latent_quartets.png?raw=true "Title")


## Installation

You can install the latest development version from GitHub with these R
commands:

``` r
install.packages("devtools")
devtools::install_github("jhullman/causalQuartet")
```

## Overview

This document provides examples of how to use the package to create quartets of latent effects under random or systematic variation, or of observables consistent with latent effects. The minimim information reqired to generate a causal quartet is an average treatment effect and a set of x observations.

### Latent quartets

#### Random variation
To generate a latent quartet depicting patterns of random variation for an average treatment effect, provide the estimate and a set of x observations representing individual units:

```{r}
ate <- 0.21
x <- c(100:120)
r <- causal_quartet(ate,x)
r
```
![Alt text](figures/latent_random_ate0.21_x100:120.png?raw=true "Latent quartet with systematic variation")

#### Systematic variation

To generate a latent quartet depicting patterns of systematic variation:

```{r}
s <- causal_quartet(ate,x,varType="systematic")
s
```
![Alt text](figures/latent_systematic_ate0.21_x100:120.png?raw=true "Latent quartet with systematic variation")

Customize the y-axis range:

```{r}
s <- causal_quartet(ate,x,y,varType="systematic",yrange=c(-0.5,1))
s
```
![Alt text](figures/latent_systematic_ate0.21_x100:120_yrange.png?raw=true "Latent quartet with systematic variation and custom yrange")

Change the baseline y value against which effects are compared from the default of 0 to another value:

```{r}
s <- causal_quartet(ate,x,y,varType="systematic",yoffset=4,yrange=c(2,6))
s
```
![Alt text](figures/latent_systematic_ate0.21_x100:120_yoffset4_yrange.png?raw=true "Latent quartet with systematic variation and custom yoffset and yrange")



### Observables quartets

To generate a quartet of hypothetical observables depicting patterns of random variation, include a set of control observations along with the average treatment effect and x observations:

#### Random variation

```{r}
y <- seq(0,1,by=0.05)
y <- y + rnorm(length(y), sd=0.1)
ro <- causal_quartet(ate,x,y,obs=TRUE)
ro
```
![Alt text](figures/observables_random_ate0.21_x100:120.png?raw=true "Observables quartet with random variation")

#### Systematic variation

To generate an observables quartet depicting patterns of systematic variation:

```{r}
so <- causal_quartet(ate,x,y,varType="systematic",obs=TRUE)
so
```

![Alt text](figures/observables_systematic_ate0.21_x100:120.png?raw=true "Observables quartet with systematic variation")
