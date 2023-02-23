# causalQuartet
The average treatment effect can often be best understood in the context of its variation. <a href="http://users.eecs.northwestern.edu/~jhullman/Causal_quartets.pdf">Causal quartets</a> depict the same average treatment effect under different patterns of heterogeneity. As with the famous correlation quartet of Anscombe (1973), causal quartets dramatize the way in which real-world variation can be more complex than simple numerical summaries.

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

#### Systematic variation

To generate a latent quartet depicting patterns of systematic variation:

```{r}
s <- causal_quartet(ate,x,varType="systematic")
s
```


### Observables quartets

To generate a quartet of hypothetical observables depicting patterns of random variation, include a set of control observations along with the averate treatment effect and x observations:

#### Random variation

```{r}
y <- seq(0,1,by=0.05)
y <- y + rnorm(length(y), sd=0.1)
r <- causal_quartet(ate,x,y,obs=TRUE)
r
```


#### Systematic variation

```{r}
s <- causal_quartet(ate,x,y,varType="systematic")
s
```

