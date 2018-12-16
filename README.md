
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Convex Optimization in R by convexjlr

[![Travis-CI Build
Status](https://travis-ci.org/Non-Contradiction/convexjlr.svg?branch=master)](https://travis-ci.org/Non-Contradiction/convexjlr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/Non-Contradiction/convexjlr?branch=master&svg=true)](https://ci.appveyor.com/project/Non-Contradiction/convexjlr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/convexjlr)](https://cran.r-project.org/package=convexjlr)
[![](http://cranlogs.r-pkg.org/badges/convexjlr)](https://cran.r-project.org/package=convexjlr)
[![](https://cranlogs.r-pkg.org/badges/grand-total/convexjlr)](https://cran.r-project.org/package=convexjlr)

`convexjlr` is an `R` package for *Disciplined Convex Programming (DCP)*
by providing a high level wrapper for Julia package
[Convex.jl](https://github.com/JuliaOpt/Convex.jl). The aim is to
provide optimization results rapidly and reliably in `R` once you
formulate your problem as a convex problem. `convexjlr` can solve linear
programs, second order cone programs, semidefinite programs, exponential
cone programs, mixed-integer linear programs, and some other
DCP-compliant convex programs through `Convex.jl`.

## Installation

`convexjlr` is on CRAN now\! To use package `convexjlr`, you first have
to install Julia <https://julialang.org/> on your computer, and then you
can install `convexjlr` just like any other R packages.

Note: `convexjlr` used to support multiple ways to connect to `julia`,
one way was through package `XRJulia` and the other way was to use
package `JuliaCall`. The latter approach was more performant and thus
the default approach. But due to the fact that `XRJulia` doesn’t support
`julia` v0.7 and v1.0 yet, only `JuliaCall` backend is supported
currently.

We hope you use `convexjlr` to solve your own problems. If you would
like to share your experience on using `convexjlr` or have any questions
about `convexjlr`, don’t hesitate to contact me: <cxl508@psu.edu>.

## Quick Example

We will show a short example for `convexjlr` in solving linear
regression problem. To use package `convexjlr`, we first need to attach
it and do the initial setup:

``` r
library(convexjlr)
#> 
#> Attaching package: 'convexjlr'
#> The following object is masked from 'package:base':
#> 
#>     norm
## If you wish to use JuliaCall backend for performance
convex_setup(backend = "JuliaCall")
#> Doing initialization. It may take some time. Please wait.
#> Julia version 1.0.2 at location /Applications/Julia-1.0.app/Contents/Resources/julia/bin will be used.
#> Loading setup script for JuliaCall...
#> Finish loading setup script for JuliaCall.
#> [1] TRUE
```

And this is our linear regression function using `convexjlr`:

``` r
linear_regression <- function(x, y){
    p <- ncol(x)
    ## n is a scalar, you don't have to use J(.) to send it to Julia.
    n <- nrow(x) ## n <- J(nrow(x))
    ## x is a matrix and y is a vector, you have to use J(.) to send them to Julia.
    x <- J(x)
    y <- J(y)
    ## coefficient vector beta and intercept b.
    beta <- Variable(p)
    b <- Variable()
    ## MSE is mean square error.
    MSE <- Expr(sumsquares(y - x %*% beta - b) / n)
    ## In linear regression, we want to minimize MSE.
    p1 <- minimize(MSE)
    cvx_optim(p1)
    list(coef = value(beta), intercept = value(b))
}
```

In the function, `x` is the predictor matrix, `y` is the response we
have. And the `linear_regression` function will return the coefficient
and intercept solved by `cvx_optim`.

Now we can see a little example using the `linear_regression` function
we have just built.

``` r
n <- 1000
p <- 5
## Sigma, the covariance matrix of x, is of AR-1 strcture.
Sigma <- outer(1:p, 1:p, function(i, j) 0.5 ^ abs(i - j))
x <- matrix(rnorm(n * p), n, p) %*% chol(Sigma)
## The real coefficient is all zero except the first, second and fourth elements.
beta0 <- c(5, 1, 0, 2, 0)
y <- x %*% beta0 + 0.2 * rnorm(n)

linear_regression(x, y)$coef
#>              [,1]
#> [1,]  5.003248799
#> [2,]  0.991593361
#> [3,] -0.013118929
#> [4,]  2.008255127
#> [5,]  0.004305963
```

## More Examples

More examples (including using `convexjlr` for Lasso, logistic
regression and Support Vector Machine) can be found in the pakage
vignette or on the github page:
<https://github.com/Non-Contradiction/convexjlr>
