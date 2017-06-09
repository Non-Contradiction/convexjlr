library(convexr)
context("Semidefinite Programming")

## The original Julia version

# y = Semidefinite(2)
# p = maximize(lambdamin(y), trace(y)<=6)
# solve!(p, SCSSolver(verbose=0))
# p.optval

## The R version with convexjl.R

y <- Semidefinite(2)
p <- maximize(lambdamin(y), trace(y) <= 6)
solve(p)

## The R version with XRJulia directly

ev <- XRJulia::RJulia()
ev$Command("using Convex")
ev$Command("y = Semidefinite(2)")
ev$Command("p = maximize(lambdamin(y), trace(y)<=6)")
ev$Command("using SCS")
ev$Command("solve!(p, SCSSolver(verbose=0))")

## Compare the results

test_that("Results for example of semidefinite programming", {
    expect_equal(optval(p), ev$Eval("p.optval"))
})
