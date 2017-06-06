source("first.R")

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

ev$Command("y = Semidefinite(2)")
ev$Command("p = maximize(lambdamin(y), trace(y)<=6)")
ev$Command("using SCS")
ev$Command("solve!(p, SCSSolver(verbose=0))")

## Compare the results

assert_check(value(p), ev$Eval("p.optval", .get = TRUE))
