source("first.R")

## The original Julia version

# x = Variable(4)
# p = satisfy(norm(x) <= 100, exp(x[1]) <= 5, x[2] >= 7, geomean(x[3], x[4]) >= x[2])
# solve!(p, SCSSolver(verbose=0))
# println(p.status)
# x.value

## The R version with convexjl.R

x <- Variable(4)
p <- satisfy(norm(x) <= 100, exp(x[1]) <= 5, x[2] >= 7, geomean(x[3], x[4]) >= x[2])
solve(p)

## The R version with XRJulia directly

ev$Command("x = Variable(4)")
ev$Command("p = satisfy(norm(x) <= 100, exp(x[1]) <= 5, x[2] >= 7, geomean(x[3], x[4]) >= x[2])")
ev$Command("using SCS")
ev$Command("solve!(p, SCSSolver(verbose=0))")

## Compare the results

assert_check(value(x), ev$Eval("x.value", .get = TRUE))
