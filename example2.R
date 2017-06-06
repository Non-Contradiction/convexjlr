source("first.R")

## The original Julia version

# X = Variable(2, 2)
# y = Variable()
# # X is a 2 x 2 variable, and y is scalar. X' + y promotes y to a 2 x 2 variable before adding them
# p = minimize(vecnorm(X) + y, 2 * X <= 1, X' + y >= 1, X >= 0, y >= 0)
# solve!(p)
# println(round(X.value, 2))
# println(y.value)
# p.optval

## The R version with convexjl.R

X <- Variable(c(2, 2))
y <- Variable()
p <- minimize(vecnorm(X) + y, 2 * X <= 1, transpose(X) + y >= 1, X >= 0, y >= 0)
solve(p)

## The R version with XRJulia directly

ev$Command("X = Variable(2, 2)")
ev$Command("y = Variable()")
ev$Command("p = minimize(vecnorm(X) + y, 2 * X <= 1, X' + y >= 1, X >= 0, y >= 0)")
ev$Command("solve!(p)")

## Compare the results

assert_check(value(p), ev$Eval("p.optval"))
assert_check(value(X), ev$Eval("X.value", .get = TRUE))
assert_check(value(y), ev$Eval("y.value", .get = TRUE))

