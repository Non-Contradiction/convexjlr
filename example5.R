source("first.R")

## The original Julia version

# x = Variable()
# y = Variable((2, 2))
# # SDP constraints
# p = minimize(x + y[1, 1], isposdef(y), x >= 1, y[2, 1] == 1)
# solve!(p)
# y.value

## The R version with convexjl.R

x <- Variable()
y <- Variable(c(2, 2))
p <- minimize(x + y[1, 1], isposdef(y), x >= 1, y[2, 1] == 1)
solve(p)

## The R version with XRJulia directly

ev$Command("x = Variable()")
ev$Command("y = Variable((2, 2))")
ev$Command("p = minimize(x + y[1, 1], isposdef(y), x >= 1, y[2, 1] == 1)")
ev$Command("solve!(p)")

## Compare the results

assert_check(value(y), ev$Eval("y.value", .get = TRUE))
