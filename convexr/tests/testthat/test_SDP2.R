library(convexr)
context("Semidefinite Programming 2")

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

ev <- XRJulia::RJulia()
ev$Command("using Convex")
ev$Command("x = Variable()")
ev$Command("y = Variable((2, 2))")
ev$Command("p = minimize(x + y[1, 1], isposdef(y), x >= 1, y[2, 1] == 1)")
ev$Command("solve!(p)")

## Compare the results

test_that("Results for example of another semidefinite programming", {
    expect_equal(value(y), ev$Eval("evaluate(y)", .get = TRUE))
})
