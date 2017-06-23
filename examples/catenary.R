library(convexjlr)
setup()

diff_mat <- function(N){
    outer(1:(N - 1), 1:N, function(i, j){(j - i == 1) - (i == j)})
}

catenary <- function(beginx, beginy, endx, endy, N, L){
    x <- Variable(N)
    y <- Variable(N)
    h <- L / (N - 1)
    diff_matrix <- J(diff_mat(N))
    diffx <- Expr(diff_matrix %*% x)
    diffy <- Expr(diff_matrix %*% y)
    p1 <- minimize(sum(y), diffx ^ 2 + diffy ^ 2 <= h ^ 2,
                   x[1] == beginx, x[N] == endx, y[1] == beginy, y[N] == endy)
    cvx_optim(p1)
    list(x = value(x), y = value(y))
}

system.time(sol <- catenary(0, 0, 1, 0, 101, 2))
plot(sol$x, sol$y,type = "l", col = "blue")

a <- 0.22964
curve(a * cosh((x - 0.5) / a) - 1.02603, 0, 1, col = "red", add = TRUE)
grid()
