library("assertthat")

check <- function(x, y, eps = 10 ^ (-4)){
    all(abs(x - y) < eps)
}

assert_check <- function(x, y, eps = 10 ^ (-4)){
    assert_that(check(x, y, eps = eps))
}