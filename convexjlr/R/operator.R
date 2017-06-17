#' Log of determinant of x
#'
#' Log of determinant of x.
#'
#' @param x input matrix, needs to be positive semidefinite.
#' @export
logdet <- function(x){
    log(det(x))
}

#' Sum of singular values of x
#'
#' Sum of singular values of x.
#'
#' @param x input matrix.
#' @export
nuclearnorm <- function(x){
    sum(svd(x)$d)
}

#' Largest singular value of x
#'
#' Largest singular value of x.
#'
#' @param x input matrix.
#' @export
operatornorm <- function(x){
    max(svd(x)$d)
}

#' Largest eigenvalues of x
#'
#' Largest eigenvalues of x.
#'
#' @param x input matrix.
#' @export
lambdamax <- function(x){
    max(eigen(x, only.values = TRUE)$values)
}

#' Smallest eigenvalues of x
#'
#' Smallest eigenvalues of x.
#'
#' @param x input matrix.
#' @export
lambdamin <- function(x){
    min(eigen(x, only.values = TRUE)$values)
}

#' x^T P^{-1} x
#'
#' x^T P^{-1} x.
#'
#' @param x input vector.
#' @param P input matrix, needs to be positive semidefinite.
#' @export
matrixfrac <- function(x, P){
    sum(x * solve(P, x))
}

#' log(sum(exp(x)))
#'
#' log(sum(exp(x))).
#'
#' @param x input vector.
#' @export
logsumexp <- function(x){
    log(sum(exp(x)))
}

#' sum(-x * log(x))
#'
#' sum(-x * log(x)).
#'
#' @param x input vector or matrix, x > 0.
#' @export
entropy <- function(x){
    sum(-x * log(x))
}

#' log(1 + exp(x))
#'
#' log(1 + exp(x)).
#'
#' @param x input vector.
#' @export
logisticloss <- function(x){
    log(1 + exp(x))
}

#' p-norm of x
#'
#' p-norm of x.
#'
#' @param x input vector.
#' @param p a number greater than 1.
#' @export
norm <- function(x, p){
    sum(x ^ p) ^ (1 / p)
}

#' p-norm of vector representation of x
#'
#' p-norm of vector representation of x.
#'
#' @param x input matrix.
#' @param p a number greater than 1.
#' @export
vecnorm <- function(x, p){
    norm(vec(x), p)
}

#' x^T P x
#'
#' x^T P x.
#'
#' @param x input vector, either x or P must be constant.
#' @param P input matrix, either x or P must be constant, P needs to be semidefinte if x is not constant.
#' @export
quadform <- function(x, P){
    sum(x * (P %*% x))
}

#' Sum of squares of x
#'
#' Sum of squares of x.
#'
#' @param x input vector.
#' @export
sumsquares <- function(x){
    sum(x ^ 2)
}

#' Square of x
#'
#' Square of x.
#'
#' @param x input vector.
#' @export
square <- function(x){
    x ^ 2
}

#' Geometric mean of x and y
#'
#' Geometrix mean of x and y.
#'
#' @param x input vector, x > 0.
#' @param y input vector, y > 0.
#' @export
geomean <- function(x, y){
    sqrt(x * y)
}

#' Huber loss
#'
#' Huber loss.
#'
#' @param x input vector.
#' @param M M >= 1.
#' @export
huber <- function(x, M = 1){
    ifelse(abs(x) <= M, x ^ 2, 2 * M * abs(x) - M ^ 2)
}

#' Vector representation
#'
#' Vector representation of input matrix x.
#'
#' @param x input matrix.
#' @export
vec <- function(x){
    as.vector(x)
}

#' Inner product
#'
#' Inner product of two input vectors.
#'
#' @param x input vector, one input vector need to be constant.
#' @param y input vector, one input vector need to be constant.
#' @export
dot <- function(x, y){
    sum(x * y)
}

#' Inner product of vector representation of two matrices
#'
#' Inner product of vector representation of two input matrices.
#'
#' @param x input matrix, one input matrices need to be constant.
#' @param y input matrix, one input matrices need to be constant.
#' @export
vecdot <- function(x, y){
    dot(vec(x), vec(y))
}

#' Sum of the largest elements
#'
#' Sum of k largest elements of input vector x.
#'
#' @param x input vector.
#' @param k a positive integer.
#' @export
sumlargest <- function(x, k){
    sum(sort(x, decreasing = TRUE)[1:k])
}

#' Sum of the smallest elements
#'
#' Sum of k smallest elements of input vector x.
#'
#' @param x input vector.
#' @param k a positive integer.
#' @export
sumsmallest <- function(x, k){
    sum(sort(x)[1:k])
}

#' Inner product of two vectors after sorted
#'
#' Inner product of two input vectors after sorted.
#'
#' @param x input vector, one input vector needs to be constant.
#' @param y input vector, one input vector needs to be constant.
#' @export
dotsort <- function(x, y){
    dot(sort(x), sort(y))
}

#' Smallest elements
#'
#' Smallest elements of input vector x.
#'
#' @param x input vector.
#' @export
minimum <- function(x){
    min(x)
}

#' Largest elements
#'
#' Largest elements of input vector x.
#'
#' @param x input vector.
maximum <- function(x){
    max(x)
}

#' Trace of matrix
#'
#' Trace of input matrix x.
#'
#' @param x input matrix.
tr <- function(x){
    sum(diag(x))
}

#' Positive parts
#'
#' Positive parts of input vector x.
#'
#' @param x input vector.
pos <- function(x){
    pmax(x, 0)
}

#' Negative parts
#'
#' Negative parts of input vector x.
#'
#' @param x input vector.
neg <- function(x){
    pmax(-x, 0)
}
