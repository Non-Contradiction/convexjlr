join <- function(ls, sep = ", "){
    do.call(paste, append(ls, list(sep = sep)))
}

tuple_text <- function(x){
    paste0("(", join(x), ")")
}

#' Make a variable to be of Julia's awareness
#'
#' Make a variable to be of Julia's awareness, so it can be further used in
#' the definition of optimization problem.
#'
#' @param x the R object sent to Julia
#' @examples
#' if (setup()) {
#'     b <- J(c(1:2))
#' }
#' @export
J <- function(x){
    r <- .convex$ev$Send(x)
    structure(x, Jname = r@.Data, proxy = r, class = c(class(x), "shared"))
}

variable_creator <- function(vtype){
    force(vtype)
    function(size = 1, sign = c("None", "Positive", "Negative")){
        .convex$vars <- .convex$vars + 1
        sign <- match.arg(sign)
        sign_text <- switch(sign,
                            Positive = ", Positive()",
                            Negative = ", Negative()",
                            None = "")
        Jname <- paste0("X_", .convex$vars)
        command <- paste0(Jname, " = ", vtype, "(", tuple_text(size), sign_text, ")")
        ## print(command)
        .convex$ev$Command(command)
        structure(Jname, size = size,
                  Jname = Jname,
                  proxy = .convex$ev$Eval(Jname),
                  class = "variable")
    }
}

#' Create variable for optimization problem
#'
#' Create variable (vector, matrix, semidefinite matrix and etc.)
#' for optimization problem.
#'
#' @param size variable size.
#' @param sign whether variable is element-wise positive, element-wise negative
#' or neither.
#' @examples
#' if (setup()) {
#'     x <- Variable(4)
#'     X <- Variable(c(4, 4), sign = "Positive")
#'     S <- Semidefinite(4)
#' }
#' @name variable_creating
NULL

#' @rdname variable_creating
#' @export
Variable <- variable_creator("Variable")
#' @rdname variable_creating
#' @export
Semidefinite <- variable_creator("Semidefinite")
