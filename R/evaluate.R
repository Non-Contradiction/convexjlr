#' Get values of expressions at optimizer
#'
#' \code{Value} returns the values of expressions at optimizer
#' (minimizer, maximizer and etc.).
#'
#' @aliases evaluate
#' @param ... expressions needed to evaluate.
#'
#' @examples
#' \dontrun{
#'     convex_setup()
#'     x <- Variable(4)
#'     b <- J(c(1:4))
#'     p <- minimize(sum((x - b) ^ 2))
#'     cvx_optim(p)
#'     value(x[1] + x[2], x[3] + x[4])
#' }
#' @export
value <- function(...){
    original_exprs <- sys.call()[-1]
    if (length(original_exprs) > 1) {
        exprs <- lapply(original_exprs, expr_text, env = parent.frame())
        commands <- paste0("evaluate(", exprs, ")")
        names(commands) <- lapply(original_exprs, deparse)
        lapply(commands, .convex$ev$Eval, .get = TRUE)
    }
    else {
        expr <- expr_text(original_exprs[[1]], env = parent.frame())
        command <- paste0("evaluate(", expr, ")")
        .convex$ev$Eval(command, .get = TRUE)
    }
}

#' @export
evaluate <- value
