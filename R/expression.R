#' Create expressions to be used for optimization problem creation
#'
#' \code{Expr} create expressions, which can be used later for problem creation.
#'
#' @param x expression to be created.
#'
#' @examples
#' if (convex_setup()) {
#'     x <- Variable(2)
#'     x1 <- Expr(x + 1)
#' }
#' @export
Expr <- function(x){
    .convex$exprs <- .convex$exprs + 1
    expression_text <- expr_text(substitute(x), env = parent.frame())
    ## print(ptext)
    Jname <- paste0("EX_", .convex$exprs)
    command <- paste0(Jname, " = ", expression_text)
    ## print(command)
    .convex$ev$Command(command)
    structure(Jname, expr = expression_text,
              command = command,
              Jname = Jname,
              proxy = .convex$ev$Eval(Jname),
              class = "expr")
}
