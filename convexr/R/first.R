.convex <- new.env(parent = emptyenv())
.convex$vars <- 0
.convex$ps <- 0

.check_install <- function(pkgname){
    command <- paste0('if Pkg.installed("', pkgname, '") == nothing Pkg.add("', pkgname, '") end')
    .convex$ev$Command(command)
}

.onLoad <- function(libname, pkgname){
    stopifnot(XRJulia::findJulia(test = TRUE))
    if (is.null(.convex$ev)) {
        .convex$ev <- XRJulia::RJulia()
        .check_install("Convex")
        .check_install("SCS")
        .convex$ev$Command("using Convex")
    }
}

join <- function(ls, sep = ", "){
    do.call(paste, append(ls, list(sep = sep)))
}

tuple <- function(x){
    structure(x, class = "tuple")
}

as.character.tuple <- function(x){
    paste0("(", join(x), ")")
}

#' Make a variable to be Julia awared.
#'
#' Make a variable to be Julia awared, so it can be further used in
#' the definition of problem.
#'
#' @examples
#' x <- Variable(4)
#' b <- J(c(1:4))
#' p <- minimize(sum())
#' solve(p)
#' @export
J <- function(x){
    r <- .convex$ev$Send(x)
    structure(x, Jname = r@.Data,
              proxy = r,
              class = c(class(x), "shared"))
}

variable_creator <- function(vtype){
    force(vtype)
    function(size = 1, sign = c("None", "Positive", "Negative")){
        .convex$vars <- .convex$vars + 1
        if (sign[1] == "Positive") {
            sign_text <- ", Positive()"
        }
        else {
            if (sign[1] == "Negative") {
                sign_text <- ", Negative()"
            }
            else {
                sign_text <- ""
            }
        }
        Jname <- paste0("X_", .convex$vars)
        command <- paste0(Jname, " = ", vtype, "(", tuple(size), sign_text, ")")
        .convex$ev$Command(command)
        structure(Jname, size = size,
                  Jname = Jname,
                  proxy = .convex$ev$Eval(Jname),
                  class = "variable")
    }
}

#' @export
Variable <- variable_creator("Variable")
#' @export
Semidefinite <- variable_creator("Semidefinite")

expr <- function(x, env){
    if (length(x) == 1) {
        r <- try(eval(x, envir = env), silent = TRUE)
        if (length(attr(r, "Jname")) == 1) {
            return(eval(parse(text = paste0("quote(", attr(r, "Jname"), ")"))))
        }
        return(x)
    }
    for (i in 1:length(x)) {
        x[[i]] <- expr(x[[i]],env)
    }
    x
}

`%>%` <- magrittr::`%>%`

expr_text <- function(x, env){
    deparse(expr(x, env)) %>%
        gsub(pattern = "\\*", replacement = ".*") %>%
        gsub(pattern = "/", replacement = "./") %>%
        gsub(pattern = "\\^", replacement = ".^") %>%
        gsub(pattern = "%.\\*%", replacement = "*")
}

problem_creator <- function(ptype) {
    force(ptype)
    function(...) {
        .convex$ps <- .convex$ps + 1
        problem <- lapply(sys.call()[-1], expr_text, env = parent.frame())
        target <- problem[[1]]
        constraints <- problem[-1]
        ptext <- join(problem)
        ## print(text)
        Jname <- paste0("P_", .convex$ps)
        command <- paste0(Jname, " = ", ptype, "(", ptext, ")")
        ## print(command)
        .convex$ev$Command(command)
        structure(Jname, problem_type = ptype,
                  target = target,
                  constraints = constraints,
                  command = command,
                  Jname = Jname,
                  proxy = .convex$ev$Eval(Jname),
                  class = "problem")
    }
}

#' @export
minimize <- problem_creator("minimize")
#' @export
maximize <- problem_creator("maximize")
#' @export
satisfy <- problem_creator("satisfy")

#' @export
solve.problem <- function(p){
    .convex$ev$Call("solve!", attr(p, "proxy"))
}

#' @export
addConstraint <- function(p, ...){
    stopifnot(attr(p, "class") == "problem")
    constraints <- lapply(sys.call()[-c(1:2)], expr_text, env = parent.frame())
    ## p.constraints += [x >= 1; x <= 10; x[2] <= 5; x[1] + x[4] - x[2] <= 10]
    command <- paste0(attr(p, "Jname"), ".constraints += [", join(constraints, sep = "; "), "]")
    .convex$ev$Command(command)
    attr(p, "command") <- append(attr(p, "command"), command)
    attr(p, "constraints") <- append(attr(p, "constraints"), constraints)
    p
}

Jproperty <- function(property){
    force(property)
    function(x){
        stopifnot(length(attr(x, "Jname")) == 1)
        .convex$ev$Eval(paste0(attr(x, "Jname"), ".", property), .get = TRUE)
    }
}

#' @export
status <- Jproperty("status")
#' @export
optval <- Jproperty("optval")

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
