.convex <- new.env(parent = emptyenv())
.convex$vars <- 0
.convex$ps <- 0
.convex$exprs <- 0
.convex$status <- FALSE

.check <- function(pkgname){
    command <- paste0('Pkg.installed("', pkgname, '") == nothing')
    !.convex$ev$Eval(command)
}

.install <- function(pkgname){
    command <- paste0('Pkg.add("', pkgname, '") end')
    .convex$ev$Command(command)
    TRUE
}

.check_install <- function(pkgname){
    if (.check(pkgname)) {return(TRUE)}
    if (interactive()) {
        cat(paste0("Do you wish to install Julia package: ", pkgname))
        yesorno <- readline(prompt = "Yes or No")
        yesorno <- match.arg(yesorno, c("No", "Yes"))
        switch(yesorno,
               Yes = {cat("You wish to install the package."); .install(pkgname)},
               No = cat("You do not wish to install the package."))
    }
    return(.check(pkgname))
}

.check_installs <- Vectorize(.check_install)

.start <- function(){
    ## stopifnot(XRJulia::findJulia(test = TRUE))
    if (!.convex$status) {
        if (XRJulia::findJulia(test = TRUE)) {
            .convex$ev <- XRJulia::RJulia()
            .convex$status <- all(.check_installs(c("Convex", "SCS")))
            if (.convex$status) {
                .convex$ev$Command("using Convex")
                .convex$ev$Command("using SCS")
                # passing in verbose=0 to hide output from SCS
                .convex$ev$Command("solver = SCSSolver(verbose=0)")
                .convex$ev$Command("set_default_solver(solver)")
            }
        }
    }
    return(.convex$status)
}

#' Doing the setup for the package convexjlr
#'
#' This function does the setup for the package convexjlr.
#' Firstly it will try to establish the connect to Julia via the XRJulia interface,
#' Secondly it will check for the Julia packages Convex and SCS,
#' if the packages are not found, it will ask user to install them.
#' Finally, it will try to load the Julia packages and do the neccessary initial setup.
#'
#' @export
setup <- function(){
    .start()
}

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
#' x <- Variable(2)
#' b <- J(c(1:2))
#' p <- minimize(sum((x - b) ^ 2))
#' @export
J <- function(x){
    if (.start()) {
        r <- .convex$ev$Send(x)
        structure(x, Jname = r@.Data, proxy = r, class = c(class(x), "shared"))
    }
    else {
        cat("Julia start failed")
        FALSE
    }
}

variable_creator <- function(vtype){
    force(vtype)
    function(size = 1, sign = c("None", "Positive", "Negative")){
        if (.start()) {
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
        else {
            cat("Julia start failed")
            FALSE
        }
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
#' x <- Variable(4)
#' X <- Variable(c(4, 4), sign = "Positive")
#' S <- Semidefinite(4)
#' @name variable_creating
NULL

#' @rdname variable_creating
#' @export
Variable <- variable_creator("Variable")
#' @rdname variable_creating
#' @export
Semidefinite <- variable_creator("Semidefinite")

expr <- function(x, env){
    if (length(x) == 1) {
        if (x == quote(pmax)) {return(quote(max))}
        if (x == quote(pmin)) {return(quote(min))}
        if (x == quote(kronecker)) {return(quote(kron))}
        if (x == quote(t)) {return(quote(transpose))}
        r <- try(eval(x, envir = env), silent = TRUE)
        if (length(attr(r, "Jname")) == 1) {
            return(eval(parse(text = paste0("quote(", attr(r, "Jname"), ")"))))
        }
        if (is.numeric(r) & length(r) == 1) {
            return(r + 0)
        }
        return(x)
    }
    for (i in 1:length(x)) {
        x[[i]] <- expr(x[[i]],env)
    }
    x
}

#' @import magrittr

expr_text <- function(x, env){
    deparse(expr(x, env)) %>%
        join(sep = "") %>%
        gsub(pattern = "\\*", replacement = ".*") %>%
        gsub(pattern = "/", replacement = "./") %>%
        gsub(pattern = "\\^", replacement = ".^") %>%
        gsub(pattern = "%.\\*%", replacement = "*")
}

#' Create expressions to be used for optimization problem creation
#'
#' \code{Expr} create expressions, which can be used later for problem creation.
#'
#' @param x expression to be created.
#'
#' @examples
#' x <- Variable(2)
#' x1 <- Expr(x + 1)
#' @export
Expr <- function(x){
    if (.start()) {
        .convex$exprs <- .convex$exprs + 1
        expression_text <- expr_text(sys.call()[[2]], env = parent.frame())
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
    else {
        cat("Julia start failed")
        FALSE
    }
}

problem_creator <- function(ptype) {
    force(ptype)
    function(...) {
        if (.start()) {
            .convex$ps <- .convex$ps + 1
            problem <- lapply(sys.call()[-1], expr_text, env = parent.frame())
            target <- problem[[1]]
            constraints <- problem[-1]
            ptext <- join(problem)
            ## print(ptext)
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
        else {
            cat("Julia start failed")
            FALSE
        }
    }
}

#' Create optimization problem
#'
#' Create different kinds of optimization problems with
#' targets and constraints.
#'
#' @param ... optimization targets and constraints.
#' @examples
#' x <- Variable(4)
#' b <- J(c(1:4))
#' p <- minimize(sum((x - b) ^ 2), x >= 0, x <= 3)
#' p <- maximize(-sum((x - b) ^ 2), x >= 0, x <= 3)
#' p <- satisfy(sum((x - b) ^ 2) <= 1, x >= 0, x <= 3)
#' @name problem_creating
NULL

#' @rdname problem_creating
#' @export
minimize <- problem_creator("minimize")
#' @rdname problem_creating
#' @export
maximize <- problem_creator("maximize")
#' @rdname problem_creating
#' @export
satisfy <- problem_creator("satisfy")

#' Solve optimization problem
#'
#' \code{cvx_optim} solves optimization problem using Convex.jl.
#'
#' @param p optimization problem to be solved.
#' @return status of optimized problem.
#'
#' @examples
#' x <- Variable()
#' b <- 1
#' p <- minimize(sum((x - b) ^ 2))
#' cvx_optim(p)
#' @export
cvx_optim <- function(p){
    if (.start()) {
        .convex$ev$Call("solve!", attr(p, "proxy"))
        status(p)
    }
    else {
        cat("Julia start failed")
        FALSE
    }
}

#' Add constraints to optimization problem
#'
#' \code{addConstraint} add additional constraints to optimization problem.
#'
#' @param p optimization problem to add constraints.
#' @param ... additional constraints.
#' @return the optimization problem with the additional constraints.
#'
#' @examples
#' x <- Variable(4)
#' b <- J(c(1:4))
#' p <- minimize(sum((x - b) ^ 2))
#' p <- addConstraint(p, x >= 0, x <= 3)
#' @export
addConstraint <- function(p, ...){
    if (.start()) {
        stopifnot(attr(p, "class") == "problem")
        constraints <- lapply(sys.call()[-c(1:2)], expr_text, env = parent.frame())
        ## p.constraints += [x >= 1; x <= 10; x[2] <= 5; x[1] + x[4] - x[2] <= 10]
        command <- paste0(attr(p, "Jname"), ".constraints += [", join(constraints, sep = "; "), "]")
        .convex$ev$Command(command)
        attr(p, "command") <- append(attr(p, "command"), command)
        attr(p, "constraints") <- append(attr(p, "constraints"), constraints)
        p
    }
    else {
        cat("Julia start failed")
        FALSE
    }
}

Jproperty <- function(property){
    force(property)
    function(p){
        if (.start()) {
            stopifnot(length(attr(p, "Jname")) == 1)
            .convex$ev$Eval(paste0(attr(p, "Jname"), ".", property), .get = TRUE)
        }
        else {
            cat("Julia start failed")
            FALSE
        }
    }
}

#' Get properties of optimization problem
#'
#' Get properties of solved optimization problem,
#' like the status of problem (optimal, infeasible and etc.),
#' or the optimal value of the solved optimization problem.
#'
#' @param p optimization problem.
#' @examples
#' x <- Variable(2)
#' b <- J(c(1:2))
#' p <- minimize(sum((x - b) ^ 2))
#' cvx_optim(p)
#' status(p)
#' optval(p)
#' @name property
NULL

#' @rdname property
#' @export
status <- Jproperty("status")
#' @rdname property
#' @export
optval <- Jproperty("optval")

#' Get values of expressions at optimizer
#'
#' \code{Value} returns the values of expressions at optimizer
#' (minimizer, maximizer and etc.).
#'
#' @aliases evaluate
#' @param ... expressions needed to evaluate.
#'
#' @examples
#' x <- Variable(4)
#' b <- J(c(1:4))
#' p <- minimize(sum((x - b) ^ 2))
#' cvx_optim(p)
#' value(x[1] + x[2], x[3] + x[4])
#' @export
value <- function(...){
    if (.start()) {
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
    else {
        cat("Julia start failed")
        FALSE
    }
}

#' @export
evaluate <- value
