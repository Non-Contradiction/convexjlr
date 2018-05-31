problem_creator <- function(ptype) {
    force(ptype)
    function(...) {
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
                  class = "problem")
    }
}

#' Create optimization problem
#'
#' Create different kinds of optimization problems with
#' targets and constraints.
#'
#' @param ... optimization targets and constraints.
#' @examples
#' \dontrun{
#'     convex_setup()
#'     x <- Variable(4)
#'     b <- J(c(1:4))
#'     p <- minimize(sum((x - b) ^ 2), x >= 0, x <= 3)
#'     p <- maximize(-sum((x - b) ^ 2), x >= 0, x <= 3)
#'     p <- satisfy(sum((x - b) ^ 2) <= 1, x >= 0, x <= 3)
#' }
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
#' @param solver convex problem solver to be used. Currently `convexjlr` supports `SCS` and `ECOS`,
#'   with `SCS` solver as the default.
#' @param ... the optional solver options, like the maximal iteration times.
#'
#' @return status of optimized problem.
#'
#' @examples
#' \dontrun{
#'     convex_setup()
#'     x <- Variable()
#'     b <- 1
#'     p <- minimize(sum((x - b) ^ 2))
#'     cvx_optim(p)
#' }
#' @export
cvx_optim <- function(p, solver = c("SCS", "ECOS"), ...){
    solver <- match.arg(solver)
    cmd <- if (solver == "SCS") SCSSolver(...) else ECOSSolver(...)
    .convex$ev$Command(paste0("solve!(", attr(p, "Jname"), ", ", cmd, ")"))
    status(p)
}

SCSSolver <- function(max_iters = 2500, verbose = 1,
                      eps = 1e-3, alpha = 1.8,
                      scale = 5.0, normalize = 1){
    deparse(match.call(), width.cutoff = 500)
}

ECOSSolver <- function(maxit = 100, verbose = 1,
                      feastol = 1e-7, abstol = 1e-7, reltol = 1e-6,
                      festol_inacc = 1e-4, abstol_inacc = 5e-5, reltol_inacc = 5e-5){
    deparse(match.call(), width.cutoff = 500)
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
#' \dontrun{
#'     convex_setup()
#'     x <- Variable(4)
#'     b <- J(c(1:4))
#'     p <- minimize(sum((x - b) ^ 2))
#'     p <- addConstraint(p, x >= 0, x <= 3)
#' }
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
    function(p){
        stopifnot(length(attr(p, "Jname")) == 1)
        .convex$ev$Eval(paste0(attr(p, "Jname"), ".", property), .get = TRUE)
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
#' \dontrun{
#'     convex_setup()
#'     x <- Variable(2)
#'     b <- J(c(1:2))
#'     p <- minimize(sum((x - b) ^ 2))
#'     cvx_optim(p)
#'     status(p)
#'     optval(p)
#' }
#' @name property
NULL

#' @rdname property
#' @export
status <- Jproperty("status")
#' @rdname property
#' @export
optval <- Jproperty("optval")
