.convex <- new.env(parent = emptyenv())
.convex$vars <- 0
.convex$ps <- 0
.convex$exprs <- 0
.convex$objs <- 0
.convex$status <- FALSE

.check <- function(pkgname){
    command <- paste0('Pkg.installed("', pkgname, '") == nothing')
    !.convex$ev$Eval(command)
}

.install <- function(pkgname){
    command <- paste0('Pkg.add("', pkgname, '")')
    .convex$ev$Command(command)
    TRUE
}

.check_install <- function(pkgname){
    if (.check(pkgname)) {return(TRUE)}
    message("convexjlr needs to install Julia package ", pkgname, ".")
    message("It will be installed into Julia.")
    .install(pkgname)
    return(.check(pkgname))
}

.check_installs <- Vectorize(.check_install)

.start <- function(backend = c("XRJulia", "JuliaCall")){
    backend <- match.arg(backend, c("XRJulia", "JuliaCall"))
    .convex$backend <- backend
    message("Doing initialization. It may take some time. Please wait.")
    ## evaluator initialization
    if (backend == "XRJulia") {
        stopifnot(XRJulia::findJulia(test = TRUE))
        .convex$ev <- XRJulia::RJulia()
    }
    if (backend == "JuliaCall") {
        ## check is JuliaCall is installed
        if (!requireNamespace("JuliaCall", quietly = TRUE)) {
            stop("Package JuliaCall needed for using this backend. Please install it.")
        }
        .convex$ev <- JuliaCall::julia_setup()
        .convex$ev$Command <- .convex$ev$command
        .convex$ev$Eval <-
            function(cmd, .get = FALSE) {
                .convex$ev$eval(cmd)
            }
        ## if using earlier version of JuliaCall, we need to define the assign function
        ## to use later.
        if (is.null(.convex$ev$assign)) {
            .convex$ev$Command("function assign(name, x) eval(Main, Expr(:(=), Symbol(name), x)) end")
            .convex$ev$assign <-
                function(x, value) .convex$ev$call("assign", x, value, need_return = FALSE)
        }
    }

    ## Packages
    if (all(.check_installs(c("Convex", "SCS")))) {
        ## if use JuliaCall backend, use julia_library instead
        if (backend == "JuliaCall") {
            JuliaCall::julia_library("Convex")
            JuliaCall::julia_library("SCS")
        }
        else {
            .convex$ev$Command("using Convex")
            .convex$ev$Command("using SCS")
        }
        # passing in verbose=0 to hide output from SCS
        .convex$ev$Command("solver = SCSSolver(verbose=0);")
        .convex$ev$Command("set_default_solver(solver);")
        .convex$status <- .convex$ev$Eval("true")
    }
    else {message("Packages' installation is not successful.")}
    .convex$status
}

#' Doing the setup for the package convexjlr (deprecated)
#'
#' The function is deprecated, should use convex_setup instead.
#'
#' @export
setup <- function(){
    .Deprecated("convex_setup")
    try(.start(), silent = FALSE)
    .convex$status
}

#' Doing the setup for the package convexjlr
#'
#' This function does the setup for the package convexjlr.
#' Firstly it will try to establish the connect to Julia via the XRJulia interface,
#' or try to embed julia in R through JuliaCall.
#' Secondly it will check for the Julia packages Convex and SCS,
#' if the packages are not found, it tries to install them into Julia.
#' Finally, it will try to load the Julia packages and do the necessary initial setup.
#'
#' @param backend whether to use XRJulia or JuliaCall as backend
#' @param JULIA_HOME the path to julia binary,
#'     if not set, convexjlr will try to use the julia in path.
#'
#' @examples
#' \dontrun{
#' convex_setup()
#' }
#' @export
convex_setup <- function(backend = c("XRJulia", "JuliaCall"), JULIA_HOME = NULL){
    options("JULIA_BIN") <- JULIA_HOME
    options("JULIA_HOME") <- JULIA_HOME
    try(.start(backend = backend), silent = FALSE)
    .convex$status
}
