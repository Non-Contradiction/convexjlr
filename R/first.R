.convex <- new.env(parent = emptyenv())
.convex$vars <- 0
.convex$ps <- 0
.convex$exprs <- 0
.convex$objs <- 0
.convex$status <- FALSE

.check <- function(pkgname){
    JuliaCall::julia_installed_package(pkg_name = pkgname) != "nothing"
    # command <- paste0('Pkg.installed("', pkgname, '") == nothing')
    # !.convex$ev$Eval(command)
}

.install <- function(pkgname){
    JuliaCall::julia_install_package(pkg_name = pkgname)
    # command <- paste0('Pkg.add("', pkgname, '")')
    # .convex$ev$Command(command)
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

.start <- function(backend = c("JuliaCall", "XRJulia")){
    backend <- match.arg(backend, c("JuliaCall", "XRJulia"))
    .convex$backend <- backend
    message("Doing initialization. It may take some time. Please wait.")
    ## evaluator initialization
    if (backend == "XRJulia") {
        stop("Only JuliaCall backend is supported currently.")
        # stopifnot(XRJulia::findJulia(test = TRUE))
        # .convex$ev <- XRJulia::RJulia()
    }
    if (backend == "JuliaCall") {
        ## check is JuliaCall is installed
        # if (!requireNamespace("JuliaCall", quietly = TRUE)) {
        #     stop("Package JuliaCall needed for using this backend. Please install it.")
        # }
        .convex$ev <- JuliaCall::julia_setup()
        .convex$ev$Command <- function(cmd) .convex$ev$command(cmd, show_value = FALSE)
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
    if (all(.check_installs(c("Convex", "SCS", "ECOS")))) {
        ## if use JuliaCall backend, use julia_library instead
        if (backend == "JuliaCall") {
            JuliaCall::julia_library("Convex")
            JuliaCall::julia_library("SCS")
            JuliaCall::julia_library("ECOS")
        }
        else {
            stop("Only JuliaCall backend is supported currently.")
            # .convex$ev$Command("using Convex")
            # .convex$ev$Command("using SCS")
            # .convex$ev$Command("using ECOS")
        }
        # passing in verbose=0 to hide output from SCS
        # .convex$ev$Command("solver = SCSSolver(verbose=0);")
        # .convex$ev$Command("set_default_solver(solver);")
        .convex$status <- .convex$ev$Eval("true")
    }
    else {message("Packages' installation is not successful.")}
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
#' @param backend the backend to use, only JuliaCall is supported currently.
#' @param JULIA_HOME the path to julia binary,
#'     if not set, convexjlr will try to use the julia in path.
#'
#' @examples
#' \dontrun{
#' convex_setup()
#' }
#' @export
convex_setup <- function(backend = c("JuliaCall"), JULIA_HOME = NULL){
    options(JULIA_BIN = JULIA_HOME)
    options(JULIA_HOME = JULIA_HOME)
    try(.start(backend = backend), silent = FALSE)
    def_func()
    .convex$status
}

def_func <- function(){
    cmd <- "function diag1(x)
                if length(size(x)) == 2 && size(x, 2) > 1
                    return(diag(x))
                else
                    return(diagm(x))
                end
            end;"

    .convex$ev$Command(cmd)

    cmd <- '@static if VERSION > v"0.6.5" const trace = tr end;'

    .convex$ev$Command(cmd)
}
