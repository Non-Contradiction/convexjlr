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

.start <- function(){
    ## stopifnot(XRJulia::findJulia(test = TRUE))
    if (!.convex$status) {
        message("Doing initialization. It may take some time. Please wait.")
        if (XRJulia::findJulia(test = TRUE)) {
            .convex$ev <- XRJulia::RJulia()
            if (all(.check_installs(c("Convex", "SCS")))) {
                .convex$ev$Command("using Convex")
                .convex$ev$Command("using SCS")
                # passing in verbose=0 to hide output from SCS
                .convex$ev$Command("solver = SCSSolver(verbose=0)")
                .convex$ev$Command("set_default_solver(solver)")
                .convex$status <- .convex$ev$Eval("true")
            }
            else {message("Packages' installation is not successful.")}
        }
        else {message("Julia installation is not found.")}
    }
    .convex$status
}

#' Doing the setup for the package convexjlr
#'
#' This function does the setup for the package convexjlr.
#' Firstly it will try to establish the connect to Julia via the XRJulia interface,
#' Secondly it will check for the Julia packages Convex and SCS,
#' if the packages are not found, it tries to install them into Julia.
#' Finally, it will try to load the Julia packages and do the neccessary initial setup.
#'
#' @examples
#' setup()
#'
#' @export
setup <- function(){
    try(.start(), silent = FALSE)
    .convex$status
}
