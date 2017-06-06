library("XRJulia")
source("check.R")
ev <- RJulia()
ev$Command("using Convex")

join <- function(ls, sep = ", "){
    do.call(paste, append(ls, list(sep = sep)))
}

vars <- 0

tuple <- function(x){
    structure(x, class = "tuple")
}

as.character.tuple <- function(x){
    paste0("(", join(x), ")")
}

J <- function(x){
    r <- ev$Send(x)
    structure(x, Jname = r@.Data,
              proxy = r,
              class = "shared")
}

variable_creator <- function(vtype){
    force(vtype)
    function(size = 1, sign = c("None", "Positive", "Negative")){
        vars <<- vars + 1
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
        command <- paste0("x", vars, " = ", vtype, "(", tuple(size), sign_text, ")")
        ev$Command(command)
        structure(vars, size = size, 
                  Jname = paste0("x", vars), 
                  proxy = ev$Eval(paste0("x", vars)), 
                  class = "variable")
    }
}

Variable <- variable_creator("Variable")
Semidefinite <- variable_creator("Semidefinite")

expr <- function(x){
    if (length(x) == 1) {
        r <- try(eval(x, envir = .GlobalEnv), silent = TRUE)
        ## print(r)
        if (length(attr(r, "Jname")) == 1) {
            return(eval(parse(text = paste0("quote(", attr(r, "Jname"), ")"))))
        }
        return(x)
    }
    for (i in 1:length(x)) {
        x[[i]] <- expr(x[[i]])
    }
    x
}

ps <- 0

problem_creator <- function(ptype) {
    force(ptype)
    function(...) {
        ps <<- ps + 1
        problem <- lapply(sys.call()[-1], expr)
        target <- problem[[1]]
        constraints <- problem[-1]
        ptext <- join(lapply(problem, deparse))
        ## print(text)
        command <- paste0("p", ps, " = ", ptype, "(", ptext, ")")
        ## print(command)
        ev$Command(command)
        structure(ps, problem_type = ptype,
                  target = target,
                  constraints = constraints,
                  command = command,
                  name = paste0("p", ps), 
                  proxy = ev$Eval(paste0("p", ps)), 
                  class = "problem")
    }
}

minimize <- problem_creator("minimize")
maximize <- problem_creator("maximize")
satisfy <- problem_creator("satisfy")

solve.problem <- function(p){
    ev$Call("solve!", attr(p, "proxy"))
}

addConstraint <- function(p, ...){
    stopifnot(attr(p, "class") == "problem")
    constraints <- lapply(sys.call()[-c(1:2)], expr)
    ## p.constraints += [x >= 1; x <= 10; x[2] <= 5; x[1] + x[4] - x[2] <= 10]
    command <- paste0(attr(p, "name"), ".constraints += [", join(lapply(constraints, deparse), sep = "; "), "]")
    ev$Command(command)
    attr(p, "command") <- append(attr(p, "command"), command)
    attr(p, "constraints") <- append(attr(p, "constraints"), constraints)
    p
}

value <- function(...) UseMethod("value")

value.default <- function(...){
    original_exprs <- sys.call()[-1]
    if (length(original_exprs) > 1) {
        exprs <- lapply(original_exprs, expr)
        commands <- paste0("evaluate(", lapply(exprs, deparse), ")")
        names(commands) <- lapply(original_exprs, deparse)
        lapply(commands, ev$Eval, .get = TRUE)
    }
    else {
        expr <- expr(original_exprs[[1]])
        command <- paste0("evaluate(", deparse(expr), ")")
        ev$Eval(command, .get = TRUE)
    }
}

value.problem <- function(p){
    command <- paste0("evaluate(", deparse(attr(p, "target")), ")")
    ev$Eval(command, .get = TRUE)
}

evaluate <- value