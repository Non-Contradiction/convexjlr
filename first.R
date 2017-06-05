## simple convex.jl example

# x = Variable(4)
# c = [1; 2; 3; 4]
# A = eye(4)
# b = [10; 10; 10; 10]
# p = minimize(dot(c, x)) # or c' * x
# p.constraints += A * x <= b
# p.constraints += [x >= 1; x <= 10; x[2] <= 5; x[1] + x[4] - x[2] <= 10]
# solve!(p)
# println(round(p.optval, 2))
# println(round(x.value, 2))
# println(evaluate(x[1] + x[4] - x[2]))

library("XRJulia")
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

Variable <- function(size = 1){
    vars <<- vars + 1
    command <- paste0("x", vars, " = Variable(", tuple(size), ")")
    ev$Command(command)
    structure(vars, size = size, 
              name = paste0("x", vars), 
              proxy = ev$Eval(paste0("x", vars)), 
              class = "variable")
}

expr <- function(x){
    if (length(x) == 1) {
        r <- try(eval(x, envir = .GlobalEnv), silent = TRUE)
        ## print(r)
        if (length(r) == 1 && length(attr(r, "class")) == 1 && attr(r, "class") == "variable") {
            return(eval(parse(text = paste0("quote(", attr(r, "name"), ")"))))
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
    exprs <- lapply(original_exprs, expr)
    commands <- paste0("evaluate(", lapply(exprs, deparse), ")")
    names(commands) <- lapply(original_exprs, deparse)
    sapply(commands, ev$Eval, .get = TRUE)
}

value.problem <- function(p){
    command <- paste0("evaluate(", deparse(attr(p, "target")), ")")
    ev$Eval(command, .get = TRUE)
}

evaluate <- value