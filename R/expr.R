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
