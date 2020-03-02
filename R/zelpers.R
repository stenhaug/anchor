quiet <- function(x){
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}

sigmoid <- function(x){
    exp(x) / (1 + exp(x))
}

