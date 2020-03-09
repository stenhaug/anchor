quiet <- function(x){
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}

sigmoid <- function(x){
    tmp <- exp(x) / (1 + exp(x))
    tmp[is.nan(tmp)] <- 1
    tmp
}


