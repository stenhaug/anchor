# this is the full one with the lapply and all
# in the other non-supplement i just run for one value
# to figure out what's going on

data <- out_3_biased_items$sim[[1]]$data
groups <- out_3_biased_items$sim[[1]]$groups

mod <-
    multipleGroup(
        data,
        1,
        itemtype = "Rasch",
        group = groups,
        invariance = c("Item_1", "free_means", "free_var")
    )

coef_1f(mod)

debug(DTF)
undebug(DTF)

DTF(mod)

# Parameters --------------------------------------------------------------
npts <- 1000
theta_lim <- c(-6, 6)

type <- "score"
integration <- "quad"
plot <- "none"
Theta_nodes <- NULL
impute <- FALSE

# Main --------------------------------------------------------------------
theta <- matrix(seq(theta_lim[1L], theta_lim[2L], length.out = npts))
Theta <- thetaComb(theta, mod@Model$nfact)
theta <- matrix(seq(theta_lim[1L], theta_lim[2L], length.out = npts))
Theta <- thetaComb(theta, mod@Model$nfact)
max_score <- sum(mod@Data$mins + mod@Data$K - 1L)
list_scores <- lapply(1L, fn, omod = mod, impute = FALSE,
                        Theta_nodes = Theta_nodes, imputenums = NULL, max_score = max_score,
                        Theta = Theta, plot = plot, integration = integration,
                        theta_lim = theta_lim, type = type)

# Depth 1 -----------------------------------------------------------------
fn <- function(x, omod, impute, imputenums, Theta, max_score,
               Theta_nodes = NULL, plot, integration, theta_lim, type,
               pre.ev, shortpars, longpars) {

    mod <- omod
    if (impute) {
        mod <- imputePars2(MGmod = mod, pre.ev = pre.ev,
                           imputenums = imputenums, shortpars = shortpars,
                           longpars = longpars)
    }
    if (!is.null(Theta_nodes)) {
        if (type == "score") {
            T1 <- expected.test(mod, Theta_nodes, group = 1L,
                                mins = FALSE)
            T2 <- expected.test(mod, Theta_nodes, group = 2L,
                                mins = FALSE)
        }
        D <- T1 - T2
        ret <- c(sDTF. = D)
        return(ret)
    }
    calc_DTFs(mod = mod, Theta = Theta, plot = plot, max_score = max_score, type = type)
}


# Depth 2 -----------------------------------------------------------------
calc_DTFs <- function (mod, Theta, plot, max_score, type) {
    Ds <- matrix(NA, nrow(Theta), 2L)
    colnames(Ds) <- c("score", "null")
    if ("score" %in% type) {
        T1 <- expected.test(mod, Theta, group = 1L, mins = FALSE)
        T2 <- expected.test(mod, Theta, group = 2L, mins = FALSE)
        Ds[, 1L] <- T1 - T2
    }
    if (plot != "none")
        return(c(T1, T2))
    uDTF <- colMeans(abs(Ds))
    uDTF_percent <- uDTF[1L]/max_score * 100
    sDTF <- colMeans(Ds)
    sDTF_percent <- sDTF[1L]/max_score * 100
    ret <- c(sDTF = sDTF[1L], `sDTF(%)` = sDTF_percent, sDTF = sDTF[-1L],
             uDTF = uDTF[1L], `uDTF(%)` = uDTF_percent, uDTF = uDTF[-1L])
    ret[!is.na(ret)]
}


# Depth 3 -----------------------------------------------------------------
expected.test <- function (x, Theta, group = NULL, mins = TRUE, individual = FALSE,
          which.items = NULL){
    if (missing(x))
        missingMsg("x")
    if (missing(Theta))
        missingMsg("Theta")
    pars <- if (is(x, "MultipleGroupClass"))
        x@ParObjects$pars[[group]]@ParObjects$pars
    else x@ParObjects$pars
    K <- extract.mirt(x, "K")
    if (is.null(which.items) || length(x@Internals$CUSTOM.IND)) {
        pick <- 1L:(length(K) + 1L)
        which.items <- 1L:length(K)
    }
    else pick <- c(which.items, length(pars))
    stopifnot(all(pick %in% 1L:(length(K) + 1L)))
    pars <- pars[pick]
    itemloc <- c(1L, 1L + cumsum(K[which.items]))
    MINS <- x@Data$mins[which.items]
    trace <- mirt:::computeItemtrace(pars, Theta, itemloc = itemloc,
                              CUSTOM.IND = x@Internals$CUSTOM.IND)
    if (individual) {
        ret <- sapply(1L:length(MINS), function(item, trace,
                                                itemloc) {
            index <- score <- itemloc[item]:(itemloc[item + 1L] -
                                                 1L)
            score <- score - min(score)
            score %*% t(trace[, index])
        }, trace = trace, itemloc = itemloc)
        if (mins)
            ret <- t(t(ret) + MINS)
    }
    else {
        score <- do.call(c, lapply(K[which.items], function(x) 0L:(x - 1L)))
        ret <- as.numeric(score %*% t(trace))
        if (mins)
            ret <- ret + sum(MINS)
    }
    return(ret)
}

