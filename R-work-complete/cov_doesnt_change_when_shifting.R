data <- out_3_biased_items$sim[[1]]$data
groups <- out_3_biased_items$sim[[1]]$groups

mod <-
    multipleGroup(
        data,
        1,
        itemtype = "Rasch",
        group = groups,
        invariance = c("free_var")
    )

coef_1f(mod)

mod <-
    multipleGroup(
        data,
        1,
        itemtype = "Rasch",
        group = groups,
        invariance = c("free_var"),
        pars = "values"
    )

mod$value[67] <- 3

mod2 <-
    multipleGroup(
        data,
        1,
        itemtype = "Rasch",
        group = groups,
        invariance = c("free_var"),
        pars = mod
    )

coef_1f(mod2)
