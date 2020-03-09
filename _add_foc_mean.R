sim_n_dif_items_to_foc_mean <- function(sim, n_dif_items){
    mod <-
        mod_flexible(
            sim$data,
            sim$groups,
            flex_items = (ncol(sim$data) - n_dif_items + 1):ncol(sim$data)
        )

    coef_1f(mod)$ability$b_foc[1]
}

add_foc_mean <- function(one, n_dif_items){
    one %>%
    mutate(foc_mean = sim %>% map_dbl(sim_n_dif_items_to_foc_mean, n_dif_items))
}

out <-
    out %>%
    mutate(bigsim = map2(bigsim, n_dif_items, add_foc_mean))

out$bigsim[[1]]$foc_mean

out$bigsim[[2]]$foc_mean

out$bigsim[[3]]$foc_mean
