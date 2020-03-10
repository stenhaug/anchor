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

out2 <-
    out %>%
    mutate(bigsim = map2(bigsim, n_dif_items, add_foc_mean))

out$bigsim[[1]]$intuitive_mod[[1]] %>%
    mod_intuitive_to_draws_df() %>%
    draws_df_to_logit_plot() +
    labs(
        x = latex2exp::TeX("$\\tilde{d_j} = \\tilde{b_j}^{ref} - \\tilde{b_j}^{foc}$")
    )

a <- dif_AOAA_OAT(out$bigsim[[1]]$sim[[1]]$data, out$bigsim[[1]]$sim[[1]]$groups)




