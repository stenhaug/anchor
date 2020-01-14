mod_to_final_dif <- function(all_other_mod){
    the_coef <- coef_1f(all_other_mod)

    the_coef$items %>%
        mutate(
            item = paste0("item", 1:8),
            dif = b_foc_easy - a_ref_easy + the_coef$ability$b_foc[1]
        ) %>%
        select(item, anchor, dif)
}

# this works for a general model where some are fixed (as opposed to mod intuitive where all flex)
mod_to_draws_df <- function(mod, n){
    par_draws <- MASS::mvrnorm(n, mu = extract.mirt(mod, "parvec"), Sigma = extract.mirt(mod, "vcov"))

    par_draws <-
        par_draws[ , str_detect(colnames(par_draws), "d")] %>%
        as_tibble() %>%
        mutate(id = row_number()) %>%
        gather(var, val, -id) %>%
        mutate(var = str_remove(var, "d\\.")) %>%
        separate(var, c("one", "two"), "\\.", fill = "right") %>%
        gather(var, item, -val, -id) %>%
        na.omit() %>%
        select(-var) %>%
        mutate(item = as.numeric(item)) %>%
        spread(item, val) %>%
        select(-id)

    draws_df <- tibble(run = 1:nrow(par_draws))

    stopifnot(ncol(par_draws) %% 2 == 0)

    n_items <- ncol(par_draws) / 2

    for (i in 1:n_items) {
        draws_df[[paste0("item", i)]] <- par_draws[[i + n_items]] - par_draws[[i]]
    }

    draws_df
}
