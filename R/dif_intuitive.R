fit_mod_intuitive <- function(data, groups){
    multipleGroup(data, 1, itemtype = "Rasch", groups, invariance = "free_var", SE = TRUE, verbose = FALSE)
}

mod_intuitive_to_draws_df <- function(mod){
    par_draws <- MASS::mvrnorm(n = 10000, mu = extract.mirt(mod, "parvec"), Sigma = extract.mirt(mod, "vcov"))

    par_draws <- par_draws[ , str_detect(colnames(par_draws), "d")]

    draws_df <- tibble(run = 1:nrow(par_draws))

    stopifnot(ncol(par_draws) %% 2 == 0)

    n_items <- ncol(par_draws) / 2

    for (i in 1:n_items) {
        draws_df[[paste0("item", i)]] <- par_draws[ , i] - par_draws[ , i + n_items]
    }

    draws_df
}

draws_df_to_logit_plot <- function(draws_df){
    draws_df %>%
        select(-run) %>%
        gather(var, val, factor_key = TRUE) %>%
        mutate(var = as_factor(paste0("Item ", parse_number(as.character(var))))) %>%
        ggplot(aes(x = val, y = var)) +
        ggridges::geom_density_ridges() +
        labs(x = "", y = "")
}

intuitive_mod_to_emlg_summary <- function(intuitive_mod){
    coef_1f(intuitive_mod)$items %>%
        select(item, ref_easy = a_ref_easy, foc_easy = b_foc_easy) %>%
        mutate(difference_in_easy = ref_easy - foc_easy)
}
