mod_to_draws_df <- function(mod){
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
        gather(var, val) %>%
        ggplot(aes(x = val, y = var)) +
        ggridges::geom_density_ridges() +
        labs(
            x = "Reference group logits - Focal group logits",
            y = "",
            subtitle = "Identify by fixing both means to 0 and let difference show up in item easiness"
        )
}
