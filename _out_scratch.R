# test run ----------------------------------------------------------------
library(furrr)
options(future.fork.enable = TRUE)
plan(multiprocess)

out <-
    tibble(n_dif_items = c(4, 6, 8)) %>%
    mutate(
        bigsim = n_dif_items %>% future_map(~ bigsim(runs = 2, n_dif_items = .))
    )

Sys.time()

# dive into test ----------------------------------------------------------
out$bigsim[[1]]$intuitive_mod[[1]] %>%
    mod_intuitive_to_draws_df() %>%
    draws_df_to_logit_plot() +
    labs(
        x = latex2exp::TeX("$\\tilde{d_j} = \\tilde{b_j}^{ref} - \\tilde{b_j}^{foc}$")
    )

out$bigsim[[1]]$pars[[1]]

out$bigsim[[3]]$AOAA_OAT_final_dif[[1]] %>% View()

debug(dif_AOAA_OAT)

a <-
    dif_AOAA_OAT(
        data = out$bigsim[[3]]$sim[[1]]$data,
        groups = out$bigsim[[3]]$sim[[1]]$groups
    )


