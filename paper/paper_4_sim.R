# (higher values of b_target are associated with less dif)

# out_noncomp$intuitive_mod[[1]] %>%
#     mod_intuitive_to_draws_df() %>%
#     draws_df_to_logit_plot()

out <-
    tibble(n_dif_items = 3:10) %>%
    mutate(
        bigsim = n_dif_items %>% map(~ bigsim(runs = 3, n_dif_items = .))
    )
