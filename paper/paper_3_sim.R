# NEED TO ADD MORE DIF
# DO AN EXTREME CASE THEN WORK BACK FROM THERE

# (higher values of b_target are associated with less dif)

# out_noncomp$intuitive_mod[[1]] %>%
#     mod_intuitive_to_draws_df() %>%
#     draws_df_to_logit_plot()

library(furrr)
options(future.fork.enable = TRUE)
plan(multiprocess)

out <-
    tibble(n_dif_items = 2:10) %>%
    mutate(
        bigsim = n_dif_items %>% future_map(~ bigsim(runs = 5, n_dif_items = .))
    )

Sys.time()

out %>% write_rds("paper/paper_3_out1.rds")
