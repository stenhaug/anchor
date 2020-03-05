# NEED TO ADD MORE DIF
# DO AN EXTREME CASE THEN WORK BACK FROM THERE

# (higher values of b_target are associated with less dif)

# out_noncomp$intuitive_mod[[1]] %>%
#     mod_intuitive_to_draws_df() %>%
#     draws_df_to_logit_plot()

out <-
    tibble(n_dif_items = c(4, 6, 8, 10)) %>%
    mutate(
        bigsim = n_dif_items %>% map(~ bigsim(runs = 2, n_dif_items = .))
    )

out %>% write_rds("paper/paper_4_out.rds")



one$intuitive_mod[[2]] %>%
    mod_intuitive_to_draws_df() %>%
    draws_df_to_logit_plot()

n_items <- 20
n_dif_items <- 10
one <- out$bigsim[[n_dif_items - 2]]

one$pars[[1]]

one %>%
    select(ends_with("mod"), -intuitive_mod) %>%
    mutate(run = row_number()) %>%
    gather(method, model, -run) %>%
    mutate(
        foc_mean = model %>% map_dbl(~ coef_1f(.)$ability$b_foc[1])
    ) %>%
    ggplot(aes(x = method, y = foc_mean)) +
    geom_point()

one %>%
    select(ends_with("mod"), -intuitive_mod, -gini_mod, -minbc_mod) %>%
    mutate(run = row_number()) %>%
    gather(method, model, -run) %>%
    mutate(anchor_summary = model %>% map(final_model_to_anchor_summary)) %>%
    select(-run, -model) %>%
    unnest(anchor_summary) %>%
    count(method, true_anchor, selected) %>%
    ggplot(aes(x = selected, y = n)) +
    geom_col() +
    facet_grid(true_anchor ~ method)

