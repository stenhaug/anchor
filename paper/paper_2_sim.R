out_3_biased_items <-
    tibble(
        pars = rerun(1, create_pars_3_biased_items())
    ) %>%
    # sim and fit intuitive
    mutate(
        sim = pars %>% map(~ sim_bias(pars = ., n_ref = 10000, n_foc = 10000, mean_foc = -1)),
        intuitive_mod = sim %>% map(~ fit_mod_intuitive(.$data, .$groups))
    ) %>%
    # dif: all other
    mutate(
        all_other_status = sim %>% map(~ dif_all_other(.$data, .$groups)),
        all_other_mod =
            map2(
                sim,
                all_other_status,
                ~ mod_flexible(
                    .x$data,
                    .x$groups,
                    which(.y$status == "flex"),
                    se = TRUE
                )
            ),
        all_other_final_dif = all_other_mod %>% map(mod_to_final_dif)
    ) %>%
    # dif: iterative backward
    mutate(
        it_back_status = map2(sim, all_other_status, ~ dif_iterative_backward(.x$data, .x$groups, .y)),
        it_back_mod =
            map2(
                sim,
                it_back_status,
                ~ mod_flexible(
                    .x$data,
                    .x$groups,
                    which(.y$status == "flex"),
                    se = TRUE
                )
            ),
        it_back_final_dif = it_back_mod %>% map(mod_to_final_dif)
    ) %>%
    # dif: tibs cluster
    mutate(
        cluster =
            intuitive_mod %>%
            map(mod_to_final_dif) %>%
            map(~ mutate(., cluster = irt_cluster(dif)))
    ) %>%
    mutate(cluster_stats = cluster %>% map(get_cluster_stats)) %>%
    mutate(cluster_status = map2(cluster, cluster_stats, left_join, by = "cluster")) %>%
    mutate(
        cluster_mod =
            map2(
                sim,
                cluster_status,
                ~ mod_flexible(
                    .x$data,
                    .x$groups,
                    which(.y$status == "flex"),
                    se = TRUE
                )
            ),
        cluster_final_dif = cluster_mod %>% map(mod_to_final_dif)
    )
