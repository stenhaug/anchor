out_3_biased_items <-
    tibble(
        pars = rerun(1, create_pars_compensatory_3_biased_items())
    ) %>%
    # sim and fit intuitive
    mutate(
        sim = pars %>% map(~ sim_students_compensatory(pars = ., n_ref = 10000, n_foc = 10000, mean_foc = -1)),
        intuitive_mod = sim %>% map(~ fit_mod_intuitive(.$data, .$groups))
    ) %>%
    # dif: AOAA
    mutate(
        AOAA_status = sim %>% map(~ dif_AOAA(.$data, .$groups)),
        AOAA_mod =
            map2(
                sim,
                AOAA_status,
                ~ mod_flexible(
                    .x$data,
                    .x$groups,
                    which(.y$status == "flex"),
                    se = TRUE
                )
            ),
        AOAA_final_dif = AOAA_mod %>% map(mod_to_final_dif)
    ) %>%
    # dif: AOAA AS
    mutate(
        AOAA_AS_status = map2(sim, AOAA_status, ~ dif_AOAA_AS(.x$data, .x$groups, .y)),
        AOAA_AS_mod =
            map2(
                sim,
                AOAA_AS_status,
                ~ mod_flexible(
                    .x$data,
                    .x$groups,
                    which(.y$status == "flex"),
                    se = TRUE
                )
            ),
        AOAA_AS_final_dif = AOAA_AS_mod %>% map(mod_to_final_dif)
    ) %>%
    # dif: AOAA OAT
    mutate(
        AOAA_OAT_status = map2(sim, AOAA_status, ~ dif_AOAA_OAT(.x$data, .x$groups, .y)),
        AOAA_OAT_mod =
            map2(
                sim,
                AOAA_OAT_status,
                ~ mod_flexible(
                    .x$data,
                    .x$groups,
                    which(.y$status == "flex"),
                    se = TRUE
                )
            ),
        AOAA_OAT_final_dif = AOAA_OAT_mod %>% map(mod_to_final_dif)
    ) %>%
    # dif: tibs cluster
    mutate(
        cluster =
            intuitive_mod %>%
            map(mod_to_final_dif) %>%
            map(~ mutate(., cluster = irt_cluster(difference_in_easy)))
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
