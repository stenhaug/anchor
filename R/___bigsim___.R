bigsim <- function(runs, n_dif_items){
    tibble(
        pars = rerun(runs,
                     create_pars_noncompensatory(
                         n_items = 20,
                         n_dif_items = n_dif_items,
                         angle_start = 30,
                         angle_end = 60,
                         b_nuisance_with_dif_sd = 0)),
        students =
            rerun(
                1,
                sim_students_2d(
                    n_ref = 5000,
                    n_foc = 5000,
                    ref_target_ability_mu = 0,
                    ref_nuisance_ability_mu = 0,
                    foc_target_ability_mu = -0.5,
                    foc_nuisance_ability_mu = -1
                )
            )
    ) %>%
        # sim
        mutate(
            p = map2(pars, students, pars_students_to_p_noncomp),
            sim = map2(students, p, students_p_to_sim)
        ) %>%
        # fit intuitive
        mutate(
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
            AOAA_OAT_status = map(sim, ~ dif_AOAA_OAT(.$data, .$groups)),
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
                map(~ mutate(., cluster = irt_cluster_fix3(difference_in_easy)))
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
        ) %>%
        mutate(
            emlg_summary = intuitive_mod %>% map(intuitive_mod_to_emlg_summary),
            gini_results = emlg_summary %>% map(emlg_to_gini_l1, lo = -2, hi = 0),
            gini_mod = map2(gini_results, sim, ~ anchptfocmean_to_final_mod(.x$gini_anchor_points, .y$data, .y$groups))
        ) %>%
        mutate(
            minbc = intuitive_mod %>% map(min_between_curves, 0.25, 1.25, 0.01),
            minbc_mod = map2(minbc, sim, ~ anchptfocmean_to_final_mod(.x$between_curves_anchor_points, .y$data, .y$groups))
        )
}
