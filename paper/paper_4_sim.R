tibble(
    pars = rerun(1, create_pars_noncompensatory(n_items = 20, n_dif_items = 8)),
    students =
        rerun(
            1,
            sim_students_2d(
                n_ref = 10,
                n_foc = 10,
                ref_target_ability_mu = 0,
                ref_nuisance_ability_mu = 0,
                foc_target_ability_mu = -0.5,
                foc_nuisance_ability_mu = -1
            )
        )
) %>%
    mutate(
        p = map2(pars, students, pars_students_to_p_noncomp),
        sim = map2(students, p, students_p_to_sim)
    )
