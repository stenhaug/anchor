# create_pars_noncompensatory(n_items = 10, n_dif_items = 5) %>%
#     graph_pars_noncompensatory(0, -1)

angle_to_a_nuisance <- function(angle){
    # uses that cos formula. assumes a_target is 1. see p 949 of walker and sahin
    # angle_to_a_nuisance(30)
    sqrt((1 - cos(angle * pi / 180)^2) / cos(angle * pi / 180)^2)
}

create_pars_noncompensatory <- function(n_items, n_dif_items, angle_start, angle_end, b_nuisance_with_dif_sd){
    # create item parameters. all target disc is 1 and nuisance disc is 0 for non-dif items and has angle that
    # is lattice from 30 to 60 for dif items
    tibble(
        item = 1:n_items,
        dif = c(rep("no", n_items - n_dif_items), rep("yes", n_dif_items))
    ) %>%
        mutate(
            a_target = 1,
            angle = c(rep(0, n_items - n_dif_items), seq(angle_start, angle_end, length.out = n_dif_items)),
            a_nuisance = angle_to_a_nuisance(angle),
            b_target = rnorm(n_items, 0, 0.0001), # CHANGE THIS BACK
            b_nuisance = c(rep(999, n_items - n_dif_items), rnorm(n_dif_items, 0, b_nuisance_with_dif_sd))
        )
}

graph_pars_noncompensatory <- function(pars, ref_mean_ability_nuisance, foc_mean_ability_nuisance){
    # graphs
    crossing(
        item = 1:nrow(pars),
        ability_target = seq(-3, 3, by = 0.1)
    ) %>%
        left_join(pars) %>%
        mutate(
            ref =
                sigmoid(a_target * ability_target + b_target) *
                sigmoid(a_nuisance * ref_mean_ability_nuisance + b_nuisance),
            foc =
                sigmoid(a_target * ability_target + b_target) *
                sigmoid(a_nuisance * foc_mean_ability_nuisance + b_nuisance)
        ) %>%
        dplyr::select(item, ability_target, ref, foc) %>%
        gather(group, prob, -item, -ability_target) %>%
        ggplot(aes(x = ability_target, y = prob, color = group)) +
        geom_point() +
        facet_wrap(~ item)
}
