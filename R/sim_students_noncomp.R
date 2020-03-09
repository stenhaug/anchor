# pars <- create_pars_noncompensatory(n_items = 10, n_dif_items = 5)
#
# students <-
#     sim_students_2d(
#         n_ref = 10,
#         n_foc = 10,
#         ref_target_ability_mu = 0,
#         ref_nuisance_ability_mu = 0,
#         foc_target_ability_mu = -0.5,
#         foc_nuisance_ability_mu = -1
#     )
#
# p <- pars_students_to_p_noncomp(pars, students)
#
# data <- p_to_data_byway_of_flipping(p)
#
# sim <- students_p_to_sim(students, p)

sim_students_2d <- function(n_ref, n_foc, ref_target_ability_mu, ref_nuisance_ability_mu, foc_target_ability_mu, foc_nuisance_ability_mu){

    sigma <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)

    bind_rows(
        MASS::mvrnorm(n_ref, mu = c(ref_target_ability_mu, ref_nuisance_ability_mu), Sigma = sigma) %>% as.data.frame() %>% as_tibble(),
        MASS::mvrnorm(n_foc, mu = c(foc_target_ability_mu, foc_nuisance_ability_mu), Sigma = sigma) %>% as.data.frame() %>% as_tibble()
    ) %>%
        mutate(groups = c(rep("a_ref", n_ref), rep("b_foc", n_foc))) %>%
        select(groups, target_ability = V1, nuisance_ability = V2)
}

get_p_noncomp <- function(a_target, a_nuisance, b_target, b_nuisance, target_ability, nuisance_ability){
    target <- sigmoid(a_target * target_ability + b_target)
    nuisance <- sigmoid(a_nuisance * nuisance_ability + b_nuisance)
    target * nuisance
}

pars_students_to_p_noncomp <- function(pars, students){
    p <- matrix(nrow = nrow(students), ncol = nrow(pars))

    for (i in 1:nrow(students)){
        for (j in 1:nrow(pars)){
            p[i, j] <-
                get_p_noncomp(
                    a_target = pars$a_target[j],
                    a_nuisance = pars$a_nuisance[j],
                    b_target = pars$b_target[j],
                    b_nuisance = pars$b_nuisance[j],
                    target_ability = students$target_ability[i],
                    nuisance_ability = students$nuisance_ability[i]
                )
        }
    }

    stopifnot(all(!is.na(p)) & all(p >= 0) & all(p <= 1))

    p
}

p_to_data_byway_of_flipping <- function(p){
    flip <- matrix(runif(prod(dim(p)), 0, 1), nrow = nrow(p), ncol = ncol(p))
    data <- ifelse(p > flip, 1, 0)
    colnames(data) <- paste0("Item_", 1:ncol(p))
    data
}

students_p_to_sim <- function(students, p){
    list(
        data = p_to_data_byway_of_flipping(p),
        groups = students$groups
    )
}
