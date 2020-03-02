create_pars_compensatory_1_biased_item <- function(){
    tibble(
        item = 1:8,
        easy = rnorm(8, 0, 1),
        target_disc = 1,
        nuisance_disc = c(rep(0, 7), rep(0.5, 1))
    )
}

create_pars_compensatory_3_biased_items <- function(){
    tibble(
        item = 1:8,
        easy = rnorm(8, 0, 1),
        target_disc = 1,
        nuisance_disc = c(rep(0, 5), rep(0.5, 3))
    )
}

create_pars_4_compensatory_biased_items_balanced <- function(){
    tibble(
        item = 1:8,
        easy = rnorm(8, 0, 1),
        target_disc = 1,
        nuisance_disc = c(rep(-0.5, 2), rep(0, 4), rep(0.5, 2))
    )
}
