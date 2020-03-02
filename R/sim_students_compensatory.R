# pars <-
#     read_csv("data-parameters/kopf2015-8.csv") %>%
#     arrange(desc(easy)) %>%
#     mutate(item = row_number())
#
# sim_bias(pars = pars, n_ref = 100000, n_foc = 100000, mean_foc = -1)

sim_students_compensatory <- function(pars, n_ref, n_foc, mean_foc){

    theta_ref <-
        tibble(
            target = rnorm(n_ref, 0, 1),
            nuisance = rep(0, n_ref)
        )

    theta_foc <-
        tibble(
            target = rnorm(n_foc, mean_foc, 1),
            nuisance = rep(-1, n_foc)
        )

    groups <- groups <- c(rep("a_ref", n_ref), rep("b_foc", n_foc))

    data <-
        simdata(
            a = pars %>% select(ends_with("disc")) %>% as.matrix(),
            d = pars$easy,
            itemtype = "2PL",
            Theta = bind_rows(theta_ref, theta_foc) %>% as.matrix()
        )

    list(data = data, groups = groups)
}
