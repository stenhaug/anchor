# this look at the idea that maybe i could just fit multi model which
# is actually pretty hard because of rotation etc unless you pin a few
# things down - prolly reason its not in the literature

# pars <- tibble(
#     easy = rnorm(50, 0, 1),
#     target_disc  = rnorm(50, 1.5, 0.5),
#     nuisance_disc  = rnorm(50, 1.5, 0.5),
# )

pars <-
    read_csv("data-parameters/kopf2015-8.csv") %>%
    arrange(desc(easy)) %>%
    mutate(item = row_number())

n_ref <- 200000
n_foc <- 200000

theta_ref <-
    tibble(
        target = rnorm(n_ref, 0, 1),
        nuisance = rnorm(n_ref, 0, 1)
    )

theta_foc <-
    tibble(
        target = rnorm(n_foc, 0, 1),
        nuisance = rnorm(n_foc, 0, 1)
    )

groups <- groups <- c(rep("a_ref", n_ref), rep("b_foc", n_foc))

data <-
    simdata(
        a = pars %>% select(ends_with("disc")) %>% as.matrix(),
        d = pars$easy,
        itemtype = "2PL",
        Theta = bind_rows(theta_ref, theta_foc) %>% as.matrix()
    )

vals <- mirt(data, 2, "2PL", pars = "values")

vals$value[which(vals$name == "a1")][-(4:8)] <- pars$target_disc[-(4:8)]

vals$est[which(vals$name == "a1")][-(4:8)] <- FALSE

mod <- mirt(data, 2, "2PL", pars = vals)

coef(mod, simplify = TRUE)
