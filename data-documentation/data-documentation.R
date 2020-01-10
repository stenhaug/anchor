pars <-
    read_csv("data-parameters/kopf2015-8.csv") %>%
    arrange(desc(easy)) %>%
    mutate(item = row_number())

sim <- sim_bias(pars = pars, n_ref = 100000, n_foc = 100000, mean_foc = -1)

sim %>% write_rds("data-documentation/sim.rds")
