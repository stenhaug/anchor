library(tidyverse)
library(mirt)
set.seed(123)
R.utils::sourceDirectory("R")

# simulate ----------------------------------------------------------------
pars <-
    tibble(
        item = 1:8,
        easy = rnorm(8, 0, 1),
        target_disc = 1,
        nuisance_disc = c(rep(0.5, 3), rep(0, 5)) # 1 + 7 = 8
    ) %>%
    arrange(desc(nuisance_disc), easy)

sim <- sim_bias(pars = pars, n_ref = 10000, n_foc = 10000, mean_foc = -1)

sim %>% write_rds("data-documentation/sim.rds")

# dif ---------------------------------------------------------------------

# intuitive
mod <- multipleGroup(sim$data, 1, itemtype = "Rasch", sim$groups, invariance = "free_var", SE = TRUE)
mod %>% mod_intuitive_to_draws_df() %>% draws_df_to_logit_plot()

# all other
all_other <- dif_all_other(sim$data, sim$groups)

# iterative backward
iterative_backward <- dif_iterative_backward(sim$data, sim$groups, all_other)
