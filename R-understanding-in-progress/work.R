# can i add some sort of sampling error somewhere

out$intuitive_mod[[1]] %>%
    mod_intuitive_to_draws_df() %>%
    View()

# was playing with clustering for each draw..

mod_intuitive <- out$intuitive_mod[[1]]



x <- mod %>% mod_to_draws_df(10)

hi <-
    x %>%
    gather(item, easy, -run) %>%
    arrange(run) %>%
    split(.$run) %>%
    map(~ irt_cluster(.$easy))

vec <- y$val[1:8]

#

library(tidyverse)
library(mirt)

pars <-
    read_csv("data-parameters/kopf2015-8.csv") %>%
    arrange(desc(easy)) %>%
    mutate(item = row_number())

n_ref <- 100000
n_foc <- 100000

sim <- sim_bias(pars = pars, n_ref = 100000, n_foc = 100000, mean_foc = -1)

# this very much works, but requires that we know the anchor items
mod1f <-
    multipleGroup(
        sim$data,
        1,
        itemtype = "2PL",
        sim$groups,
        invariance = c("free_means", paste0("Item_", 4:8)),
        technical = list(NCYCLES = 100)
    )

coef_1f(mod1f)

# not sure why all of the action happens in item 1 here
# perhaps evidence that modeling multidimensionally is hard
# and maybe something to do with variance of that second dimension being 0 but us not inputting that
mod2f <- multipleGroup(sim$data, 2, itemtype = "2PL", sim$groups, invariance = c("free_means","slopes", "intercepts"), technical = list(NCYCLES = 50))
coef(mod2f)

summary(mod2f, rotate = "oblimin")
