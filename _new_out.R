# EDIT BACK FROM 20000 to 5000

library(furrr)
options(future.fork.enable = TRUE)
plan(multiprocess)

out <-
    tibble(n_dif_items = c(4, 6, 8)) %>%
    mutate(
        bigsim = n_dif_items %>% future_map(~ bigsim(runs = 4, n_dif_items = .))
    )

Sys.time()
