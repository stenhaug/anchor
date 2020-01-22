a <- c(0, 0, 1, 1)
b <- c(0, 0, 0, 0)

a <- c(0, 0, 0, 1, 1)
b <- c(0, 0, 0, 0, 0)

a <- c(0.1, 0.2, 0.3, 1, 1)
b <- c(0, 0, 0, 0, 0)

library(tidyverse)
library(DescTools)

tibble(
    p = seq(0, 1, 0.05)
) %>%
    mutate(
        gini = p %>% map_dbl(~ Gini(abs(a - b - .))),
        #llog = p %>% map_dbl(~ mean(log(abs(a - b - .)))),
        #l12 = p %>% map_dbl(~ mean(abs((a - b - .))^1/2)),
        l1 = p %>% map_dbl(~ mean(abs((a - b - .))^1)),
        #l2 = p %>% map_dbl(~ mean(abs((a - b - .))^2)),
        #l3 = p %>% map_dbl(~ mean(abs((a - b - .))^3))
    ) %>%
    gather(var, val, -p) %>%
    ggplot(aes(x = p, y = val, color = var)) +
    geom_path()

mean((values - 0.5)^2)

Gini(values - 0)

Gini
