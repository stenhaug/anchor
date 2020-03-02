library(tidyverse)
library(MASS)

# abilities
sigma <- cbind(c(1, 0.5), c(0.5, 1))

ref <- mvrnorm(10000, mu=c(0, 0), Sigma = sigma)
var(ref)
cor(ref)
colMeans(ref)

foc <- mvrnorm(10000, mu=c(-0.5, -1), Sigma = sigma)
var(foc)
cor(foc)
colMeans(foc)

# items
n_items <- 20
n_dif_items <- 5

# go from disc2 to angle
(angle <- acos(1 / sqrt(1 + disc2^2)) * 180 / pi)

# and from angle to disc2
sqrt((1 - cos(angle * pi / 180)^2) / cos(angle * pi / 180)^2)

angle_to_a_nuisance <- function(angle){
    # uses that cos formula. assumes a_target is 1
    sqrt((1 - cos(angle * pi / 180)^2) / cos(angle * pi / 180)^2)
}

angle_to_a_nuisance(30)

items <-
    tibble(
        item = 1:n_items,
        dif = c(rep("no", n_items - n_dif_items), rep("yes", n_dif_items))
    ) %>%
    mutate(
        a_target = 1,
        angle = ifelse(dif == "no", 0, seq(30, 60, length.out = n_dif_items)),
        a_nuisance = angle_to_a_nuisance(angle),
        b_target = rnorm(n_items, 0.5, 0.5),
        b_nuisance = 1
    )




