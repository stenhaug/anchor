# i was trying to figure out why increasing theta didn't show linear increases in my between curves dif stat.
# the reason is because of the shape of the inverse logit function which i show below
# makes me slightly wonder if you'd want to switch to like a log likelihood or something that does increase linearly
# and is a bit more in line with maximum likelihood estimation but probably not for now
# basically i think what im suggesting is that to get around the identifiability problem
# where we can always change both the item parameters and group ability and wind up with the same likelihood
# is that we should prefer models where the differences in item parameters are the smallest (rmse is a simple solution and in the rasch case
# total dif is another solution that performs similarly but i think will work better in other models)

# show not linear ---------------------------------------------------------
library(tidyverse)
library(DescTools)

mod <- out_3_biased_items$intuitive_mod[[1]]

coef_1f(mod)


f <- function(mod, foc_worse_by){
    Theta <- seq(-6, 6, length.out = 61)

    data <-
        crossing(item = 1:length(mod@Data$K)) %>%
        mutate(
            a_ref = item %>% map(~ probtrace(extract.item(extract.group(mod, 1), .), Theta)[ , 2]),
            b_foc = item %>% map(~ probtrace(extract.item(extract.group(mod, 2), .), Theta + foc_worse_by)[ , 2])
        ) %>%
        unnest(cols = c(a_ref, b_foc)) %>%
        mutate(
            theta = rep(Theta, length(mod@Data$K)),
            weight = rep(dnorm(Theta) / sum(dnorm(Theta)), length(mod@Data$K))
        ) %>%
        group_by(item) %>%
        summarize(dif = sum(abs(a_ref - b_foc) * weight))

    sum(data$dif)
}

by_item <- coef_1f(mod)$items %>% transmute(item, foc_worse_by = a_ref_easy - b_foc_easy)

a <- coef_1f(mod)$items$a_ref_easy
b <- coef_1f(mod)$items$b_foc_easy

out <-
    tibble(
        foc_worse_by = seq(2, 10, by = 1)
    )  %>%
    mutate(
        dif_stat = foc_worse_by %>% map_dbl(~ f(mod, .)),
        gini = foc_worse_by %>% map_dbl(~ Gini(abs(a - b - .))),
        l1 = foc_worse_by %>% map_dbl(~ mean(abs((a - b - .))^1)),
    )

out %>%
    mutate(gini = -gini) %>%
    gather(var, val, -foc_worse_by) %>%
    ggplot(aes(x = foc_worse_by, y = val, color = var)) +
    geom_point() +
    geom_path() +
    geom_vline(data = by_item, aes(xintercept = foc_worse_by))

# figure out why not linear -----------------------------------------------

Theta <- seq(-6, 6, length.out = 61)

data1 <-
    crossing(item = 1:length(mod@Data$K)) %>%
    mutate(
        a_ref = item %>% map(~ probtrace(extract.item(extract.group(mod, 1), .), Theta)[ , 2]),
        b_foc = item %>% map(~ probtrace(extract.item(extract.group(mod, 2), .), Theta + 0)[ , 2])
    ) %>%
    unnest(cols = c(a_ref, b_foc)) %>%
    mutate(
        theta = rep(Theta, length(mod@Data$K)),
        weight = rep(dnorm(Theta) / sum(dnorm(Theta)), length(mod@Data$K))
    )

data0 %>%
    filter(item == 1) %>%
    gather(var, val, -item, -theta, -weight) %>%
    ggplot(aes(x = theta, y = val, color = var)) +
    geom_point()

data2 <-
    crossing(item = 1:length(mod@Data$K)) %>%
    mutate(
        a_ref = item %>% map(~ probtrace(extract.item(extract.group(mod, 1), .), Theta)[ , 2]),
        b_foc = item %>% map(~ probtrace(extract.item(extract.group(mod, 2), .), Theta + 2)[ , 2])
    ) %>%
    unnest(cols = c(a_ref, b_foc)) %>%
    mutate(
        theta = rep(Theta, length(mod@Data$K)),
        weight = rep(dnorm(Theta) / sum(dnorm(Theta)), length(mod@Data$K))
    )

data2 %>%
    filter(item == 1) %>%
    gather(var, val, -item, -theta, -weight) %>%
    ggplot(aes(x = theta, y = val, color = var)) +
    geom_point()

data3 <-
    crossing(item = 1:length(mod@Data$K)) %>%
    mutate(
        a_ref = item %>% map(~ probtrace(extract.item(extract.group(mod, 1), .), Theta)[ , 2]),
        b_foc = item %>% map(~ probtrace(extract.item(extract.group(mod, 2), .), Theta + 3)[ , 2])
    ) %>%
    unnest(cols = c(a_ref, b_foc)) %>%
    mutate(
        theta = rep(Theta, length(mod@Data$K)),
        weight = rep(dnorm(Theta) / sum(dnorm(Theta)), length(mod@Data$K))
    )

data3 %>%
    filter(item == 1) %>%
    gather(var, val, -item, -theta, -weight) %>%
    ggplot(aes(x = theta, y = val, color = var)) +
    geom_point()


data4 <-
    crossing(item = 1:length(mod@Data$K)) %>%
    mutate(
        a_ref = item %>% map(~ probtrace(extract.item(extract.group(mod, 1), .), Theta)[ , 2]),
        b_foc = item %>% map(~ probtrace(extract.item(extract.group(mod, 2), .), Theta + 4)[ , 2])
    ) %>%
    unnest(cols = c(a_ref, b_foc)) %>%
    mutate(
        theta = rep(Theta, length(mod@Data$K)),
        weight = rep(dnorm(Theta) / sum(dnorm(Theta)), length(mod@Data$K))
    )

data4 %>%
    filter(item == 1) %>%
    gather(var, val, -item, -theta, -weight) %>%
    ggplot(aes(x = theta, y = val, color = var)) +
    geom_point()

two <- data2 %>% filter(item == 1, theta == 3) %>% pull(b_foc)
three <- data3 %>% filter(item == 1, theta == 3) %>% pull(b_foc)
four <- data4 %>% filter(item == 1, theta == 3) %>% pull(b_foc)

four - three

three - two
