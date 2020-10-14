library(tidyverse)
library(ggtext)
library(here)
library(mirt)
library(cluster)
library(DescTools)
library(knitr)
library(kableExtra)
set.seed(12345)
R.utils::sourceDirectory(here("R"))
mirtCluster()

library(wordbankr)

admins <- get_administration_data(language = "English (American)", form = "WS")

items <- get_item_data(language = "English (American)", form = "WS")

d <-
    get_instrument_data(language = "English (American)", form = "WS") %>%
    left_join(items) %>%
    left_join(admins) %>%
    filter(!is.na(sex))

data <-
    d %>%
    filter(type == "word") %>%
    select(sex, kid = data_id, item = definition, cor = value, age, category, lexical_category) %>%
    mutate(
        sex = as.character(sex),
        cor = ifelse(cor == "", 0, 1)
    )

data_full_wide <-
    data %>%
    select(sex, kid, item, cor) %>%
    spread(item, cor)

data_responses <- data_full_wide %>% select(-sex, -kid)

data_kids <- data_full_wide %>% select(sex, kid)

intuitive_mod <- fit_mod_intuitive(data_responses, data_kids$sex)
intuitive_mod_2pl <- multipleGroup(data_responses, 1, itemtype = "2PL", data_kids$sex, SE = TRUE)

anova(intuitive_mod, intuitive_mod_2pl)
