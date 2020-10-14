# Prepare -----------------------------------------------------------------
library(tidyverse)
library(ggtext)
library(here)
library(mirt)
library(cluster)
library(DescTools)
R.utils::sourceDirectory(here("R"))

verbal <- read_rds("~/Desktop/anchor-2pl/data/verbal.rds")

groups <- verbal$gender
data <-
    verbal %>%
    select(-gender)

names(data) <- paste0("Item_", 1:ncol(data))

groups <- ifelse(groups == "female", "a_ref", "b_foc")

# Run ---------------------------------------------------------------------
out <-
    tibble(
        sim = list(list(data = data, groups = groups))
    ) %>%
    # sim and fit intuitive
    mutate(
        intuitive_mod = sim %>% map(~ fit_mod_intuitive(.$data, .$groups))
    ) %>%
    # dif: AOAA
    mutate(
        AOAA_status = sim %>% map(~ dif_AOAA(.$data, .$groups)),
        AOAA_mod =
            map2(
                sim,
                AOAA_status,
                ~ mod_flexible(
                    .x$data,
                    .x$groups,
                    which(.y$status == "flex"),
                    se = TRUE
                )
            ),
        AOAA_final_dif = AOAA_mod %>% map(mod_to_final_dif)
    ) %>%
    # dif: AOAA AS
    mutate(
        AOAA_AS_status = map2(sim, AOAA_status, ~ dif_AOAA_AS(.x$data, .x$groups, .y)),
        AOAA_AS_mod =
            map2(
                sim,
                AOAA_AS_status,
                ~ mod_flexible(
                    .x$data,
                    .x$groups,
                    which(.y$status == "flex"),
                    se = TRUE
                )
            ),
        AOAA_AS_final_dif = AOAA_AS_mod %>% map(mod_to_final_dif)
    ) %>%
    # dif: AOAA OAT
    mutate(
        AOAA_OAT_status = map(sim, ~ dif_AOAA_OAT(.$data, .$groups)),
        AOAA_OAT_mod =
            map2(
                sim,
                AOAA_OAT_status,
                ~ mod_flexible(
                    .x$data,
                    .x$groups,
                    which(.y$status == "flex"),
                    se = TRUE
                )
            ),
        AOAA_OAT_final_dif = AOAA_OAT_mod %>% map(mod_to_final_dif)
    ) %>%
    # dif: tibs cluster
    mutate(
        cluster =
            intuitive_mod %>%
            map(mod_to_final_dif) %>%
            map(~ mutate(., cluster = irt_cluster(difference_in_easy)))
    ) %>%
        mutate(cluster_stats = cluster %>% map(get_cluster_stats)) %>%
        mutate(cluster_status = map2(cluster, cluster_stats, left_join, by = "cluster")) %>%
        mutate(
            cluster_mod =
                map2(
                    sim,
                    cluster_status,
                    ~ mod_flexible(
                        .x$data,
                        .x$groups,
                        which(.y$status == "flex"),
                        se = TRUE
                    )
                ),
            cluster_final_dif = cluster_mod %>% map(mod_to_final_dif)
        )

out %>% write_rds("VERBAL2.rds")

# test --------------------------------------------------------------------

AOAA_status <- dif_AOAA(data, groups)

AOAA_AS_status <- dif_AOAA_AS(data, groups, AOAA_status)

debug(dif_AOAA_AS)

# look at -----------------------------------------------------------------

out$AOAA_status[[1]] %>%
    rename(aoaa_status = status) %>%
    left_join(
        out$AOAA_AS_status[[1]] %>% rename(aoaa_as_status = status),
        by = "item"
    ) %>%
    left_join(
        out$AOAA_OAT_status[[1]] %>% rename(aoaa_oat_status = status),
        by = "item"
    ) %>%
    select(item, starts_with("aoaa")) %>%
    View()


