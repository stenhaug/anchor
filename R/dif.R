dif_constrained_baseline_one_at_a_time <- function(data, groups){
    tibble(
        item = 1:ncol(data)
    ) %>%
        mutate(p = item %>% map_dbl(~ constrained_baseline_test_flex_items(data, groups, flex_items = .)))
}
