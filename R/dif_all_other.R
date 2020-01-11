dif_all_other <- function(data, groups){
    tibble(
        item = 1:ncol(data)
    ) %>%
        mutate(
            p = item %>% map_dbl(~ constrained_baseline_test_flex_items(data, groups, flex_items = .)),
            status = ifelse(p > 0.05, "unknown", "flex")
        )
}
