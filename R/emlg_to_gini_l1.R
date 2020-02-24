emlg_to_gini_l1 <- function(emlg, lo = -2, hi = 2){

    stopifnot(all(c("ref_easy", "foc_easy", "difference_in_easy") %in% names(emlg)))

    grid <-
        tibble(
            anchor_point = seq(lo, hi, 0.0001)
        ) %>%
        mutate(
            gini = anchor_point %>% map_dbl(~ Gini(abs(emlg$ref_easy - emlg$foc_easy + .))),
            l1 = anchor_point %>% map_dbl(~ mean(abs((emlg$ref_easy - emlg$foc_easy + .))^1))
        )

    list(
        grid = grid,
        gini_anchor_points = grid$anchor_point[grid$gini == max(grid$gini)],
        l1_anchor_points = grid$anchor_point[grid$l1 == min(grid$l1)]
    )
}
