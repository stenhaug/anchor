final_model_to_anchor_summary <- function(final_model){
    coef_1f(final_model)$items %>%
        mutate(
            true_anchor =
                c(
                    rep("no dif", n_items - n_dif_items),
                    rep("some dif", floor(n_dif_items / 2)),
                    rep("high dif", ceiling(n_dif_items / 2))
                )
        ) %>%
        group_by(true_anchor) %>%
        summarize(selected = sum(anchor), total = n())
}
