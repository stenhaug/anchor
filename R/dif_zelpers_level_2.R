mod_constrained_baseline <- function(data, groups){
    multipleGroup(
        data,
        1,
        itemtype = "Rasch",
        groups,
        invariance = c("free_means", "free_var", "intercepts", "slopes"),
        technical = list(NCYCLES = 100),
        verbose = FALSE
    )
}

mod_flexible <- function(data, groups, flex_items, se = FALSE){
    multipleGroup(
        data,
        1,
        itemtype = "Rasch",
        groups,
        invariance = c("free_means", "free_var", paste0("Item_", (1:ncol(data))[-flex_items])),
        technical = list(NCYCLES = 100),
        SE = se,
        verbose = FALSE
    )
}
