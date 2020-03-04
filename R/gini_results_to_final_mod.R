anchptfocmean_to_final_mod <- function(anchptfocmean, data, groups){
    stopifnot(length(anchptfocmean) == 1)

    tmp <-
        multipleGroup(
            data,
            1,
            itemtype = "Rasch",
            groups,
            invariance = c("free_var"),
            pars = "values"
        ) %>%
        as_tibble()

    tmp$value[length(tmp$value) - 1] <- anchptfocmean

    multipleGroup(
        data,
        1,
        itemtype = "Rasch",
        groups,
        pars = tmp %>% as.data.frame(),
        SE = TRUE,
        technical = list(NCYCLES = 500),
        verbose = FALSE,
    )
}
