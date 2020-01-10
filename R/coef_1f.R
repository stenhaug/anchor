# sim <- read_rds("data-documentation/sim.rds")
#
# mod <-
#     multipleGroup(
#         sim$data,
#         1,
#         sim$groups,
#         invariance = c("free_means", paste0("Item_", 4:8)),
#         technical = list(NCYCLES = 100)
#     )
#
# coef_1f(mod)

coef_1f <- function(mod){

    x <- coef(mod, simplify = TRUE)

    items <-
        bind_cols(
            x$a_ref$items %>%
                as_tibble() %>%
                select(a_ref_disc = a1, a_ref_easy = d),
            x$b_foc$items %>%
                as_tibble() %>%
                select(b_foc_disc = a1, b_foc_easy = d)
        ) %>%
        mutate(
            item = row_number(),
            anchor = a_ref_disc == b_foc_disc & a_ref_easy == b_foc_easy
        ) %>%
        select(item, anchor, ends_with("easy"), ends_with("disc"))

    ability <-
        tibble(
            what = c("mean", "cov"),
            a_ref = c(x$a_ref$means, x$a_ref$cov),
            b_foc = c(x$b_foc$means, x$b_foc$cov)
        )

    list(items = items, ability = ability)
}
