mod_to_final_dif <- function(all_other_mod){
    the_coef <- coef_1f(all_other_mod)

    the_coef$items %>%
        mutate(
            item = paste0("item", 1:8),
            dif = b_foc_easy - a_ref_easy + the_coef$ability$b_foc[1]
        ) %>%
        select(item, anchor, dif)
}
