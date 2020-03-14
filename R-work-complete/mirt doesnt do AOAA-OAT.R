data <- out_3_biased_items$sim[[1]]$data
groups <- out_3_biased_items$sim[[1]]$groups


model_constrained <-
    multipleGroup(
        data, 1, groups, SE = TRUE,
        invariance = c(colnames(data), "free_means", "free_var")
    )

DIF(model_constrained, 'd', scheme = 'drop_sequential')

coef_1f(model_constrained)
