out$intuitive_mod[[1]] %>%
    mod_intuitive_to_draws_df() %>%
    draws_df_to_logit_plot() +
    geom_point(data = out$all_other_final_dif[[1]], aes(x = dif, y = item), color = "red", shape = 1) +
    geom_vline(xintercept = filter(out$all_other_final_dif[[1]], anchor)$dif[1], linetype = "dashed", color = "red") +
    geom_point(data = out$it_back_final_dif[[1]], aes(x = dif, y = item), color = "blue", shape = 3) +
    geom_vline(xintercept = filter(out$it_back_final_dif[[1]], anchor)$dif[1], linetype = "dotted", color = "blue")
