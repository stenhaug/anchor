out <- read_rds("paper/paper_4_out1.rds")

n_items <- 20
n_dif_items <- 6

one <- out$bigsim[[n_dif_items - 1]]
pars <- one$pars[[4]]

# graph 1
one$pars[[1]] %>% graph_pars_noncompensatory(0, -1)

# graph 2
one$intuitive_mod[[1]] %>%
    mod_intuitive_to_draws_df() %>%
    draws_df_to_logit_plot()

# function - one to achievement gap
one_to_achievement_gap <- function(one){
    one %>%
        select(ends_with("mod"), -intuitive_mod) %>%
        mutate(run = row_number()) %>%
        gather(method, model, -run) %>%
        mutate(
            n_anchors = model %>% map_int(~ sum(coef_1f(.)$items$anchor)),
            foc_mean = model %>% map_dbl(~ coef_1f(.)$ability$b_foc[1])
        ) %>%
        filter(n_anchors != 0 | method %in% c("gini_mod", "minbc_mod")) %>%
        group_by(method) %>%
        summarize(out = mean(foc_mean), runs_with_nonzero_anchors = n())
}

getfull <- function(one){
    one %>%
        select(ends_with("mod"), -intuitive_mod) %>%
        mutate(run = row_number()) %>%
        gather(method, model, -run) %>%
        mutate(
            n_anchors = model %>% map_int(~ sum(coef_1f(.)$items$anchor)),
            foc_mean = model %>% map_dbl(~ coef_1f(.)$ability$b_foc[1])
        ) %>%
        filter(n_anchors != 0 | method %in% c("gini_mod", "minbc_mod"))
}

# graph 3
out %>%
    mutate(achievegap = bigsim %>% map(one_to_achievement_gap)) %>%
    select(-bigsim) %>%
    unnest(achievegap) %>%
    ggplot(aes(x = n_dif_items, y = out)) +
    geom_point() +
    facet_wrap(~ method) +
    geom_hline(yintercept = -0.5, linetype = "dashed")

out %>%
    mutate(achievegap = bigsim %>% map(getfull)) %>%
    select(-bigsim) %>%
    unnest(achievegap) %>%
    ggplot(aes(x = n_dif_items, y = foc_mean)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ method) +
    geom_hline(yintercept = -0.5, linetype = "dashed")

# function - one to anchors
one_to_anchors <- function(one, n_dif_items){
    one %>%
        select(ends_with("mod"), -intuitive_mod, -gini_mod, -minbc_mod) %>%
        mutate(run = row_number()) %>%
        gather(method, model, -run) %>%
        mutate(anchor_summary = model %>% map(final_model_to_anchor_summary, n_dif_items)) %>%
        select(-run, -model) %>%
        unnest(anchor_summary) %>%
        group_by(method, true_anchor, total) %>%
        summarize(mean_selected = mean(selected)) %>%
        ungroup()
}

temp <-
    out %>%
    mutate(anchors = map2(bigsim, n_dif_items, one_to_anchors)) %>%
    select(-bigsim) %>%
    unnest(anchors)

# graph 4
temp %>%
    filter(true_anchor == "no dif") %>%
    gather(var, val, -n_dif_items, -method, -true_anchor) %>%
    ggplot(aes(x = n_dif_items, y = val, color = var)) +
    geom_point() +
    ylim(0, 20) +
    facet_wrap(~ method)

temp %>%
    filter(true_anchor != "no dif") %>%
    group_by(n_dif_items, method) %>%
    summarize(total = sum(total), mean_selected = sum(mean_selected)) %>%
    gather(var, val, -n_dif_items, -method) %>%
    ggplot(aes(x = n_dif_items, y = val, color = var)) +
    geom_point() +
    ylim(0, 20) +
    facet_wrap(~ method)


n_dif_items <- 2
out$bigsim[[n_dif_items - 1]]$AOAA_OAT_mod %>%
    map(~ coef_1f(.)$items) %>%
    map(~ filter(slice(., 1:18), !anchor))

out$bigsim[[n_dif_items - 1]]$intuitive_mod[[3]] %>%
    mod_intuitive_to_draws_df() %>%
    draws_df_to_logit_plot()

