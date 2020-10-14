library(tidyverse)
library(mirt)
R.utils::sourceDirectory(here::here("R"))
out <- read_rds("VERBAL2.rds")
theme_set(theme_bw(base_size = 14))

verbal <- read_rds("verbal.rds")

data <- verbal %>% select(-gender)
groups <- ifelse(verbal$gender == "female", "a_ref", "b_foc")

vars <-
    verbal %>%
    select(-gender) %>%
    names() %>%
    str_replace("_", " - ") %>%
    str_replace("s1", "Bus doesn't stop - ") %>%
    str_replace("s2", "Miss a train - ") %>%
    str_replace("s3", "Grocery store closed - ") %>%
    str_replace("s4", "Operator disconnects me - ")

intuitive_mod <- fit_mod_intuitive(data, groups)

int_mod_df <-
    intuitive_mod %>%
    coef_1f() %>%
    .$items %>%
    mutate(title = paste0(item, " - ", vars)) %>%
    select(item, title, a_ref_easy, b_foc_easy) %>%
    mutate(male_more = b_foc_easy - a_ref_easy) %>%
    mutate(title = fct_reorder(title, male_more))

vars_df <-
    tibble(
        i = 1:24 %>% as.character(),
        var = paste0("item", 1:24),
        title = int_mod_df$title
    )

intuitive_mod %>%
    mod_intuitive_to_draws_df() %>%
    select(-run) %>%
    gather(var, val) %>%
    left_join(vars_df) %>%
    mutate(val = -val) %>%
    mutate(title = fct_reorder(title, val)) %>%
    ggplot(aes(x = val, y = title)) +
    ggridges::geom_density_ridges() +
    labs(
        x = latex2exp::TeX("$\\tilde{b_j}^{male} - \\tilde{b_j}^{female}$")
    )

intuitive_mod %>%
    coef_1f() %>%
    .$items %>%
    mutate(title = paste0(item, " - ", vars)) %>%
    select(item, title, a_ref_easy, b_foc_easy) %>%
    mutate(male_more = b_foc_easy - a_ref_easy) %>%
    mutate(title = fct_reorder(title, male_more)) %>%
    ggplot(aes(x = male_more, y = title)) +
    geom_point()

p_df <-
    verbal %>%
    set_names(c("gender", vars_df$var)) %>%
    gather(var, val, -gender) %>%
    left_join(vars_df %>% select(-i)) %>%
    select(-var) %>%
    group_by(gender, title) %>%
    summarize(p = mean(val)) %>%
    ungroup() %>%
    spread(gender, p) %>%
    select(title, male, female) %>%
    arrange(desc(title))

get_name <- function(x, spot){
    as.character(x) %>% str_split("-") %>% map_chr(spot)
}

p_df %>%
    mutate(
        sit = title %>% get_name(2),
        desire = title %>% get_name(3),
        action = title %>% get_name(4)
    ) %>%
    select(-title) %>%
    select(sit, action, desire, male, female) %>%
    arrange(sit, action, desire) %>%
    filter(sit == " Miss a train ") %>%
    write_csv("~/Desktop/verbaltrain.csv")

p_df %>%
    mutate(
        male_lo = log(male / (1 - male)),
        female_lo = log(female / (1 - female))
    ) %>%
    ggplot(aes(x = male_lo - female_lo, y = title)) +
    geom_point() +
    geom_vline(
        data = lines,
        aes(xintercept = val, linetype = `Male Mean`, color = `Male Mean`)
    ) +
    theme(legend.position="bottom") +
    labs(
        x = latex2exp::TeX("$\\tilde{b_j}^{male} - \\tilde{b_j}^{female}$"),
        y = ""
    )

gini = 0.155
min_bc = 0.27

lines <-
    tibble(
        `Male Mean` = c("AOAA-AS", "AOAA-OAT", "MAXGI", "MINBC"),
        val = c(0.197, 0.01, 0.155, 0.27)
    )

mod <- intuitive_mod
par_draws <- MASS::mvrnorm(n = 10000, mu = extract.mirt(mod, "parvec"), Sigma = extract.mirt(mod, "vcov"))
par_draws <- par_draws[ , str_detect(colnames(par_draws), "d")]

par_draws %>%
    as_tibble() %>%
    set_names(c(paste0(vars_df$var, "_female"), paste0(vars_df$var, "_male"))) %>%
    mutate(run = row_number()) %>%
    gather(var, val, -run) %>%
    separate(var, c("item", "sex"), "_") %>%
    left_join(vars_df %>% select(-i), by = c("item" = "var")) %>%
    ggplot(aes(x = val, y = title, fill = sex)) +
    ggridges::geom_density_ridges(alpha = 0.5)

df <-
    intuitive_mod %>%
    coef_1f() %>%
    .$items %>%
    left_join(out$AOAA_OAT_status[[1]] %>% select(item, `AOAA-OAT` = order)) %>%
    left_join(out$AOAA_AS_status[[1]] %>% select(item, `AOAA-AS` = order)) %>%
    mutate(title = paste0(item, " - ", vars)) %>%
    mutate(male_more = b_foc_easy - a_ref_easy) %>%
    select(title, male_more, starts_with("AOAA")) %>%
    mutate(title = fct_reorder(title, male_more))

df2 <- df %>% gather(var, val, -title, -male_more)

df2 %>%
    ggplot(aes(x = male_more, y = title)) +
    geom_point(aes(alpha = is.na(var))) +
    scale_alpha_manual(values = c(1, 0)) +
    geom_label(
        data = df2 %>% filter(!is.na(val)),
        aes(x = male_more, y = title, label = val),
        size = 2.5
    ) +
    facet_wrap(~ var) +
    theme(legend.position = "none") +
    geom_vline(
        data = tibble(
            var = c("AOAA-OAT", "AOAA-AS"),
            gap = c(0.01, 0.197)
        ),
        aes(xintercept = gap),
        linetype = "dashed"
    ) +
    labs(
        x = latex2exp::TeX("$\\tilde{b_j}^{male} - \\tilde{b_j}^{female}$"),
        y = ""
    )



out %>%
    select(ends_with("mod")) %>%
    gather(var, val) %>%
    mutate(
        gap = val %>% map_dbl(~ coef(.)$b_foc$GroupPars[1,1])
    )



coef(out$AOAA_AS_mod[[1]])


# gini --------------------------------------------------------------------

emlg <-
    coef_1f(intuitive_mod)$items %>%
    select(item, ref_easy = a_ref_easy, foc_easy = b_foc_easy) %>%
    mutate(difference_in_easy = ref_easy - foc_easy)

gini_results <- emlg_to_gini_l1(emlg, lo = -1, hi = 1)

gini_results$grid %>%
    ggplot(aes(x = anchor_point, y = gini)) +
    geom_point() +
    labs(
        x = latex2exp::TeX("$\\mu^{foc}$"),
        y = latex2exp::TeX("$G(|\\mu^{foc} + \\tilde{\\mathbf{d}}|)$")
    ) +
    # geom_point(
    #     data = gini_results$grid %>% filter(gini == max(gini)),
    #     color = "green",
    #     size = 3
    # ) +
    geom_vline(
        xintercept = gini_results$grid %>% filter(gini == max(gini)) %>% pull(anchor_point),
        linetype = "dashed"
    )


# minbc -------------------------------------------------------------------
min_bc <-
    intuitive_mod %>%
    min_between_curves(lo = -1, hi = 1, by = 0.1)

min_bc$grid %>% View()
