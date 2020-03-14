# This is all about thinking through how the difference between curves integration
# depends on which group I integrate based on.
# Below I first show that this matters in a simple example
# Then I walk through integration based on reference group, then focal group, and
# finally based on combining the two populations (which is what I end up using)

# FOR THE 2PL:
# useful to think in terms of that grid and then find which minimizes
# the difference in curves where the two parameters are how
# much i add or shift ability and then how much i divide each by

# VERIFY THAT WHICH GROUP I INTEGRATE BASED ON MATTERS --------------------
# imagine a single item
# there is no bias
# but ref_mu = 0 and foc_mu = -1

Th <- seq(-6, 6, 0.01)

df <-
    tibble(
        theta = Th
    ) %>%
    mutate(
        ref = boot::inv.logit(theta + 0),
        foc = boot::inv.logit(theta + -1),
        bet_curv = abs(ref - foc)
    ) %>%
    mutate(
        wt_ref = dnorm(Th, 0, 1) / sum(dnorm(Th, 0, 1)),
        wt_foc = dnorm(Th, -1, 1) / sum(dnorm(Th, -1, 1))
    )

df %>%
    select(theta, ref, foc) %>%
    gather(var, val, -theta) %>%
    ggplot(aes(x  = theta, y = val, color = var)) +
    geom_point()

df %>%
    ggplot(aes(x = theta, y = bet_curv)) +
    geom_point()

df %>%
    select(theta, starts_with("wt")) %>%
    gather(var, val, -theta) %>%
    ggplot(aes(x = theta, y = val, color = var)) +
    geom_point()

weighted.mean(df$bet_curv, df$wt_ref)

weighted.mean(df$bet_curv, df$wt_foc)

# FIX PARAMETERS ----------------------------------------------------------
intuitive_mod <- out_3_biased_items$intuitive_mod[[1]]
lo <- 0.9
hi <- 1.1
by <- 0.01

# REFERENCE GROUP FOCUS ---------------------------------------------------
REF_min_between_curves <- function(intuitive_mod, lo, hi, by){

    n_items <- length(intuitive_mod@Data$K)

    Theta <- seq(-6, 6, length.out = 61)

    mu <- 0 # ref group never changes
    sd <- sqrt(coef(intuitive_mod, simplify = TRUE)$a_ref$cov[1]) # ref group

    weights <- dnorm(Theta, mu, sd) / sum(dnorm(Theta, mu, sd))

    # plot(Theta, weights)

    a_ref_probtrace <- 1:n_items %>% map(~ probtrace(extract.item(extract.group(intuitive_mod, 1), .), Theta)[ , 2])

    f <- function(intuitive_mod, anchor_point){
        # anchor_point <- 1

        data <-
            tibble(
                item = 1:n_items,
                a_ref = a_ref_probtrace
            ) %>%
            mutate(
                b_foc = item %>% map(~ probtrace(extract.item(extract.group(intuitive_mod, 2), .), Theta + anchor_point)[ , 2])
            ) %>%
            unnest(cols = c(a_ref, b_foc)) %>%
            mutate(
                theta = rep(Theta, n_items),
                weight = rep(weights, n_items)
            ) %>%
            group_by(item) %>%
            summarize(between_curves = sum(abs(a_ref - b_foc) * weight))

        sum(data$between_curves)
    }

    grid <-
        tibble(
            anchor_point = seq(lo, hi, by)
        )  %>%
        mutate(total_between_curves = anchor_point %>% map_dbl(~ f(intuitive_mod, .)))

    list(
        grid = grid,
        graph =
            grid %>%
            ggplot(aes(x = anchor_point, y = total_between_curves)) +
            geom_point() +
            geom_path(),
        between_curves_anchor_points = grid$anchor_point[grid$total_between_curves == min(grid$total_between_curves)]
    )
}

# FOCAL GROUP FOCUS -------------------------------------------------------
FOC_min_between_curves <- function(intuitive_mod, lo, hi, by){

    n_items <- length(intuitive_mod@Data$K)
    sd <- sqrt(coef(intuitive_mod, simplify = TRUE)$b_foc$cov[1])

    f <- function(intuitive_mod, anchor_point){

        Theta <- seq(-6 + anchor_point, 6 + anchor_point, length.out = 61)

        a_ref_probtrace <- 1:n_items %>% map(~ probtrace(extract.item(extract.group(intuitive_mod, 1), .), Theta)[ , 2])

        weights <- dnorm(Theta, anchor_point, sd) / sum(dnorm(Theta, anchor_point, sd))

        data <-
            tibble(
                item = 1:n_items,
                a_ref = a_ref_probtrace
            ) %>%
            mutate(
                b_foc = item %>% map(~ probtrace(extract.item(extract.group(intuitive_mod, 2), .), Theta + anchor_point)[ , 2])
            ) %>%
            unnest(cols = c(a_ref, b_foc)) %>%
            mutate(
                theta = rep(Theta, n_items),
                weight = rep(weights, n_items)
            ) %>%
            group_by(item) %>%
            summarize(between_curves = sum(abs(a_ref - b_foc) * weight))

        sum(data$between_curves)
    }

    grid <-
        tibble(
            anchor_point = seq(lo, hi, by)
        )  %>%
        mutate(total_between_curves = anchor_point %>% map_dbl(~ f(intuitive_mod, .)))

    list(
        grid = grid,
        graph =
            grid %>%
            ggplot(aes(x = anchor_point, y = total_between_curves)) +
            geom_point() +
            geom_path(),
        between_curves_anchor_points = grid$anchor_point[grid$total_between_curves == min(grid$total_between_curves)]
    )
}

# COMBINE GROUP FOCUS -------------------------------------------------------
COMBINE_min_between_curves <- function(intuitive_mod, lo, hi, by){

    n_items <- length(intuitive_mod@Data$K)
    Theta <- seq(-6, 6, length.out = 61)

    a_ref_probtrace <- 1:n_items %>% map(~ probtrace(extract.item(extract.group(intuitive_mod, 1), .), Theta)[ , 2])

    mu_ref <- 0
    sd_ref <- sqrt(coef(intuitive_mod, simplify = TRUE)$a_ref$cov[1])
    weights_ref <- dnorm(Theta, mu_ref, sd_ref) / sum(dnorm(Theta, mu_ref, sd_ref))

    f <- function(intuitive_mod, anchor_point){

        mu_foc <- anchor_point
        sd_foc <- sqrt(coef(intuitive_mod, simplify = TRUE)$b_foc$cov[1])
        weights_foc <- dnorm(Theta, mu_foc, sd_foc) / sum(dnorm(Theta, mu_foc, sd_foc))

        weights <- (weights_foc + weights_ref) / 2

        data <-
            tibble(
                item = 1:n_items,
                a_ref = a_ref_probtrace
            ) %>%
            mutate(
                b_foc = item %>% map(~ probtrace(extract.item(extract.group(intuitive_mod, 2), .), Theta + anchor_point)[ , 2])
            ) %>%
            unnest(cols = c(a_ref, b_foc)) %>%
            mutate(
                theta = rep(Theta, n_items),
                weight = rep(weights, n_items)
            ) %>%
            group_by(item) %>%
            summarize(between_curves = sum(abs(a_ref - b_foc) * weight))

        sum(data$between_curves)
    }

    grid <-
        tibble(
            anchor_point = seq(lo, hi, by)
        )  %>%
        mutate(total_between_curves = anchor_point %>% map_dbl(~ f(intuitive_mod, .)))

    list(
        grid = grid,
        graph =
            grid %>%
            ggplot(aes(x = anchor_point, y = total_between_curves)) +
            geom_point() +
            geom_path(),
        between_curves_anchor_points = grid$anchor_point[grid$total_between_curves == min(grid$total_between_curves)]
    )
}

# COMPARE -----------------------------------------------------------------
REF <- REF_min_between_curves(intuitive_mod, 0.9, 1.1, 0.1 / 3)
FOC <- FOC_min_between_curves(intuitive_mod, 0.9, 1.1, 0.1 / 3)
COMBINE <- COMBINE_min_between_curves(intuitive_mod, 0.9, 1.1, 0.1 / 3)

order(REF$grid$total_between_curves)
order(FOC$grid$total_between_curves)
order(COMBINE$grid$total_between_curves)

