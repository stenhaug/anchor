# for walkthrough see doc_min_between_curves.R
# see note on assuming cov stays constant below ...  think about if going to 2PL
# see note on Theta + anchor_point below which needs thinking if I do the 2PL

min_between_curves <- function(intuitive_mod, lo, hi, by){

    n_items <- length(intuitive_mod@Data$K)
    Theta <- seq(-6, 6, length.out = 61)

    a_ref_probtrace <- 1:n_items %>% map(~ probtrace(extract.item(extract.group(intuitive_mod, 1), .), Theta)[ , 2])

    mu_ref <- 0
    sd_ref <- sqrt(coef(intuitive_mod, simplify = TRUE)$a_ref$cov[1])
    weights_ref <- dnorm(Theta, mu_ref, sd_ref) / sum(dnorm(Theta, mu_ref, sd_ref))

    f <- function(intuitive_mod, anchor_point){

        mu_foc <- anchor_point
        # assumes cov doesn't change when I shift mean
        # which I mostly cerified for Rasch in
        # R-work-complete > cov_doesnt_change_when_shifting
        sd_foc <- sqrt(coef(intuitive_mod, simplify = TRUE)$b_foc$cov[1])
        weights_foc <- dnorm(Theta, mu_foc, sd_foc) / sum(dnorm(Theta, mu_foc, sd_foc))

        weights <- (weights_foc + weights_ref) / 2 # assumes 50% in each group which I could of course change

        data <-
            tibble(
                item = 1:n_items,
                a_ref = a_ref_probtrace
            ) %>%
            # Theta + anchor_point was throwing me for a loop
            # remember that intuitive_mod had fixed means and independent item parameters across groups
            # so this is a shortcut for shifting all of the item parameters for the focal group
            # will need to rethink this if I move to the 2PL
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
