# my whole goal here is to think about what happens when we move from Rasch to the 2PL
# in particular with the cluster based approach how will I cluster on both of these things
# what I confirm below is that just like for Rasch fixing means across groups is enough,
# for the 2PL, fixing means and variance across groups is enough
# the way to think about it is the standard for the Rasch or 2PL is to fix one group
# to N(0, 1) and then have the other group N(?, ?) and all of the item parameters unknown
# I basically need to fix one thing for the Rasch - in the paper I argue the first ?
# and two things for the 2PL - both of the ? ?
# the end of page 335 of the bechger piece is useful for thinking
# and confirms like we think of differences in difficulty we can think of
# ratios of discriminations
# two ideas for the cluster scenario
# 1. just choose which items to anchor for difficulty and discrimination separately
# 2. do some cluster on the matrix of differences in difficulty and ratio of discrimination
# but that kind of seems like it puts us in some weird space
# SEE doc_min_between_curves for some thinking on doing this with the integration thing
# (basically first intuition is to make a grid of group means and variance slopes to divide by
# and then calculate at each of them)

pars <-
    tibble(
        item = 1:8,
    ) %>%
    mutate(
        easy = 0,
        disc = 1
    )

theta_ref <- tibble(target = rnorm(100000, 0, 1))
theta_foc <- tibble(target = rnorm(100000, 0, 2))

hi <- simdata(
    a = pars %>% select(disc) %>% as.matrix(),
    d = pars$easy,
    itemtype = "2PL",
    Theta = bind_rows(theta_ref, theta_foc) %>% as.matrix(),
    returnList = TRUE
)

mod <-
    multipleGroup(
        hi$data,
        1,
        itemtype = "2PL",
        group = c(rep("aref", 100000), rep("bfoc", 100000)),
        technical = list(NCYCLES = 500),
        verbose = TRUE
    )

coef(mod)

mod <-
    multipleGroup(
        hi$data,
        1,
        itemtype = "2PL",
        group = c(rep("ref", 100000), rep("foc", 100000)),
        technical = list(NCYCLES = 500),
        verbose = TRUE,
        invariance = c("intercepts","slopes", "free_var")
    )

coef(mod)

# to get ref down we have sqrt(5.47 * 1.15^4)
