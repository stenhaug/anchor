# the way i am thinking about this is that we always start wtih all as anchors

# start by doing AOAA

# AS: make all p < 0.05 flex and then keep going

# OAT: make min(p) flex and then keep going

# For both, whenever all p > 0.05 everything in the anchor set is anchors and you are done

# Potential problem: what if in the first AOAA all p < 0.05? this can also happen
# in future iterations of AS but can never happen with OAT

# data <- out_3_biased_items$sim[[1]]$data
# groups <- out_3_biased_items$sim[[1]]$groups
# dif_aoaa <- dif_AOAA(data, groups)

# AOAA --------------------------------------------------------------------
dif_AOAA <- function(data, groups){
    tibble(
        item = 1:ncol(data)
    ) %>%
        mutate(
            p = item %>% map_dbl(~ constrained_baseline_test_flex_items(data, groups, flex_items = .)),
            status = ifelse(p > 0.05, "anchor", "flex")
        )
}

get_next_status <- function(status, data, groups){
    the_flex <- which(status$status == "flex")

    status %>%
        filter(status == "unknown") %>%
        mutate(
            p =
                item %>%
                map_dbl(~ test_with_known_flex(data, groups, flex_known = the_flex, flex_test = .))
        ) %>%
        select(-status)
}

get_next_status_OAT <- function(status, data, groups){
    the_flex <- which(status$status == "flex")

    status %>%
        filter(status == "unknown") %>%
        mutate(
            anova = item %>% map(~ quiet(anova(mod_flexible(data, groups, the_flex), mod_flexible(data, groups, flex_items = c(the_flex, .))))),
            X2 = anova %>% map_dbl(~ .$X2[2]),
            p = anova %>% map_dbl(~ .$p[2])
        ) %>%
        select(-status)
}

# AOAA - AS ---------------------------------------------------------------
dif_AOAA_AS <- function(data, groups, dif_aoaa){

    # if no items are significant, then they are all anchors and we are done
    if (all(dif_aoaa$p > 0.05)){
        return(dif_aoaa %>% mutate(status = "anchor"))
    }

    # if all tests are significant to start with, then we have no anchors and it's unclear what to do so we just return as is
    # [NOW THE MODEL IS UNIDENTIFIED AND NEED TO CHECK FOR THIS DOWNSTREAM]
    if (all(dif_aoaa$p < 0.05)){
        return(dif_aoaa)
    }

    # if some items are significant, then ALL OF those items become flex (are removed from the anchor set)
    status <-
        dif_aoaa %>%
        mutate(
            status = ifelse(p < 0.05, "flex", "unknown"),
            order = ifelse(p < 0.05, 1, NA)
        )

    track_order <- 2
    while(TRUE){
        # for each unknown item, we use all other unknown items to test it
        new_status <- get_next_status(status, data, groups)

        # need to update the status df
        status <-
            status %>%
            select(-p) %>%
            left_join(new_status %>% select(-order), by = "item")

        # if no new significant tests then we're done
        if(all(status$p > 0.05, na.rm = TRUE)){break}

        # remove ALL SIGNIFICANT items from the anchor set
        status$status[which(status$p < 0.05)] <- "flex"
        status$order[which(status$p < 0.05)] <- track_order
    }

    # unknowns become anchors
    status %>%
        mutate(status = ifelse(status == "unknown", "anchor", status))
}

# AOAA - OAT --------------------------------------------------------------
dif_AOAA_OAT <- function(data, groups){
    # I was using p-values but those get to 0 so switched to X2 values
    # It works but did so in not the absolute best code style

    dif_aoaa <-
        tibble(
            item = 1:ncol(data)
        ) %>%
        mutate(
            anova = item %>% map(~ quiet(anova(mod_constrained_baseline(data, groups), mod_flexible(data, groups, flex_items = .)))),
            X2 = anova %>% map_dbl(~ .$X2[2]),
            p = anova %>% map_dbl(~ .$p[2])
        )

    # if no items are significant, then they are all anchors and we are done
    if (all(dif_aoaa$p > 0.05)){
        return(dif_aoaa %>% mutate(status = "anchor"))
    }

    # now we know at least one item is significant and we remove it from the anchor set
    status <-
        dif_aoaa %>%
        mutate(
            status = ifelse(item == which.max(X2), "flex", "unknown"),
            order = ifelse(item == which.max(X2), 1, NA)
        )

    track_order <- 2
    while(TRUE){
        # for each unknown item, we use all other unknown items to test it
        new_status <- get_next_status_OAT(status, data, groups)

        # need to update the status df
        status <-
            status %>%
            select(-anova, -X2, -p) %>%
            left_join(new_status %>% select(-order), by = "item")

        # if no new significant tests then we're done
        if(all(status$p > 0.05, na.rm = TRUE)){break}

        # remove just THE MOST significant item from the anchor set
        status$status[which.max(status$X2)] <- "flex"
        status$order[which.max(status$X2)] <- track_order

        track_order <- track_order + 1
    }

    # unknowns become anchors now
    status %>%
        mutate(status = ifelse(status == "unknown", "anchor", status))
}
