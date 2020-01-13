get_next_status <- function(status, data, groups){
    the_flex <- which(status$status == "flex")

    status %>%
        filter(status == "unknown") %>%
        mutate(
            p = item %>%
                map_dbl(~ test_with_known_flex(data, groups, flex_known = the_flex, flex_test = .))
        ) %>%
        select(-status)
}

dif_iterative_backward <- function(data, groups, all_other){

    if (all(all_other$p > 0.05)){
        return(
            all_other %>%
                mutate(
                    status = ifelse(item == which.min(p), "flex", "unknown")
                )
        )
    }

    status <-
        all_other %>%
        mutate(
            status = ifelse(item == which.min(p), "flex", "unknown")
        )

    while(TRUE){
        new_status <- get_next_status(status, data, groups)

        status <-
            status %>%
            select(-p) %>%
            left_join(new_status, by = "item")

        if(all(status$p > 0.05, na.rm = TRUE)){break}

        status$status[which.min(status$p)] <- "flex"
    }

    status
}
