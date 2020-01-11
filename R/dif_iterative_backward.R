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

dif_iterative_backward <- function(data, groups){

    dif_cb <- dif_all_other(data, groups)

    if (all(dif_cb$p > 0.05)){
        return(
            dif_cb %>%
                mutate(
                    status = ifelse(item == which.min(p), "flex", "unknown")
                )
        )
    }

    status <-
        dif_cb %>%
        mutate(
            status = ifelse(item == which.min(p), "flex", "unknown")
        )

    while(TRUE){
        new_status <- get_next_status(status, sim_1_dif$data, sim_1_dif$groups)

        status <-
            status %>%
            select(-p) %>%
            left_join(new_status, by = "item")

        if(all(status$p > 0.05, na.rm = TRUE)){break}

        status$status[which.min(status$p)] <- "flex"
    }

    status
}
