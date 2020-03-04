
eight %>%
    select(ends_with("mod"))

a <- eight$cluster_final_dif[[1]]
a

irt_cluster(a$difference_in_easy)
