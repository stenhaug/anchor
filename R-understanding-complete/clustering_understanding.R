emlg <- read_rds(here("data-documentation/emlg.rds"))

add_clusters <- emlg %>% mutate(cluster = irt_cluster(difference_in_easy))

add_clusters %>% left_join(get_cluster_stats(add_clusters))
