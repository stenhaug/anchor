# https://blog.dominodatalab.com/clustering-in-r/
# https://towardsdatascience.com/10-tips-for-choosing-the-optimal-number-of-clusters-277e93d72d92
# https://github.com/pimentel/cluster/blob/master/R/clusGap.R

# the fancy stuff below was just giving one cluster so change to this
irt_cluster_fix3 <- function(vec){
    pam1(vec, 4)$cluster
}

pam1 <- function(x,k) list(cluster = pam(x, k, cluster.only=TRUE))

irt_cluster <- function(vec){

    theGap <- clusGap(matrix(vec), FUNcluster = pam1, K.max = 4, spaceH0 = "original")

    n_clust <-
        maxSE(
            f = theGap$Tab[ , "gap"],
            SE.f = theGap$Tab[ , "SE.sim"],
            method = "Tibs2001SEmax",
            SE.factor = 1
        )

    pam1(matrix(vec), n_clust)$cluster

}

get_cluster_stats <- function(cluster){
    cluster %>%
        group_by(cluster) %>%
        summarize(
            n = n(),
            sd = sd(difference_in_easy)
        ) %>%
        arrange(desc(n), sd) %>% # chooses based on the greatest number of items
        mutate(status = ifelse(row_number() == 1, "unknown", "flex"))
}
