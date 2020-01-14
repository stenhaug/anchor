# https://blog.dominodatalab.com/clustering-in-r/
# https://towardsdatascience.com/10-tips-for-choosing-the-optimal-number-of-clusters-277e93d72d92
# https://github.com/pimentel/cluster/blob/master/R/clusGap.R

pam1 <- function(x,k) list(cluster = pam(x, k, cluster.only=TRUE))

irt_cluster <- function(vec){

    theGap <- clusGap(matrix(vec), FUNcluster = pam1, K.max = 4)

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
            sd = sd(dif)
        ) %>%
        arrange(desc(n), sd) %>%
        mutate(status = ifelse(row_number() == 1, "unknown", "flex"))
}
