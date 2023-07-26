#Loading Data

setwd("/Users/shwetshah/Desktop")
df <- read.csv(file='AirlineLoyalty.csv', stringsAsFactors = FALSE)
row.names(df) <- df[,1];
df.norm <- data.frame(sapply(df[, -1], scale));
row.names(df.norm) <- df$ID.

#Hierarchical clustering with Euclidean distance and Ward’s method
dist <- dist(df.norm, method = "euclidean")
dist
hclus <- hclust(dist, method = "ward.D")
hclus

plot(hclus, hang = -1, ann = FALSE)
rect.hclust(hclus, k = 2, border = "black")
member_slink_1 <- cutree(hclus, k = 2)
table(member_slink_1)

df.norm$Label <- member_slink_1

#Using silhouette score to find the best k

library(cluster)

choosek <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(choosek) <- c("numClusters", "totWithinSS", "avg_silhouette")

for (k in 1:10) 
{
  set.seed(123)
 
  kmeans_result <- kmeans(df.norm, centers = k, nstart = 10)
  clusters <- kmeans_result$cluster
  
  if (k == 1) 
  {
    ss <- 0
  } 
  else 
  {
    valid_indices <- which(!is.na(clusters) & is.finite(clusters))
    ss <- silhouette(clusters[valid_indices], dist(df.norm))[valid_indices, 3]
  }
  
  choosek <- rbind(choosek, c(k, kmeans_result$tot.withinss, mean(ss)))
}  

library(ggplot2)

g <- ggplot(choosek, aes(X1, X0))
g <- g + geom_line() + geom_point()

g <- g + labs(x = "Number of Clusters (k)", y = "Average Silhouette")

g <- g + geom_text(aes(label = round(X0, 3)), vjust = -0.3)

g <- g + scale_x_continuous(breaks = seq(1, 10))
g



#Comapring the 2 clusters

cluster1_hc <- data[data$hc_cluster == 1, ]
cluster2_hc <- data[data$hc_cluster == 2, ]
cluster1_kmeans <- data[data$kmeans_cluster == 1, ]
cluster2_kmeans <- data[data$kmeans_cluster == 2, ]

#Cluster 1 comprises customers who have higher balances, more qualifying miles, higher cc1_miles, 
#and more bonus miles and transactions,indicating they are more loyal and frequent customers. 
#To retain their loyalty, exclusive discounts, and premium services can be offered as rewards and incentives.

#On the other hand, Cluster 2 customers have lower balances, fewer qualifying miles, lower cc1_miles, and fewer bonus miles
#and transactions, indicating less frequent or less loyal customers. 
#To encourage their engagement, targeted promotions aligned with their interests and preferences and bonus miles 
#for specific activities could be offered as incentives.


