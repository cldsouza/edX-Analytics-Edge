#read the doc 
dailykos = read.csv("dailykos.csv")

# Compute distances
distance = dist(dailykos, method = "euclidean")

# Hierarchical clustering
kos.h.cluster = hclust(distance, method="ward.D")

# Plot the dendrogram
plot(kos.h.cluster)

# Assign points to clusters
clusterGroups = cutree(kos.h.cluster, k = 7)

#use the subset function to subset our data by cluster. Create 7 new datasets, 
#each containing the observations from one of the clusters. 

cluster1 = subset(dailykos, clusterGroups==1)
cluster2 = subset(dailykos, clusterGroups==2)
cluster3 = subset(dailykos, clusterGroups==3)
cluster4 = subset(dailykos, clusterGroups==4)
cluster5 = subset(dailykos, clusterGroups==5)
cluster6 = subset(dailykos, clusterGroups==6)
cluster7 = subset(dailykos, clusterGroups==7)

#There is a very useful function in R called the "split" function. 
#Given a vector assigning groups like clusterGroups, you could split dailykos 
#into the clusters by typing:

#HierCluster = split(dailykos, clusterGroups)

#Then cluster 1 can be accessed by typing HierCluster[[1]], 
#cluster 2 can be accessed by typing HierCluster[[2]], etc. 
#If you have a variable in your current R session called "split",
#you will need to remove it with rm(split) before using the split function.

# we'll just look at the top 6 words in each cluster

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))


##############################################################################
# K-means clustering
##############################################################################

# Specify number of clusters
k = 7

set.seed(1000)
KMeansCluster = kmeans(dailykos, centers = k)
str(KMeansCluster)

# Subset your data into the 7 clusters (7 new datasets) by using the "cluster"
# variable of your kmeans output. 

KMScluster1 = subset(dailykos, KMeansCluster$cluster == 1)
KMScluster2 = subset(dailykos, KMeansCluster$cluster == 2)
KMScluster3 = subset(dailykos, KMeansCluster$cluster == 3)
KMScluster4 = subset(dailykos, KMeansCluster$cluster == 4)
KMScluster5 = subset(dailykos, KMeansCluster$cluster == 5)
KMScluster6 = subset(dailykos, KMeansCluster$cluster == 6)
KMScluster7 = subset(dailykos, KMeansCluster$cluster == 7)

# we'll just look at the top 6 words in each cluster

tail(sort(colMeans(KMScluster1)))
tail(sort(colMeans(KMScluster2)))
tail(sort(colMeans(KMScluster3)))
tail(sort(colMeans(KMScluster4)))
tail(sort(colMeans(KMScluster5)))
tail(sort(colMeans(KMScluster6)))
tail(sort(colMeans(KMScluster7)))


