#Balance = number of miles eligible for award travel
#QualMiles = number of miles qualifying for TopFlight status
#BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
#BonusTrans = number of non-flight bonus transactions in the past 12 months
#FlightMiles = number of flight miles in the past 12 months
#FlightTrans = number of flight transactions in the past 12 months
#DaysSinceEnroll = number of days since enrolled in the frequent flyer program


# Read the dataset AirlinesCluster.csv into R and call it "airlines"

airlines = read.csv("AirlinesCluster.csv")
summary(airlines)


##############################################################################
# Normalize the Data
##############################################################################

library(caret)

# create a normalized data frame called "airlinesNorm" by running the following
# commands:

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

#The first command above pre-processes the data, and the second command performs 
#the normalization


##############################################################################
# Hierarchical Clustering
##############################################################################

# Compute distances on normalized data
distance = dist(airlinesNorm, method = "euclidean")

# Hierarchical clustering
air.h.cluster = hclust(distance, method="ward.D")

# Plot the dendrogram
plot(air.h.cluster)

# Assign points to clusters
h.clusterGroups = cutree(air.h.cluster, k = 5)

#use tapply to compare the average values in each of the variables for the 5 
#clusters (the centroids of the clusters). You may want to compute the average
#values of the unnormalized data so that it is easier to interpret

tapply(airlines$Balance, h.clusterGroups, mean)
tapply(airlines$QualMiles, h.clusterGroups, mean)
tapply(airlines$BonusMiles, h.clusterGroups, mean)
tapply(airlines$BonusTrans, h.clusterGroups, mean)
tapply(airlines$FlightMiles, h.clusterGroups, mean)
tapply(airlines$FlightTrans, h.clusterGroups, mean)
tapply(airlines$DaysSinceEnrol, h.clusterGroups, mean)


#Advanced Explanation:
#Instead of using tapply, you could have alternatively used colMeans 
#and subset, as follows:

#colMeans(subset(airlines, h.clusterGroups == 1))
#colMeans(subset(airlines, h.clusterGroups == 2))
#colMeans(subset(airlines, h.clusterGroups == 3))
#colMeans(subset(airlines, h.clusterGroups == 4))
#colMeans(subset(airlines, h.clusterGroups == 5))

#OR n even more compact way of finding the centroids would be to use the
#function "split" to first split the data into clusters, and then to use the 
#function "lapply" to apply the function "colMeans" to each of the clusters

lapply(split(airlines, h.clusterGroups), colMeans)


##############################################################################
# K-means clustering
##############################################################################

#run the k-means clustering algorithm on the normalized data, again creating 
#5 clusters

# Specify number of clusters
k = 5

set.seed(88)
KMeansAirCluster = kmeans(airlinesNorm, centers = k , iter.max = 1000)
str(KMeansAirCluster)

#check size of each cluster
table(KMeansAirCluster$cluster)

#Ind those with size greater than 1000
table(KMeansAirCluster$cluster) > 1000

KMeansAirCluster$centers



KMSAircluster1 = subset(airlinesNorm, KMeansAirCluster$cluster == 1)










