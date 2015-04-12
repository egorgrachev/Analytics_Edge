airlines = read.csv("AirlinesCluster.csv")
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

distance = dist(airlinesNorm, method="euclidean")
clusterH = hclust(distance, method="ward.D")
plot(clusterH)

k = 5
clustersH = cutree(clusterH, k=5)
plot(clustersH)
summary(clustersH)
Cluster1H = subset(airlinesNorm, clustersH==1)
nrow(Cluster1)

tapply(airlines$Balance,clustersH,mean)
tapply(airlines$QualMiles,clustersH,mean)
tapply(airlines$BonusMiles,clustersH,mean)
tapply(airlines$BonusTrans,clustersH,mean)
tapply(airlines$FlightMiles,clustersH,mean)
tapply(airlines$FlightTrans,clustersH,mean)
tapply(airlines$DaysSinceEnroll,clustersH,mean)


set.seed(88)
k=5
clustersK = kmeans(airlinesNorm, centers=k)
KMC = clustersK$cluster

Cluster1 = subset(airlines, KMC == 1)
Cluster2 = subset(airlines, KMC == 2)
Cluster3 = subset(airlines, KMC == 3)
Cluster4 = subset(airlines, KMC == 4)
Cluster5 = subset(airlines, KMC == 5)

nrow(Cluster1)
nrow(Cluster2)
nrow(Cluster3)
nrow(Cluster4)
nrow(Cluster5)

