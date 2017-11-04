dailyKos<-read.csv("dailykos.csv")
str(dailyKos)

distances=dist(dailyKos,method="euclidean")

set.seed(1000)
clusterkos = hclust(distances, method = "ward.D")

plot(clusterkos)



clusterGroups1=cutree(clusterkos,k=7)

cluster=subset(dailyKos,clusterGroups1==1)

cluster2=subset(dailyKos,clusterGroups1==2)

cluster3=subset(dailyKos,clusterGroups1==3)

cluster4=subset(dailyKos,clusterGroups1==4)

cluster5=subset(dailyKos,clusterGroups1==5)

cluster6=subset(dailyKos,clusterGroups1==6)

cluster7=subset(dailyKos,clusterGroups1==7)

str(cluster3)

tail(sort(colMeans(cluster3)))

KmeansCluster = kmeans(dailyKos, centers=7)

kcluster=subset(dailyKos,KmeansCluster$cluster==1)

kcluster2=subset(dailyKos,KmeansCluster$cluster==2)

kcluster3=subset(dailyKos,KmeansCluster$cluster==3)

kcluster4=subset(dailyKos,KmeansCluster$cluster==4)

kcluster5=subset(dailyKos,KmeansCluster$cluster==5)

kcluster6=subset(dailyKos,KmeansCluster$cluster==6)

kcluster7=subset(dailyKos,KmeansCluster$cluster==7)

table(clusterGroups1,KmeansCluster$cluster)