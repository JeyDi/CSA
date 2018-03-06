#CSAUni = Cluster Soccer Analysis
#This file implement k-means cluster 
rm(list = ls())
cat("\014")


path <- dirname(parent.frame(2)$ofile)
setwd(path)

#call the utility.r file for usefull environment and dataset functions

source("utility.R")

environmentSettings()

FinalDatasetLight <- loadDataset(path, "FinalDatasetLight.csv")

FinalDatasetLight <- trasformDataset(FinalDatasetLight)

FinalDatasetLight %>% select(-player_name, -strong_foot, -quote, -birthday, -id, -work_rate_def, -work_rate_att ) -> PCADatasetLightEdit

#Scale the dataset for better classification
scale(PCADatasetLightEdit) -> PCADatasetLightEdit

#---------------------------------------------------------------------------------------#

cat("\nKMEANS Clustering \n")
#Set the seed and the number of clusters and do the kmeans
set.seed(38)
n_clusters <- 3
soccerCluster = kmeans(PCADatasetLightEdit, n_clusters)


# Visualize kmeans clustering (BAD)
fviz_cluster(soccerCluster, PCADatasetLightEdit, ellipse.type = "norm")+
  theme_minimal() %>% print()

#Another visualization of cluster
clusterVizKMeans <- fviz_cluster(soccerCluster, data = PCADatasetLightEdit, geom = "point",
                  stand = FALSE, frame.type = "norm")
print(clusterVizKMeans)


#Visualize the cluster aggregation
soccerClusterAggregation <- aggregate(PCADatasetLightEdit,by=list(soccerCluster$cluster),FUN=mean)
names(soccerClusterAggregation)[1]<-paste("ClusterId" )
writeDatasetCSV(soccerClusterAggregation, "kmeansClusterAggregation")

#Save the centers from the clusterization for the plot
soccerClusterCenters=as.data.frame(soccerCluster$centers)

#insert in the PCADatasetLight the clusters predicted
FinalDatasetLight$cluster <- factor(soccerCluster$cluster)

writeDatasetCSV(FinalDatasetLight, "FinalDatasetLight") #output the result to CSV

#Get the top 20 players
top20 <- 
  FinalDatasetLight %>% 
  arrange(desc(overall)) %>% 
  head(n = 20) %>%
  as.data.frame()


#Display the players tabel for export
top20 %>% 
  select(player_name,cluster, height, weight, overall, potential, strong_foot,interceptions,gk_diving,finishing) %>% 
  datatable(., options = list(pageLength = 10),caption=(h1="Best Players")) -> datatableTop20
DT::saveWidget(datatableTop20,file="dataTableCluster.html")
print(top20)


#Attaccanti
cat("\nClusterizzazione Attaccanti appartenenti alla classe 1\n")
FinalDatasetLight %>%
  filter(cluster==1) %>%
  select(player_name, cluster, interceptions, gk_diving, finishing, quote, overall) %>% 
  top_n(.,30,overall) %>%
  print() %>% 
  writeDatasetCSV(., "result-clusterAttaccanti") -> ClusterAttaccanti

#Difensori
cat("\nClusterizzazione Difensori appartenenti alla classe 2\n")
FinalDatasetLight %>%
  filter(cluster==2) %>% 
  select(player_name, cluster, interceptions, gk_diving, finishing, quote, overall) %>% 
  top_n(.,30,overall) %>%
  print() %>% 
  writeDatasetCSV(., "result-clusterDifensori")-> ClusterDifensori

#Portieri
cat("\nClusterizzazione Portieri appartenenti alla classe 3\n")
FinalDatasetLight %>% filter(cluster==3) %>% 
  select(player_name, cluster, interceptions, gk_diving, finishing, quote, overall) %>% 
  top_n(.,30,overall) %>%
  print() %>% 
  writeDatasetCSV(., "result-clusterPortieri") -> ClusterPortieri

#Create the 3D Plot with Plotly
#You have to print manually with print(Plot3D)
cat("\nplot the 3D visualization of cluster by attributes")
cat("Need to launch the plot manually if you want to display the results\n")
Plot3D <- plot_ly(FinalDatasetLight, x = ~interceptions, y = ~gk_diving, z = ~finishing, color = ~cluster, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Interception'),
                      yaxis = list(title = 'GK Diving'),
                      zaxis = list(title = 'Finishing'))) %>% 
  layout(title = 'Classification with principal attributes for players')
htmlwidgets::saveWidget(Plot3D,file="kmeans3D.html")


#---------------------------------------------------------------------------------------#
cat("\nPAM Clustering \n")
#PAM Cluster
set.seed(38)
n_clusters <- 3
soccerClusterPAM <- pam(PCADatasetLightEdit, n_clusters)

clusterVizKMeans <- fviz_cluster(soccerClusterPAM, stand = FALSE, geom = "point",
                        frame.type = "norm")
print(clusterVizKMeans)


#---------------------------------------------------------------------------------------#


#Silhouette KMEANS
cat("\nCompute Silhouette Kmeans Method\n")
kms <- silhouette(soccerCluster$cluster, dist(PCADatasetLightEdit))
rownames(kms) <- rownames(PCADatasetLightEdit)
head(kms[, 1:3])
#windows() #Rstudio sometimes doesn't display the plot correctly
plot(kms,col=1:8,border=NA) %>% print()
fviz_silhouette(kms) %>% print() #better visualization of silhouette


#Silhouette PAM
cat("\nCompute Silhouette PAM Method\n")
pam <- silhouette(soccerClusterPAM$cluster, dist(PCADatasetLightEdit))
rownames(pam) <- rownames(PCADatasetLightEdit)
head(pam[, 1:3])
#windows() #Rstudio sometimes doesn't display the plot correctly
plot(pam,col=1:8,border=NA) %>% print()
fviz_silhouette(pam) %>% print() #better visualization of silhouette

# #Other method to find best cluster number
# cat("\nMy method to find best number of clusters\n")
# gap_stat <- clusGap(PCADatasetLightEdit, FUN = kmeans, nstart = 25, 
#                     K.max = 10, B = 500) 
# fviz_gap_stat(gap_stat)

#Trovare il numero di cluster ottimale:
cat("\nStandard Method to find best number of clusters\n")
nk = 2:10
set.seed(22)

sw = sapply(nk, function(k) {
  cluster.stats(dist(PCADatasetLightEdit), kmeans(PCADatasetLightEdit, centers=k)$cluster)$avg.silwidth
})

plot(nk, sw,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

cat("\nMethod to find best number of clusters with fviz and Kmeans\n")
fviz_nbclust(PCADatasetLightEdit, kmeans, method = "silhouette") %>% print()

cat("\nMethod to find best number of clusters with fviz and PAM\n")
fviz_nbclust(PCADatasetLightEdit, pam, method = "silhouette") %>% print()


cat("\nMethod to find best number of clusters with fviz and Hierarchical Clustering\n")
fviz_nbclust(PCADatasetLightEdit, hcut, method = "silhouette") %>% print()

#---------------------------------------------------------------------------------------#

# MATRICE DI DISSIMILARITA
# Misura della qualità di un cluster (heat-map) che plotta i
# coefficienti di dissimilarità di un oggetto intracluster
# e intercluster

cat("\nCalcolo matrice di Dissimilarità\n")
dissplot(dist(PCADatasetLightEdit), labels=soccerCluster$cluster,options=list(main="Kmeans Soccer Clustering With k=3"))


cat("\nFINISH...\n")

