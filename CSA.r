#CSA = Cluster Soccer Analysis
rm(list = ls())
cat("\014")


path <- dirname(parent.frame(2)$ofile)
setwd(path)


#call the utility.r file for usefull environment and dataset functions
source("utility.R")

environmentSettings()

environmentSettings()

FinalDatasetLight <- loadDataset(path, "FinalDatasetLight.csv")

FinalDatasetLight <- trasformDataset(FinalDatasetLight)

avg_scores <- FinalDatasetLight %>% select(-player_name, -strong_foot, -quote, -birthday, -work_rate_att, -work_rate_def)
#select(FinalDatasetLight, -player_name) 

#principal component analysis
avg_scores_pca=prcomp(avg_scores,scale. = T)

#using a smooth scatter because too many points prevent from 
#seeing the global picture
smoothScatter(avg_scores_pca$x[,1:2],nrpoints = 0, 
              colramp = colorRampPalette(c("white", "gray5")))


#Implement the Gaussian Mixture Model
xyMclust <- Mclust(data.frame (avg_scores_pca$x[,1],avg_scores_pca$x[,2]),3)
plot(xyMclust,what = "uncertainty",main=FALSE,xlab="PC1",ylab = "PC2")

player_class=xyMclust$classification
#number of players in each cluster
table(player_class)

cat("\n--------\n")

#examples of players from each cluster 1 (Difensori)
cluster1Names <- as.numeric(names(player_class[player_class==1][1:20]))
FinalDatasetLight[cluster1Names,ncol(FinalDatasetLight)] %>% print()

cat("\n--------\n")

#examples of players from each cluster 2 (Portieri)
cluster2Names <- as.numeric(names(player_class[player_class==2][1:20]))
FinalDatasetLight[cluster2Names,ncol(FinalDatasetLight)] %>% print()

cat("\n--------\n")

#examples of players from each cluster 3 (Offensivi)
cluster3Names <- as.numeric(names(player_class[player_class==3][1:20]))
FinalDatasetLight[cluster3Names,ncol(FinalDatasetLight)] %>% print()

cat("\n--------\n")

player_class[which(player_class==1)]="difensivi"
player_class[which(player_class==2)]="offensivi"
player_class[which(player_class==3)]="portieri"
player_class=factor(player_class, levels = c("difensivi", "offensivi", "portieri"))

#CROSS CHECK 
#Permette di verificare quanto le feature sono correlate rispetto al ruolo del cluster di ognuno dei 3 ruoli individuati
#TODO: trovare i valori giusti di k per estrarre le feature
#offensivi (finishing)
k <- which(colnames(avg_scores)=="finishing")
boxplot(avg_scores[,k]~player_class,main=colnames(avg_scores)[k],
        ylab="Average Score")

#portieri (gk_diving)
k <- which(colnames(avg_scores)=="gk_diving")
boxplot(avg_scores[,k]~player_class,main=colnames(avg_scores)[k],
        ylab="Average Score")

#difensivi (interceptions)
k <- which(colnames(avg_scores)=="interceptions")
boxplot(avg_scores[,k]~player_class,main=colnames(avg_scores)[k],
        ylab="Average Score")

#border players (min probabilities)
tot=data.frame(player_class,xyMclust$z[,1:3])
tot$player_name = FinalDatasetLight$player_name
colnames(tot)[2:4]=c("prob_difensivo","prob_offensivo","prob_portiere")

defensive=tot[which(tot[,1]=="difensivi"),]
offensive=tot[which(tot[,1]=="offensivi"),]
goalkeper=tot[which(tot[,1]=="portieri"),]

cat("\n--------")
print("MOLTO DIFENSIVI")
#very defensive
head(defensive[order(defensive[,2],decreasing = T),]) %>% print()
top_n(defensive[order(defensive[,2],decreasing = T),],10) %>% print()

cat("\n--------")
print("MOLTO OFFENSIVI")
#very offensive
head(offensive[order(offensive[,3],decreasing = T),]) %>% print()
top_n(offensive[order(offensive[,3],decreasing = T),],10) %>% print()

cat("\n--------")
print("MOLTO PORTIERI")
#very goalKeaper
head(goalkeper[order(goalkeper[,4],decreasing = T),]) %>% print()
top_n(goalkeper[order(goalkeper[,4],decreasing = T),],10) %>% print()


#border line, difficoult to say something
cat("\n--------\n")
print("BORDERLINE TRA DIFENSIVI E OFFENSIVI")
head(defensive[order(defensive[,2]),]) %>%  print()


