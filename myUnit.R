#This is the preparatory script for the personal learning unit

data<-read.csv("maya-growth.csv")
data<-transform(data, KID.AGE.MONTHS=round(data$KID.AGE.YEARS*12) )
data<-transform(data, KID.AGE.DAYS=round(data$KID.AGE.MONTHS*30) )
data<-data[data$KID.ID %in% names(which(table(data$KID.ID)>8)),]
data<-addWGSR(data=data, sex="SEX", firstPart="KID.WEIGHT.KG", secondPart="KID.HEIGHT.CM", thirdPart="KID.AGE.DAYS", index="bfa", output="ZBMI", digits=2)
mayaGrowthData<-data[,c("KID.ID", "KID.AGE.MONTHS", "ZBMI")]
mayaGrowthData<-dcast(mayaGrowthData, KID.ID~KID.AGE.MONTHS, value.var="ZBMI", fun.aggregate = mean)
save(mayaGrowthData, file="mayaGrowthData.RData")

#the first column is the kids' IDs, so we save them to use them as labels
#in the clustering
kids<-mayaGrowthData[,1]
kidsNew<-paste0("X.", kids)
#remove the first two ID column
unnamedData<-mayaGrowthData[,c(-1)]

#growthPLA will be our primary PLA matrix, which we will use for clustering
growthPLA = {}
min<-61
for(i in 1:nrow(unnamedData)) {
  row<-na.omit(t(unnamedData[i,]))
  if(min>length(row)) {
    min<-length(row)
  }
}
print(min)
times<-min
for(i in 1:nrow(unnamedData)) {
  #read unnamedData per row (per child) and remove NA from each row
  row = na.omit(t(unnamedData[i,]))
  print(i)
  if(times != length(row)) {
    #do PLA if length>times
    growthPLA<-rbind(growthPLA, repr_pla(t(row), times=times-1))
  }
  #If the kid has the same number of measurements as "times", we add the entire trajectory.
  #Why? Because we cannot ask for a PLA with the same number of observations, i.e., length=times.
  else {
    growthPLA<-rbind(growthPLA, t(row))
  }
}
row.names(growthPLA)<-kidsNew

PLA_EUCL_dist = diss(growthPLA, "EUCL")
fviz_dist(PLA_EUCL_dist)
fviz_nbclust(growthPLA, pam, method = "wss") +
  geom_vline(xintercept = 4, linetype=2)
mayaPAM<-pam(growthPLA, 4)
clusplot(mayaPAM, main = "Maya Child Growth Data, clusters=4", color=TRUE)
fviz_silhouette(silhouette(mayaPAM))

mayaHclust<-hclust(PLA_EUCL_dist)
fviz_dend(mayaHclust, k=4)

intern <- clValid(growthPLA, 2:10, clMethods=c("hierarchical","kmeans","pam"),
                  validation="internal")
summary(intern)
plot(intern)

stability <- clValid(growthPLA, 2:10, clMethods=c("hierarchical","kmeans","pam"),
                     validation="stability")
summary(stability)
plot(stability)

name<-"MyUnit/maya_plot_cluster_"
clust<-mayaPAM$clustering
names(clust)<-kids
for(j in 1:nrow(mayaPAM$medoids)) {
  kids_of_cluster<-clust[which(clust == j)]
  sb<-{}
  for(i in seq_along(kids_of_cluster)) {
    sb=rbind(sb, data[which(data$KID.ID==names(kids_of_cluster)[i]),])
  }
  medoidID<-unlist(strsplit(rownames(mayaPAM$medoids)[1], "[.]"))[2]
  medoid_data = data[which(data$KID.ID == medoidID),]
  ggplot(sb, aes(KID.AGE.MONTHS,ZBMI, group=KID.ID))+geom_line()+geom_line(data=medoid_data, aes(x=KID.AGE.MONTHS,y=ZBMI),size=2,col="red")
  ggsave(paste(name,"_",j,".png",sep=""))
}
