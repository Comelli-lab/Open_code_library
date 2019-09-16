#======= Script Information ====================
# Author: Paraskevi Massara
# Created: Aug 20th, 2019
# Last_updated:
# History:Version 1.0
# Resources:
#
#================================================
#
#
#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                   Line
#TOC> -------------------------------------------------------
#TOC>   1        Installing packages                      21
#TOC>   2        Data preparation                         58
#TOC>   3        Data analysis                           106
#TOC>  
#TOC> ===========================================================================

#========== 1. Packages' installation ==========

if (!require(dplyr, quietly=TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(plyr, quietly=TRUE)) {
  install.packages("plyr")
  library(plyr)
}

if (!require(SNFtool, quietly=TRUE)) {
  install.packages("SNFtool")
  library(SNFtool)
}

if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
  BiocManager::install("impute", version = "3.8")
}

if (!require(glmnet, quietly=TRUE)) {
  install.packages("glmnet")
  library(glmnet)
}

if (!require(randomForest, quietly=TRUE)) {
  install.packages("randomForest")
  library(randomForest)
}

if (!require(RColorBrewer, quietly=TRUE)) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

#========== 2. Data Preparation ===========

#---- 2.1 Read the data----------

features<- readRDS("no_NA_features_kids.rds")


# Data set with annual zlen measurments (NAs were extrapolated using splines)
splines_zlen<- read.csv("splines_annual_zlen_2.csv",check.names = FALSE, header = TRUE)
colnames(splines_zlen)[1]<-"Subject"


common <- intersect(features$Subject, splines_zlen$Subject)  

#Extract the subjects IDs from the splines_zlen file to match them with the features files  

features<-features[features$Subject %in% common,]
splines_zlen<-splines_zlen[splines_zlen$Subject %in% common,]
saveRDS(splines_zlen, "snf_splines.rds")

#splines_zlen<-splines_zlen[,-1]
#features<-features[,-1]
#Check that there are identical individuals in both datasets 
splines_zlen <- readRDS(splines_zlen, "snf_splines.rds")

#Transform Subject column to rownames and remove the non-numeric column
rownames(features)<-features[,1]
features<-features[,-1]
rownames(splines_zlen) <- splines_zlen[,1]
splines_zlen<-splines_zlen[,-1]

#features are factors. Convert to numeric so that the standardisation works
for(col in colnames(features)) {
  features[[col]]<-as.numeric(features[[col]])
}

#Load clustering with zbmi
km_zbmi_2 <- readRDS("splines_annual_zbmi_2_kmeans_2.rds")

#Create clustering data frame with id (Subject) and cluster columns (clusts)
Subjects <- names(km_zbmi_2$cluster)
zbmi_map <- data.frame(Subject=Subjects, clusts=km_zbmi_2$cluster)

#Confirm that the two datasets have identical Subjects and merge them in a list
identical(rownames(splines_zlen), rownames(features))
splines_features_list <- list(splines_zlen, features)
names(splines_features_list) <- c("splines_zlen", "features" )

#========== 3. Analysis ===========
#-------- set parameteres-------------
K = 20 ##number of neighbors, must be greater than 1. usually (10~30)
alpha = 0.5 ##hyperparameter, usually (0.3~0.8)
T = 20 ###Number of Iterations, usually (10~50)

# -------- Standarize our parameteres---------
std.list <- lapply(splines_features_list,standardNormalization)
#Calculate distances
dist.list <- lapply(std.list, function(x){dist2(x,x)^(1/2)})
#Create affinity matrices (and put them in a list)
aff.list <- lapply(dist.list, function(x){affinityMatrix(x,K,T)})

#Estimate number of clusters for the two datasets
estimateNumberOfClustersGivenGraph(aff.list[["splines_zlen"]],NUMC=2:6)
estimateNumberOfClustersGivenGraph(aff.list[["features"]],NUMC=2:6)

#Cluster subjects based on affinity using spectral clustering
#(clusters=2 according to the previous step)
grps.splines <- spectralClustering(affinity = aff.list[["splines_zlen"]], K=2)
grps.features <- spectralClustering(affinity = aff.list[["features"]], K=2)

table(grps.features)
table(grps.splines)
table(grps.splines,grps.features)

barplot(table(grps.features))
barplot(table(grps.splines))
barplot(table(grps.splines,grps.features))

#Heatmaps for the individual clusters
displayClustersWithHeatmap(W=aff.list[["splines_zlen"]],group = grps.splines , col= brewer.pal(name = "Purples", n=8))
displayClustersWithHeatmap(W=aff.list[["features"]],group = grps.features , col= brewer.pal(name = "Greens", n=8))

#Start SNF
snf.mat <- SNF(aff.list, K, alpha)
colnames(snf.mat) <- rownames(snf.mat) <-rownames(splines_zlen)

#Cluster SNF merged dataset
estimateNumberOfClustersGivenGraph(snf.mat, NUMC = 2:8)
clusts <- spectralClustering(affinity = snf.mat, K=2)
displayClustersWithHeatmap(snf.mat,group = clusts , col= brewer.pal(name = "Spectral", n=8))

#Preparation for comparison
#Merge all clusters (SNF, zlen and features) in one dataframe
clust.df <- data.frame(cbind(rownames(splines_features_list[[1]]), clusts, grps.features, grps.splines))
colnames(clust.df)<- c("Subject", "snf.clusts", "features.clusts", "splines.clusts")

#Merge truth (zbmi) with clust.df
merged.grth.map <- merge(zbmi_map, clust.df, by="Subject")

#Evaluate quality of SNF clustering based on truth
table(merged.grth.map$snf.clusts, merged.grth.map$clusts)
barplot(table(merged.grth.map$snf.clusts, merged.grth.map$clusts), col = brewer.pal(n=2, name = "Spectral"))









