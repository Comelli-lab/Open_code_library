#======= Script Information ====================
# Author: Paraskevi Massara
# Created: April 4th, 2019
# Last_updated:
# History:Version 1.0
# Resources:
#
#================================================
#Notes:
#
#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                   Line
#TOC> -------------------------------------------------------
#TOC>   1        Intalling the required packages          22                         
#TOC>   2        Preparatory steps                        83                       
#TOC>   3        Slope and area under the curve          102  
#TOC>   4        Tempo                                   138
#TOC>
#TOC> ===========================================================================


#========== 1. Packages' installation ==========

if (!require(dplyr, quietly=TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(qwraps2, quietly=TRUE)) {
  install.packages("qwraps2")
  library(qwraps2)
  options(qwraps2_markup = 'markdown')
}

if (!require(sitar, quietly=TRUE)) {
  install.packages("sitar")
  library(sitar)
}

#========== 2. Preparatory steps and discriptives of the reduced dataset =================

wide_zwei <- read.csv("wide_zwei.csv")
kids_zw<-as.character(wide_zwei[,1])

wide_zbmi <- read.csv("wide_zbmi.csv")
kids_zb<-as.character(wide_zbmi[,1])

wide_zlen <- read.csv("wide_zlen.csv")
kids_zl<-as.character(wide_zlen[,1])

wide_zlen <- read.csv("wide_zlen.csv")
kids_zl<-as.character(wide_zlen[,1])

wide_wei <- read.csv("wide_weight.csv")
kids_wei<-as.character(wide_wei[,1])

wide_hei <- read.csv("wide_height.csv")
kids_hei<-as.character(wide_hei[,1])

mylist <- c("kids_hei","kids_wei","kids_zb", "kids_zl","kids_zw")
red_kids <- union(kids_hei,kids_wei)
red_kids <- union(red_kids, mylist)
red_kids <- red_kids[1:1702]
final <- read.csv("final.csv")

# red_kids has children with measurments available over 90 months
# reduced_dataset2 is the substraction of red_kids from final

reduced_dataset<- final[which(final$Subject %in% red_kids),]
reduced_dataset2<- final[-which(final$Subject %in% red_kids),]


# Correct the irregular time series (function can be found in "myfuctions" script)

labels <- colnames(reduced_dataset2[c(13:22,30,39,32,59)])

for (i in labels) {
  reduced_dataset<-find_irts_TK(reduced_dataset2,"Subject",as.character(i))
}
saveRDS(reduced_dataset2, "reduced_dataset2.rds")


#========== 3. Slope and area under the curve ==========
m<-"zwei"
wide = read.csv(paste0("cleaned_wide_",m,"_r.csv"), check.names = F, header = T)
#the second column is the kids, so we save them to use them as labels
#in the clustering
Subject=as.character(wide[,1])
ageinmonths<-colnames(wide)[-1]
#remove the first two columns (index and Subject.X)
wide = wide[,-c(1)]
#features<-data.frame(Subject, stringsAsFactors = FALSE)



#Slope represents the average growth velocity, assuming a linear growth model from the first measurement to the last
slopes <- data.frame(Subject, stringsAsFactors = FALSE)
slopes<-mutate(slopes, slopes_zwei=numeric(length(Subject)))
#AUC is the total growth size, 
auc<- data.frame(Subject, stringsAsFactors = FALSE)
auc<-mutate(auc, auc_zwei=numeric(length(Subject)))
#Tempo is ...
tempo <- data.frame(Subject, stringsAsFactors = FALSE)
tempo <- mutate(tempo, tempo_zwei = numeric(length(Subject)))

for(i in 1:nrow(wide)) {
  print(i)
  #read total_PLA per row (per child) and remove NA from each row
  row.df<-cbind(as.numeric(ageinmonths), as.numeric(t(wide)[,i]))
  row.df<-as.data.frame(row.df)
  colnames(row.df)<-c("ageinmonths", m)
  row.df <- na.omit(row.df)
  row_lm = lm(row.df[[m]]~row.df$ageinmonths)
  slopes[which(slopes$Subject==Subject[i]),]$slopes_zwei<-row_lm$coefficients[2]
  integrand<-function(x) {row_lm$coefficients[2]*x+row_lm$coefficients[1]}
  auc[which(auc$Subject==Subject[i]),]$auc_zwei<-integrate(integrand, row.df$ageinmonths[1], row.df$ageinmonths[length(row.df$ageinmonths)])$value
  tempo[which(tempo$Subject==Subject[i]),]$tempo_zwei<-auc[which(auc$Subject==Subject[i]),]$auc_zwei/(row.df[nrow(row.df),]$ageinmonths - row.df[1,]$ageinmonths)
}

features<-full_join(features,slopes, by="Subject")
features<-full_join(features,auc, by="Subject")
features<-full_join(features,tempo, by="Subject")

#We define the columns we need to add plus the "Subject" column, which will be used as the "by" argument for join.
cols<-c("Subject", "CHILDGENDER", "birthweight_kilo.x", "birthage", "n", "FULLTERM_YN")
#Next, we subset the dataframe to get just these columns.
subset<-distinct_total[cols]
#We join the subset with the origin data frame by the Subject
features<-left_join(features, subset, by="Subject")

saveRDS(features,"features1.rds")

names(features)[12]<-"auc_zlen"

#features<- right_join(distinct_total, features, by="Subject")



saveRDS(features,"features1.rds")

#========== 4. Tempo ==========

reduced_dataset <-readRDS("reduced_dataset.rds")
tempo<-auc/(ageinmonts[length(ageinmonths)]-ageinmonths[length(1)]) 

tempo_denm <-{}
for (i in reduced_dataset$Subject) {
 tempo_denm <-(reduced_dataset[i,]$ageinmonths-reduced_dataset[i,]$ageinmonths[length(reduced_dataset[i,]$ageinmonths)])
}
