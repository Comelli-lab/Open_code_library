# Time-Series_Clustering.R
#
#
# Version:  1.1
#
# Date:     2018  12
# Author:   Paraskevi Massara (p.massara@utoronto.ca)
#
# Versions:
#   
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================


#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                        Line
#TOC> ------------------------------------------------------------
#TOC>   1        Functions                                      44
#TOC>   2        Preparations                                   71
#TOC>   3        Time-series representation                    104
#TOC>   3.1      Piecewise-Linear Approximation                106
#TOC>   4        Growth trajectory clustering                  146
#TOC>   4.1      Distance                                      148
#TOC>   4.1.1    Practice                (part 1/2)            168
#TOC>   4.2      More alignments                               178
#TOC>   4.2.1    Practice                (part 2/2)            200
#TOC>
#TOC> ==========================================================================

# =    1  Functions  ===========================================================

plotPAMdata <- function(pam, name) {
  # Function to plot trajectories per cluster and their medoids.
  #
  # Parameters:
  #   pam   pam.object   the result of the pam() function.
  #   name  chr          a filename without extensions. The function will add the name of the cluster
  #                      and the extension.
  # Value:
  #   none               it creates a plot in a file for each cluster in pam.
  
  clust<-pam$clustering
  names(clust)<-kids
  for(j in 1:nrow(pam$medoids)) {
    kids_of_cluster<-clust[which(clust == j)]
    sb<-{}
    for(i in seq_along(kids_of_cluster)) {
      sb=rbind(sb, data[which(data$KID.ID==names(kids_of_cluster)[i]),])
    }
    medoidID<-unlist(strsplit(rownames(pam$medoids)[j], "[.]"))[2]
    medoid_data = data[which(data$KID.ID == medoidID),]
    ggplot(sb, aes(KID.AGE.MONTHS,ZBMI, group=KID.ID))+geom_line()+geom_line(data=medoid_data, aes(x=KID.AGE.MONTHS,y=ZBMI),size=2,col="red")
    ggsave(paste(name,"_",j,".png",sep=""))
  }
}

# =    2  Preparations  ========================================================

if (! require(zscorer, quietly=TRUE)) {
  install.packages("zscorer")
  library(zscorer)
}

if (! require(reshape2, quietly=TRUE)) {
  install.packages("reshape2")
  library(reshape2)
}

if (! require(TSrepr, quietly=TRUE)) {
  install.packages("TSrepr")
  library(TSrepr)
}

if (! require(TSclust, quietly=TRUE)) {
  install.packages("TSclust")
  library(TSclust)
}

if (! require(factoextra, quietly=TRUE)) {
  install.packages(factoextra)
  library(factoextra)
}

# Read the input data from the providede CSV file.
## The file contains anthropometric (age, height, weight) of Maya children and their mothers.
## Measurements were taken at different ages of the children, which provides us with a growth
## trajectory for each child.
data<-read.csv("maya-growth.csv")

# The data measures the age in years, but the representation needs the age in months
# and the calculation of zBMI scores in days.
data<-transform(data, KID.AGE.MONTHS=round(data$KID.AGE.YEARS*12) )
data<-transform(data, KID.AGE.DAYS=round(data$KID.AGE.MONTHS*30) )

# In practice, PLA is a dimensionality reduction method. The need is to have all trajectories be
# of equal size to enable the calculation of distances. However, we need to maintain long enough
# trajectories to get meaningful results. For this, we exclude children that have less than
# 8 measurements.
data<-data[data$KID.ID %in% names(which(table(data$KID.ID)>8)),]

# We calculate the BMI index for each child normalized for age (z-scores for BMI).
data<-addWGSR(data=data, sex="SEX", firstPart="KID.WEIGHT.KG", secondPart="KID.HEIGHT.CM", thirdPart="KID.AGE.DAYS", index="bfa", output="ZBMI", digits=2)

# For the clustering analysis we will need  only the children BMI measurements and the time of the
# measurements.
mayaGrowthData<-data[,c("KID.ID", "KID.AGE.MONTHS", "ZBMI")]
head(mayaGrowthData)

# You will notice that we have a long table. This means that every measurement is a different row.
# This results in multiple rows for every child. Since our goal is to cluster children, we will
# need a row for every child. So, we transform the long table in a wide one, where the IDs are
# the rows, the measurements are the columns and the values are the zBMI scores.
mayaGrowthData<-dcast(mayaGrowthData, KID.ID~KID.AGE.MONTHS, value.var="ZBMI", fun.aggregate = mean)

# =    3  Time-series representation  ========================================================

# =    3.1  Piecewise-Linear Approximation  ========================================================

# The first column in our table is the kids' IDs, so we save them to use them as labels
# in the clustering
kids<-mayaGrowthData[,1]

# This step is optional. Since the IDs are numbers, this may cause some confusion later, so
# we transform the numbers in strings.
kidsNew<-paste0("X.", kids)

# For PLA, we will need only the values so we will need to exclude the ID column for now.
unnamedData<-mayaGrowthData[,c(-1)]

# growthPLA will be our primary PLA matrix, which we will use for clustering
growthPLA = {}

# We decided already that the minimum number of measurements is 8, which will also be the length
# of our PLA trajectories.
times=4

#We need to translate every kid, i.e., every row of measurements, into its PLA format.
#Thus, we traverse each row of our data and we calculate its PLA representation
#for the minimum number of measurements (after we remove NA values).
for(i in 1:nrow(unnamedData)) {
  # Read unnamedData per row (per child) and remove NA from each row.
  # Careful! We need to transpose the row, because na.omit() will remove an entire row
  # if it finds a NA value.
  row = na.omit(t(unnamedData[i,]))
  print(i)
  if(times != length(row)) {
    # Do PLA if length>times
    growthPLA<-rbind(growthPLA, repr_pla(t(row), times=times-1))
  }
  # If the kid has the same number of measurements as "times", we add the entire trajectory.
  # Why? Because we cannot ask for a PLA with the same number of observations, i.e., length=times.
  else {
    growthPLA<-rbind(growthPLA, t(row))
  }
}

# Name our PLA table.
row.names(growthPLA)<-kidsNew

# =    4  Growth trajectory clustering  ========================================================

# =    4.1  Distance  ==========================================================================

# Calculate the euclidean distance between children on our PLA data.
PLA_EUCL_dist = diss(growthPLA, "EUCL")

# We can visualize the distances using the factoextra package.
# Blue is "cold" (greater distance), red is "hot" (greater similarity).
# Even in this visualization, you should start discern some groups (boxes of red or blue).
fviz_dist(PLA_EUCL_dist)

# Depending how conservative we want to be with the cohesiveness of our clusters
# (i.e., how red a box easy) we can pick between 2 and 4 clusters. Let's get some more help.
fviz_nbclust(growthPLA, pam, method = "wss") +
  geom_vline(xintercept = 4, linetype=2)
# This visualization should show you how the within sum of square (WSS) value diminishes as we
# increase the number of clusters. Naturally, if we had as many clusters as data points our
# cohesiveness would be perfect, but that's hardly useful. Instead, we pick the value where we
# stop seeing a dramatic improvement in the WSS value (here is at k=4). This is called the
# elbow method (because of the shape of the curve)

# =    4.1.1  Practice   (part 1/2)  =============================================

# Write and submit code to:
## - calculate the DTWARP distances for the Maya growth data.
## - produce the distance visualization.
# Compare the euclidean and the dynamic time warping distances.
## - do you notice significant differences?
## - do you believe that these differences can affect the clustering results?
##   (you can confirm your presumptions later)

# =    4.2  Clustering  =======================================================================

# Now we are ready to apply our clustering, using the pam() method.
mayaPAM<-pam(growthPLA, 4)

mayaPAM #an overview of the results
mayaPAM$clusinfo #check the clustering results
mayaPAM$clustering #check the actual clustering
mayaPAM$medoids #check the medoid data
mayaPAM$silinfo #check the quality by silhouette

# We can also plot the results using either the clusplot() or the fviz_cluster function.
# The results should be similar. Notice that cluster 1 has an isolated point, which can also
# been seen in the elevated silhouette width for this cluster.Notice also that clusters 2 and 3
# seem to have overlapping elements. This is because the data cannot be explained with just
# two dimensions, but this is not important for now.
fviz_cluster(mayaPAM)
clusplot(mayaPAM, main = "Maya Child Growth Data, clusters=4", color=TRUE)

# We can also visualize the silhouette of the clustering to assess its quality.
fviz_silhouette(silhouette(mayaPAM))

# =    4.2.1  Practice  (part 2/2)  ============================================

# Write and submit code to:
## - create a pam clustering using the DTWARP distance;
## - create a hierarchical clustering using the Euclidean distance;
## - create a hierarchical clustering using the DTWARP distance.
# Create the visualizations for all clusterings (cluster plots and dendrograms, colored).
## - Are the clusters similar?
## - Use the cluster.evaluation() function of the TSclust package to quantify the differences.
##   Consider our initial pam-EUCL clustering as the ground truth.