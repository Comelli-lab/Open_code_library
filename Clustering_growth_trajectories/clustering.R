# CLUSTERING.R
#
#
# Version:  1.1
#
# Date:     2020  Jan
# Author:   Paraskevi Massara (p.massara@utoronto.ca)
#
# Versions:
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on.
#
# ==============================================================================

#== Objectives ==================================================================

# We will identify clusters using k-means and hierarchical clustering.
# Next, we will visualize the obtained clusters and we will combine clustering with
# dimention reduction methods.

#RESOURCES:http://www.sthda.com/english/

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                            Line
#TOC> -----------------------------------------------------------------
#TOC>   1        Packages                                          39
#TOC>   2        Data preparation                                  61
#TOC>   3        Clustering with K-means                           73
#TOC>   4        Clustering with PAM                               123
#TOC>   5        Hierarchical clustering                           172
#TOC> ===========================================================================

# =    1  Packages  =============================================================

if (!require(factoextra, quietly = TRUE)) {
  install.packages(factoextra)
  library(factoextra)
}

if (!require(clustertend, quietly = TRUE)) {
  install.packages(clustertend)
  library(clustertend)
}

if (!require(igraph, quietly = TRUE)) {
  install.packages(igraph)
  library(igraph)
}

if (!require(cluster, quietly = TRUE)) {
  install.packages(cluster)
  library(cluster)
}

# =    2  Data preparation  ====================================================

# We will use a R-built in dataset
# - USArrests

# Subset of the data

data("USArrests") # Loading the data set
df <- scale(USArrests) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)

# =    3  Clustering with k-means  ====================================================

# kmeans(x, centers, iter.max = 10, nstart = 1)

# .x: numeric matrix, numeric data frame or a numeric vector
# . centers: Possible values are the number of clusters (k) or a set of initial (distinct)
# cluster centers. If a number, a random set of (distinct) rows in x is chosen as
# the initial centers.
# . iter.max: The maximum number of iterations allowed. Default value is 10.
# . nstart: The number of random starting partitions when centers is a number.
# Trying nstart > 1 is often recommended.

# The function requires to specify the number of clusters.
# How to choose the right number of expected clusters?

# We will conduct k-means clustering using different values of clusters k.
# Next, the wss (within sum of square) is drawn according
# to the number of clusters. The location of a bend (knee) in the plot is generally
# considered as an indicator of the appropriate number of clusters.

fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

# Print the results
print(km.res)

# Compute the mean of each variables of each cluster

aggregate(USArrests, by = list(cluster = km.res$cluster), mean)

dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)

# Visualize the results

# If we have a multi-dimensional data set (more than one variable), we can perform
# Principal Component Analysis (PCA) and to plot data points according to the first
# two principal components coordinates

fviz_cluster(
  km.res,
  data = df,
  palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
  ellipse.type = "euclid",
  # Concentration ellipse
  star.plot = TRUE,
  # Add segments from centroids to items
  repel = TRUE,
  # Avoid label overplotting (slow)
  ggtheme = theme_minimal()
)
# =    4  Clustering with PAM  ====================================================

# pam(x, k, metric = "euclidean", stand = FALSE)

# x: possible values includes:
#   - Numeric data matrix or numeric data frame: each row corresponds to an
# observation, and each column corresponds to a variable.
# - Dissimilarity matrix: in this case x is typically the output of daisy() or
# dist()
# . k: The number of clusters
# . metric: the distance metrics to be used. Available options are "euclidean" and
# "manhattan".
# . stand: logical value; if true, the variables (columns) in x are standardized before
# calculating the dissimilarities. Ignored when x is a dissimilarity matrix.

#Estimate the optimal number of clusters.

fviz_nbclust(df, pam, method = "silhouette") +
  theme_classic()


# Compute PAM with k = 2
pam.res <- pam(df, 2)
# Print the results
print(pam.res)

# The printed output shows:
#   . the cluster medoids: a matrix, which rows are the medoids and columns are
# variables
# . the clustering vector: A vector of integers (from 1:k) indicating the cluster to
# which each point is allocated
# print(km.res)

# If you want to add the point classifications to the original data, use this:
dd <- cbind(USArrests, cluster = pam.res$cluster)
head(dd, n = 3)

# Visualize the results

# If we have a multi-dimensional data set (more than one variable), we can perform
# Principal Component Analysis (PCA) and to plot data points according to the first
# two principal components coordinates

fviz_cluster(
  pam.res,
  palette = c("#00AFBB", "#FC4E07"),
  # color palette
  ellipse.type = "t",
  # Concentration ellipse
  repel = TRUE,
  # Avoid label overplotting (slow)
  ggtheme = theme_classic()
)
# =   5  Hierarchical clustering  ====================================================

# Compute the dissimilarity matrix
# df = the standardized data

res.dist <- dist(df, method = "euclidean")

as.matrix(res.dist)[1:6, 1:6]  #display the first 6 rows and columns of the distance matrix

res.hc <- hclust(d = res.dist, method = "complete")

# Visualize the results

fviz_dend(res.hc, cex = 0.5)# cex: label size

# Cut the dendrogram into different groups

# Hierarchical clustering does not tell us how many clusters there are,
# or where to cut the dendrogram to form clusters

# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
head(grp, n = 4)

# Number of members in each cluster
table(grp)

# Get the names for the members of cluster 1
rownames(df)[grp == 1]


fviz_dend(
  res.hc,
  k = 4,
  # Cut in four groups
  cex = 0.5,
  # label size
  k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
  color_labels_by_k = TRUE,
  # color labels by groups
  rect = TRUE # Add rectangle around groups
)

# Circular dendrogram using the option type = "circular".

fviz_dend(
  res.hc,
  cex = 0.5,
  k = 4,
  k_colors = "jco",
  type = "circular"
)

# Phylogenic trees
fviz_dend(
  res.hc,
  k = 4,
  # Cut in four groups
  k_colors = "jco",
  type = "phylogenic",
  repel = TRUE,
  phylo_layout = "layout.gem"
)

# Saving dendrogram into a large PDF page
pdf("dendrogram.pdf", width = 30, height = 15) # Open a PDF
p <-
  fviz_dend(res.hc,
            k = 4,
            cex = 1,
            k_colors = "jco") # Do plotting
print(p)
dev.off()


## Quiz: Repeat hierarchical clustering using "single" as linkage method.
