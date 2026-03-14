# DISTANCE MATRIX COMPUTATION.R
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
# We'll start by describing the different measures in the clValid package for comparing
# clustering algorithms. Next, we'll present the function *clValid*(). Finally, we'll
# provide R scripts for validating clustering results and comparing clustering algorithms. 
#
# ==============================================================================

#== Objectives ==================================================================

# We will start by computing the distance matrix using different methods. 
# Next, we will compute the distance matrix after standarizing the data.  
# Finally, we will visualize the distance matrices.

#RESOURCES:http://www.sthda.com/english/

#
# =    1  Packages  =============================================================

if (! require(clValid, quietly=TRUE)) {
  install.packages(clValid)
  library(clValid)
}

if (! require(clustertend, quietly=TRUE)) {
  install.packages(clustertend)
  library(clustertend)
}

# Iris data set:
# - Remove Species column and scale
df <- scale(iris[, -5])
# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(df, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)

# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(df, nClust = 2:6, clMethods = clmethods,
                validation = "stability")

# Display only optimal Scores

# The average proportion of non-overlap (APN) measures the average proportion of observations not placed in the
# same cluster by clustering based on the full data and clustering based on the
# data with a single column removed.
# . The average distance (AD) measures the average distance between observations placed in the same
# cluster under both cases (full data set and removal of one column).
# . The ADM measures the average distance between cluster centers for observations
# placed in the same cluster under both cases.
# . The FOM measures the average intra-cluster variance of the deleted column,
# where the clustering is based on the remaining (undeleted) columns



optimalScores(stab)


# For the APN and ADM measures, hierarchical clustering with two clusters again
# gives the best score. For the other measures, PAM with six clusters has the best
# score.

























