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
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. 
#
# ==============================================================================

#== Objectives ==================================================================

# We will start by computing the distance matrix using different methods. 
# Next, we will compute the distance matrix after standarizing the data.  
# Finally, we will visualize the distance matrices.

#RESOURCES:http://www.sthda.com/english/

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                            Line
#TOC> -----------------------------------------------------------------
#TOC>   1        Packages                                          39
#TOC>   2        Data preparation                                  52      
#TOC>   3        Computing distances                               62
#TOC>   4        Data standarization                               78                
#TOC>
#TOC> ===========================================================================

# =    1  Packages  =============================================================

if (! require(factoextra, quietly=TRUE)) {
  install.packages(factoextra)
  library(factoextra)
}

if (! require(clustertend, quietly=TRUE)) {
  install.packages(clustertend)
  library(clustertend)
}


# =    2  Data preparation  ====================================================

# We will use a R-built in dataset
# - USArrests

# Subset of the data
set.seed(123)
ss <- sample(1:50, 15) # Take 15 random rows
df <- USArrests[ss, ] # Subset the 15 rows

# =   3  Computing distances  ====================================================

# Using euclidean metric
dist.eucl <- dist(df, method = "euclidean")

# Using manhattan metric
dist.manh <- dist(df, method = "manhattan")

# Other metrics "binary", "minkowski"

# Let's visualize the distance matrices.
raw_eucl_m<-fviz_dist(dist.eucl)
raw_manh_m<-fviz_dist(dist.manh)

# Quiz: Do you observe any differences? Why?

# =   4  Data standarization  ====================================================

# Let's repeat the experiment using standarized data this time. 

df.scaled <- scale(df) # Standardize the variables
# Using euclidean metric

dist.eucl <- dist(df.scaled, method = "euclidean")

# Using manhattan metric
dist.manh <- dist(df.scaled, method = "manhattan")

# Let's visualize the distance matrices.
std_eucl_m<-fviz_dist(dist.eucl)
fviz_dist(dist.manh)

# Quiz: Do you observe any differences? Why?


