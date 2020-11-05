# NANOSTRING NORM
#
#
# Version: 3.0
#
# Date: April 30, 2020
# Updated: May 6, 2020
# Authors: Zoe Lofft  (email: zlofft@gmail.com)
# Edits: Evi Massara (massaraevi@yahoo.com)
#
# OBJECTIVES:

# NOTES:
# RESOURCES:

#TOC> ==========================================================================
#TOC>
#TOC>   Pipeline Section 1) Normalization                                Line
#TOC> --------------------------------------------------------------------------
#TOC>   1        Packages
#TOC>   2        Dataset Exploration
#TOC>   3        Normalization
#TOC>   4        Exporting Matrix of Normalized Genes
#TOC> ==========================================================================

# ====    1  Packages  =========================================================
if (!require(NanoStringNorm, quietly = TRUE)) {
  install.packages("NanoStringNorm")
  library(NanoStringNorm)
}

if (!require(BiocManager, quietly = TRUE)) {
  install.packages("BiocManager")
  library(BiocManager)
}

if (!require(vsn, quietly = TRUE)) {
  BiocManager::install("vsn")
  library(vsn)
}

# ====    2 Dataset Exploration  ===============================================
# Import your raw data set from NanoString
# Keep first 3 columns (Code_Class, Name, Accession, and all samples of interest with corresponding counts)
ns_norm <- read.delim("~/Desktop/ns_norm_raw.txt")
str(ns_norm)

# You will need to turn the following factor variables into character variables 
ns_norm$Code.Class <- as.character(ns_norm$Code.Class)
ns_norm$Name <- as.character(ns_norm$Name)
ns_norm$Accession <- as.character(ns_norm$Accession)

# View the structure to see if it worked
str(ns_norm)

# ====    3 Normalization  ===============================================
# Now you will apply the normalization parameters by calling the function "NanoStringNorm"
# Arguments in this function can be chaned to best suit the data you are normalizing 

data_normalized_test <-
  NanoStringNorm(
    x = ns_norm,
    CodeCount = "geo.mean",
    Background = "mean",
    SampleContent = "top.geo.mean",
    round.values = TRUE,
    take.log = TRUE,
    return.matrix.of.endogenous.probes = F
  )

# The number of genes/miRNAs that remain will be listed, everyone's will be different depending on
# your data and the normalization parameters you selected 
# 798 Endogenous genes,  332 Endogenous genes have less than 90% missing.

# ====    4 Exporting Matrix of Normalized Genes  ===============================================
complete_genes <-
  data_normalized_test$gene.summary.stats.norm[which(data_normalized_test$gene.summary.stats.norm$Missing <
                                                       90), ]

keep_genes <- grep("hsa", rownames(complete_genes), value = TRUE)

gene_matrix <-NanoStringNorm(
  x = ns_norm,
  CodeCount = "geo.mean",
  Background = "mean",
  SampleContent = "top.geo.mean",
  round.values = TRUE,
  take.log = TRUE,
  return.matrix.of.endogenous.probes = T
)

# This final matrix is the genes/miRNAs remaining after normalization and their normalized log2 transformed expression counts
# You will use this matrix for downstream analyses/ clustering 
# It is the MOST IMPORTANT
final_matrix <- gene_matrix[keep_genes,]

write.csv(final_matrix, "~/Desktop/final_matrix_KEEP.csv")

# This will show you only the genes/miRNAs expressed in your sample (aka remaining after normalization)
write.csv(complete_genes,"~/Desktop/complete_genes.csv")

# This will show you every single gene/miRNA and their corresponding counts after normalization
write.csv(gene_matrix,"~/Desktop/gene_matrix.csv" )




