# Date: May 7, 2020
# Author: Zoe Lofft
#
#
# ========= Table of contents ==================================================
#TOC> ==========================================================================
#TOC>
#TOC> Section    Title                                            Line
#TOC> --------------------------------------------------------------------------
#TOC>   1        Test
#TOC>   2        End of test
#TOC>   3        Packages
#TOC>   4        Phenotype Information & Normalization
#TOC>   5        Contrast Vector Set Ups
#TOC>   6        All Comparisons 1-10
#TOC>   7        Convert Results to Tables
#TOC> ==========================================================================

# ====  1 Test    ===============================================
library(NanoStringDiff)
library(Biobase)

directory <-
  "~/Documents/GitHub/Differential_expression_analysis/Differential_expression_analysis/Data/ns_diff_final.csv"
path <- paste(directory, sep = "/")


designs <-
  data.frame(
    group = c(
      "dmem_no",
      "dmem_no",
      "dmem_no",
      "pac_no",
      "pac_no",
      "pac_no",
      "met1_no",
      "met1_no",
      "met1_no",
      "met2_no",
      "met2_no",
      "met2_no",
      "dmem_yes",
      "dmem_yes",
      "dmem_yes",
      "pac_yes",
      "pac_yes",
      "pac_yes",
      "met1_yes",
      "met1_yes",
      "met1_yes",
      "met2_yes",
      "met2_yes",
      "met2_yes"
    )
  )


ns_final <-
  createNanoStringSetFromCsv(path, header = TRUE, designs)

pheno <- pData(ns_final)
group <- pheno$group
design.full <- model.matrix ( ~ 0 + group)
design.full

# Need to specify contrast vectors
# DMEM vs PAC

contrast1 <- c(1, -1, 0, 0, 0, 0, 0, 0)

#normalization
ns_final <- estNormalizationFactors(ns_final)

# DMEM vs PAC
ns_final_result1 <-
  glm.LRT(ns_final, design.full, contrast = contrast1)

# Data table
table1 <- ns_final_result1[["table"]]

##CSV
write.csv(table1, file = "~/Documents/miRNA Analysis-Jan2020/may7_DMEMvsPAC_result.csv")

# ====  2 End of Test    ===============================================

# ====  3 Packages    ===============================================
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("NanoStringDiff")

library(NanoStringDiff)
library(Biobase)


# ====  4 Phenotype Information & Normalization   ==============================

# file for directory = "ns_diff_all_groups.csv" it is in onedrive
directory <-
  "~/Documents/miRNA Analysis-Jan2020/R_csv_input/ns_diff_all_groups.csv"

directory <-"~/Documents/GitHub/Differential_expression_analysis/Differential_expression_analysis/Data/ns_diff_final.csv"

path <- paste(directory, sep = "/")


designs <-
  data.frame(
    group = c(
      "dmem_no",
      "dmem_no",
      "dmem_no",
      "pac_no",
      "pac_no",
      "pac_no",
      "met1_no",
      "met1_no",
      "met1_no",
      "met2_no",
      "met2_no",
      "met2_no",
      "dmem_yes",
      "dmem_yes",
      "dmem_yes",
      "pac_yes",
      "pac_yes",
      "pac_yes",
      "met1_yes",
      "met1_yes",
      "met1_yes",
      "met2_yes",
      "met2_yes",
      "met2_yes"
    )
  )
designs

ns_final <-
  createNanoStringSetFromCsv(path, header = TRUE, designs)

pheno <- pData(ns_final)
group <- pheno$group
design.full <- model.matrix ( ~ 0 + group)
design.full

#normalization
ns_final <- estNormalizationFactors(ns_final)

# ====  5 Contrast Vector Set Ups   ===============================================

contrast1 <- c(1, -1, 0, 0, 0, 0, 0, 0)
contrast2 <- c(1, 0, -1, 0, 0, 0, 0, 0)
contrast3 <- c(1, 0, 0, -1, 0, 0, 0, 0)
contrast4 <- c(0, 0, 0, 0, 1, -1, 0, 0)
contrast5 <- c(0, 0, 0, 0, 1, 0, -1, 0)
contrast6 <- c(0, 0, 0, 0, 1, 0, 0, -1)
contrast7 <- c(1, 0, 0, 0, -1, 0, 0, 0)
contrast8 <- c(0, 1, 0, 0, 0, -1, 0, 0)
contrast9 <- c(0, 0, 1, 0, 0, 0, -1, 0)
contrast10 <- c(0, 0, 0, 1, 0, 0, 0, -1)

contrast_list <-
  list(
    contrast1,
    contrast2,
    contrast3,
    contrast4,
    contrast5,
    contrast6,
    contrast7,
    contrast8,
    contrast9,
    contrast10
  )

# ====  6 All Comparisons 1-10   ===============================================

table_list <- list()
for (contrast in contrast_list) {
  ns_final_result <-
    glm.LRT(ns_final, design.full, contrast = contrast_list)
  my_table <- ns_final_result[["table"]]
  table_list <- list(table_list, my_table)
}


# contrast 1: DMEM vs PAC
ns_final_result1 <-
  glm.LRT(ns_final, design.full, contrast = contrast1)

# contrast 2: DMEM vs MET1
ns_final_result2 <-
  glm.LRT(ns_final, design.full, contrast = contrast2)

# contrast 3: DMEM vs MET2
ns_final_result3 <-
  glm.LRT(ns_final, design.full, contrast = contrast3)

# contrast 4: DMEM+ vs PAC+
ns_final_result4 <-
  glm.LRT(ns_final, design.full, contrast = contrast4)

# contrast 5: DMEM+ vs MET1+
ns_final_result5 <-
  glm.LRT(ns_final, design.full, contrast = contrast5)

# contrast 6: DMEM+ vs MET2+
ns_final_result6 <-
  glm.LRT(ns_final, design.full, contrast = contrast6)

# contrast 7: DMEM vs DMEM+
ns_final_result7 <-
  glm.LRT(ns_final, design.full, contrast = contrast7)

# contrast 8: PAC vs PAC+
ns_final_result8 <-
  glm.LRT(ns_final, design.full, contrast = contrast8)

# contrast 9: MET1 vs MET1+
ns_final_result9 <-
  glm.LRT(ns_final, design.full, contrast = contrast9)

# contrast 10: MET2 vs MET2+
ns_final_result10 <-
  glm.LRT(ns_final, design.full, contrast = contrast10)


# ====  7 Convert Results to Tables   ==========================================
# Data tables for export into csvs
table1 <- ns_final_result1[["table"]]

table2 <- ns_final_result2[["table"]]
table3 <- ns_final_result3[["table"]]
table4 <- ns_final_result4[["table"]]
table5 <- ns_final_result5[["table"]]
table6 <- ns_final_result6[["table"]]
table7 <- ns_final_result7[["table"]]
table8 <- ns_final_result8[["table"]]
table9 <- ns_final_result9[["table"]]
table10 <- ns_final_result10[["table"]]




