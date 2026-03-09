# Exploring Missingness & Data Cleaning for Growth Dataset.R
#
#
# Version:  1.0
#
# Date:     2026  Feb
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

# We will:
# 1. Explore the structure of the dataset
# 2. Quantify and visualize missingness
# 3. Identify implausible age and zBMI values
# 4. Clean growth data using WHO-style plausibility cut-offs
# 5. Generate a cleaned dataset for downstream analysis

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                  Line
#TOC> -----------------------------------------------------------------
#TOC>   1        Packages                                44
#TOC>   2        Data loading                            60
#TOC>   3        Missingness exploration                 75
#TOC>   4        Distribution inspection                 120
#TOC>   5        Identifying implausible values          155
#TOC>   6        Data cleaning                           200
#TOC>   7        Post-cleaning diagnostics               240
#TOC>
#TOC> ==========================================================================

# ======    1  Packages  =======================================================

packages <- c("tidyverse", "naniar", "skimr", "ggplot2")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =    2  Data loading  =========================================================

# Set working directory if needed
# setwd("your/path/here")

data <- read_csv("mock_precision_growth_dataset.csv")

# Inspect structure
glimpse(data)
summary(data)

# =====   3  Missingness exploration  ===========================================

# 3.1 Percent missing per variable

missing_summary <- data %>%
  summarise(across(everything(),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(missing_summary)

# 3.2 Visual inspection of missing data


vis_miss(data)

# Missingness combinations
gg_miss_upset(data)


# 3.3 Missingness by subgroup (example: sex)


if ("sex" %in% names(data)) {
  
  data %>%
    group_by(sex) %>%
    summarise(across(everything(),
                     ~ mean(is.na(.)) * 100))
}

# ======    4  Distribution inspection  =========================================

# 4.1 zBMI distribution


if ("zbmi_24" %in% names(data)) {
  
  ggplot(data, aes(x = zbmi_24)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of zBMI at 24 months")
}


# 4.2 CRP distribution


if ("crp" %in% names(data)) {
  
  ggplot(data, aes(x = crp)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of CRP")
  
  # Log-transform CRP (common due to right skewness)
  data <- data %>%
    mutate(log_crp = log(crp))
  
  ggplot(data, aes(x = log_crp)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of log(CRP)")
}

# =====    5  Identifying implausible values  ===================================

# 5.1 Implausible age values


if ("age_months" %in% names(data)) {
  
  implausible_age <- data %>%
    filter(age_months < 0 | age_months > 60)
  
  print(implausible_age)
}


# 5.2 WHO-style plausibility cut-offs for zBMI


# WHO commonly flags z-scores < -5 or > +5 as implausible

if ("zbmi_24" %in% names(data)) {
  
  implausible_zbmi <- data %>%
    filter(zbmi_24 < -5 | zbmi_24 > 5)
  
  print(implausible_zbmi)
}


# 5.3 Extreme CRP values (possible acute infection)


if ("crp" %in% names(data)) {
  
  extreme_crp <- data %>%
    filter(crp > 10)
  
  print(extreme_crp)
}

# =====    6  Data cleaning  ===================================================

clean_data <- data

# Remove implausible age
if ("age_months" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(age_months >= 0 & age_months <= 60)
}

# Remove implausible zBMI
if ("zbmi_24" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(zbmi_24 >= -5 & zbmi_24 <= 5)
}

# Optional: Remove extreme CRP > 10 mg/L
if ("crp" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(crp <= 10 | is.na(crp))
}

# =====    7  Post-cleaning diagnostics  =======================================

# Compare sample size
cat("Original N:", nrow(data), "\n")
cat("Cleaned N:", nrow(clean_data), "\n")

# Recalculate missingness after cleaning
missing_summary_clean <- clean_data %>%
  summarise(across(everything(),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(missing_summary_clean)

# Save cleaned dataset
write_csv(clean_data, "clean_precision_growth_dataset.csv")
