
if (!require(qwraps2, quietly=TRUE)) {
  install.packages("qwraps2")
  library(qwraps2)
  options(qwraps2_markup = 'markdown')
}


if (!require(dplyr, quietly=TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(BSDA, quietly=TRUE)) {
  install.packages("BSDA")
  library(BSDA)
}

#Imput saveRDS(reduced_dataset, "reduced_dataset.rds")
reduced_dataset <-readRDS("reduced_dataset.rds")
reduced_dataset2 <-readRDS("reduced_dataset2.rds")

#============== Descriptives ===============================

options(qwraps2_markup = "markdown")
summary_table(reduced_dataset)

#make a data frame with only the distinct cases (drd=distinct reduced dataset)

drd <- reduced_dataset %>% distinct(Subject,.keep_all = TRUE)

# Add number of participants and number of visits 
my_raw_summary <-
  list(
    "Child gender"=
      list("Boys"= ~ n_perc(drd$CHILDGENDER == "Male")),
    "Type of visits"=
      list("Routine"= ~ n_perc(drd$source == "Retro")),
    "Country mom was born" =
      list("Canada" = ~ n_perc(drd$COUNTRYBIOMOMBORN.x == "CANADA")),
    "Country father was born" =
      list("Canada" = ~ n_perc(drd$COUNTRYBIOFATHERBORN.x == "CANADA")),
    "Country child was born" =
      list("Canada" = ~ n_perc(drd$COUNTRYCHILDBORN.x == "CANADA")),
    "Mom employed" =
      list("Yes" = ~ n_perc(drd$MOM_EMPLOYED_YN.x == "Yes"),
           "No" = ~ n_perc(drd$MOM_EMPLOYED_YN.x == "No")),
    "Father employed" =
      list("Yes" = ~ n_perc(drd$FATHER_EMPLOYED_YN.x == "Yes"),
           "No" = ~ n_perc(drd$FATHER_EMPLOYED_YN.x == "No")),
    "Gestation age"=
      list("38 - 41 weeks" = ~ n_perc(drd$GESTATION_AGE.x == "38 - 41 weeks"),
           "37-40 weeks" = ~ n_perc(drd$GESTATION_AGE.x == "37-40 weeks gestation"),
           "37 weeks" = ~ n_perc(drd$GESTATION_AGE.x == "37 weeks"),
           "36 weeks" = ~ n_perc(drd$GESTATION_AGE.x == "36 weeks"),
           "35 weeks" = ~ n_perc(drd$GESTATION_AGE.x == "35 weeks"),
           "32-36 weeks" = ~ n_perc(drd$GESTATION_AGE.x == "32-36 weeks gestation"),
           ">41 weeks" = ~ n_perc(drd$GESTATION_AGE.x == ">41 weeks")),
    "Birth weight"=
      list("mean (sd)" = ~ qwraps2::mean_sd(drd$birthweight_kilo.x)),
    "Maternal age"=
      list("mean (sd)" = ~ qwraps2::mean_sd(drd$MOM_AGE)))


whole <- summary_table(drd, my_raw_summary)   

my_raw_summary2 <-
  list("BMI Z-Score"=
         list("min" = ~ min(reduced_dataset$zbmi.x),
              "max" = ~ max(reduced_dataset$zbmi.x),
              "mean (sd)" = ~ qwraps2::mean_sd(reduced_dataset$zbmi.x)),
       "Lenght for age Z-Score"=
         list("min" = ~ min(reduced_dataset$zlen.x),
              "max" = ~ max(reduced_dataset$zlen.x),
              "mean (sd)" = ~ qwraps2::mean_sd(reduced_dataset$zlen.x)),
       "Weight for age Z-Score"=
         list("min" = ~ min(reduced_dataset$zwei.x),
              "max" = ~ max(reduced_dataset$zwei.x),
              "mean (sd)" = ~ qwraps2::mean_sd(reduced_dataset$zwei.x)),
       "Weight"=
         list("min" = ~ min(reduced_dataset$CHILDWEIGHT),
              "max" = ~ max(reduced_dataset$CHILDWEIGHT),
              "mean (sd)" = ~ qwraps2::mean_sd(reduced_dataset$CHILDWEIGHT)),
       "Height"=
         list("min" = ~ min(reduced_dataset$HEIGHT),
              "max" = ~ max(reduced_dataset$HEIGHT),
              "mean (sd)" = ~ qwraps2::mean_sd(reduced_dataset$HEIGHT)))

whole2<- summary_table(drd, my_raw_summary2)

count(df, Subject)

#==================== Statistics ===========================
reduced_dataset <-readRDS("reduced_dataset.rds")
reduced_dataset2 <-readRDS("reduced_dataset2.rds")

#Prepare the datasets
reduced_dataset <-mutate(reduced_dataset,Ds_group=1)
number_of_visits <-count(reduced_dataset, Subject)
reduced_dataset<- right_join(reduced_dataset, number_of_visits, by="Subject")

reduced_dataset2 <-mutate(reduced_dataset2,Ds_group=2)
number_of_visits2 <-count(reduced_dataset2, Subject)
reduced_dataset2<- right_join(reduced_dataset2, number_of_visits2, by="Subject")

total<-union(reduced_dataset,reduced_dataset2)
labels <- colnames(total[c(13:17)])
number_of_visits <-count(total, Subject)
total<- right_join(total, number_of_visits, by="Subject")

# Keep only distinct cases
distinct_total <- total %>% distinct(Subject,.keep_all = TRUE)


# Z test

x<-na.omit(reduced_dataset$CHILDWEIGHT)
y<-na.omit(reduced_dataset2$CHILDWEIGHT)

z.test(x, y,  sigma.x = sd(x), sigma.y = sd(y), conf.level = 0.95)


#Welch-test

for (i in labels) {
  total<-find_irts_TK(total,"Subject",as.character(i))
}

values <- colnames(total[c(6:11, 28)])
for (i in values){
 t.test(total$as.numeric(i)~total$Ds_group) 
}

t.test(distinct_total$birthweight_kilo.x ~distinct_total$Ds_group) 



# Chi-squared
total.data = table(total$Subject, total$Ds_group) 
print(total.data)
print(chisq.test(total.data))    
#G-tests 
Matriz = as.matrix(read.table(total.data))




