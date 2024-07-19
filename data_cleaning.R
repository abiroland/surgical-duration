#Regression analysis project
#Edited on 05-01-2023
#by: Roland Abi

#Prep -----------------------------------------------------------

library(tidyverse)
library(skimr)
library(visdat)
library(viridis)

data <- read_csv("data.csv")

# Recoding and cleaning ------------------------------------------

datacopied <- data %>%
  select(
    Age, bmi, Female, Race, Season,
    Diabetes, ChronicRenalFailure, ASAstatus,
    VitaminD, DurationSurgery
  ) %>%
  rename(
    "Gender" = "Female",
    "BMI" = "bmi"
  ) %>%
  mutate(
    Gender = as.factor(
      if_else(Gender == 1, "Female", "Male")),
    Race = as.factor(case_when(
      Race == 1 ~ "White", 
      Race == 2 ~ "Black",
      Race == 3 ~ "Others")),
    ASAstatus = as.factor(case_when(
      ASAstatus == 1 ~ "Normal Healthy Patient",
      ASAstatus == 2 ~ "Mild Systemic Disease",
      ASAstatus %in% c(3,4) ~ "Severe Systemic Disease")),
    Diabetes = as.factor(
      if_else(Diabetes == 0, "No", "Yes")),
    ChronicRenalFailure = as.factor(
      if_else(ChronicRenalFailure == 1, "Yes", "No")),
    Season = as.factor(case_when(
      Season == 1 ~ "Fall",
      Season == 2 ~ "Winter",
      Season == 3 ~ "Spring",
      Season == 4 ~ "Summer"))
  )

# Droping non useful variable --------------------------------------------------
skim(datacopied)

proj_df <- datacopied %>%
  select(-Season, -VitaminD)

# checking for missing observation --------------------------------------------

colSums(is.na(proj_df))

#create new variable, body mass index -----------------------------------------
proj_bmi_df <- proj_df %>%
  mutate(
    grpbmi = case_when(
      BMI < 18.5 ~ "Underweight",
      between(BMI, 18.25,24.9) ~ "Normal weight",
      between(BMI, 25, 29.9) ~ "Overweight",
      BMI > 30 ~ "Obesity"
    ), 
    grpbmi = factor(grpbmi, levels = c(
      "Normal weight", "Underweight", "Overweight", "Obesity" 
    ))
  )

#prep for dumbell plot ---------------------------------------------------------
#dumbell <- proj_bmi_df %>%
 # select(grpbmi, Gender, DurationSurgery) %>%
  #pivot_wider(id_cols = "grpbmi", 
   #           names_from = "Gender",
    #          values_from = "DurationSurgery",
     #         values_fn = mean) %>%
  #mutate(
   # gap = `Male` - `Female`
  #) %>%
  #arrange(desc(gap)) %>%
  #na.omit()

proj_line_df <- proj_bmi_df %>%
  select(grpbmi, DurationSurgery, Gender, Age) %>%
  filter(Age >= 60) %>%
  group_by(grpbmi, Gender) %>%
  na.omit()

#proj_line_df2 <- proj_bmi_df %>%
 # select(grpbmi, DurationSurgery, Gender, ASAstatus, Age) %>%
  #filter(Age >= 60) %>%
  #group_by(grpbmi, Gender, ASAstatus) %>%
  #summarize(
   # meanduration = mean(DurationSurgery)
  #) %>%
  #na.omit() %>%
  #pivot_wider(id_cols = grpbmi, 
   #           names_from = Gender, 
    #          values_from = meanduration,
     #         values_fn = median)