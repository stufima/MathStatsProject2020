# Globals for the app ----
## Prerequisites ====
library(shiny)
library(tidyverse)

df_survey <- read_csv("../data/survey_results_public.csv")
print(paste("Survey colnames count", length(df_survey)))
#df_survey <- read_csv("data/survey_results_public.csv")
#df_survey_team <- df_survey %>% select(
#    Respondent
#  , MainBranch
#  , Hobbyist
#  , Employment
#  , Country
#  , Student
#  , EdLevel
#  , OrgSize
#  , DevType
#  , YearsCode
#  , ConvertedComp
#  , WorkWeekHrs
#  , WorkRemote
#  , ImpSyn
#  , PurchaseWhat
#  , LanguageWorkedWith
#  , Age
#  , Gender
#  , Trans
#  , Sexuality
#  , Ethnicity
#  , Dependents
#)
#out_file <- file.path(getwd(), "survey_results_public_t.csv")
#write_csv(df_survey_team, out_file)
