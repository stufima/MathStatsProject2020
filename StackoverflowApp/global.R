# Globals for the app ----
## Prerequisites ====
library(shiny)          # framework to generate this web-app
library(shinydashboard) # generate a dashboard frontend
library(plotly)         # generates interactive web-graphics
library(tidyverse)      # dplyr and co.
debug <- FALSE
# Data ----
## Import Data ====
df_survey <- read_csv("../data/survey_results_public.csv")
# test without shiny app
#TODO siehe Abschnitt AGE, wenn hier sofort auf integer geht, kommen NA
#df_survey <- read_csv("data/survey_results_public.csv")
#                      col_types = cols(Age = col_integer()))
if (debug) {
  print(paste("Survey colnames count", length(df_survey)))
}

## Export Data ====
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

## Select Data ====
#str(df_survey)
### Age ####
if (debug) {
  range(df_survey$Age)
  df_ages_na <- df_survey %>% 
    filter(is.na(Age))
}
df_ages_nna <- df_survey %>% 
  filter(!is.na(Age))
if (debug) {
  range(df_ages_nna$Age)
  print(unique(df_ages_nna$Age))
}
#TODO Team-Decision
df_ages_nna$Age <- round(df_ages_nna$Age,0)
if (debug) {
  print(unique(df_ages_nna$Age))
}
df_ages <- df_ages_nna %>% 
  filter(Age >  10) %>%
  filter(Age <= 80) %>%
  arrange(Age) 
if (debug) {
  print(unique(df_ages$Age))
}
df_ages$Age <- as.integer(df_ages$Age)

df_ages_test <- df_ages %>%
  select(
    Age
    #    Respondent
    #  , MainBranch
    #  , Hobbyist
    #  , Employment
    #  , Country
    #  , Student
    #  , EdLevel
    #  , OrgSize
    #  , DevType
    , YearsCode
    #  , ConvertedComp
    #  , WorkWeekHrs
    #  , WorkRemote
    #  , ImpSyn
    #  , PurchaseWhat
    #  , LanguageWorkedWith
    #  , Age
    , Gender
    , Trans
    , Sexuality
    , Ethnicity
    , Dependents
  )

getDataAges <- function(){
   l_dataAges <- df_ages_test %>%
     slice(1:10)
# without order
#     top_n(20)
  return(l_dataAges)
}

# UI ----

## Header ====
getHeader <- function(){
  l_app_header <- dashboardHeader(
    title = textOutput(outputId = "appTitle")
  )
  return(l_app_header)
}
## Sidebar ====
df_tab_ids <- data.frame(
  list(c("Survey", "About")
       , c("tab_survey", "tab_about")
       #https://fontawesome.com/icons?d=gallery&q=globe&m=free
       #    "globe-europe"
       , c("globe", "qrcode")
  )
)
colnames(df_tab_ids) <- c("label","id","icon")

## Survey - Input Elements ====
min_age <- 15
max_age <- 80

getUISurveyInputTabBox <- function(){
  l_in_sur_tb <- box(
    title = textOutput("select_below")
    , width = 12
    , collapsible = TRUE
    , sliderInput(inputId = "ages"
                  , label = "Age in years:"
                  , min = min_age
                  , max = max_age
                  , value = 30
    )
  )
  return(l_in_sur_tb)
}
## Survey - Output Plots ====
getUISurveyOutputTabBoxPlots <- function(){
  l_out_sur_tb_pl <- box(
    width = 12
    , tabBox(
      id = "tabsetPlotsGermany"
      , width = "500px"
      , tabPanel(
        "Ages"
        , textOutput("plotCasesTitle")
        , plotlyOutput("plotCases"
                       , width = 600
        )
      )
      , tabPanel(
        "Gender"
        , textOutput("plotCasesPer100kTitle")
        , plotlyOutput("plotCasesPer100K"
                       , width = 600
        )
      )
    )
  )
  return(l_out_sur_tb_pl)
}
## Survey - Output Data Tables ====
getUISurveyOutputTabBoxDataTables <- function(){
  l_out_sur_tb_dt <- box(
    title = "Details to selection"
    , width = 12
    , collapsible = TRUE
    , tabBox(
      id = "tabsetDetails"
      , width = "400px"
      , tabPanel("Result of input selection"
                 , tableOutput("dataAges")
      )
      ### Germany - Output details to plot Ages ####
      , tabPanel("Details to plot ages"
                 , tableOutput("dataDetailsAges")
      )
    )
  )
  return(l_out_sur_tb_dt)
}
