# TODOS ----
#NAs entfernen
# Jakob  = Respondent,MainBranch,Hobbyist,Employment,Country,Student,EdLevel,
# Bente  = OrgSize DevType,YearsCode,ConvertedComp,WorkWeekHrs,WorkRemote,ImpSyn,
# Marcus = PurchaseWhat,LanguageWorkedWith,Age,Gender,Trans,Sexuality,Ethnicity,Dependents

# Werte übersetzen übernimmt Dummy der Linearen Regression

# Anzahl der Kategorien mit überprüfen distinct oder unique 

# Globals for the app ----
## Prerequisites ====
library(shiny)          # framework to generate this web-app
library(shinydashboard) # generate a dashboard frontend
library(plotly)         # generates interactive web-graphics
library(tidyverse)      # dplyr and co.
debug <- FALSE
# Data ----
## Import Data ====
# df_survey <- read_csv("../data/survey_results_public.csv")
# test without shiny app
#TODO siehe Abschnitt AGE, wenn hier sofort auf integer geht, kommen NA
 df_survey <- read_csv("data/survey_results_public.csv")
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
### Respondent ####
# Verwendung dieser Spalte als Primärschlüssel
# Überprüfung ob Werte alle befüllt und einmalig sind:
sum(is.na(df_survey$Respondent))                # Bedingung erfüllt!
sum(isTRUE(duplicated(df_survey$Respondent)))   # Bedingung erfüllt!    

### MainBranch ####
# Anzeige des Wertebereiches
df_survey %>%
  count(MainBranch)

# Datensatz bereinigen, behalten der professionellen und Teilzeit-Entwickler
df_survey<- df_survey %>% 
  filter(MainBranch %in% c("I am a developer by profession", "I am not primarily a developer, but I write code sometimes as part of my work"))

### Hobbyist ####
# Anmerkung: ich halte diese Variable für nicht aussagekräftig. Hier einen Filter anzuwenden ist m.E. nach nicht zielführend.
# Überprüfung des Wertebereiches
df_survey %>%
  count(Hobbyist)

# Bereinigung des Datensatzes um Entwickler, welche (zusätzlich?) in Ihrer Freizeit programmieren (Achtung: großer Datenverlust!)
df_survey <- df_survey %>%
  filter(Hobbyist == "No")

### Employment ####
# Überprüfung des Wertebereiches
df_survey %>%
  count(Employment)

# Bereinigung des Datensatzes. Behalten der Vollzeit-Angestellten und der Freelancer
df_survey <- df_survey %>%
  filter(Employment %in% c("Employed full-time", "Independent contractor, freelancer, or self-employed"))

### Country ####
# Überprüfung des Wertebereiches
countries <- df_survey %>%
  count(Country)

# Entfernung der Länder mit weniger als 50 Entwicklern - zu prüfen: Ergibt das Sinn?
countries <- countries %>%
  filter(n >= 50)

df_survey <- df_survey %>%
  filter(Country %in% countries$Country)

### Student ####
# Überprüfen des Wertebereiches
df_survey %>%
  count(Student)

# Bereinigen des Datensatzes. Behalten der Nicht-Studenten
df_survey <- df_survey %>%
  filter(Student == "No")

### EdLevel ####
# Überprüfen des Wertebereiches
df_survey %>%
  count(EdLevel)

# Bereinigen des Datensatzes, behalten ausgewählter EdLevels
df_survey <- df_survey %>%
  filter(EdLevel %in% c("Associate degree",
                        "Bachelor’s degree (BA, BS, B.Eng., etc.)",
                        "Master’s degree (MA, MS, M.Eng., MBA, etc.)",
                        "Other doctoral degree (Ph.D, Ed.D., etc.)",
                        "Some college/university study without earning a degree"))

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
  filter(Age >=  min_age) %>%
  filter(Age <= max_age) %>%
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
min_age <- 25
max_age <- 70

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
