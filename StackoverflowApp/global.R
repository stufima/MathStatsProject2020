# TODOS ----
# Berücksichtigung von Auswirkung der Wochenarbeitszeit auf das Gehalt (Stundenlohn)
# Werte übersetzen übernimmt Dummy der Linearen Regression

# Anzahl der Kategorien mit überprüfen distinct oder unique 

# Globals for the app ----
## Prerequisites and constants ====
library(shiny)          # framework to generate this web-app
library(shinydashboard) # generate a dashboard frontend
library(plotly)         # generates interactive web-graphics
library(tidyverse)      # dplyr and co.
# deactivate when starting the app
debug <- FALSE
# Survey - Input Elements
min_age <- 20
max_age <- 70
min_week_hours <- 8
max_week_hours <- 70
min_ConvertedComp <- 5000
max_ConvertedComp <- 300000
# Data ----
## Import Data ====
# test without shiny app
#TODO siehe Abschnitt AGE, wenn hier sofort auf integer geht, kommen NA
if (debug) {
  df_survey <- read_csv("data/survey_results_public.csv")
  #                      col_types = cols(Age = col_integer()))
  print(paste("Survey colnames count", length(df_survey)))
} else {
  df_survey <- read_csv("../data/survey_results_public.csv")
}

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

### Employment ####
# Überprüfung des Wertebereiches
df_survey %>%
  count(Employment)

# Bereinigung des Datensatzes. Behalten der Vollzeit-Angestellten und der Freelancer
df_survey <- df_survey %>%
  filter(Employment %in% c("Employed full-time", "Independent contractor, freelancer, or self-employed", "Employed part-time"))

### Country ####
# Überprüfung des Wertebereiches
countries <- df_survey %>%
  count(Country)

# Entfernung der Länder mit weniger als 50 Entwicklern - zu prüfen: Ergibt das Sinn?
countries <- countries %>%
  filter(n >= 500)

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
if (debug){
  age <- df_survey %>%
    count(Age)
  
  print(unique(df_survey$Age))   # count() ist für Betrachtung sinnvoller
}

df_survey <- df_survey %>%
  filter(!is.na(Age)) %>%
  filter(Age >=  min_age) %>%
  filter(Age <= max_age) %>%
  mutate(Age = as.integer(Age))

# für die Anzeige im UI benötigt
getDataAges <- function(){
   l_dataAges <- df_survey %>%
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
       , Dependents
     ) %>%
     slice(1:10)
# without order
#     top_n(20)
  return(l_dataAges)
}

### YearsCode ####
YearsCode <- df_survey %>%
  count(YearsCode)

df_survey <- df_survey %>%
  filter(!is.na(YearsCode)) %>%
  filter(!YearsCode %in% c("Less than 1 year", "More than 50 years"))
  
df_survey$YearsCode <- as.numeric(df_survey$YearsCode)

### WorkWeekHrs ####
WorkWeekHrs <- df_survey %>%
  count(WorkWeekHrs)

df_survey <- df_survey %>%
  filter(WorkWeekHrs >= min_week_hours) %>%
  filter(WorkWeekHrs <= max_week_hours)

### ConvertedComp ####
ConvertedComp <- df_survey %>%
  count(ConvertedComp)

df_survey <- df_survey %>%
  filter(ConvertedComp >= min_ConvertedComp) %>%
  filter(ConvertedComp <= max_ConvertedComp)

### OrgSize ####
df_survey %>%
  count(OrgSize)

df_survey <- df_survey %>%
  filter(!is.na(OrgSize)) 

df_survey <- df_survey %>%  
  mutate(OrgSize = ifelse(df_survey$OrgSize %in% c("Just me - I am a freelancer, sole proprietor, etc.",
                                                   "2-9 employees",
                                                   "10 to 19 employees",
                                                   "20 to 99 employees"), "1-99",
                          ifelse(df_survey$OrgSize %in% c("100 to 499 employees",
                                                          "500 to 999 employees"), "100-999",
                                 ifelse(df_survey$OrgSize %in% c("1,000 to 4,999 employees",
                                                                 "5,000 to 9,999 employees",
                                                                 "10,000 or more employees"), "1000+","0"))))

### Gender ####
df_survey %>%
  count(Gender)

df_survey <- df_survey %>%
  filter(!is.na(Gender)) %>%
  filter(Gender %in% c("Man", "Woman"))

### Dependents ####
df_survey %>%
  count(Dependents)

df_survey <- df_survey %>%
  filter(!is.na(Dependents))
# Regression ----
rm <- lm(data = df_survey, formula = ConvertedComp ~ 
       MainBranch 
     + Employment 
     + Country
     # + Student (nicht im Modell, hat nur eine Ausprägung)
     + EdLevel
     # + Age (nicht im Modell, da YearsCode diesen Wert "entsignifiziert")
     + YearsCode
     + WorkWeekHrs
     + OrgSize
     + Gender
     + Dependents)

regression <- as.data.frame(summary(rm)$coefficients) 
regression <- regression %>%
  mutate("Coefficients" = rownames(regression))

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



getUISurveyInputTabBox <- function(){
  l_in_sur_tb <- box(
    title = textOutput("select_below")
    , width = 12
    , collapsible = TRUE
   # MainBranch 
    , selectInput(inputId = "MainBranch", label = "MainBranch", choices = unique(df_survey$MainBranch))
    # + Employment 
   , selectInput(inputId = "Employment", label = "Employment", choices = unique(df_survey$Employment))
    # + Country
   , selectInput(inputId = "Country", label = "Country", choices = unique(df_survey$Country))
   # # + Student (nicht im Modell, hat nur eine Ausprägung)
    # + EdLevel
   , selectInput(inputId = "EdLevel", label = "EdLevel", choices = unique(df_survey$EdLevel))
    # # + Age (nicht im Modell, da YearsCode diesen Wert "entsignifiziert")
   , sliderInput(inputId = "Age"
                 , label = "Age in years:"
                 , min = min_age
                 , max = max_age
                 , value = 30
   )
   # + YearsCode
   , sliderInput(inputId = "YearsCode"
                 , label = "Code in years:"
                 # , min = min(df_survey$YearsCode)
                 , min = 0
                 , max = max(df_survey$YearsCode)
                 , value = 0
   )
    # + WorkWeekHrs
   , sliderInput(inputId = "WorkWeekHrs"
                 , label = "Work Week in hours:"
                 , min = min_week_hours
                 , max = max_week_hours
                 , value = 30
   )
    # + OrgSize
   , selectInput(inputId = "OrgSize", label = "OrgSize", choices = unique(df_survey$OrgSize))
    # + Gender
   , radioButtons(inputId = "Gender", label = "Gender", choices = unique(df_survey$Gender))
    # + Dependents
   , selectInput(inputId = "Dependents", label = "Dependents", choices = unique(df_survey$Dependents))
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
        "YearsCode"
        , textOutput("plotCasesTitle")
        # , infoBox("Fitted Value(Income)", 10 * 2, icon = icon("credit-card"), fill = TRUE)
        # , infoBox("yearsCodeFitted")
, valueBoxOutput("yearsCodeFitted")        
        , plotlyOutput("plotCases"
                       , width = 600
        )
      )
      , tabPanel(
        "Ages"
        , textOutput("plotCasesPer100kTitle")
        # , infoBox("Fitted Value(Income)", 10 * 2, icon = icon("credit-card"), fill = TRUE)
        # , infoBox("agesFitted")
        , valueBoxOutput("agesFitted")
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
