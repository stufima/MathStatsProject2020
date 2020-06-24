#
# This is the user-interface definition of a Shiny web application. 
# You can run the application by clicking 'Run App' above.
#
# Find out more about Shiny web application here:
#
#    https://github.com/stufima/MathStatsProject2020/
#

# Define UI for application that draws diagrams ----
shinyUI(
## Page-definiton ====    
  dashboardPage(
### Header - Application title ####
        getHeader()
### Menu ####
        , dashboardSidebar(
            sidebarMenuOutput("left_menu")
        )
### Begin of dashboardBody ####
    , dashboardBody(
        tabItems(
### Tab - Survey ####
          tabItem(
            tabName = df_tab_ids$id[[1]]
            , fluidRow(
                  getUISurveyInputTabBox()
                , getUISurveyOutputTabBoxPlots()
                , getUISurveyOutputTabBoxDataTables()
            )
### End-Tab - Survey ####
          )
### Tab - About ####
          , tabItem(
              tabName = df_tab_ids$id[[2]]
              , fluidRow(
                  box(
                      # title = "Information",
                      width = 12,
                      collapsible = FALSE,
                      # , textOutput("information")
                      # , textOutput("source")
                      ## Tab About - Output ====
                      tags$h2("Some Informations about the Survey"),
                      tags$p("Source-Dataset: 88,883 observations of 85 variables "),
                      tags$p("Used-Dataset: 29,573 observations of 13 variables "),
                      # tags$p("Source: https://insights.stackoverflow.com/survey")
                      tags$a(href="https://insights.stackoverflow.com/survey", "to survey Stackoverflow")
                  )
              )
### End-Tab - About ####
          )
# End of dashBoardBody ------ 
        )
    )
# End of Page ------ 
  )
# End of Shiny App ------ 
) 