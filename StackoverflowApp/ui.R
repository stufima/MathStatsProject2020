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
                      title = "Informations",
                      width = 12,
                      collapsible = FALSE
                      , textOutput("informations")
                      , textOutput("source")
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