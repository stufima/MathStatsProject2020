#
# This is the server logic of a Shiny web application. 
#
# Find out more about the applications with Shiny here:
#
#    https://github.com/stufima/MathStatsProject2020/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
## Header ====
    output$appTitle <- renderText({
        paste("2019 Stack Overflow Developer Survey")
    })
## Menu ====
    output$left_menu <- renderMenu({
        sidebarMenu(
            id = "tabSidebar"
            , menuItem(selected = TRUE, text = df_tab_ids$label[[1]], tabName = df_tab_ids$id[[1]], icon = icon(df_tab_ids$icon[[1]]))
            , menuItem(text = df_tab_ids$label[[2]], tabName = df_tab_ids$id[[2]], icon = icon(df_tab_ids$icon[[2]]))
        )
    })  

### Menu item selection ####
    s_min_age <- min_age
    s_max_age <- max_age
    
    tabPerform <- reactive({
        selectedTab <- input$tabSidebar
        if (length(selectedTab) > 0){
            switch (selectedTab,
                "tab_survey" = {
#                    updateSelectizeInput(session
#                                        , inputId = "regionchoice"
#                                        , choices = temp_regionList
#                                        , label = "Select up to 2 regions of interest:"
#                                        , selected = c("Germany")
#                           )
                            s_min_date <<- (min(df_ages$ages))
                            s_max_date <<- (max(df_ages$ages))
                            #
                            updateSliderInput(session
                                              , inputId = "ages"
                                              , min = s_min_date
                                              , max = s_max_date
                                              , value = c(s_min_date, s_max_date)
                            )
                    }
                    , {
                        paste("Keine Aktion für",selectedTab)
                    }
            )
        } else {
            paste("Keine Aktion, da tablength 0 ist", length(selectedTab))
        }
    })
    
## Tab Survey ====
### Survey - Input ####    
### Survey - Plots ####    
    output$distPlot <- renderPlot({
        NULL
    })
    
    output$yearsCodeFitted <- renderValueBox({
        valueBox(
            paste0(input$Age, "$/year")
            , "Fitted Value(Income)"
            , icon = icon("list"),
            color = "purple"
        )
    })    
    output$agesFitted <- renderValueBox({
        valueBox(
            paste0(input$Age, "$/year"), "Fitted Value(Income)"
            , icon("credit-card")
            , color = "purple"
        )        
    })    
### Survey - Data tables ####   
    output$dataAges <- renderTable({
        print("output$dataAges")
        getDataAges()
    })  
    
    output$dataDetailsAges <- renderTable({
        print("output$dataDetailsAges")
        getDataAges()
    })  
    
## Tab About - Output ====
    output$informations <- renderText({
        paste("Some Informations about the Survey")
    })
    output$source <- renderText({
        paste("Source:","https://insights.stackoverflow.com/survey")
    })
# End of Server ----
})
