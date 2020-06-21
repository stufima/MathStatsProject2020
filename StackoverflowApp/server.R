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
### Survey - Output ####
getEstimate <- function(pInputField, pSelection) {
    key <- paste0(pInputField, pSelection)
    if (key %in% regression$Coefficients){
        amountV <- regression %>% filter(Coefficients == paste0(pInputField, pSelection)) %>% select(Estimate)
        return(amountV[[1]])
    } 
    return(0)
}
    
    output$yearsCodeFitted <- renderValueBox({
        # inputs <- c(
        #     regression %>% filter(Coefficients == "(Intercept)") %>% select(Estimate)
        #     , regression %>% filter(Coefficients == paste0("MainBranch", input$MainBranch)) %>% select(Estimate)
        #     , regression %>% filter(Coefficients == paste0("Employment", input$Employment)) %>% select(Estimate)
        #     , regression %>% filter(Coefficients == paste0("Country", input$Country)) %>% select(Estimate)
        #     , regression %>% filter(Coefficients == paste0("EdLevel", input$EdLevel)) %>% select(Estimate)
        #     , regression %>% filter(Coefficients == paste0("YearsCode", input$YearsCode)) %>% select(Estimate)
        #     , regression %>% filter(Coefficients == paste0("WorkWeekHrs", input$WorkWeekHrs)) %>% select(Estimate)
        #     , regression %>% filter(Coefficients == paste0("OrgSize", input$OrgSize)) %>% select(Estimate)
        #     , regression %>% filter(Coefficients == paste0("Gender", input$Gender)) %>% select(Estimate)
        #     , regression %>% filter(Coefficients == paste0("Dependents", input$Dependents)) %>% select(Estimate)
        # )
        interceptV <- regression %>% filter(Coefficients == "(Intercept)") %>% select(Estimate)
        amount <- interceptV + 
            getEstimate("MainBranch", input$MainBranch) + 
            getEstimate("Employment", input$Employment) +
            getEstimate("Country", input$Country) + 
            getEstimate("EdLevel", input$EdLevel) +
            getEstimate("OrgSize", input$OrgSize) +
            getEstimate("Gender", input$Gender) +
            getEstimate("Dependents", input$Dependents)

        amount <- amount + (getEstimate("YearsCode", "") * input$YearsCode)
        # + getEstimate("WorkWeekHrs", input$WorkWeekHrs)
        
        valueBox(
            paste0(round(amount,digits = 2), "$/year")
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
        # getDataAges()
        regression
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
