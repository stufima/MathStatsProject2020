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

## Tab Survey ====
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
        interceptV <- regression %>% filter(Coefficients == "(Intercept)") %>% select(Estimate)
        amount <- interceptV + 
            getEstimate("MainBranch", input$MainBranch) + 
            getEstimate("Employment", input$Employment) +
            getEstimate("Country", input$Country) + 
            getEstimate("EdLevel", input$EdLevel) +
            getEstimate("OrgSize", input$OrgSize) +
            getEstimate("Gender", input$Gender) +
            getEstimate("Dependents", input$Dependents) +
            (getEstimate("YearsCode", "") * input$YearsCode)
        
        amount <- amount * input$WorkWeekHrs

        valueBox(
            paste(round(amount,digits = 2), "$/year")
            , "Income (Fitted Value)"
            , icon = icon("credit-card"),
            color = "purple"
        )
    })    
### Survey - Data tables ####   
    output$dataAges <- renderTable({
         regression
        })  

# End of Server ----
})
