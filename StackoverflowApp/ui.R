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
    fluidPage(

### Application title ####
    titlePanel(textOutput("title"))
    , textOutput("source")

### Application header/sidebar ####
    # Sidebar with a slider input for Demographic Information like ages
    , sidebarLayout(
        sidebarPanel(
            sliderInput("ages"
                        , "Age in years:"
                        , min = 15
                        , max = 80
                        , value = 30
                        )
        ),
### Application body ####
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
