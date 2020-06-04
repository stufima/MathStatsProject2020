#
# This is the server logic of a Shiny web application. 
#
# Find out more about the applications with Shiny here:
#
#    https://github.com/stufima/MathStatsProject2020/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$distPlot <- renderPlot({
        NULL
    })

    output$title <- renderText({
        paste("2019 Stack Overflow Developer Survey")
    })
    
    output$source <- renderText({
        paste("Source:","https://insights.stackoverflow.com/survey")
    })
    
})
