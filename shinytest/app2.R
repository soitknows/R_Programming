library(shiny)

ui <- fluidPage(
    sliderInput(inputId = "bin",
                label = "Bin width",
                min = 1, max = 10, value = 7),
    
    sliderInput(inputId = "mean",
                label = "Mean",
                min = 1, max = 100, value = 70),
    
    plotOutput(outputId = "hist")
)

server <- function(input, output) {
    binwidth <- reactive({ input$bin })
    mean <- reactive({ input$mean })
    
    output$hist <- renderPlot({
        hist(rnorm(100,input$mean,10),
             breaks = binwidth())
    })
}

shinyApp(ui = ui, server = server)

