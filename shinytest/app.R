library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Number",
              min = 1, max = 100, value = 50),
  plotOutput(outputId = "hist")
)

server <- function(input,output){
  output$hist <- renderPlot({
    title = "100 random normal values"
    hist(rnorm(input$num), main = title)
    })
}

shinyApp(ui = ui, server = server)

