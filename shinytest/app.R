library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Number",
              min = 1, max = 100, value = 50),
  
  textInput(inputId = "title",
            label = "Plot Tile",
            value = "Entere plot title here"),
  
  plotOutput(outputId = "hist"),
  
  verbatimTextOutput(outputId = "stats"),
  
  actionButton(inputId = "submit", label = "Submit")
)

server <- function(input,output){
    data <- reactive({ rnorm(input$num) })
    
    output$hist <- renderPlot({
        title = "100 random normal values"
        hist(data(), main = isolate(input$title))
    })
    
    output$stats <- renderPrint({
        summary(data())
    })
    
    observeEvent(input$submit, {
        print(as.numeric(input$submit))
    })
}

shinyApp(ui = ui, server = server)

