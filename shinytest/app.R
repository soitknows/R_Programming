library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Number",
              min = 1, max = 100, value = 50),
<<<<<<< HEAD
  plotOutput(outputId = "hist")
)

server <- function(input,output){
  output$hist <- renderPlot({
    title = "100 random normal values"
    hist(rnorm(input$num), main = title)
=======
  
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
>>>>>>> 1fd5aee6d510374b27f9c6c17bd4cce32b22d492
    })
}

shinyApp(ui = ui, server = server)

