library(shiny)

outcomes <- read.csv("outcome-of-care-measures.csv")
outcomes <- outcomes[,c(2,7,11,17,23)]
colnames(outcomes) <- c("Hospital.Name",
                        "State",
                        "heart attack",
                        "heart failure", 
                        "pneumonia")


ui <- fluidPage(
    selectInput(inputId = "cond",
                label = "Health Condition",
                choices = colnames(outcomes)[3:5]),
    
    selectInput(inputId = "state",
                label = "State",
                choices = unique(outcomes$State)),
    
    sliderInput(inputId = "breaks",
                label = "Histogram Breaks",
                min = 1, max = 100, value = 50),
    
    plotOutput(outputId = "hist"),
    
) 


server <- function(input,output) {
    data_h <- reactive({
         df <- outcomes[outcomes$State == input$state &
                 outcomes[input$cond] != "Not Available",
                 input$cond]
         df <- sapply(df, as.numeric)
    })
    
    data_b <- reactive ({
        df <- outcomes
    })
    
    output$hist <- renderPlot({
        title = "Hospital 30 Day Mortality by Health Condition and State"
        hist(data_h(), main = title, breaks = input$breaks)
    })
    
}

shinyApp(ui = ui, server = server)