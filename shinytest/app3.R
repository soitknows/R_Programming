library(shiny)
library(ggplot2)

# Load raw data for case creations and closures
cre <- read.csv("../created.csv"
                )
res <- read.csv("../closes.csv")

# Create data frames to be ound together later, and add month variable
cre <- data.frame(product = cre$Reg.Product, 
                  date = cre$Created.On,
                  month = factor(months(as.Date(cre$Created.On, "%m/%d/%Y"))))

res <- data.frame(product = res$Reg.Product, 
                  date = res$Created.On,
                  month = factor(months(as.Date(res$Resolved.On, "%m/%d/%Y"))))

# Reorder levels so month facets on plot are chronological
cre$month <- factor(cre$month, levels=c("January","February","March","April",
                                        "May","June","July","August","September",
                                        "October","November","Decmeber"))

res$month <- factor(res$month, levels=c("January","February","March","April",
                                        "May","June","July","August","September",
                                        "October","November","Decmeber"))

# Add type variables to data frames so case types can be deliniated in plot
cre$type <- factor("Created")
res$type <- factor("Resolved")

# Bind both data frames together to create full data set for plot
df <- rbind(cre,res)

# Create list of distinct prodcut types for picklist in ShinyApp
prods <- sort(unique(df$product))


ui <- fluidPage(
    selectInput(inputId = "product",
                label = "Registered Product",
                choices = prods),
    
    plotOutput(outputId = "stats")
)

server <- function(input,output) {
    output$stats <- renderPlot({
        ggplot(df[df$product == input$product,], aes(x=type, fill=type)) +
            geom_bar() +
            facet_wrap(~month) +
            labs(x="Cases",title="Case Creation / Closures by Product") +
            theme(plot.title = element_text(size = rel(2),hjust = 0.5))
    })
}

shinyApp(ui = ui, server = server)
