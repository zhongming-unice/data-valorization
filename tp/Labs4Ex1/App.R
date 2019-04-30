# Solution Exercise 1
# Author: Lionel Fillatre

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Labs 4, Exercise 1"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ,
    sliderInput("nsamples",
                "Number of samples:",
                min = 10,
                max = 10000,
                value = 100)
    
    ,
    numericInput("mean", "Mean of the samples", 
                 value=0, 
                 min = -10, 
                 max = 10, 
                 step = 0.5)
    ,
    selectInput("var", 
                label = "Choose the graph to display",
                choices = c("Histogram", "Probability density function", "Both histogram and probability density function"),
                selected = "Both histogram and probability density function")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)




# Define server required to draw a histogram
server <- function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    m <- input$mean
    x <- rnorm(input$nsamples, mean = m, sd = 1)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    
    if (input$var=="Histogram")
    {
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'skyblue', border = 'white')
      
    }
    else if (input$var=="Probability density function") {
      plot(bins,dnorm(x= bins, mean = m, sd = 1))
    } 
    else { 
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'skyblue', border = 'white')
      par(new=T)
      plot(bins,dnorm(x= bins, mean = m, sd = 1))
    }
  })
}

shinyApp(ui = ui, server = server)