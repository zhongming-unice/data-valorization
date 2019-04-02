# install.packages("shiny")
library(shiny)

ui <- fluidPage(
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 20),
      sliderInput(inputId = "n",
                  label = "Number of samples:",
                  min = 10,
                  max = 10000,
                  value = 2000),
      numericInput(inputId = 'mean', 
                   label = "Mean", 
                   min = -10, 
                   max = 10, 
                   value = 0,
                   step = 0.5
                   ),
      selectInput("variable", "Variable:",
                  c("hist" = "a",
                    "pdf" = "b",
                    "both" = "c")),
      tableOutput("data")
      
    ),




    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
)
)
  



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    n <- input$n
    means <- input$mean
    x <- rnorm(n, mean=0, sd=1.0)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    h<-hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    if(input$variable = "a")
    h
    xfit<-seq(min(x),max(x),length=40) 
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    lines(xfit, yfit, col="blue", lwd=2)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
