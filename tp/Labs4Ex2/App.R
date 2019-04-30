# Solution Exercise 2
# Author: Lionel Fillatre

library(shiny)

# load the twitter cloud
source("helpers.R")

# connect to twitter (only once)
connectTwitter()

# user interface
ui <- fluidPage(
  titlePanel("Twitter Application with Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a word and the size of the cloud."),
      
      # choose the word to process
      textInput("processedWord", "Word of interest:", "Enter the word..."),
      
      # select the size of the cloud
      sliderInput("range", 
                  label = "Number of printed words:",
                  min = 5, max = 30, value = 10)
    ),
    
    mainPanel(plotOutput("cloud"))
  )
)




# Define server 
server <- function(input, output) {

  output$cloud <- renderPlot({
    processedWord <- input$processedWord
    cloud_twitter(processedWord, nbTweets = 200, maxWords = input$range)
  })
}

shinyApp(ui = ui, server = server)