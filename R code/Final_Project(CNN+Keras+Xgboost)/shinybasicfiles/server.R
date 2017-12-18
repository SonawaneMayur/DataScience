library(shiny)
library(datasets)



# Define server logic requirments
shinyServer(function(input, output) {
  
  
  # Compute the forumla text in a reactive expression
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$semeionplot<- renderPlot(plot(history))
})