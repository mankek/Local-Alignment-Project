#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Local Alignment Heatmap"),
  
  # Sidebar with a slider input for number of bins 
   textInput("seq1",
               "Sequence 1:",
               value = "ATG"),
   textInput("seq2",
              "Sequence 2:",
              value = "ATG"),
   plotlyOutput("p")
)
)
