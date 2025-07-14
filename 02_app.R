
#ISPE 2025
#Introduction to R Shiny for Pharmacoepidemiologists: Interactive Visualization in Real-World Evidence Studies

#Install shiny package if it is not installed
#install.packages("shiny")

library(shiny)

ui <- fluidPage(
  "Hello, ISPE! :)"
)

server <- function(input, output, session){

}

shinyApp(ui,server)