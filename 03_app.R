
#ISPE 2025
#Introduction to R Shiny for Pharmacoepidemiologists: Interactive Visualization in Real-World Evidence Studies

#Install shiny package if it is not installed
#install.packages("shiny")
library(shiny)

ui <- fluidPage(

  shiny::numericInput(
    inputId = "gracePeriod",
    label = "Grace period",
    value = 30
  ),

  textOutput(outputId = "gracePeriodNote")

)

server <- function(input, output, session){

  output$gracePeriodNote <- renderText({

    paste("The grace period is:", input$gracePeriod)

  })

}

shinyApp(ui,server)