
#ISPE 2025
#Introduction to R Shiny for Pharmacoepidemiologists: Interactive Visualization in Real-World Evidence Studies

#Install shiny package if it is not installed
#install.packages("shiny")

library(shiny)

ui <- fluidPage(

  shiny::numericInput(
    inputId = "gracePeriod",
    label = "Grace period",
    value = 30),

  actionButton(inputId = "button",
               label = "Click me"),

  textOutput(outputId = "gracePeriodNote")
)

server <- function(input, output, session){

  output$gracePeriodNote <- renderText({
    paste("The grace period is:", input$gracePeriod)
  }) |> bindEvent(input$button)

}

shinyApp(ui,server)