
#ISPE 2025
#Introduction to R Shiny for Pharmacoepidemiologists: Interactive Visualization in Real-World Evidence Studies

#Install shiny package if it is not installed
#install.packages("shiny")

library(shiny)

ui <- fluidPage(

  numericInput(
    inputId = "gracePeriod",
    label   = "Grace period",
    value   = 30),

  actionButton(
    inputId = "button",
    label   = "Click me"),

  textOutput(outputId = "gracePeriodNote")
)

server <- function(input, output, session){

  #Implemented a reactive expression
  note <- eventReactive(input$button, {
    paste("The grace period is:", input$gracePeriod)
  })

  output$gracePeriodNote <- renderText({
    note()  #Call the reactive object like a function
  })
}

shinyApp(ui, server)
