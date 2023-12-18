#Author: Diego Miguel M. Villamil
#Description: This is the backend part of the Rshiny app

server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
} # server