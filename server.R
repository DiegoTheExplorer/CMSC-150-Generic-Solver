#Author: Diego Miguel M. Villamil
#Description: This is the back end part of the Rshiny app

source('otherRScripts/polyReg.R')

server <- function(input, output) {
  
  #Start of Polynomial Regression handling
  
  observeEvent(input$polyBtn, {
    req(input$polyFile,input$polyOrder,input$polyX,cancelOutput = TRUE)    #Check if all input fields have inputs
    
    data <- read.csv(input$polyFile$datapath, header = FALSE)
    numDataPoints <- nrow(data)
    if(input$polyOrder >= numDataPoints){     #Check if the input polynomial order is equal to or less than the number of data points
      output$errorMsg <- renderText({
        paste("Polynomial order input is not less than the number of datapoints")
      }) #error message output
      polyRegOutput <- PolynomialRegression(input$polyOrder, c(data["V1"],data["V2"]))
      output$polyFunction <- renderText({
        paste("")
      }) 
      output$polyEstimate <- renderText({
        paste("")
      }) 
    }else{
      output$errorMsg <- renderText({    
        paste("")
      }) 
      
      polyRegOutput <- PolynomialRegression(input$polyOrder, c(data["V1"],data["V2"]))
      output$polyFunction <- renderText({
        paste(polyRegOutput["polynomial_string"][[1]])
      }) #Function string output
      output$polyEstimate <- renderText({
        paste(as.character(polyRegOutput["polynomial_function"][[1]](input$polyX)))
      }) #Estimate output
    }
    
  }) #polyBtn Observer
  
  #End of Polynomial Regression handling
  
  
  
} # server
