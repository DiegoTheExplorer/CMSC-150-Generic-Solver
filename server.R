#Author: Diego Miguel M. Villamil
#Description: This is the back end part of the Rshiny app

server <- function(input, output) {
  
  #Start of Polynomial Regression handling
  
  observeEvent(input$polyBtn, {
    req(input$polyFile,input$polyOrder,input$polyX,cancelOutput = TRUE)    #Check if all input fields have inputs
    
    data <- read.csv(input$polyFile$datapath, header = FALSE)
    colnames(data) <- c("x","f(x)")
    data <- data[order(data$x, decreasing = FALSE),]
    numDataPoints <- nrow(data)
    if(input$polyOrder >= numDataPoints){     #Check if the input polynomial order is equal to or less than the number of data points
      output$errorMsg <- renderText({
        paste("Error: Polynomial order input is not less than the number of datapoints")
      }) #error message output
      polyRegOutput <- PolynomialRegression(input$polyOrder, c(data["x"],data["f(x)"]))
      output$polyFunction <- renderText({
      }) 
      output$polyEstimate <- renderText({
      })
      output$polyTable <- renderTable({
        
      })
    }else{
      output$errorMsg <- renderText({    
      }) 
      
      polyRegOutput <- PolynomialRegression(input$polyOrder, c(data["x"],data["f(x)"]))
      output$polyFunction <- renderText({
        paste(polyRegOutput["polynomial_string"][[1]])
      }) #Function string output
      output$polyEstimate <- renderText({
        paste(as.character(polyRegOutput["polynomial_function"][[1]](input$polyX)))
      }) #Estimate output
      output$polyTable <- renderTable({
        data
      })#input x and y values
    }
    
  }) #polyBtn Observer
  
  #End of Polynomial Regression handling
  
  #Start of Quadratic Spline Interpolation Handling
  observeEvent(input$quadBtn, {
    req(input$quadFile,input$quadX,cancelOutput = TRUE)    #Check if all input fields have inputs
    
    data <- read.csv(input$quadFile$datapath, header = FALSE)
    numDataPoints <- nrow(data)
    colnames(data) <- c("x","f(x)")
    
    data <- data[order(data$x, decreasing = FALSE),]
    
    quadOutput <- QuadraticSplineInterpolation(input$quadX,c(data["x"],data["f(x)"]))
    
    if(typeof(quadOutput) == typeof(list())){
      output$console1 <- renderPrint({
        print(quadOutput)
      })
      
      output$errorMsg2 <- renderText({
      })
      
      output$quadEstimate <- renderText({
        paste(as.character(quadOutput$estimate))
      }) #Estimate output
      
      output$quadTable <- renderTable({
        data
      })#input x and y values
    }else{
      output$errorMsg2 <- renderText({
        paste("Error: The input value for x was not within the interval of the input data")
      }) #Error message
      output$console1 <- renderPrint({
      })
      output$quadEstimate <- renderText({
      }) #Estimate output
      output$quadTable <- renderTable({
      })#input x and y values
    }
    
  }) #quadBtn Observer
  #End of Quadratic Spline Interpolation Handling
  
  #Start of Simplex Diet Solver handling
  observeEvent(input$dietBtn, {
    req(input$foodChoices,cancelOutput = TRUE)    #Check if the user chose food
    
    foodInp <- input$foodChoices
    dietOutput <- SimplexDietSolver(foodInp)
    
    #convert from f(x1,..xn) c1 * x1 + .... + cn * xn to (Total Cost) Z = c1 * x1 + .... + cn * xn
    output$objFunction <- renderText({
      objFuncText <- strsplit(dietOutput$objective_function, split = "\\) ")[[1]][2]
      objFuncText <- paste("(Total Cost) Z =",objFuncText)
      paste(objFuncText)
    }) #Objective function output
    
  })
  
  observeEvent(input$foodChoices, {
    fc <- input$foodChoices
    foodIndex <- match(fc,unlist(nvt["Foods"]))
    output$poodsTable <- renderTable({
      nvt[foodIndex,]
    })
  })
  #End of Simplex Diet Solver handling
  
} # server
