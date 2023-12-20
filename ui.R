#Author: Diego Miguel M. Villamil
#Description: This file is for the UI elements of the Rshiny app

ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  "CMSC 150 Generic Solver",
                  tabPanel("Polynomial Regression",
                           sidebarPanel(
                             tags$h3("Input:"),
                             numericInput("polyOrder", "Enter the order of the polynomial:", ""),    #Polynomial function order input
                             numericInput("polyX", "Enter the value of x to be estimated:", ""),     #x value to be estimated
                             fileInput("polyFile", "Select a file", accept = ".csv"),                #csv file selection
                             actionButton("polyBtn", "Estimate x"),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Ouput"),
                             
                             h4("Generated function (f(x)):"),
                             verbatimTextOutput("polyFunction"),      #Output for the Polynomial Regression Function
                             
                             h4("Estimated value of f(x):"),
                             verbatimTextOutput("polyEstimate"),     #Output for the estimated value of x
                             verbatimTextOutput("errorMsg"),
                           ) # mainPanel
                           
                  ), # Polynomial Regression Tab Panel
                  tabPanel("Quadratic Spline Interpolation",
                           sidebarPanel(
                             tags$h3("Input:"),
                             numericInput("quadX", "Enter the value of x to be estimated:", ""),     #x value to be estimated
                             fileInput("quadFile", "Select a file", accept = ".csv"),                #csv file selection
                             actionButton("quadBtn", "Estimate x"),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Ouput"),
                             
                             h4("Generated functions (f(x)):"),
                             verbatimTextOutput("console1"),                    #Output for the interpolating functions
                             
                             h4("Estimated value of f(x):"),
                             verbatimTextOutput("quadEstimate"),                #Output for the estimated value of x
                             verbatimTextOutput("errorMsg2"),
                           ) # mainPanel
                  ), # Quadratic Spline Interpolation Tab Panel
                  tabPanel("Simplex Diet Input Calculator",
                             mainPanel(
                               tags$h3("Food list"),
                               selectInput(
                                 "foodChoices",
                                 "Select foods to be part of the diet: ",
                                 choices = nvt["Foods"][[1]],
                                 selected = NULL,
                                 width = NULL,
                                 multiple = TRUE
                               ),
                               actionButton("dietBtn", "Optimize Diet"),
                               h4("Objective Function:"),
                               verbatimTextOutput("objFunction"), 
                             ), # Main panel
                ),# Diet Calculator Input Page
                tabPanel("Diet Calculator Output",
                         h1("Ouput"),
                ), # Diet Calculator Output Page
                  
              )

) # fluidPage     