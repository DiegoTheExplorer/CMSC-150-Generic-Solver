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
                             sidebarPanel(
                               tags$h3("Food list"),
                               selectInput(
                                 "foodChoices",
                                 "Select foods to be part of the diet: ",
                                 choices = nvt["Foods"][[1]],
                                 selected = NULL,
                                 width = NULL,
                                 multiple = TRUE
                               ),
                               actionButton("dietBtn", "Estimate x"),
                             ), # food selection panel
                             mainPanel(
                               h1("Constraints"),
                               h4("Set the nutritional value constraints here"),
                               numericInput(                                    #Calories
                                 "cons1",
                                 "Calories",
                                 2000,
                                 min = 2000,
                                 max = 2250,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Cholesterol
                                 "cons2",
                                 "Cholesterol",
                                 0,
                                 min = 0,
                                 max = 300,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Total Fat
                                 "cons3",
                                 "Total Fat",
                                 0,
                                 min = 0,
                                 max = 65,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Sodium
                                 "cons4",
                                 "Sodium",
                                 0,
                                 min = 0,
                                 max = 2400,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Carbohydrates
                                 "cons5",
                                 "Carbohydrates",
                                 0,
                                 min = 0,
                                 max = 300,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Dietary Fiber
                                 "cons6",
                                 "Dietary Fiber",
                                 25,
                                 min = 25,
                                 max = 100,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Protein
                                 "cons7",
                                 "Protein",
                                 50,
                                 min = 50,
                                 max = 100,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Vitamin A
                                 "cons8",
                                 "Vitamin A",
                                 5000,
                                 min = 5000,
                                 max = 50000,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Vitamin C
                                 "cons9",
                                 "Vitamin C",
                                 50,
                                 min = 50,
                                 max = 20000,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Calcium
                                 "cons10",
                                 "Calcium",
                                 800,
                                 min = 800,
                                 max = 1600,
                                 step = NA,
                                 width = NULL
                               ),
                               numericInput(                                    #Iron
                                 "cons11",
                                 "Iron",
                                 10,
                                 min = 10,
                                 max = 30,
                                 step = NA,
                                 width = NULL
                               ),
                             ) # mainPanel
                ),# Diet Calculator Input Page
                tabPanel("Diet Calculator Output",
                         h1("Ouput"),
                         
                         h4("Objective Function:"),
                         verbatimTextOutput("objFunction"), 
                ), # Diet Calculator Output Page
                  
              )

) # fluidPage     