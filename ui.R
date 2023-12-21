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
                             verbatimTextOutput("polyEstimate"),      #Output for the estimated value of x
                             verbatimTextOutput("errorMsg"),
                             h4("Values of x and f(x) from input .csv:"),
                             tableOutput("polyTable"),
                             tags$head(tags$style("#polyTable .table {background-color:  #001e3a;}", media="screen", type="text/css")) 
                             #https://stackoverflow.com/questions/21904188/r-shiny-how-to-change-background-color-of-a-table
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
                             tableOutput("quadTable"),
                             tags$head(tags$style("#quadTable .table {background-color:  #001e3a;}", media="screen", type="text/css")) 
                           ) # mainPanel
                  ), # Quadratic Spline Interpolation Tab Panel
                  tabPanel("Simplex Diet Calculator Input",
                             mainPanel(
                               tags$h3("Food list"),
                               pickerInput(
                                 "foodChoices",
                                 "Select foods to be part of the diet: ",
                                 choices = nvt["Foods"][[1]],
                                 selected = NULL,
                                 width = NULL,
                                 multiple = TRUE,
                                 options = list('actions-box' = TRUE),
                               ),
                               actionButton("dietBtn", "Optimize Diet"),
                               h4("Objective Function"),
                               h6("Minimize:"),
                               verbatimTextOutput("objFunction"), 
                               h6("Selected foods:"),
                               tableOutput("poodsTable"),
                               tags$head(tags$style("#poodsTable .table {background-color:  #001e3a;}", media="screen", type="text/css")) 
                             ), # Main panel
                ),# Diet Calculator Input Page
                tabPanel("Simplex Diet Calculator Output",
                         h1("Ouput"),
                ), # Diet Calculator Output Page
                  
              )

) # fluidPage     