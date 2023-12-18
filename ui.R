#Author: Diego Miguel M. Villamil
#Description: This file is for the UI elements of the Rshiny app

ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  "CMSC 150 Generic Solver",
                  tabPanel("Polynomial Regression",
                           sidebarPanel(
                             tags$h3("Input:"),
                             numericInput("polyOrder", "Enter the order of the polynomial:", ""),    #Polynomial function order input
                             fileInput("polyFile", "Select a file"),                         #csv file selection
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Ouput"),
                             
                             h4("Generated function (f(x)):"),
                             h4("Estimated value of f(x):"),
                             
                           ) # mainPanel
                           
                  ), # Polynomial Regression Tab Panel
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage     