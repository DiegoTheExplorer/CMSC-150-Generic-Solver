#Author: Diego Miguel M. Villamil
#Description: This file is for the UI elements of the Rshiny app

ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  "CMSC 150 Generic Solver",
                  tabPanel("Polynomial Regression",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Enter the order of the polynomial:", ""),    #
                             fileInput("filePoly", "Select a file"),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Ouput"),
                             
                             h4("Generated function (f(x)):"),
                             h4("Estimated value of f(x):"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage