#Author: Diego Miguel M. Villamil
#Description: This file is the main app

library(shiny)
library(shinythemes)  #https://rstudio.github.io/shinythemes/
                      #Used for the dark mode theme

library(shinyWidgets) #https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html#ref-examples
                      #Used for the pickerInput widget

source('otherRScripts/acm.R')
source('otherRScripts/gaussianMethod.R')
source('otherRScripts/polyReg.R')
source('otherRScripts/quadSpline.R')
source('otherRScripts/simplexMin.R')
source('otherRScripts/diet.R')

appDir <- getwd()
shinyAppDir(appDir, options = list())
