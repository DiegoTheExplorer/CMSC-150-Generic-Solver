#Author: Diego Miguel M. Villamil
#Description: This file is the main app

library(shiny)
library(shinythemes)

source('otherRScripts/acm.R')
source('otherRScripts/gaussianMethod.R')
source('otherRScripts/polyReg.R')
source('otherRScripts/quadSpline.R')
source('otherRScripts/diet.R')

appDir <- getwd()
shinyAppDir(appDir, options = list())
