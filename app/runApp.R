#loading the necessary libraries and packages
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
source("global.R")
source("ui.R")
source("server.R")

# Calling the other files
shinyApp(ui = ui, server = server)