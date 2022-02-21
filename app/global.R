if (!require("devtools")) {
  install.packages("devtools")
  library(devtools)
}
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}
if (!require("mapview")) {
  install.packages("mapview")
  library(mapview)
}
if (!require("leafsync")) {
  install.packages("leafsync")
  library(leafsync)
}
if (!require("sqldf")) {
  install.packages("sqldf")
  library(sqldf)
}
if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}
if (!require("ggmap")) {
  install.packages("ggmap")
  library(ggmap)
}

library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(dplyr)
library(magrittr)
library(mapview)
library(leafsync)
library(readxl)





#Data Processing
rstrt_insp <- read_excel("../data/New_York_City_Restaurant_Inspection_Results.xlsx")
rs_open <- read.csv("../data/Open_Restaurant_Applications.csv")









