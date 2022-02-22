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
if (!require("mapproj")) {
  install.packages("mapproj")
  library(mapproj)
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
if (!require("geojsonio")) {
  install.packages("geojsonio")
  library(geojsonio)
}
if (!require("broom")) {
  install.packages("broom")
  library(broom)
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

# Geo data
precinct_csv = "https://raw.githubusercontent.com/OkeydokeyWang/gr5243_nvc_data_files/main/precinct.csv"
precinct_geojson = "https://raw.githubusercontent.com/OkeydokeyWang/gr5243_nvc_data_files/main/precinct.geojson"

# Safety Data
arrest_csv = "https://raw.githubusercontent.com/OkeydokeyWang/gr5243_nvc_data_files/main/arrest.csv"
force_csv = "https://raw.githubusercontent.com/OkeydokeyWang/gr5243_nvc_data_files/main/force.csv"
court_csv = "https://raw.githubusercontent.com/OkeydokeyWang/gr5243_nvc_data_files/main/court.csv"
shooting_csv = "https://raw.githubusercontent.com/OkeydokeyWang/gr5243_nvc_data_files/main/shooting.csv"

# Read all CSV files
shooting_df <- read.csv(shooting_csv)
arrest_df <- read.csv(arrest_csv)
court_df = read.csv(court_csv)
force_df = read.csv(force_csv)

# Read geojson data for precinct
spdf <- geojson_read(precinct_geojson,  what = "sp")
spdf_fortified_raw <- tidy(spdf, region = "precinct")

# Preprocess data for plotting use
shooting_count_df <- as.data.frame(table(shooting_df$PRECINCT))
shooting_count_df$Var1 <- as.character(shooting_count_df$Var1)
arrest_count_df <- as.data.frame(table(arrest_df$ARREST_PRECINCT))
arrest_count_df$Var1 <- as.character(arrest_count_df$Var1)
force_count_df <- as.data.frame(table(force_df$Incident.Pct))
force_count_df$Var1 <- as.character(force_count_df$Var1)

#Covid data
cases_by_day_df <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/cases-by-day.csv")

# Restaurant seating data
rs_open <- read.csv("data/Open_Restaurant_Applications.csv")
rs_open <- rs_open[which(!is.na(rs_open$Latitude) & !is.na(rs_open$Longitude)), ]
rs_open$Building.Number <- ifelse(rs_open$Building.Number == "undefined", "", rs_open$Building.Number)

# Restaurant Inspection data
rs_inspect <- read_excel("data/New_York_City_Restaurant_Inspection_Results.xlsx")
rs_inspect <- rs_inspect %>%
  mutate(grades = ifelse(SCORE < 14, "A", ifelse(SCORE >= 14 & SCORE <= 27, "B", "C")))
rs_inspect <- rs_inspect[which(rs_inspect$INSPECTION_DATE >= "2020-06-01"), ]
rs_inspect <- sqldf("
                     select distinct(CAMIS), DBA, BORO, BUILDING, STREET,ZIPCODE, `CUISINE DESCRIPTION`, 
                     SCORE, INSPECTION_DATE, Longitude, Latitude, grades
                    from rs_inspect
                    where Longitude is not NULL and
                    Latitude is not NULL and
                    grades is not NULL and
                    ZIPCODE is not NULL
                    ")

print("processed all data")



