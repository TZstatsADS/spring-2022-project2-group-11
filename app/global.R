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
if (!require("janitor")) {
  install.packages("janitor")
  library(janitor)
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
shooting_df$PRECINCT_5243 <- shooting_df$PRECINCT
shooting_df$DATE_5243 <- shooting_df$OCCUR_DATE
shooting_count_df$Var1 <- as.character(shooting_count_df$Var1)
arrest_count_df <- as.data.frame(table(arrest_df$ARREST_PRECINCT))
arrest_df$PRECINCT_5243 <- arrest_df$ARREST_PRECINCT
arrest_df$DATE_5243 <- arrest_df$ARREST_DATE
arrest_count_df$Var1 <- as.character(arrest_count_df$Var1)
force_count_df <- as.data.frame(table(force_df$Incident.Pct))
force_df$PRECINCT_5243 <- force_df$Incident.Pct
force_df$DATE_5243 <- force_df$Occurrence.Date
force_count_df$Var1 <- as.character(force_count_df$Var1)

all_precinct_ids <- unique(unique(shooting_count_df$Var1, arrest_count_df$Var1), force_count_df$Var1)
all_precinct_ids_with_all <- cbind(all_precinct_ids, c("all"))

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

print("processed all safety data")







#_________________________________________________
#           Asthetics
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("devtools")) {
  install.packages("devtools")
  library(devtools)
}
if (!require("rgdal")) {
  install.packages("rgdal")
  library(rgdal)
}
if (!require("rgeos")) {
  install.packages("rgeos")
  library(rgeos)
}
if (!require("maptools")) {
  install.packages("maptools")
  library(maptools)
}
if (!require("ggalt")) {
  install.packages("ggalt")
  library(ggalt)
}
if (!require("ggthemes")) {
  install.packages("ggthemes")
  library(ggthemes)
}
if (!require("ggrepel")) {
  install.packages("ggrepel")
  library(ggrepel)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}
if (!require("padr")) {
  install.packages("padr")
  library(padr)
}





# reference: https://github.com/emilyhalford/nyc_death_post

death = read_csv("data/nyc_death.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    race_ethnicity != "Not Stated/Unknown",
    race_ethnicity != "Other Race/ Ethnicity",
  ) %>% 
  mutate(
    deaths = as.numeric(deaths),
    death_rate = as.numeric(death_rate),
    age_adjusted_death_rate = as.numeric(age_adjusted_death_rate))

death = 
  death %>% 
  mutate(
    leading_cause = str_replace_all(leading_cause, "[//(//)]", ""),
    leading_cause = str_replace(leading_cause, "Influenza Flu and Pneumonia J09-J18", "Influenza & Pneumonia"),
    leading_cause = str_replace(leading_cause, "Accidents Except Drug Posioning V01-X39, X43, X45-X59, Y85-Y86", "Accidents"),
    leading_cause = str_replace(leading_cause, "Cerebrovascular Disease Stroke: I60-I69", "Cerebrovascular Disease"),
    leading_cause = str_replace(leading_cause, "Assault Homicide: Y87.1, X85-Y09", "Assault"),
    leading_cause = str_replace(leading_cause, "Essential Hypertension and Renal Diseases (I10, I12)", "Hypertension & Renal Dis."),
    leading_cause = str_replace(leading_cause, "Human Immunodeficiency Virus Disease HIV: B20-B24", "HIV"),
    leading_cause = str_replace(leading_cause, "Diseases of Heart I00-I09, I11, I13, I20-I51", "Diseases of Heart"),
    leading_cause = str_replace(leading_cause, "Alzheimer's Disease G30", "Alzheimer's Disease"),
    leading_cause = str_replace(leading_cause, "Chronic Liver Disease and Cirrhosis K70, K73", "Chronic Liver Disease/Cirrhosis"),
    leading_cause = str_replace(leading_cause, "Malignant Neoplasms Cancer: C00-C97", "Malignant Neoplasms"),
    leading_cause = str_replace(leading_cause, "Diabetes Mellitus E10-E14", "Diabetes Mellitus"),
    leading_cause = str_replace(leading_cause, "Mental and Behavioral Disorders due to Accidental Poisoning and Other Psychoactive Substance Use F11-F16, F18-F19, X40-X42, X44", "Accidental Poisoning/Substance Use"),
    leading_cause = str_replace(leading_cause, "Septicemia A40-A41", "Septicemia"),
    leading_cause = str_replace(leading_cause, "Chronic Lower Respiratory Diseases J40-J47", "Chronic Lower Respiratory Dis."),
    leading_cause = str_replace(leading_cause, "Nephritis, Nephrotic Syndrome and Nephrisis N00-N07, N17-N19, N25-N27", "Nephritis"),
    leading_cause = str_replace(leading_cause, "Certain Conditions originating in the Perinatal Period P00-P96", "Perinatal Period Conditions"),
    leading_cause = str_replace(leading_cause, "Viral Hepatitis B15-B19", "Viral Hepatitis"),
    leading_cause = str_replace(leading_cause, "Intentional Self-Harm Suicide: X60-X84, Y87.0", "Suicide"),
    leading_cause = str_replace(leading_cause, " All Other Causes", "Other")
  )

write_csv(death, "data/cleaned_nyc_data.csv")
