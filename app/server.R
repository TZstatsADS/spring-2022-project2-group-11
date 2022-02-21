#   ____________________________________________________________________________
#   Server                                                                  ####

library(shiny)
library(rgdal)
library(leaflet)
library(sp)
library(plotly)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggmap)
library(xts)
library(shinyjs)
library(jsonlite)
library(urltools)
library(utils)
library(rvest)
library(stringr)
library(rgeos)
library(xml2)
library(selectr)
library(raster)
library(purrr)
library(RColorBrewer)
library(DT)
library(shinyBS)
library(ggplot2)

# source("keyring.R")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Pretty-print function                                                   ####

server <- function(input, output) {
    

##  ............................................................................
##  Safety Tab                                                              ####
   safety_map_data <- reactive({
      if ( "shooting" %in% input$safety_measure_type){
        # Make the merge
        print("loading shooting data")
        spdf_fortified = spdf_fortified_raw %>%
          left_join(. , shooting_count_df, by=c("id"="Var1"))
        
        # Note that if the number of restaurant is NA, it is in fact 0
        spdf_fortified$Freq[ is.na(spdf_fortified$Freq)] = 0.001
        return(spdf_fortified)
      }
     
     if ( "use_of_force" %in% input$safety_measure_type){
       # Make the merge
       
       print("loading use_of_force data")
       spdf_fortified = spdf_fortified_raw %>%
         left_join(. , force_count_df, by=c("id"="Var1"))
       
       # Note that if the number of restaurant is NA, it is in fact 0
       spdf_fortified$Freq[ is.na(spdf_fortified$Freq)] = 0.001
       return(spdf_fortified)
     }
      if ( "arrest" %in% input$safety_measure_type){
        # Make the merge
        
        print("loading arrest data")
        spdf_fortified = spdf_fortified_raw %>%
          left_join(. , arrest_count_df, by=c("id"="Var1"))
        
        # Note that if the number of restaurant is NA, it is in fact 0
        spdf_fortified$Freq[ is.na(spdf_fortified$Freq)] = 0.001
        return(spdf_fortified)
      }
    })
   
   safety_trend_data <- reactive({
     if ( "shooting" %in% input$safety_measure_type){
       data = shooting_df
       
       return( data %>% mutate(Month = format(as.Date(OCCUR_DATE, format = "%m/%d/%Y"), "%Y/%m"))
               %>%group_by(Month)%>%summarise( num = length(OCCUR_DATE)) 
               %>%mutate( month = paste(Month,"/01", sep = ""))) 
     }
     if ( "use_of_force" %in% input$safety_measure_type){
       df = force_df
       
       df$Occurrence.Date <- as.Date(df$Occurrence.Date, format = "%m/%d/%Y")
       data = subset(df, Occurrence.Date > "2020-12-31" & Occurrence.Date < "2022-01-01")
       
       return( data %>% mutate(Month = format(Occurrence.Date, "%Y/%m"))
               %>%group_by(Month)%>%summarise( num = length(Occurrence.Date)) 
               %>%mutate( month = paste(Month,"/01", sep = ""))) 
     }
     if ( "arrest" %in% input$safety_measure_type){
       data = arrest_df
       
       return( data %>% mutate(Month = format(as.Date(ARREST_DATE, format = "%m/%d/%Y"), "%Y/%m"))
               %>%group_by(Month)%>%summarise( num = length(ARREST_DATE)) 
               %>%mutate( month = paste(Month,"/01", sep = ""))) 
     }
   })

  output$safetyMapPlot <- renderPlot({
    data1 = safety_map_data()
    print("loaded safety_map_data")
    ggplot() +
      geom_polygon(data = data1, aes(fill = Freq, x = long, y = lat, group = group)) +
      theme_void() +
      coord_map()
  })
  output$safetyTrend <- renderPlot({
    # Time Series
    process <- function() {
      safety_trend_data()
    }
    data = process()
    plot(num ~ as.Date(month), data, xaxt = "n", type = "o", pch = 22, lty = 1, pty = 2,
         ylab = "monthly instances ", xlab = "",
         main = paste("Number of safety instance in all precincts"))
    axis.Date(1, at = data$month, format= "%m-%Y", las = 1)
  })
  
  output$covidTrend <- renderPlot({
    # Time Series
    df = cases_by_day_df
    
    df$date_of_interest <- as.Date(df$date_of_interest, format = "%m/%d/%Y")
    df = subset(df, date_of_interest > "2020-12-31" & date_of_interest < "2022-01-01")
    
    process <- function() { return(df %>% mutate(Month = format(as.Date(date_of_interest, format = "%m/%d/%Y"), "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = sum(CASE_COUNT)) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
    }
    data = process()
    plot(num ~ as.Date(month), data, xaxt = "n", type = "o", pch = 22, lty = 1, pty = 2,
         ylab = "monthly covid instances ", xlab = "",
         main = paste("Number of covid cases in all precincts"))
    axis.Date(1, at = data$month, format= "%m-%Y", las = 1)
  })
    
}
