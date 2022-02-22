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
library(readxl)
library(htmltools)
library(htmlwidgets)
library(sqldf)

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

  
##  ............................................................................
##  Restaurant Tab                                                              ####  
  # ----------------- Restaurant Seating --------------------------
  # Restaurant Map
  output$rs_map <- renderLeaflet({
    leaflet(rs_open, options = leafletOptions(minZoom = 2, maxZoom = 20)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.95, lat = 40.72, zoom = 10) %>%
      addMarkers(lng = rs_open$Longitude, lat = rs_open$Latitude,
                 clusterOptions = markerClusterOptions(),
                 label = lapply(
                   lapply(seq(nrow(rs_open)), function(i){
                     paste0('<b>',rs_open[i, "Restaurant.Name"], '</b>', '<br/>', 
                            'Address: ', rs_open[i, "Street"], " ", rs_open[i, "Building.Number"], '<br/>',
                            'Zipcode: ', rs_open[i, "Postcode"], '<br/>',
                            'Alcohol: ', rs_open[i, "Qualify.Alcohol"]) }), htmltools::HTML)) %>%
      addResetMapButton()
  })
  
  # filtered data for zooming in specific area in the map
  selected_boro_map <- reactive({
    
    if(is.null(input$borough) && is.null(input$check_seating)){
      rs_open %>%
        filter(Borough %in% levels(rs_open$Borough))}
    else if (!is.null(input$borough) && is.null(input$check_seating)){
      rs_open %>%
        filter(Borough %in% input$borough)}
    else if (is.null(input$borough) && !is.null(input$check_seating)){
      rs_open %>%
        filter(seating %in% input$check_seating)}
    else{
      rs_open %>%
        filter(seating %in% input$check_seating) %>%
        filter(Borough %in% input$borough)}
    
  })
  
  observe({
    temp_map = selected_boro_map()
    leafletProxy('rs_map',data = temp_map) %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude)) %>% 
      clearMarkerClusters()%>%
      clearMarkers() %>% 
      addMarkers(lng = temp_map$Longitude, lat = temp_map$Latitude,
                 clusterOptions = markerClusterOptions(),
                 label = lapply(
                   lapply(seq(nrow(temp_map)), function(i){
                     paste0('<b>',temp_map[i, "Restaurant.Name"], '</b>', '<br/>', 
                            'Address: ', temp_map[i, "Street"], " ", temp_map[i, "Building.Number"], '<br/>',
                            'Zipcode: ', temp_map[i, "Postcode"], '<br/>',
                            'Seating: ', temp_map[i, "seating"],'<br/>',
                            'Alcohol: ', temp_map[i, "Qualify.Alcohol"]) }), htmltools::HTML))
  })
  
  #-------------------- Restaurant Inspection ---------------------
  select_boro_inspect <- reactive({
    rs_inspect %>%
      filter(BORO %in% input$boro)
  })
  
  output$rs_inspect_plot <- renderPlot({
    if(input$check == FALSE){
      ggplot(select_boro_inspect()
             %>% count(grades, `CUISINE DESCRIPTION`), 
             aes(fill=grades, y=n, x=`CUISINE DESCRIPTION`)) + 
        geom_bar(position="stack", stat="identity") + 
        theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1),
              plot.title=element_text(hjust = 0.5)
        ) + 
        ggtitle(paste("Counts of Restaurants in", input$boro)) + 
        xlab("cuisine type") + ylab("counts") + 
        scale_fill_manual("grades", values = c("A" = "#CCFFCC", "B" = "#99CCFF", "C" = "#FF9999"))
    }
    
    else if(input$check == TRUE){
      ggplot(rs_inspect 
             %>% count(grades, BORO), 
             aes(fill=grades, y=n, x=BORO)) + 
        geom_bar(stat="identity") + 
        theme(plot.title=element_text(hjust = 0.5)) + 
        ggtitle(paste("Counts of Restaurants in Each Borough")) + 
        xlab("boroughs") + ylab("counts") + 
        scale_fill_manual("grades", values = c("A" = "#CCFFCC", "B" = "#99CCFF", "C" = "#FF9999"))
    }
    
  }) #renderPlot end
  
  
  
  
  
  
      
} # server end

