#   ____________________________________________________________________________
#   Server                                                                  ####
if (!require("shiny")) {
  install.packages("shiny")
  library(devtools)
}
if (!require("rgdal")) {
  install.packages("rgdal")
  library(devtools)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(devtools)
}
if (!require("sp")) {
  install.packages("sp")
  library(devtools)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(devtools)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(devtools)
}
if (!require("tidyr")) {
  install.packages("tidyr")
  library(devtools)
}
if (!require("magrittr")) {
  install.packages("magrittr")
  library(devtools)
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library(devtools)
}
if (!require("ggmap")) {
  install.packages("ggmap")
  library(devtools)
}
if (!require("xts")) {
  install.packages("xts")
  library(devtools)
}
if (!require("shinyjs")) {
  install.packages("shinyjs")
  library(devtools)
}
if (!require("urltools")) {
  install.packages("urltools")
  library(devtools)
}
if (!require("utils")) {
  install.packages("utils")
  library(devtools)
}
if (!require("rvest")) {
  install.packages("rvest")
  library(devtools)
}
if (!require("stringr")) {
  install.packages("stringr")
  library(devtools)
}
if (!require("rgeos")) {
  install.packages("rgeos")
  library(devtools)
}
if (!require("xml2")) {
  install.packages("xml2")
  library(devtools)
}
if (!require("selectr")) {
  install.packages("selectr")
  library(devtools)
}
if (!require("purrr")) {
  install.packages("purrr")
  library(devtools)
}
if (!require("raster")) {
  install.packages("raster")
  library(devtools)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(devtools)
}
if (!require("DT")) {
  install.packages("DT")
  library(devtools)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(devtools)
}
if (!require("readxl")) {
  install.packages("readxl")
  library(devtools)
}
if (!require("htmltools")) {
  install.packages("htmltools")
  library(devtools)
}
if (!require("htmlwidgets")) {
  install.packages("htmlwidgets")
  library(devtools)
}
if (!require("sqldf")) {
  install.packages("sqldf")
  library(devtools)
}

library(shiny)
library(leaflet)
library(shiny)
library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)
library(stringr)
library(leaflet)
require(rgdal)
require(ggplot2)
library(shinythemes)
library(broom)

# source("keyring.R")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Pretty-print function                                                   ####

server <- function(input, output) {
    

##  ............................................................................
##  Safety Tab                                                              ####
  
  safety_df <- reactive({
    if ( "shooting" %in% input$safety_measure_type){
      if ( "all" %in% input$safety_police_precinct){
        return(shooting_df)
      } else {
        return(shooting_df %>% filter(PRECINCT_5243 == input$safety_police_precinct))
      }
    }
    if ( "use_of_force" %in% input$safety_measure_type){
      if ( "all" %in% input$safety_police_precinct){
        return(force_df)
      } else {
        return(force_df %>% filter(PRECINCT_5243 == input$safety_police_precinct))
      }
    }
    if ( "arrest" %in% input$safety_measure_type){
      if ( "all" %in% input$safety_police_precinct){
        return(arrest_df)
      } else {
        return(arrest_df %>% filter(PRECINCT_5243 == input$safety_police_precinct))
      }
    }
  })
  
  safety_count_df <- reactive({
    if ( "shooting" %in% input$safety_measure_type){
      if ( "all" %in% input$safety_police_precinct){
        return(shooting_count_df)
      } else {
        return(shooting_count_df %>% filter(Var1 == input$safety_police_precinct))
      }
    }
    if ( "use_of_force" %in% input$safety_measure_type){
      if ( "all" %in% input$safety_police_precinct){
        return(force_count_df)
      } else {
        return(force_count_df %>% filter(Var1 == input$safety_police_precinct))
      }
    }
    if ( "arrest" %in% input$safety_measure_type){
      if ( "all" %in% input$safety_police_precinct){
        return(arrest_count_df)
      } else {
        return(arrest_count_df %>% filter(Var1 == input$safety_police_precinct))
      }
    }
  })
  
  safety_summary_data <- reactive({
    text <- c("There are ", sum(safety_count_df()$Freq), input$safety_measure_type, 
              " instance(s) in ", input$safety_police_precinct, " precinct during year of 2021")
    return(paste(text))
    
  })
  
   safety_map_data <- reactive({
      spdf_fortified = spdf_fortified_raw %>%
        left_join(. , safety_count_df(), by=c("id"="Var1"))
      
      # Note that if the number of restaurant is NA, it is in fact 0
      spdf_fortified$Freq[ is.na(spdf_fortified$Freq)] = 0.001
      return(spdf_fortified)
    })
   
   safety_trend_data <- reactive({
       df = safety_df()
       
       df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
       data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
       
       return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
               %>%group_by(Month)%>%summarise( num = length(DATE_5243)) 
               %>%mutate( month = paste(Month,"/01", sep = ""))) 
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
  
  output$safety_summary <- renderText({
    safety_summary_data()
  })
  
  ### Safety comparisons
  
  safety_comp_summary_data <- reactive({
    p1_shooting <- shooting_count_df %>% filter(Var1 == input$safety_precinct_1)
    p1_arrest <- arrest_count_df %>% filter(Var1 == input$safety_precinct_1)
    p1_force <- force_count_df %>% filter(Var1 == input$safety_precinct_1)
    
    p2_shooting <- shooting_count_df %>% filter(Var1 == input$safety_precinct_2)
    p2_arrest <- arrest_count_df %>% filter(Var1 == input$safety_precinct_2)
    p2_force <- force_count_df %>% filter(Var1 == input$safety_precinct_2)
    
    text <- c("There are ", sum(p1_shooting$Freq), "shooting instance(s) in ", 
              input$safety_precinct_1, " precinct during year of 2021. ",
              "In comparison",
              "there are ", sum(p2_shooting$Freq), "shooting instance(s) in ", 
              input$safety_precinct_2, " precinct during year of 2021. ",
              "There are ", sum(p1_arrest$Freq), "arrest instance(s) in ", 
              input$safety_precinct_1, " precinct during year of 2021. ",
              "In comparison",
              "there are ", sum(p2_arrest$Freq), "arrest instance(s) in ", 
              input$safety_precinct_2, " precinct during year of 2021. ",
              "There are ", sum(p1_force$Freq), "use of force instance(s) in ", 
              input$safety_precinct_1, " precinct during year of 2021. ",
              "In comparison",
              "there are ", sum(p2_force$Freq), "use of force instance(s) in ", 
              input$safety_precinct_2, " precinct during year of 2021. "
              
              )
    return(paste(text))
  })
  
  output$safety_comparison_summary <- renderText({safety_comp_summary_data()})
  
  pie_charts <- reactive({
    p1_shooting <- shooting_count_df %>% filter(Var1 == input$safety_precinct_1)
    p1_arrest <- arrest_count_df %>% filter(Var1 == input$safety_precinct_1)
    p1_force <- force_count_df %>% filter(Var1 == input$safety_precinct_1)
    
    p2_shooting <- shooting_count_df %>% filter(Var1 == input$safety_precinct_2)
    p2_arrest <- arrest_count_df %>% filter(Var1 == input$safety_precinct_2)
    p2_force <- force_count_df %>% filter(Var1 == input$safety_precinct_2)
    
    slices_s <- c(sum(p1_shooting$Freq), sum(p2_shooting$Freq))
    lbls <- c(input$safety_precinct_1, input$safety_precinct_2)
    
    slices_a <- c(sum(p1_arrest$Freq), sum(p2_arrest$Freq))
    
    slices_f <- c(sum(p1_force$Freq), sum(p2_force$Freq))
    
    par(mfrow=c(1,3))
    pie(slices_s, labels = lbls, main="Pie Chart of Shooting Instance Comparison")
    pie(slices_a, labels = lbls, main="Pie Chart of Arrest Instance Comparison")
    pie(slices_f, labels = lbls, main="Pie Chart of Use Of Force Instance Comparison")
  })
  
  output$safety_pies <- renderPlot({
    pie_charts()
  })
  
  shooting_comp_skeleton <- reactive({
    df = shooting_df
    df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
    data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
    
    return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = 0) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
  })
  
  shooting_comp_data1 <- reactive({
    df = shooting_df %>% filter(PRECINCT_5243 == input$safety_precinct_1)
    
    df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
    data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
    
    return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = length(DATE_5243)) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
  })
  
  shooting_comp_data2 <- reactive({
    df = shooting_df %>% filter(PRECINCT_5243 == input$safety_precinct_2)
    
    df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
    data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
    
    return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = length(DATE_5243)) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
  })
  
  output$safety_shooting_comparison <- renderPlot({
    data <- shooting_comp_skeleton()
    data1 <- shooting_comp_data1()
    data2 <- shooting_comp_data2()
    # Time Series
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    plot(num ~ as.Date(month), data,
         ylab = "monthly instances ", xlab = "", col="black",
         ylim=c(min(min(data1$num), min(data2$num)), max(max(data1$num), max(data2$num))),
         main = paste("Number of shooting instance in all precincts during 2021"))
    lines(num ~ as.Date(month), data1, xaxt = "n", type = "o", pch = 22, lty = 1, pty = 1,
          col="blue")
    lines(num ~ as.Date(month), data2, xaxt = "n", type = "o", pch = 22, lty = 2, pty = 2,
          col="red")
    legend("topright", inset=c(-0.2,0), legend=c(input$safety_precinct_1, input$safety_precinct_2), lty=c(1,2), title="Precinct")
  })
  
  
  arrest_comp_skeleton <- reactive({
    df = arrest_df
    df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
    data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
    
    return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = 0) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
  })
  
  arrest_comp_data1 <- reactive({
    df = arrest_df %>% filter(PRECINCT_5243 == input$safety_precinct_1)
    
    df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
    data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
    
    return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = length(DATE_5243)) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
  })
  
  arrest_comp_data2 <- reactive({
    df = arrest_df %>% filter(PRECINCT_5243 == input$safety_precinct_2)
    
    df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
    data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
    
    return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = length(DATE_5243)) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
  })
  
  output$safety_arrest_comparison <- renderPlot({
    data <- arrest_comp_skeleton()
    data1 <- arrest_comp_data1()
    data2 <- arrest_comp_data2()
    # Time Series
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    plot(num ~ as.Date(month), data,
         ylab = "monthly instances ", xlab = "", col="black",
         ylim=c(min(min(data1$num), min(data2$num)), max(max(data1$num), max(data2$num))),
         main = paste("Number of arrest instance in all precincts during 2021"))
    lines(num ~ as.Date(month), data1, xaxt = "n", type = "o", pch = 22, lty = 1, pty = 1,
          col="blue")
    lines(num ~ as.Date(month), data2, xaxt = "n", type = "o", pch = 22, lty = 2, pty = 2,
          col="red")
    legend("topright", inset=c(-0.2,0), legend=c(input$safety_precinct_1, input$safety_precinct_2), lty=c(1,2), title="Precinct")
  })
  
  force_comp_skeleton <- reactive({
    df = force_df
    df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
    data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
    
    return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = 0) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
  })
  
  force_comp_data1 <- reactive({
    df = force_df %>% filter(PRECINCT_5243 == input$safety_precinct_1)
    
    df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
    data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
    
    return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = length(DATE_5243)) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
  })
  
  force_comp_data2 <- reactive({
    df = force_df %>% filter(PRECINCT_5243 == input$safety_precinct_2)
    
    df$DATE_5243 <- as.Date(df$DATE_5243, format = "%m/%d/%Y")
    data = subset(df, DATE_5243 > "2020-12-31" & DATE_5243 < "2022-01-01")
    
    return( data %>% mutate(Month = format(DATE_5243, "%Y/%m"))
            %>%group_by(Month)%>%summarise( num = length(DATE_5243)) 
            %>%mutate( month = paste(Month,"/01", sep = ""))) 
  })
  
  output$safety_force_comparison <- renderPlot({
    data <- force_comp_skeleton()
    data1 <- force_comp_data1()
    data2 <- force_comp_data2()
    # Time Series
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    plot(num ~ as.Date(month), data,
         ylab = "monthly instances ", xlab = "", col="black",
         ylim=c(min(min(data1$num), min(data2$num)), max(max(data1$num), max(data2$num))),
         main = paste("Number of use of force instance in all precincts during 2021"))
    lines(num ~ as.Date(month), data1, xaxt = "n", type = "o", pch = 22, lty = 1, pty = 1,
          col="blue")
    lines(num ~ as.Date(month), data2, xaxt = "n", type = "o", pch = 22, lty = 2, pty = 2,
          col="red")
    legend("topright", inset=c(-0.2,0), legend=c(input$safety_precinct_1, input$safety_precinct_2), lty=c(1,2), title="Precinct")
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
  
  #---------Events tab---------------
  events = read.csv("data/Events/Events.csv")
  Event_filter <- reactive({
    req(input$RegionFinder)
    req(input$CategoryFinder)
    req(input$metricChoice)
    req(input$TimeFinderMin)
    req(input$TimeFinderMax)
    
    startTime = strftime(as.POSIXct(paste(input$TimeFinderMin,":00",sep = ""),format="%H:%M:%S"),"%H:%M:%S")
    endTime = strftime(as.POSIXct(paste(input$TimeFinderMax,":00",sep = ""),format="%H:%M:%S"),"%H:%M:%S")
    
    filter(events, Borough %in% input$RegionFinder) %>%
      filter(Category %in% input$CategoryFinder) %>%
      filter(strftime(time,"%H:%M:%S") > startTime) %>%
      filter(strftime(time,"%H:%M:%S") < endTime)
  })
  
  output$plot1 <- renderPlot({
    input$RegionFinder
    input$CategoryFinder
    input$metricChoice
    if(input$metricChoice == "Attendance"){
      ggplot()+
        geom_bar(data = Event_filter(),aes(y = Attendance, x = Borough, fill = Category), stat = "summary", fun = "sum")+
        ggtitle("Total Space in Each Borough")+
        theme(plot.title = element_text(hjust = 0.5))
    }else if(input$metricChoice == "Num"){
      ggplot(data = Event_filter(), aes(x = Borough, fill = Category))+
        geom_bar()+
        ggtitle("Number of Events in Each Borough")+
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  #Plot the subway station map
  output$mymap <- renderLeaflet({
    leaflet(data = Event_filter()) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat, label = ~Location)
  })
  
  
  
      
} # server end

