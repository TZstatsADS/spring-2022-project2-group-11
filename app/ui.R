#   ____________________________________________________________________________
#   UI                                                                      ####

library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)
library(stringr)
require(rgdal)
require(ggplot2)
library(shinythemes)
library(broom)

source("appParts.R")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue

ui <- navbarPage(title = "NYC in Covid",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                              tags$link(rel = "icon", 
                                        type = "image/png", 
                                        href = "images/logo_icon.png")
                            )
                   ),

                   
                   # ----------------------------------
                   # tab panel 3 - Location Comparison
                   tabPanel("Aesthetics",
                            fluidPage()
                   ),
                   
                 
                   # ----------------------------------
                   # tab panel 3 - Location Comparison
                   tabPanel("Events",
                            fluidPage(  sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput(inputId = "RegionFinder",
                                                   label = "Select Regions(s):",
                                                   choices = c("Manhattan" = "Manhattan", "Brooklyn" = "Brooklyn", "Queens" = "Queens", "Staten Island" = "Staten Island", "Bronx" = "Bronx"),
                                                   selected = c("Manhattan","Brooklyn","Queens","Staten Island", "Bronx")),
                                checkboxGroupInput(inputId = "CategoryFinder",
                                                   label = "Select Event Categories(s):",
                                                   choices = c("Fitness" = "Fitness", "Nature" = "Nature", "Arts/Culture" = "Arts/Culture", "Academic" = "Academic/Out of School time", "Performance" = "Performance", "Family Festival" = "Family Festival", "Sport" = "Sport", "Mobile Unit" = "Mobile Unit", "KIM" = "KIM"),
                                                   selected = c("Fitness")),
                                radioButtons(inputId="metricChoice", label="Select a metric you want to see", 
                                             choices=c("Total Space" = "Attendance", "Number of Events" = "Num")),
                                fluidRow(column(5,
                                                textInput(inputId = "TimeFinderMin",
                                                          label = "From:",
                                                          value = "1:00",
                                                          width = "100px")
                                ),
                                column(5, ofset = 3,
                                       textInput(inputId = "TimeFinderMax",
                                                 label = "To:",
                                                 value = "23:00",
                                                 width = "100px")
                                )),
                                helpText("Please enter time 00:00-23:59"),
                              ),
                              mainPanel(
                                plotOutput("plot1"),
                                leafletOutput("mymap")
                              )
                            ))
                   ),                 
                 
                   # ----------------------------------
                   # tab panel 3 - Location Comparison
                 tabPanel("Restaurant",
                          tabsetPanel(
                            tabPanel("Restaurant Seating",
                                     fluidPage(
                                       fluidRow(
                                         column(6, selectInput(
                                           'borough', 'Select Borough(s)',
                                           choices = c('Choose Borough' = '',
                                                       'Bronx', 
                                                       'Brooklyn',
                                                       'Manhattan',
                                                       'Queens',
                                                       'Staten Island'), multiple = T)
                                         ), 
                                         column(6, checkboxGroupInput("check_seating", "Check Seating", 
                                                                      choices = c('Sidewalk' = "sidewalk",
                                                                                  'Roadway' = 'roadway',
                                                                                  'Both' = 'both'), 
                                                                      selected = NULL)
                                         ) 
                                       )  # fluidRow end
                                     ),  # fluidPage end
                                     leafletOutput('rs_map', height = "600px")
                            ), # tabPanel end
                            
                            tabPanel("Restaurant Inspection", fluidPage(
                              fluidRow(
                                column(2,
                                       selectInput(
                                         'boro',
                                         'Select A Borough',
                                         choices = c('Bronx',
                                                     'Brooklyn',
                                                     'Manhattan',
                                                     'Queens',
                                                     'Staten Island'), 
                                         selected = 'Bronx', multiple = F),
                                       checkboxInput("check", "Compare each borough", value = FALSE)
                                ), #column end
                                column(10, mainPanel(plotOutput('rs_inspect_plot', 
                                                                width = "1000px", height = "650px")
                                ))
                              ) # fluidRow end
                            ) # fluidPage end
                            )
                          ) # tabsetPanel end
                 ), # tabPanel end
                   
                   # ----------------------------------
                   
                   # tab panel 4 - Location Comparison
                   tabPanel("Safety", fluidPage(
                     
                     # App title ----
                     titlePanel("Safety Information"),
                     
                     # Sidebar layout with input and output definitions ----
                     sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       sidebarPanel(
                         
                         # Input: Select for the borough ----
                         selectInput(inputId = "safety_measure_type",
                                     label = "Choose a safety type of interest:",
                                     choices = c("shooting", "arrest", "use_of_force"))
                       ),
                       
                       # Main panel for displaying outputs ----
                       mainPanel(
                         
                         # Output: tsPlot on borough ----
                         plotOutput(outputId = "safetyMapPlot"),
                         
                         plotOutput(outputId = "safetyTrend"),
                         
                         plotOutput(outputId = "covidTrend")
                         
                       )
                     )
                   )
                   ),
                   
                   # ----------------------------------
                   # tab panel 5 - About
                   tabPanel("AboutUS",
                            includeHTML("about.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                                tags$link(rel = "stylesheet", 
                                          type = "text/css", 
                                          href = "plugins/carousel.css"),
                                tags$script(src = "plugins/holder.js")
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            )
                   )
                   
)