#   ____________________________________________________________________________
#   UI                                                                      ####

library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)

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
                   # tab panel 2 - Neighborhood Browser
                   tabPanel("Dumbo",
                            neighborhoodDescription(),
                            includeHTML("scrollToTop.html")
                   ),
                   
                   # ----------------------------------
                   # tab panel 3 - Location Comparison
                   tabPanel("NEIGHBORHOOD2",
                            fluidPage()
                   ),
                   
                   # ----------------------------------
                   # tab panel 3 - Location Comparison
                   tabPanel("NEIGHBORHOOD3",
                            fluidPage()
                   ),
                   
                   # ----------------------------------
                   
                   # tab panel 4 - Location Comparison
                   tabPanel("NEIGHBORHOOD4",
                            fluidPage()
                   ),
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