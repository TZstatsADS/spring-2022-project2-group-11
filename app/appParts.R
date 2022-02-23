
source("server.R")
#   ____________________________________________________________________________
#   Neighborhood Browser                                                    ####

neighborhoodDescription <- function() {
    tagList(
        div(class = "container",
            h1("Neighborhood Browser", class = "title fit-h1"),
            p("You are new to New York City or real estate investment? Use Intelligentsia's neighborhood browser to identify pockets of opportunity in the city."),
            p("Use the map to browse New York City's gentrifying census tracts. Click on any one of them to get more detailed information. Use the slider to show only the top k tracts."),
            fluidRow(
                column(7,
                       sliderInput("topK","Show top k census tracts",
                                   10, 0, 50, 10, width = "100%"),
                       leafletOutput("map", height = 600)
                       ),
                hidden(column(5, class = "hood-info", id = "reactiveOutput1",
                       h1(textOutput("hood"), class = "heading"),
                       htmlOutput("hoodInfo")
                       ))
            ),
            hidden(
                div(class = "kpi-group",
                    fluidRow(style = "margin-top: 10px;",
                                    id = "reactiveOutput2a",
                             column(3,
                                    div(plotlyOutput("donut", height = "100%"), align = "center"),
                                    h3("Intelligentsia Score", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi1"), class = "kpi"),
                                    h3("Median Home Value", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi2"), class = "kpi"),
                                    h3("Median Housing Cost", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi3"), class = "kpi"),
                                    h3("Year built (median)", class = "kpi-name")
                             )
                    ),
                    fluidRow(id = "reactiveOutput2b",
                             column(3,
                                    h2(textOutput("kpi4"), class = "kpi"),
                                    HTML("<h3 class='kpi-name'>Avg. Yelp Rating<sup>1</sup></h3>")
                             ),
                             column(3,
                                    h2(textOutput("kpi5"), class = "kpi"),
                                    HTML("<h3 class='kpi-name'>Walkability Index<sup>2</sup></h3>")
                             ),
                             column(3,
                                    h2(textOutput("kpi6"), class = "kpi"),
                                    HTML("<h3 class='kpi-name'># of Trees<sup>3</sup></h3>")
                             ),
                             column(3,
                                    h2(textOutput("kpi7"), class = "kpi"),
                                    HTML("<h3 class='kpi-name'>% Share of Taxicab Rides at Night<sup>4</sup></h3>")
                             )
                    ),
                    fluidRow(id = "reactiveOutput2c",
                             column(3,
                                    h2(textOutput("kpi8"), class = "kpi"),
                                    h3("% College Education", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi9"), class = "kpi"),
                                    h3("Median Age", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi10"), class = "kpi"),
                                    h3("% Family Households", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi11"), class = "kpi"),
                                    h3("% Crime", class = "kpi-name")
                             )
                    ),
                    fluidRow(column(12,tags$small("Sources: U.S. Census Bureau, 1. Yelp, 2. BEH, 3. NYC Open Data, 4. NYC TLC")))
                ),
                hr(),
                fluidRow(id = "reactiveOutput3",
                    column(12,
                           h2("Development of real estate prices"),
                           div(plotlyOutput("zillow1", width = "100%"), align = "center", 
                               class = "chart", width = "100%")
                           )
                ),
                hr(),
                fluidRow(id = "reactiveOutput4",
                         column(12,
                                h2("Yelp reviews over time"),
                                div(plotlyOutput("yelp", height = "420px"), align = "center",
                                    class = "chart")
                                )
                ),
                hr(),
                fluidRow(id = "reactiveOutput4a1",
                         column(12,h2("Online Activities"))),
                fluidRow(id = "reactiveOutput4a",
                         column(6,
                                h3("Google Search Trends"),
                                div(plotlyOutput("google"), align = "center",
                                    class = "chart")
                                ),
                         column(6,
                                h3("Wikipedia Edits"),
                                div(plotlyOutput("wiki"), align = "center",
                                    class = "chart")
                         )
                ),
                hr(),
                fluidRow(id = "reactiveOutput5",
                         column(12,
                                h2("Taxi trips over time"),
                                div(plotlyOutput("taxi"), align = "center", 
                                    class = "chart")
                                )
                ),
                hr(),
                fluidRow(id = "reactiveOutput6",
                         column(12,
                                h2("Properties currently for sale on Zillow"),
                                div(htmlOutput("propertiesForSale"),
                                    class = "property-card-container"))
                         )
            )
        )
    )
}


#   ____________________________________________________________________________
#   Location Comparison                                                     ####

legend <- "<div class='legend-custom'>
               <div class='legend-group'>
                   <div class='legend-element-label' color='location'></div>
                   <div class='legend-element-name'>Property</div>
               </div>
               <div class='legend-group'>
                   <div class='legend-element-label' color='yelp'></div>
                   <div class='legend-element-name'>Yelp</div>
               </div>
               <div class='legend-group'>
                   <div class='legend-element-label' color='schools'></div>
                   <div class='legend-element-name'>Schools</div>
               </div>
           </div>"


#   ____________________________________________________________________________
#   Safety Page                                                             ####
safetyPage <- function() {
  fluidPage(
    
    # App title ----
    titlePanel("Safety Information"),
    
    
    tabsetPanel(
      tabPanel("Precinct Safety Status",
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
          
          # Sidebar panel for inputs ----
          sidebarPanel(
            
            # Input: Select for the borough ----
            selectInput(inputId = "safety_measure_type",
                        label = "Choose a safety type of interest:",
                        choices = c("shooting", "arrest", "use_of_force")),
            
            selectInput(inputId = "safety_police_precinct",
                        label = "Choose a precinct to view details:",
                        choices = all_precinct_ids_with_all)
          ),
          
          # Main panel for displaying outputs ----
          mainPanel(
            textOutput(outputId = "safety_summary"),
            # Output: tsPlot on borough ----
            plotOutput(outputId = "safetyMapPlot"),
            
            plotOutput(outputId = "safetyTrend"),
            
            plotOutput(outputId = "covidTrend")
            
          )
        )
      ),
      
      tabPanel("Precinct Safety Comparison",
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
         
         # Sidebar panel for inputs ----
         sidebarPanel(
           
           # Input: Select for the borough ----
           selectInput(inputId = "safety_precinct_1",
                       label = "Choose the first precinct to compare:",
                       choices = all_precinct_ids),
           
           selectInput(inputId = "safety_precinct_2",
                       label = "Choose the second precinct to compare:",
                       choices = all_precinct_ids)
         ),
         
         # Main panel for displaying outputs ----
         mainPanel(
           textOutput(outputId = "safety_comparison_summary"),
           plotOutput(outputId = "safety_pies"),
           # Output: tsPlot on borough ----
           plotOutput(outputId = "safety_shooting_comparison"),
           
           plotOutput(outputId = "safety_arrest_comparison"),
           
           plotOutput(outputId = "safety_force_comparison")
           
         )
        )
      )
    )
  )
}




googleKey <- "AIzaSyCm-rFyJg0_QZ-EoCFe9ah78e46PSYcypY"
register_google(key = googleKey) #register for ggmap
nyc_mortality = read_csv("data/cleaned_nyc_data.csv")
#   ____________________________________________________________________________
#   Safety Page                                                             ####
astheticPage <- function() {
  fluidPage(
    
    # Application title
    titlePanel("What to be afraid of in NYC"),
    
    # Sidebar with a 3 inputs 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "year",
                    label = "Select Year:",
                    choices = c("2007",
                                "2008",
                                "2009",
                                "2010",
                                "2011",
                                "2012",
                                "2013",
                                "2014")),
        radioButtons(inputId = "sex",
                     label = "Sex:",
                     choices = c(
                       "Female" = "F",
                       "Male" = "M"
                     )),
        radioButtons(inputId = "race",
                     label = "Race/Ethnicity:",
                     choices = unique(nyc_mortality$race_ethnicity))
      ),
      
      # Show plot and table
      mainPanel(
        plotOutput("deathPlot"),
        DT::dataTableOutput("deathTable")
      )
    )
  )

}