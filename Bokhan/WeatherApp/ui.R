library("shiny")
library("plotly")
library("shinyjs")

fluidPage(
  includeCSS("styles.css"),
  
  titlePanel("Difference in flight duration vs. Weather Parameters (2005-2007)"),
  p("We have taken data with average weather parameters for each day (in years 2005-2007) on the territory of California from https://www.visualcrossing.com/weather/weather-data-services. We also took data about flights, but only those flights that had the entire flight occurring on the territory of California. We studied the correlations between weather parameters and the difference between the planned flight time and the actual flight time."),
  sidebarLayout(
    sidebarPanel(
      selectInput("weather_parameter", "Select weather parameter :", choices = c(
        "precipcover", "sealevelpressure", "icon", "humidity",
        "cloudcover", "moonphase", "windspeed", "temp", "visibility", "tempmax", "tempmin", "feelslike", "dew"
      )),
      selectInput("aircraft_type", "Select aircraft type :", choices = c(
        "All Types", "Balloons", "Fixed Wing Multi-Engine", "Fixed Wing Single-Engine"
      ))
    ),
    mainPanel(
      plotlyOutput("plot1"),
    )
  ),
  h4("Result"),
  p("As we can see, there is a correlation between Difference In Flight Duration and precipcover, sealevelpreassure, humidity, cloudcover, temperature, visibility. But absolutely no correlation between Difference In Flight Duration and moonphase."),
  titlePanel("Average Delay vs Number of flights from airport A to airport B (2007)"),
  p("Here we have studied the question \"Does depDelay (arrDelay) depend on the frequency of flights in a given direction?\" "),
  sidebarLayout(
    sidebarPanel(
      selectInput("delay_type", "Select type of delay:", choices = c(
        "Departure Delay", "Arrival Delay")),
    ),
    mainPanel(
      plotlyOutput("plot6"),
    )
  ), 
  titlePanel("Flights Cancelled Because Of Disasters (2004-2007)"),
  p("It was interesting for us to know which weather disasters most often cause flight cancellations, how it depends on the state (territory), the season. We have taken data about disasters on the territory of the USA from https://www.kaggle.com/datasets/fema/federal-disasters ."),
  sidebarLayout(
    sidebarPanel(
      p("Barplot representing the number of cancelled flights because of weather per each month in years 2004-2007")
    ),
    mainPanel(
      plotlyOutput("plot2"),
    )
  ), 
  sidebarLayout(
    sidebarPanel(
      p("U.S. map representing the number of cancelled flights because of weather per state in years 2004-2007")
    ),
    mainPanel(
      plotlyOutput("plot3"),
    )
  ), 
  sidebarLayout(
    sidebarPanel(
      p("U.S. map representing the type of disaster that caused the biggest number of flight cancellations (per state for the selected season)"),
      selectInput("season", "Select season:", choices = c(
        "winter", "spring", "summer","autumn"
      )),
    ),
    mainPanel(
      plotlyOutput("plot4"),
    )
  ), 
  
  sidebarLayout(
    sidebarPanel(
      p("Barplot representing the type of disaster and the number of flights cancelled due to this type of disaster (on the territory of the selected state)"),
      selectInput("state", "Select state:", choices = c("CA", "AK", "AZ", "AR", "AL", "CO", "CT", "DE", "FL", "GA",
                                                        "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                                                        "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                                                        "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                                                        "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")),
          ),
    mainPanel(
      plotlyOutput("plot5"),
    )
  ), 
  
  actionButton("ButtonPrevious", "Previous", class="ButtonPrevious", onclick ="window.location.href='https://milanna.shinyapps.io/deploy/'"),
  actionButton("ButtonNext", "Next", class="ButtonNext", onclick ="window.location.href='https://glebbadzeika.shinyapps.io/apppy/'")
)