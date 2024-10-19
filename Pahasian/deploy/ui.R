library(shiny)
library(leaflet)
library(ggplot2)

ui <- fluidPage(
  includeCSS("styles.css"),
  
  titlePanel("Data Expo 2009"),
  tags$div(
    tags$b("Użycie Samolotów:"), " przedstawia wykorzystanie różnych modeli samolotów w ciągu lat.
           Funkcjonalność umożliwia analizowanie trendów w podróżach lotniczych, pozwala monitorować popularność 
           i wydajność poszczególnych modeli samolotów.",
    style = "font-size: 18px;"
  ),
  tags$div(
    tags$b("Najczęstsze Loty:"), " prezentuje informacje o najczęściej wykonywanych trasach lotniczych 
    (1988, 1989, 1995, 1998, 2001, 2002, 2007). Daje możliwość analizy popularności i częstotliwości 
    lotów między lokalizacjami. ",
    style = "font-size: 18px;"
  ),
  fluidRow(
    column(width = 12,
           tabsetPanel(
             tabPanel("Użycie Samolotów",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("model", "Model:", choices = c("Boeing", "Embraer", "Airbus Industrie", "Bombardier Inc")),
                          selectInput("year", "Rok:", choices = c("2003", "2005", "2007"))
                        ),
                        mainPanel(
                          plotOutput("lineplot")
                        )
                      )
             )),
           tabPanel("Najczęstsze Loty",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput(
                          inputId = "num",
                          label = "Liczba Lotów",
                          min = 1,
                          max = 100,
                          value = 20
                        )
                      ),
                      mainPanel(
                        leafletOutput(outputId = "map")
                      )
                    )
           )
    )
  ),
  actionButton("ButtonPrevious", "Previous", class="ButtonPrevious", onclick ="window.location.href='https://glebbadzeika.shinyapps.io/apppy/'"),
  actionButton("ButtonNext", "Next", class="ButtonNext", onclick ="window.location.href='https://ykbluy-kate-qwerty.shinyapps.io/WeatherApp/'")
)
