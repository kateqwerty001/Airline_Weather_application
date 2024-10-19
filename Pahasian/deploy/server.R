server <- function(input, output) {
  tails_2003_b300<- read.csv("./tails_2003_b300.csv")
  tails_2005_b300<- read.csv("./tails_2005_b300.csv")
  tails_2007_b300<- read.csv("./tails_2005_b300.csv")
  
  tails_2003_e<- read.csv("./tails_2003_e.csv")
  tails_2005_e300<- read.csv("./tails_2005_e300.csv")
  tails_2007_e300<- read.csv("./tails_2007_e300.csv")
  
  tails_2003_ai300<- read.csv("./tails_2003_ai300.csv")
  tails_2005_ai300<- read.csv("./tails_2005_ai300.csv")
  tails_2007_ai300<- read.csv("./tails_2007_ai300.csv")
  tails_2003_bi<- read.csv("./tails_2003_bi.csv")
  tails_2005_bi300<- read.csv("./tails_2005_bi300.csv")
  tails_2007_bi300<- read.csv("./tails_2007_bi300.csv")
  
  us_flights<- read.csv("./most_freq_flights_100.csv")

  filterData <- reactive({
    model <- input$model
    year <- input$year
    
    if (model == "Boeing") {
      filtered_data <- switch(year,
                              "2003" = tails_2003_b300,
                              "2005" = tails_2005_b300,
                              "2007" = tails_2007_b300)
    } else if (model == "Embraer") {
      filtered_data <- switch(year,
                              "2003" = tails_2003_e,
                              "2005" = tails_2005_e300,
                              "2007" = tails_2007_e300)
    } else if (model == "Airbus Industrie") {
      filtered_data <- switch(year,
                              "2003" = tails_2003_ai300,
                              "2005" = tails_2005_ai300,
                              "2007" = tails_2007_ai300)
    } else if (model == "Bombardier Inc") {
      filtered_data <- switch(year,
                              "2003" = tails_2003_bi,
                              "2005" = tails_2005_bi300,
                              "2007" = tails_2007_bi300)
    }
    
    filtered_data
  })

  output$lineplot <- renderPlot({
    filtered_data <- filterData()
    
    ggplot(filtered_data, aes(x = seq_along(tailnum), y = Count, group = 1)) +
      geom_line(size = 0.2) +
      annotate("text", x = max(seq_along(filtered_data$tailnum)) - 0.4,
               y = mean(filtered_data$Count) - 50,
               label = paste("", round(mean(filtered_data$Count), 0)),
               hjust = -0.2, vjust = 0.5, color = "red") +
      geom_hline(yintercept = mean(filtered_data$Count), linetype = "dashed", color = "red") +
      labs(x = "Numer Samolotu", y = "Liczba Lotów",
           title = input$model) +
      scale_x_continuous(labels = NULL)
  })
  

  output$map <- renderLeaflet({
    filtered_flights <- head(us_flights, n = input$num)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95.7, lat = 37.0, zoom = 5) %>%
      addPolylines(
        data = filtered_flights,
        lng = ~c(Origin_Lon, Dest_Lon),
        lat = ~c(Origin_Lat, Dest_Lat),
        color = "red",
        weight = 2
      )
  })
}
