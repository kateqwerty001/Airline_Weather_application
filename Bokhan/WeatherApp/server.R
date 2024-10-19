library("shiny")

function(input, output, session) {
  library("usmap")
  library("shiny")
  library("dplyr")
  dataAllTypes <- as.data.frame(read.csv("data/dataAllTypes.csv"))
  dataBalloon <- as.data.frame(read.csv("data/dataBalloon.csv"))
  dataSingleEngine <- as.data.frame(read.csv("data/dataSingleEngine.csv"))
  dataMultiEngine <- as.data.frame(read.csv("data/dataMultiEngine.csv"))
  flights_cancelled_because_of_weather_monthly <- as.data.frame(read.csv("data/cancelledEachMonthWeather.csv"))
  number_of_cancelled_per_state <-as.data.frame(read.csv("data/numberOfCancelledPerState.csv"))
  disasters_autumn <-as.data.frame(read.csv("data/disastersAutumn.csv"))
  disasters_summer <-as.data.frame(read.csv("data/disastersSummer.csv"))
  disasters_winter <-as.data.frame(read.csv("data/disastersWinter.csv"))
  disasters_spring <-as.data.frame(read.csv("data/disastersSpring.csv"))
  mean_dep_delay_per_flight <- as.data.frame(read.csv("data/depDelay.csv"))
  mean_arr_delay_per_flight <- as.data.frame(read.csv("data/arrDelay.csv"))
  
  DataPlot1 <- reactive({
    if (input$aircraft_type == "Balloons") {
      data<-dataBalloon
    } else if (input$aircraft_type == "Fixed Wing Single-Engine") {
      data<-dataSingleEngine
    } else if (input$aircraft_type == "Fixed Wing Multi-Engine") {
      data <- dataMultiEngine
    } else {
      data <- dataAllTypes
    }
    
    data
  })
  
  DataPlot4 <- reactive({
    if (input$season == "summer") {
      data<-disasters_summer
    } else if (input$season == "winter") {
      data<-disasters_winter
    } else if (input$season == "autumn") {
      data <- disasters_autumn
    } else {
      data <- disasters_spring
    }
    
    data
  })
  
  DataPlot5 <- reactive({
    st = input$state
    flights_cancelled_because_of_disasters <-as.data.frame(read.csv("data/flightsDisasters.csv"))
    data <- flights_cancelled_because_of_disasters%>%
      filter(state==st) %>%
      group_by(incident_type) %>%
      count()
    data
  })
  
  DataPlot6 <-reactive({
    if(input$delay_type == "Departure Delay"){
      data <- mean_dep_delay_per_flight
    }
    else{
      data <- mean_arr_delay_per_flight
    }
    data
  })
  
  output$plot1 <- renderPlotly({
    ggplotly(ggplot(DataPlot1(), aes_string(x = input$weather_parameter, y = "mean_difference")) +
      geom_point(size = 1) +
      geom_smooth(method = "loess") +
      geom_smooth(method = "lm", color = "red")+
      labs(x = input$weather_parameter, y = "Average difference in flight duration"))
  })

  
  output$plot2 <-renderPlotly({
    library("stringr")
    flights_cancelled_because_of_weather_monthly$Month <- str_pad(flights_cancelled_because_of_weather_monthly$Month, width = 2, pad = "0")
    barplot <- ggplot(flights_cancelled_because_of_weather_monthly, aes(x = paste(Year, Month, sep = "/"), y = n)) +
      geom_bar(stat = "identity", fill = "seagreen") +
      labs(x = "Year/Month", y = "Number of cancelled flights") +
      ggtitle("Flights Cancelled Because of Weather Per Month in Years 2004-2007") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(barplot)
  })
  
  output$plot3 <-renderPlotly({
    ggplotly(plot_usmap(data=number_of_cancelled_per_state, value = "Cancelled.B.", labels=TRUE))
  })
  
  output$plot4 <-renderPlotly({
    ggplotly(plot_usmap(data = DataPlot4(), values = "value", labels = TRUE)+
      scale_fill_manual(values = c("Snowstorm"="yellow","Coastal Storm"="red", "Earthquake"="steelblue", "Fire" = "darkorange", "Hurricane"="darkmagenta", "Severe Storm"="darkcyan", "NA"="gainsboro", "Flood"="aquamarine", "Freezing"="dodgerblue", "Severe Ice Storm"="blueviolet", "Severe Storm"="darkkhaki", "Tornado"="firebrick")))
  })

  
  output$plot5 <-renderPlotly({
    ggplotly(ggplot(DataPlot5(), aes(x = incident_type, y = n)) +
               geom_bar(stat = "identity", fill="seagreen") +
               xlab("Incident Type") +
               ylab("Number of Cases") +
               geom_text(aes(label = paste0(round(n / sum(n) * 100), "%"))))
    
  })
  
  output$plot6 <-renderPlotly({
    ggplotly(ggplot(DataPlot6(), aes(x = n, y = mean_delay)) +
      geom_point(size = 1)+
      geom_smooth(method = "loess")+
      labs(x = "Number of flights in 2007", y = "Average Delay During Year 2007"))
  })
}

################################################################################################
#This is how we got our new data frames used in plots with weather conditions
# data about weather was taken from https://www.visualcrossing.com/weather/weather-data-services
#data2005 <- as.data.frame(read.csv("data/2005.csv"))
#data2006 <- as.data.frame(read.csv("data/2006.csv"))
#data2007 <- as.data.frame(read.csv("data/2007.csv"))
#data <- rbind(data2005, data2006, data2007)
#data<-binded_data
#binded_data <- data

#data2005<-NULL
#data2006 <- NULL
#data2007 <- NULL

#we will consider the average values of weather parameters only in the area of California
#airports <- as.data.frame(read.csv("data/airports.csv"))%>%
#  filter(state == "CA")
#planes <- as.data.frame(read.csv("data/plane-data.csv"))

#weather <- as.data.frame(read.csv("data/CaliforniaWeatherExtended.csv"))

#create a new сolumn difference_in_time_flight
#data$difference_in_time_flight <- abs(data$ActualElapsedTime - data$CRSElapsedTime)
#data <- left_join(data, airports, by = c("Origin" = "iata"))
#data <- left_join(data, airports, by = c("Dest" = "iata"))
#data <- left_join(data, planes, by = c("TailNum"= "tailnum"))
#data <- data %>%
#  filter(state.y == "CA" & state.x == "CA" & aircraft_type=="Fixed Wing Multi-Engine") %>%
#  group_by(Year, Month, DayofMonth) %>%
#  summarise(mean_difference = mean(difference_in_time_flight, na.rm = TRUE), .groups = "drop")

#adding three columns DayOfMonth, Month, Year
#weather <- weather %>%
#  mutate(DayofMonth = day(as.Date(datetime)),
#         Month = month(as.Date(datetime)),
#         Year = year(as.Date(datetime)))

#data <- merge(data, weather, by = c('Month', 'Year', 'DayofMonth')) 

#write.csv(data, "dataMultiEngine.csv")





#This is this some other code, which we used to get needed info from original datasets our new data frames used in plots with disasters
# data about disasters was taken from https://www.kaggle.com/datasets/fema/federal-disasters
# 
# library("dplyr")
# library("tidyr")
# library("lubridate")
# library("purrr")
# library("usmap")
# library('ggplot2')
# library("RColorBrewer")
# library("plotly")
# library("stringr")
# 
# 
# #We are going to study only years 2004 - 2007
# 
# #PREPARING DATA
# 
# ######## preparing dataset with disasters
# prepare_df_disasters <- function(){
#   disasters_data <-as.data.frame(read.csv('data/us_disaster_declarations.csv'))
#   disasters_data <- disasters_data %>%
#     filter(fy_declared>2003 & fy_declared<2008)
#   disasters_data <- disasters_data[!duplicated(disasters_data[c("disaster_number", "state")]), ] #delete dublicates
#   disasters_data$start_date <- ymd_hms(disasters_data$incident_begin_date)
#   disasters_data$end_date <- ymd_hms(disasters_data$incident_end_date)
#   disasters_data <- disasters_data %>%
#     mutate(end_date = coalesce(end_date, start_date)) # if the end_date is NA, then we make end_date = start_date, the duration of disaster will be one day then
#   
#   disasters_data <- disasters_data %>%
#     mutate(date = map2(start_date, end_date, seq, by = 'day')) %>%
#     unnest(date) # expanding our dataset disasters. We want every day, when the disaster was in duration to be in its own row
#   
#   ##creating new columns with proper names
#   disasters_data <- disasters_data %>%
#     mutate(DayofMonth = day(as.Date(date, format = "YYYY-MM-DD")),
#            Month = month(as.Date(date, format = "YYYY-MM-DD")),
#            Year = year(as.Date(date, format = "YYYY-MM-DD")))
#   return(disasters_data)
# }
# 
# ######## preaparing dataset with flights
# prepare_df_flights <- function(){
#   airports <- read.csv('data/airports.csv')
#   data2004 <- read.csv('data/2004.csv')
#   data2005 <- read.csv('data/2005.csv')
#   data2006 <- read.csv('data/2006.csv')
#   data2007 <- read.csv('data/2007.csv')
#   
#   #We are going to study only data with CancellationCode == "B" so:
#   data2004 <- as.data.frame(data2004[data2004$CancellationCode=='B',])
#   data2005 <- as.data.frame(data2005[data2005$CancellationCode=='B',])
#   data2006 <- as.data.frame(data2006[data2006$CancellationCode=='B',])
#   data2007 <- as.data.frame(data2007[data2007$CancellationCode=='B',])
#   
#   flights <- rbind(data2004, data2005, data2006, data2007)
#   flights <- unique(flights) #deleting dublicates
#   flights <- left_join(flights, airports, by = c('Origin'='iata') ) # joining with airports
#   
#   return(flights)
# }
# 
# disasters_data <- prepare_df_disasters()
# number_of_days_with_disasters <-nrow(disasters_data)
# flights <- prepare_df_flights()
# number_of_cancelled_flights <- nrow(flights)
# flights_cancelled_because_of_disasters <- merge(flights, disasters_data, by = c('state', 'Month', 'Year', 'DayofMonth'))
# 
# write.csv(flights_cancelled_because_of_disasters, "flightsDisasters")
# 
# ##Starting studying the dataset
# 
# get_barplot_with_number_of_cancelled_because_of_weather_per_month<-function(){
#   flights$Month <- str_pad(flights$Month, width = 2, pad = "0")
#   result<-flights %>%
#     group_by(Year, Month) %>%
#     count() %>%
#     arrange(Year, Month)
#   
#   write.csv(result, "cancelledEachMonthWeather")
#   barplot <- ggplot(result, aes(x = paste(Year, Month, sep = "/"), y = n)) +
#     geom_bar(stat = "identity", fill = "seagreen") +
#     labs(x = "Year/Month", y = "Number of cancelled flights") +
#     ggtitle("Flights Cancelled Because of Weather Per Month in Years 2004-2007") +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   ggplotly(barplot)
# }
# get_barplot_with_number_of_cancelled_because_of_weather_per_month()
# 
# get_usa_map_with_number_of_cancelledB_per_state <-function(){
#   result <- flights %>%
#     group_by(state) %>%
#     count()
#   colnames(result)[2]="Cancelled(B)"
#   
#   write.csv(result, "number_of_cancelled_per_state.csv")
#   ggplotly(plot_usmap(data=result, value = "Cancelled(B)"))
# }
# 
# get_usa_map_with_number_of_cancelledB_per_state()
# 
# get_usa_map_with_most_common_disaster_per_state <-function(){
#   get_disaster <- function(st){
#     result <- disasters_data %>%
#       filter(state == st) %>%
#       group_by(incident_type) %>%
#       count()
#     result <- arrange(result, desc(n))
#     result <- result$incident_type[1]
#     return(result)
#   }
#   state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
#                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
#                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
#                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
#                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
#   result <- sapply(state_abbreviations, get_disaster)
#   result <- data.frame(state = state.abb, value = result)
#   colnames(result)[2] = "Disaster"
#   plot_usmap(data = result, values = "Disaster")+
#     scale_fill_manual(values = c("Coastal Storm"="red", "Earthquake"="steelblue", "Fire" = "darkorange", "Hurricane"="darkmagenta", "Severe Storm"="darkcyan", "NA"="gainsboro", "Flood"="aquamarine", "Freezing"="dodgerblue", "Severe Ice Storm"="blueviolet", "Severe Storm"="darkkhaki", "Tornado"="firebrick"))
# }
# 
# get_usa_map_with_most_common_disaster_per_state()
# 
# get_barplot_with_disasters_and_number_of_cancelled_flights<-function(){
#   result <- flights_cancelled_because_of_disasters%>%
#     #filter(Month ==12 | Month==1 | Month == 2) %>%
#     group_by(incident_type)%>%
#     count()
#   plot <- ggplot(result, aes(x = incident_type, y = n)) +
#     geom_bar(stat = "identity") +
#     scale_y_log10() +
#     coord_cartesian(ylim = c(1, 25000)) +
#     xlab("Incident Type") +
#     ylab("Count (log scale)") +
#     ggtitle("Bar Plot with Logarithmic Scale")
#   print(plot)
# }
# 
# 
# get_barplot_with_disasters_and_number_of_cancelled_flights()
# 
# 
# get_usa_map_with_most_common_disaster_to_cancell_flight_per_state_autumn<-function(){
#   get_disaster <- function(st){
#     result <- flights_cancelled_because_of_disasters %>%
#       filter(state==st & ((Month ==9)|(Month==10)|(Month==11))) %>%
#       group_by(incident_type) %>%
#       count()
#     result <- arrange(result, desc(n))
#     result <- result$incident_type[1]
#     return(result)
#   }
#   state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
#                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
#                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
#                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
#                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
#   result <- sapply(state_abbreviations, get_disaster)
#   result <- data.frame(state = state.abb, value = result)
#   plot_usmap(data = result, values = "value", labels = TRUE)+
#     scale_fill_manual(values = c("Coastal Storm"="red", "Earthquake"="steelblue", "Fire" = "darkorange", "Hurricane"="darkmagenta", "Severe Storm"="darkcyan", "NA"="gainsboro", "Flood"="aquamarine", "Freezing"="dodgerblue", "Severe Ice Storm"="blueviolet", "Severe Storm"="darkkhaki", "Tornado"="firebrick"))
# }
# 
# get_usa_map_with_most_common_disaster_to_cancell_flight_per_state_spring<-function(){
#   get_disaster <- function(st){
#     result <- flights_cancelled_because_of_disasters %>%
#       filter(state==st & ((Month ==3)|(Month==4)|(Month==5))) %>%
#       group_by(incident_type) %>%
#       count()
#     result <- arrange(result, desc(n))
#     result <- result$incident_type[1]
#     return(result)
#   }
#   state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
#                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
#                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
#                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
#                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
#   result <- sapply(state_abbreviations, get_disaster)
#   result <- data.frame(state = state.abb, value = result)
#   
#   plot_usmap(data = result, values = "value", labels = TRUE)+
#     scale_fill_manual(values = c("Coastal Storm"="red", "Earthquake"="steelblue", "Fire" = "darkorange", "Hurricane"="darkmagenta", "Severe Storm"="darkcyan", "NA"="gainsboro", "Flood"="aquamarine", "Freezing"="dodgerblue", "Severe Ice Storm"="blueviolet", "Severe Storm"="darkkhaki", "Tornado"="firebrick"))
# }
# 
# get_usa_map_with_most_common_disaster_to_cancell_flight_per_state_summer<-function(){
#   get_disaster <- function(st){
#     result <- flights_cancelled_because_of_disasters %>%
#       filter(state==st & ((Month ==6)|(Month==7)|(Month==8))) %>%
#       group_by(incident_type) %>%
#       count()
#     result <- arrange(result, desc(n))
#     result <- result$incident_type[1]
#     return(result)
#   }
#   state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
#                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
#                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
#                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
#                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
#   result <- sapply(state_abbreviations, get_disaster)
#   result <- data.frame(state = state.abb, value = result)
#   
#   
#   plot_usmap(data = result, values = "value", labels = TRUE)+
#     scale_fill_manual(values = c("Coastal Storm"="red", "Earthquake"="steelblue", "Fire" = "darkorange", "Hurricane"="darkmagenta", "Severe Storm"="darkcyan", "NA"="gainsboro", "Flood"="aquamarine", "Freezing"="dodgerblue", "Severe Ice Storm"="blueviolet", "Severe Storm"="darkkhaki", "Tornado"="firebrick"))
# }
# get_usa_map_with_most_common_disaster_to_cancell_flight_per_state_winter<-function(){
#   get_disaster <- function(st){
#     result <- flights_cancelled_because_of_disasters %>%
#       filter(state==st & ((Month ==12)|(Month==1)|(Month==2))) %>%
#       group_by(incident_type) %>%
#       count()
#     result <- arrange(result, desc(n))
#     result <- result$incident_type[1]
#     return(result)
#   }
#   state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
#                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
#                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
#                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
#                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
#   result <- sapply(state_abbreviations, get_disaster)
#   result <- data.frame(state = state.abb, value = result)
#   
#   write.csv(result, "disastersWinter.csv")
#   
#   plot_usmap(data = result, values = "value", labels = TRUE)+
#     scale_fill_manual(values = c("Coastal Storm"="red", "Earthquake"="steelblue", "Fire" = "darkorange", "Hurricane"="darkmagenta", "Severe Storm"="darkcyan", "NA"="gainsboro", "Flood"="aquamarine", "Freezing"="dodgerblue", "Severe Ice Storm"="blueviolet", "Severe Storm"="darkkhaki", "Tornado"="firebrick"))
# }
# 
# get_usa_map_with_most_common_disaster_to_cancell_flight_per_state()
# 
# get_usa_map_with_most_common_disaster_to_cancell_flight_per_state_winter()
# 
# 
# 
# 
# ggplotly(ggplot(flights_cancelled_because_of_disasters%>%
#                   filter(state=="TX") %>%
#                   group_by(incident_type) %>%
#                   count(), aes(x = incident_type, y = n)) +
#            geom_bar(stat = "identity", fill="steelblue") +
#            xlab("Incident Type") +
#            ylab("Number of Cases") +
#            geom_text(aes(label = paste0(round(n / sum(n) * 100), "%"), vjust = -3.5, size = 9))
# )
#########################################################################################



