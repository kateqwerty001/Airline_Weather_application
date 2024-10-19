library(readxl)
library(data.table)
library(dplyr)


data_2001 <-read.csv(bzfile("./data/2001.csv.bz2"))
data_2002 <-read.csv(bzfile("./data/2002.csv.bz2"))
data_1998 <-read.csv(bzfile("./data/1998.csv.bz2"))
data_1995 <-read.csv(bzfile("./data/1995.csv.bz2"))
data_2007 <-read.csv(bzfile("./data/2007.csv.bz2"))
data_1988 <-read.csv(bzfile("./data/1988.csv.bz2"))
data_1989 <-read.csv(bzfile("./data/1989.csv.bz2"))

data <- rbind(data_1988[c(17,18)],data_1989[c(17,18)],data_1995[c(17,18)],
              data_1998[c(17,18)],data_2001[c(17,18)],data_2002[c(17,18)],
              data_2007[c(17,18)])

flights <- data %>%
  group_by(Origin, Dest) %>%
  summarise(Count = n())


airports <- read.csv("./airports.csv")

latlong<- airports[c(1,6,7)]
df<- merge(flights,latlong, by.x="Origin", by.y="iata" )
setnames(df, c("lat","long"), c("Origin_Lat","Origin_Lon"))


df<- merge(df,latlong, by.x="Dest", by.y="iata" )
setnames(df, c("Dest","lat","long"), c("Destination","Dest_Lat","Dest_Lon"))

df <- df[order(df$Count, decreasing = TRUE),]
most_freq_flights_100<- head(df[c(1,2,4,5,6,7)], 100)

