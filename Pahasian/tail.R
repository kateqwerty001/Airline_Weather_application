library(readxl)
library(dplyr)

plane_data <- read.csv("./plane-data.csv")

plane_data_freq<- plane_data%>%group_by(manufacturer) %>%
  summarise(Count = n())%>% top_n(5, Count)%>% filter(manufacturer!="")


data_2003 <-read.csv(bzfile("./data/2003.csv.bz2"))
#boeing
tails_2003 <- data_2003[c(11)] %>%
  group_by(TailNum) %>%
  summarise(Count = n())

plane_boeing <- plane_data[plane_data$manufacturer=="BOEING",]
plane_boeing <- plane_boeing[c(1)]
tails_2003_b <- merge(plane_boeing, tails_2003, by.x="tailnum", by.y="TailNum")
tails_2003_b300<- tails_2003_b %>% sample_n(300, replace = FALSE)

#embrier
plane_embraer <- plane_data[plane_data$manufacturer=="EMBRAER",]
plane_embraer <- plane_embraer[c(1)]
tails_2003_e <- merge(plane_embraer, tails_2003, by.x="tailnum", by.y="TailNum")

#AIRBUS INDUSTRIE
plane_airbus <- plane_data[plane_data$manufacturer=="AIRBUS INDUSTRIE",]
plane_airbus <- plane_airbus[c(1)]
tails_2003_ai <- merge(plane_airbus, tails_2003, by.x="tailnum", by.y="TailNum")
tails_2003_ai300<- tails_2003_ai %>% sample_n(300, replace = FALSE)

#BOMBARDIER INC
plane_bomb <- plane_data[plane_data$manufacturer=="BOMBARDIER INC",]
plane_bomb <- plane_bomb[c(1)]
tails_2003_bi <- merge(plane_bomb, tails_2003, by.x="tailnum", by.y="TailNum")



data_2005 <-read.csv(bzfile("./data/2005.csv.bz2"))
#boeing
tails_2005 <- data_2005[c(11)] %>%
  group_by(TailNum) %>%
  summarise(Count = n())

tails_2005_b <- merge(plane_boeing, tails_2005, by.x="tailnum", by.y="TailNum")
tails_2005_b300<- tails_2005_b %>% sample_n(300, replace = FALSE)

#embrier
tails_2005_e <- merge(plane_embraer, tails_2005, by.x="tailnum", by.y="TailNum")
tails_2005_e300<- tails_2005_e %>% sample_n(300, replace = FALSE)

#AIRBUS INDUSTRIE
tails_2005_ai <- merge(plane_airbus, tails_2005, by.x="tailnum", by.y="TailNum")
tails_2005_ai300<- tails_2005_ai %>% sample_n(300, replace = FALSE)

#BOMBARDIER INC
tails_2005_bi <- merge(plane_bomb, tails_2005, by.x="tailnum", by.y="TailNum")
tails_2005_bi300<- tails_2005_bi %>% sample_n(300, replace = FALSE)



data_2007 <-read.csv(bzfile("./data/2007.csv.bz2"))
#boeing
tails_2007 <- data_2007[c(11)] %>%
  group_by(TailNum) %>%
  summarise(Count = n())

tails_2007_b <- merge(plane_boeing, tails_2007, by.x="tailnum", by.y="TailNum")
tails_2007_b300<- tails_2007_b %>% sample_n(300, replace = FALSE)

#embrier
tails_2007_e <- merge(plane_embraer, tails_2007, by.x="tailnum", by.y="TailNum")
tails_2007_e300<- tails_2007_e %>% sample_n(300, replace = FALSE)

#AIRBUS INDUSTRIE
tails_2007_ai <- merge(plane_airbus, tails_2007, by.x="tailnum", by.y="TailNum")
tails_2007_ai300<- tails_2007_ai %>% sample_n(300, replace = FALSE)

#BOMBARDIER INC
tails_2007_bi <- merge(plane_bomb, tails_2007, by.x="tailnum", by.y="TailNum")
tails_2007_bi300<- tails_2007_bi %>% sample_n(300, replace = FALSE)
