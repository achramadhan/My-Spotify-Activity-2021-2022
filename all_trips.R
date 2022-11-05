library(lubridate)

library(tidyverse)
library(ggplot2)
library(dplyr)
all_trips <- read.csv("all_trips.csv")

all_trips <- all_trips %>%  select(-c(start_lat, start_lng, end_lat, end_lng))
all_trips <- all_trips[!(all_trips$ride_length<0),]


summary(all_trips$ride_length)

aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday",
                                                                       "Wednesday", "Thursday", 
                                                                       "Friday", "Saturday"))


all_trips_day <- all_trips %>%  
  mutate (weekday= day_of_week, label=TRUE) %>% 
  group_by(weekday, member_casual) %>% 
  summarise(number_of_ride = n(),
            average_duration = mean(ride_length),
            max_duration = max(ride_length)) %>% 
  arrange(weekday) %>% 
  ggplot(aes(weekday, average_duration, fill=member_casual))+
  geom_col(position = "dodge")+
  geom_text(aes(label=average_duration),position= position_dodge(0.9), vjust = -0.5, size = 2.5, angle = 80)
write.csv(all_trips_day, file = 'all_trips_day.csv')


all_trips_month <- all_trips %>%  
  mutate (mont = month, label=TRUE) %>% 
  group_by(mont, member_casual) %>% 
  summarise(number_of_ride = n(),
            average_duration = mean(ride_length),
            max_ride = max(ride_length)) %>% 
  arrange(mont) %>% 
  ggplot(aes(mont, average_duration, fill=member_casual))+
  geom_col(position = "dodge")+
  geom_text(aes(label=average_duration),position= position_dodge(0.9), vjust = -0.5, size = 2.5, angle = 80)

write.csv(all_trips_month, file = "all_trips_month.csv")

all_trips %>%  
  mutate (mont = month, label=TRUE) %>% 
  group_by(mont, member_casual, rideable_type) %>% 
  summarise(number_of_ride = n(),
            average_duration = mean(ride_length),
            max_ride = max(ride_length)) %>% 
  arrange(mont) 