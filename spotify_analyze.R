library(dplyr)
library(tidyverse)
library(tidyr)
library(janitor)
library(ggplot2)
library(lubridate)

#install rjson untuk membaca file json
install.packages("rjson")
library(rjson)

#membaca file json dan menyimpan ke data result
result <- fromJSON(file= "MyData/StreamingHistory0.json")
#data berbentuk list
str(result)
#mengubah list menjadi data frame
history <- do.call(rbind.data.frame, result)
#melihat data apakah berubah
str(history)

result1 <- fromJSON(file= "MyData/StreamingHistory1.json")
history1 <- do.call(rbind.data.frame, result1)

result2 <- fromJSON(file= "MyData/StreamingHistory2.json")
history2 <- do.call(rbind.data.frame, result2)

#penggabungan 3 data frame  history menjadi satu data frame
streaming_history <- bind_rows(history, history1, history2)
str(streaming_history)
streaming_history <- clean_names(streaming_history)

#mendifinisikan kolom sebagai date time
streaming_history$end_time <- ymd_hm(streaming_history$end_time)

#quick analyze
summary(streaming_history)

#top 10 listened by track name
streaming_history %>% group_by(track_name) %>% 
  summarise(total_listened = n()) %>% 
  arrange(desc(total_listened)) %>% 
  slice(1:10)

#top 10 listened by artist and total played as a second and converted to hour or day
streaming_history %>% group_by(artist_name) %>% 
  summarise(total_listened = n(),
            duration = dmilliseconds(sum(ms_played))) %>% 
  arrange(desc(total_listened)) %>% 
  slice(1:10)


#total lagu yang didengarkan berdasarkan jam
streaming_history %>% mutate(hour = hour(end_time)) %>% 
  group_by(hour) %>% 
  summarise(total_listened = n()) %>%
  ggplot(aes(factor(hour), total_listened))+
  geom_bar(stat='identity', fill='orange')+
  geom_text(aes(label=total_listened),hjust=0.5, size=3, angle = 90, )+
  xlab("hour")

streaming_history %>% 
  mutate(month = factor(months(end_time), levels=month.name)) %>%
  group_by(month) %>%
  summarise(total_listened = n()) %>%
  arrange(month) %>% 
  ggplot(aes(month, total_listened, fill =month))+
  geom_bar(stat = 'identity')+
  xlab("Month")

streaming_history %>% 
  mutate(day_of_week = factor(weekdays(end_time), levels=c("Sunday", "Monday", "Tuesday","Wednesday", "Thursday","Friday", "Saturday"))) %>%
  group_by(day_of_week) %>%
  summarise(total_listened = n()) %>%
  arrange(day_of_week) %>% 
  ggplot(aes(day_of_week, total_listened, fill = day_of_week))+
  geom_bar(stat = 'identity')+
  xlab("Day of Week")+ ylab("Total Listened")


