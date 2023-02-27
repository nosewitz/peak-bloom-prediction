library(tidyverse)
library(rnoaa)
source("hours_below.R")

chill.wash <- ghcnd_search(stationid = wash_ids[1],
                           #refresh = TRUE,
                           var = c("tmax", "tmin"))
chill.wash.join <-
  left_join(wash.temp1$tmax, wash.temp1$tmin, by = c("id", "date")) %>% 
  filter(!is.na(tmax) & !is.na(tmin)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  mutate(year = parse_number(format(date, "%Y"))) %>% 
  group_by(year, month) %>% 
  nest() %>% 
  mutate(row_num = map(data, nrow)) %>% 
  unnest(row_num) %>% 
  filter(row_num == 31, month == 12 | month == 1)
#nrow(chill.wash.join)

chill.wash.join$data[[1]][[2]]
hours_below(chill.wash.join$data[[1]]$tmin, chill.wash.join$data[[1]]$tmax)

chill.wash.join %>% 
  mutate(chill_sum = map(data, hours_below2)) %>% 
  unnest(chill_sum)

hours_below2(chill.wash.join)

x<- chill.wash.join$data[[1]] %>% 
  mutate(daily_hours_below = -1, 
         daily_hours_below = ifelse(tmax <= 45, 24, daily_hours_below), 
         daily_hours_below = ifelse(tmin >= 45, 0, daily_hours_below), 
         daily_hours_below = ifelse(
           daily_hours_below < 0, 
           ((pi - acos((45 - (tmax + tmin)/2)/(tmax - (tmax + tmin)/2)))/pi)*24, 
           daily_hours_below)) %>% 
  summarise(cum_hours_below = sum(daily_hours_below))


acos((45 - mean(c(44,106)))/(106 - mean(c(44, 106))))
acos((45 - (tmax + tmin)/2)/(tmax - (tmax + tmin)/2))
((pi - acos((45 - (tmax + tmin)/2)/(tmax - (tmax + tmin)/2)))/pi)*24
tmax <- 56
tmin<- -6
chill.wash.join$data[[1]]$tmax %>% typeof()
2.272031/3.14

pi
