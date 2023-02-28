# Code creates variables:
# wash_chill_hours
# kyoto_chill_hours
# lies_chill_hours
# vanc_chill_hours

library(tidyverse)
library(rnoaa)
source("hours_below.R")

# function takes df and calculates total chill hours for each year
get_total <- function(df) {
  x <- df
  x$total_hours <- NA
  for (i in 2:nrow(x)) {
    if (x$month[i] == 1) {
      current_year <- x$year[i]
      prev_year <- x$year[i - 1]
      dec_index <- which(x$month == 12 & x$year == prev_year)
      if (length(dec_index) > 0) {
        x$total_hours[i] <- x$cum_hours_below[i] + x$cum_hours_below[dec_index]
      }
    }
  }
  return(x)
}

# Washington DC ####

chill.wash <- ghcnd_search(stationid = "USW00013743",
                           #refresh = TRUE,
                           var = c("tmax", "tmin"))
chill.wash.join <-
  left_join(chill.wash$tmax, chill.wash$tmin, by = c("id", "date")) %>% 
  filter(!is.na(tmax) & !is.na(tmin)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  mutate(year = parse_number(format(date, "%Y"))) %>% 
  group_by(year, month) %>% 
  nest() %>% 
  mutate(row_num = map(data, nrow)) %>% 
  unnest(row_num) %>% 
  filter(row_num == 31, month == 12 | month == 1) %>% 
  mutate(chill_sum = map(data, hours_below)) %>% 
  unnest(chill_sum)
#nrow(chill.wash.join)
wash_chill_hours <- get_total(chill.wash.join) %>% 
  ungroup() %>% 
  filter(month == 1) %>% 
  select(year, data, chill_hours = total_hours)

# Kyoto ####

chill.kyoto <- ghcnd_search(stationid = "JA000047759",
                           #refresh = TRUE,
                           var = c("tmax", "tmin"))
chill.kyoto.join <-
  left_join(chill.kyoto$tmax, chill.kyoto$tmin, by = c("id", "date")) %>% 
  filter(!is.na(tmax) & !is.na(tmin)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  mutate(year = parse_number(format(date, "%Y"))) %>% 
  group_by(year, month) %>% 
  nest() %>% 
  mutate(row_num = map(data, nrow)) %>% 
  unnest(row_num) %>% 
  filter(row_num == 31, month == 12 | month == 1) %>% 
  mutate(chill_sum = map(data, hours_below)) %>% 
  unnest(chill_sum)
#nrow(chill.wash.join)
kyoto_chill_hours <- get_total(chill.kyoto.join) %>% 
  ungroup() %>% 
  filter(month == 1) %>% 
  select(year, data, chill_hours = total_hours)


# Liestal-Weideli (Switzerland) ####

chill.lies <- ghcnd_search(stationid = "SZ000001940",
                           #refresh = TRUE,
                           var = c("tmax", "tmin"))
chill.lies.join <-
  left_join(chill.lies$tmax, chill.lies$tmin, by = c("id", "date")) %>% 
  filter(!is.na(tmax) & !is.na(tmin)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  mutate(year = parse_number(format(date, "%Y"))) %>% 
  group_by(year, month) %>% 
  nest() %>% 
  mutate(row_num = map(data, nrow)) %>% 
  unnest(row_num) %>% 
  filter(row_num == 31, month == 12 | month == 1) %>% 
  mutate(chill_sum = map(data, hours_below)) %>% 
  unnest(chill_sum)
#nrow(chill.wash.join)
lies_chill_hours <- get_total(chill.lies.join) %>% 
  ungroup() %>% 
  filter(month == 1) %>% 
  select(year, data, chill_hours = total_hours)


# Vancouver ####

chill.vanc <- ghcnd_search(stationid = "CA001108447",
                           #refresh = TRUE,
                           var = c("tmax", "tmin"))
chill.vanc.join <-
  left_join(chill.vanc$tmax, chill.vanc$tmin, by = c("id", "date")) %>% 
  filter(!is.na(tmax) & !is.na(tmin)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  mutate(year = parse_number(format(date, "%Y"))) %>% 
  group_by(year, month) %>% 
  nest() %>% 
  mutate(row_num = map(data, nrow)) %>% 
  unnest(row_num) %>% 
  filter(row_num == 31, month == 12 | month == 1) %>% 
  mutate(chill_sum = map(data, hours_below)) %>% 
  unnest(chill_sum)
#nrow(chill.wash.join)
vanc_chill_hours <- get_total(chill.vanc.join) %>% 
  ungroup() %>% 
  filter(month == 1) %>% 
  select(year, data, chill_hours = total_hours)

### Join all 
chills <-
tibble(location = "washingtondc", wash_chill_hours ) |>
bind_rows(tibble(location = "liestal", lies_chill_hours )) |>
bind_rows(tibble(location = "kyoto", kyoto_chill_hours )) |>
bind_rows(tibble(location = "vancouver", vanc_chill_hours) )

# junk ####

x <- chill.lies.join
x$total_hours <- c(0, diff(x$year))
x$total_hours[x$month == 1] <- 
  ifelse(x$total_hours[x$month == 1] == 1, 
         x$cum_hours_below[x$total_hours == 1 & x$month == 1] + 
           x$cum_hours_below[], 
         x$cum_hours_below[x$month == 1])
x$total_hours[x$month == 12] <- NA


x<- chill.lies.join[2:length(row.names(chill.lies.join)), ]
x$total_hours <- rep(x$cum_hours_below[c(TRUE, FALSE)] + x$cum_hours_below[c(FALSE, TRUE)], each = 2)
x$total_hours


x <- chill.lies.join
x <- x %>% mutate(adj_year = ifelse(month == 12, year + 1, year))
x$total_hours <- c(0, diff(x$year))

x.wide <-  x %>% pivot_wider(names_from = month, values_from = cum_hours_below)

# x <- chill.lies.join
# x$total_hours[x$month == 1] <- 
#   ifelse(x$total_hours[x$month == 1] == 1, 
#          x$cum_hours_below[x$total_hours == 1 & x$month == 1] + 
#            , 
#          x$cum_hours_below[x$month == 1])
# x$total_hours[x$month == 12] <- NA

x <- chill.lies.join
x$total_hours <- NA
for (i in 2:nrow(x)) {
  if (x$month[i] == 1) {
    current_year <- x$year[i]
    prev_year <- x$year[i - 1]
    dec_index <- which(x$month == 12 & x$year == prev_year)
    if (length(dec_index) > 0) {
      x$total_hours[i] <- x$cum_hours_below[i] + x$cum_hours_below[dec_index]
    }
  }
}


# x <- df
# x$total_hours <- c(0, diff(x$year))
# x$total_hours[x$month == 1] <- 
#   ifelse(x$total_hours[x$month == 1] == 1, 
#          x$cum_hours_below[x$total_hours == 1] + 
#            x$cum_hours_below[x$month == 12 & x$year[x$month == 12] %in% 
#                                (x$year[x$month == 1 & x$total_hours == 1] - 1)], 
#          x$cum_hours_below[x$month == 1])
# x$total_hours[x$month == 12] <- NA
# return(x)