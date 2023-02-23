library(tidyverse)
library(rnoaa)

date.range <- as.Date("2023-01-01")

# Washington DC ####

wash <- data.frame(id = "washington", latitude = 38.8853, longitude = -77.0386)
wash_stations <- meteo_nearby_stations(lat_lon_df = wash, limit = 10, 
                                          var = c("PRCP", "TMAX"), year_min = 2000, year_max = 2000)
wash_stations$washington

wash_ids <- wash_stations$washington %>% select(id)
wash_ids <- wash_ids[[1]][1:3]


#stations <- ghcnd_stations()
stations %>% filter(id == wash_ids[1]) %>% 
  filter(element == "TMAX" | element == "TMIN") # 1941 to 2023
stations %>% filter(id == wash_ids[2]) %>% 
  filter(element == "TMAX" | element == "TMIN") # 1948 to 2022
stations %>% filter(id == wash_ids[3]) %>% 
  filter(element == "TMAX" | element == "TMIN") # 1948 to 2022

# count data

wash.temp1 <- ghcnd_search(stationid = wash_ids[1],
                      #refresh = TRUE,
                      var = c("tmax", "tmin"))
wash.temp1.join <-
  left_join(wash.temp1$tmax, wash.temp1$tmin, by = c("id", "date")) %>% 
  filter(!is.na(tmax) & !is.na(tmin)) %>% 
  mutate(month = lubridate::month(date)) %>% filter(month < 6) %>% 
  mutate(year = parse_number(format(date, "%Y"))) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(row_num = map(data, nrow)) %>% 
  unnest(row_num) %>% 
  filter(row_num > 150)
nrow(wash.temp1.join)

wash.temp2 <- ghcnd_search(stationid = wash_ids[2],
                      #refresh = TRUE,
                      var = c("tmax", "tmin"))
wash.temp2.join <-
  left_join(wash.temp2$tmax, wash.temp2$tmin, by = c("id", "date")) %>% 
  filter(!is.na(tmax) & !is.na(tmin)) %>% 
  mutate(month = lubridate::month(date)) %>% filter(month < 6) %>% 
  mutate(year = parse_number(format(date, "%Y"))) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(row_num = map(data, nrow)) %>% 
  unnest(row_num) %>% 
  filter(row_num > 150)
nrow(wash.temp2.join)

wash.temp3 <- ghcnd_search(stationid = wash_ids[3],
                      #refresh = TRUE,
                      var = c("tmax", "tmin"))
wash.temp3.join <-
  left_join(wash.temp3$tmax, wash.temp3$tmin, by = c("id", "date")) %>% 
  filter(!is.na(tmax) & !is.na(tmin)) %>% 
  mutate(year = parse_number(format(date, "%Y"))) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(row_num = map(data, nrow)) %>% 
  unnest(row_num) %>% 
  filter(row_num > 364)
nrow(wash.temp3.join)


# Kyoto ####
kyoto <- data.frame(id = "kyoto", latitude = 35.0120, longitude = 135.6761)
kyoto_stations <- meteo_nearby_stations(lat_lon_df = kyoto, limit = 10, 
                                        var = c("PRCP", "TMAX"), year_min = 2000, year_max = 2000)
summary(kyoto$lat)
summary(kyoto_stations)
kyoto_stations$kyoto
kyoto_stations

kyoto_station <- ghcnd_search(
  stationid = 'JA000047759',
)

#stations <- ghcnd_stations()
stations %>% filter(id == 'JA000047759') %>% 
  filter(element == "TMAX" | element == "TMIN") # from 1951 to 2023

# count data

kyoto.temp1 <- ghcnd_search(stationid = 'JA000047759',
                           #refresh = TRUE,
                           var = c("tmax", "tmin"))
kyoto.temp1.join <-
  left_join(kyoto.temp1$tmax, kyoto.temp1$tmin, by = c("id", "date")) %>% 
  filter(!is.na(tmax) & !is.na(tmin)) %>% 
  mutate(month = lubridate::month(date)) %>% filter(month < 6) %>% 
  mutate(year = parse_number(format(date, "%Y"))) %>%
  group_by(year) %>%
  nest() %>%
  mutate(row_num = map(data, nrow)) %>%
  unnest(row_num) %>%
  filter(row_num > 10)
nrow(kyoto.temp1.join$year)

# Liestal-Weideli ####

liestal <- data.frame(id = "liestal", latitude = 47.4814, longitude = 7.730519)
liestal_stations <- meteo_nearby_stations(lat_lon_df = liestal, limit = 10, 
                                        var = c("PRCP", "TMAX"), year_min = 2000, year_max = 2000)
summary(liestal_stations)
liestal_stations$liestal
liestal_stations

liestal_station <- ghcnd_search(
  stationid = 'SZ000001940',
)

stations <- ghcnd_stations()
stations %>% filter(id == 'SZ000001940') %>% 
  filter(element == "TMAX" | element == "TMIN") # from 1901 to 2023

# Vancouver ####

vanc <- data.frame(id = "vancouver", latitude = 49.2237, longitude = -123.1636)
vanc_stations <- meteo_nearby_stations(lat_lon_df = vanc, limit = 10, 
                                          var = c("PRCP", "TMAX"), year_min = 2000, year_max = 2000)
vanc_stations$vancouver

vanc_ids <- vanc_stations$vancouver %>% select(id)
vanc_ids <- vanc_ids[[1]][1:7]

vanc_station <- ghcnd_search(
  stationid = 'CA001108473',
)

stations <- ghcnd_stations()
stations %>% filter(id == vanc_ids[1]) %>% 
  filter(element == "TMAX" | element == "TMIN") # 1999 to 2004
stations %>% filter(id == vanc_ids[2]) %>% 
  filter(element == "TMAX" | element == "TMIN") # 1957 to 2023
stations %>% filter(id == vanc_ids[3]) %>% 
  filter(element == "TMAX" | element == "TMIN") # 1937 to 2013
stations %>% filter(id == vanc_ids[4]) %>% 
  filter(element == "TMAX" | element == "TMIN") # none
stations %>% filter(id == vanc_ids[5]) %>% 
  filter(element == "TMAX" | element == "TMIN") # 1964
stations %>% filter(id == vanc_ids[6]) %>% 
  filter(element == "TMAX" | element == "TMIN") # 1977 to 2023
stations %>% filter(id == vanc_ids[7]) %>% 
  filter(element == "TMAX" | element == "TMIN") # 1958 to 2023

