library("rnoaa")
library(tidyverse)

kyoto <- data.frame(id = "kyoto", latitude = 35.01198, longitude = 135.6761)
kyoto_stations <- meteo_nearby_stations(lat_lon_df = kyoto, limit = 3, 
                                        var = c("PRCP", "TMAX"), year_min = 2000, year_max = 2000)



summary(kyoto$lat)
summary(kyoto_stations)
kyoto_stations$kyoto
kyoto_stations

kyoto_station <- ghcnd_search(
  stationid = 'JA000047759',
)

stations <- ghcnd_stations()
stations %>% filter(id == 'JA000047759') %>% head()


summary(kyoto_station['tmax'])
