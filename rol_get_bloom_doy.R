library(tidyverse)


kyoto.bloom <- read_csv("data/kyoto.csv") %>% 
  select(year, bloom_doy)
# japan.kyoto.bloom <- read_csv("data/japan.csv") %>% 
#   filter(location == "Japan/Kyoto") %>% 
#   select(year, bloom_doy)
# join <- inner_join(kyoto, japan, by = "year")
# join %>% filter(bloom_doy.x != bloom_doy.y)


liestal.bloom <- read_csv("data/liestal.csv") %>% 
  select(year, bloom_doy)


vancouver.bloom <- read_csv("data/vancouver.csv") %>% 
  select(year, bloom_doy)

washington.bloom <- read_csv("data/washingtondc.csv") %>% 
  select(year, bloom_doy)

