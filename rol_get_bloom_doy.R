library(tidyverse)


kyoto.bloom <- read_csv("data/kyoto.csv") %>% 
  select(year, bloom_doy)
# japan.kyoto.bloom <- read_csv("data/japan.csv") %>%
#   filter(location == "Japan/Kyoto") %>%
#   select(year, bloom_doy)
# join <- inner_join(kyoto.bloom, japan.kyoto.bloom, by = "year")
# join <- join %>% filter(bloom_doy.x != bloom_doy.y) %>% 
#   mutate(diff = bloom_doy.x - bloom_doy.y)
# sqrt(sqrt(sum((join$diff)^2)))

liestal.bloom <- read_csv("data/liestal.csv") %>% 
  select(year, bloom_doy)


vancouver.bloom <- read_csv("data/vancouver.csv") %>% 
  select(year, bloom_doy)

washington.bloom <- read_csv("data/washingtondc.csv") %>% 
  select(year, bloom_doy)

