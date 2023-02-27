library(tidyverse)
source("rol_get_bloom_doy.R")

bloom_doy <- washington.bloom %>% inner_join(kyoto.bloom, by = "year") %>% 
  inner_join(liestal.bloom, by = "year")
names(bloom_doy) <- c("Year", "Washington", "Kyoto", "Liestal")
bloom_doy <- bloom_doy %>% 
  pivot_longer(cols = c("Washington", "Kyoto", "Liestal"), names_to = "Location", 
               values_to = "Bloom.Day")
bloom_doy <- bloom_doy %>% 
  mutate(Latitude = 
           case_when(
             Location == "Washington" ~ 38.8853,
             Location == "Kyoto" ~ 35.012, 
             Location == "Liestal" ~ 47.4814
             ), 
         Elevation = 
           case_when(
             Location == "Washington" ~ 0,
             Location == "Kyoto" ~ 44, 
             Location == "Liestal" ~ 350
             )
         )
mod <- lm(data = bloom_doy, Bloom.Day ~ Year + Latitude + Elevation)
mod %>% summary()

vancouver <- data.frame(Year = 1922:2023, 
                        Location = "Vancouver", 
                        Latitude = 49.2237, 
                        Elevation = 24)

vanc.preds <- predict(mod, vancouver)

ggplot(bloom_doy, aes(x = Year, y = Bloom.Day, color = Location)) +
  geom_point()
vancouver$Bloom.Day <- vanc.preds
(all_blooms <- bloom_doy %>% bind_rows(vancouver))
ggplot(all_blooms, aes(x = Year, y = Bloom.Day, color = Location)) +
  geom_point()

