chill.wash.join %>% 
  mutate(chill_sum = map(data, hours_below2)) %>% 
  unnest(chill_sum)