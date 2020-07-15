temp <- census_match_input %>%
  group_by(ethnic) %>%
  summarize(n_census = n()) %>%
  rename(cat = ethnic) %>%
  mutate(var = "ethnic") %>%
  relocate(var, .before = cat)



) %>% 
  arrange(match(cat, cat_order))
