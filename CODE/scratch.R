census_match_cat_count <- var_order_census_match %>%
  map_df(
    ~
      census_match_input %>%
      group_by(across(all_of(.x))) %>%
      summarize(n_census = n()) %>%
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x),
             pct_census = n_census/10) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))

cat_count_comp <- var_order_census_match %>%
  map_df(
    ~
      original_input %>%
      group_by(across(all_of(.x))) %>%
      summarize(n_sample = n()) %>%
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x),
             pct_sample = n_sample/10) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order)) %>% 
  bind_cols(census_match_cat_count[c("n_census", "pct_census")]) %>% 
  mutate(pct_diff = pct_sample - pct_census)

cat_count_comp <- bind_cols(census_match_cat_count, original_input_cat_count[c("n_sample", "pct_sample")])
