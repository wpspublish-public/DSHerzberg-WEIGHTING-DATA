suppressMessages(suppressWarnings(library(tidyverse)))

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/WEIGHTING-DATA/master/INPUT-FILES/"
fileName_path   <- "data-input-sim.csv"

census_match_input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

var_order_census_match  <- c("gender", "educ", "ethnic", "region")

census_match_cat_count_gender <- census_match_input %>%
  group_by(gender) %>%
  summarize(n_census = n()) %>%
  rename(demo_cat = gender) %>%
  mutate(demo_var = "gender") %>%
  relocate(demo_var, .before = demo_cat)

census_match_cat_count_educ <- census_match_input %>%
  group_by(educ) %>%
  summarize(n_census = n()) %>%
  rename(demo_cat = educ) %>%
  mutate(demo_var = "educ") %>%
  relocate(demo_var, .before = demo_cat)

census_match_cat_count_ethnic <- census_match_input %>%
  group_by(ethnic) %>%
  summarize(n_census = n()) %>%
  rename(demo_cat = ethnic) %>%
  mutate(demo_var = "ethnic") %>%
  relocate(demo_var, .before = demo_cat)

census_match_cat_count_region <- census_match_input %>%
  group_by(region) %>%
  summarize(n_census = n()) %>%
  rename(demo_cat = region) %>%
  mutate(demo_var = "region") %>%
  relocate(demo_var, .before = demo_cat)

census_match_cat_count <- var_order_census_match %>%
  map(
    ~
      census_match_input %>%
      group_by(!!.x) %>%
      summarize(n_census = n()) %>%
      rename(demo_cat = !!.x) %>%
      mutate(demo_var = .x) %>%
      relocate(demo_var, .before = demo_cat)
  )

