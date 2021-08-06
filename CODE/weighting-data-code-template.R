suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(survey)))
set.seed(123)

# set input parameters
var_order_census_match  <- c("gender", "educ", "ethnic", "region")

cat_order <- c(
  # age
  NA, "5", "6", "7", "8", "9", "10", "11", "12",
  # age_range
  NA, "5 to 8 yo", "9 to 12 yo", 
  # Gender
  NA, "male", "female",
  # educ
  NA, "no_HS", "HS_grad", "some_college", "BA_plus",
  # Ethnicity
  NA, "hispanic", "asian", "black", "white", "other",
  # Region
  NA, "northeast", "south", "midwest", "west")


urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-WEIGHTING-DATA/master/INPUT-FILES/"
fileName_path   <- "unweighted-input.csv"

# read data-to-be-processed with non-census-matched demographics
original_input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

# read in census-matched data
fileName_path   <- "data-input-sim.csv"

census_match_input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

# The next snippet uses map_df() to iterate over vec of census cats and create
# table of counts per census cat. The key line is group_by(across(.x)), the use
# of across() here allows you to use the elements of a char vec as grouping
# variables within map(), without having to get into the complexities of NSE.
census_match_cat_count <- var_order_census_match %>%
  map_df(
    ~
      census_match_input %>%
      group_by(across(all_of(.x))) %>%
      summarize(n_census = n()) %>%
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x)) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))


# this snippet breaks out the census counts into separate dfs, which are needed
# as input by survey::rake()
var_order_census_match %>%
  map(
    ~ census_match_cat_count %>%
      filter(var == all_of(.x)) %>%
      select(-var) %>%
      rename(!!.x := cat, Freq = n_census)
  ) %>%
  setNames(str_c(var_order_census_match, "_census")) %>%
  list2env(envir = .GlobalEnv)


# create survey objects that represent weights
unweighted_survey_object <- svydesign(ids = ~1, 
                                      data = original_input, 
                                      weights = NULL)

# rake input data to create case-wise weights
rake_original_input <- rake(design = unweighted_survey_object,
                              sample.margins = list(~gender, ~educ, ~ethnic, ~region),
                              population.margins = list(gender_census, educ_census, 
                                                        ethnic_census, region_census))

# bind demo weights to original input data
input_demo_wts <- bind_cols(
  rake_original_input[["variables"]],  
  data.frame(rake_original_input[["prob"]]), 
  data.frame(demo_wt = weights(rake_original_input))
) %>% 
  rename(samp_prob = rake_original_input...prob...) %>% 
  mutate(ratio = samp_prob / demo_wt) %>% 
  select(ID:clin_status, samp_prob, demo_wt, ratio, everything()) %>% 
  arrange(desc(samp_prob))

rm(list = ls(pattern = "_census|object|rake"))

# apply weights and calculate raw scores

# In the unweighted data set, item responses do not have weighting multipliers
# applied; item names have `_uw` suffix.
unweighted_input <- input_demo_wts %>% 
  select(-c(samp_prob, ratio)) %>%
  rename_with(~ str_c("i", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"), "_uw"), 
    i01:i50) %>% 
  mutate(
    TOT_raw_unweight = rowSums(.[str_c("i", str_pad(
      as.character(1:50), 2, side = "left", pad = "0"), "_uw")])
  ) %>% 
  relocate(TOT_raw_unweight, .after = demo_wt)

# write an output file with demo weights, unweighted item scores, and unweighted
# total score. This may be needed for some types of downstream analysis.

write_csv(
  unweighted_input,
  here(
   "OUTPUT-FILES/unweighted-data-for-analysis.csv" 
  ),
  na = ""
)

# Extract demo weights to create a weighted data set.
ID_weights <- unweighted_input %>% 
  select(ID, demo_wt)
    
# In the weighted data set, each item score has its case's weighting multiplier
# applied; item names have `_w` suffix.
weighted_input <- original_input %>%
  left_join(ID_weights, by = "ID") %>%
  rename_with(~ str_c("i", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"
  ), "_w"),
  i01:i50) %>%
  mutate(across(c(i01_w:i50_w),
                ~ . * demo_wt)) %>%
  mutate(TOT_raw_weight = rowSums(.[str_c("i", str_pad(as.character(1:50), 2, 
                                                       side = "left", pad = "0"), "_w")])) %>% 
  relocate(demo_wt, TOT_raw_weight, .before = i01_w)
  

# write an output file with demo weights, weighted item scores, and weighted
# total score. This may be needed for some types of downstream analysis.

write_csv(
  weighted_input,
  here(
    "OUTPUT-FILES/weighted-data-for-analysis.csv" 
  ),
  na = ""
)




