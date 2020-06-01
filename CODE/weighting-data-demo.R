suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(survey)))
set.seed(123)

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/WEIGHTING-DATA/master/INPUT-FILES/"
fileName_path   <- "unweighted-input.csv"

unweighted_input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

var_order <- c("age", "age_range", "gender", "educ", "ethnic", "region", "clin_status")

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
  NA, "northeast", "midwest", "south", "west")


freq_demos_unweighted <- unweighted_input %>%
  pivot_longer(age_range:clin_status, names_to = 'var', values_to = 'cat') %>%
  group_by(var, cat) %>%
  count(var, cat) %>%
  arrange(match(var, var_order), match(cat, cat_order)) %>%
  ungroup() %>%
  mutate(
   pct_samp = round(((n / nrow(unweighted_input)) * 100), 1)
  ) %>%
  select(var, cat, n, pct_samp)

# create survey objects that represent weights
unweighted_survey_object <- svydesign(ids = ~1, 
                                     data = unweighted_input, 
                                     weights = NULL)

# create census percentages
gender_census <- tibble(gender = c("female", "male"), 
                        Freq = nrow(unweighted_input)*c(0.53, 0.47))
educ_census <- tibble(educ = c("no_HS", "HS_grad", "some_college", "BA_plus"), 
                      Freq = nrow(unweighted_input)*c(0.119, 0.263, 0.306, 0.311))
ethnic_census <- tibble(ethnic = c("hispanic", "asian", "black", "white", "other"), 
                        Freq = nrow(unweighted_input)*c(0.239, 0.048, 0.136, 0.521, .056))
region_census <- tibble(region = c("northeast", "south", "midwest", "west"), 
                        Freq = nrow(unweighted_input)*c(0.166, 0.383, 0.212, 0.238))

# rake input data to create case-wise weights
rake_unweighted_input <- rake(design = unweighted_survey_object,
                          sample.margins = list(~gender, ~educ, ~ethnic, ~region),
                          population.margins = list(gender_census, educ_census, 
                                                    ethnic_census, region_census))

# bind gender weights to original data
input_demo_wts <- bind_cols(
  rake_unweighted_input[["variables"]], 
  data.frame(rake_unweighted_input[["prob"]])
  ) %>% 
  rename(demo_wt = rake_unweighted_input...prob...) %>% 
  select(ID:clin_status, demo_wt, everything())

