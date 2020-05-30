suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(survey)))
set.seed(123)

urlRemote_path  <- "https://raw.githubusercontent.com/"
Github_path <- "DSHerzberg/WEIGHTING-DATA/master/INPUT-FILES/"
fileName_path   <- "unweighted-input.csv"

unweighted_input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, Github_path, fileName_path)
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








# create dummy data
dummy_data_age <- sample(c(18:90), 100, replace = TRUE)
dummy_data_income <- sample(c(0:100000), 100, replace = TRUE)
dummy_data_gender <- sample(c("Male", "Female"), 100, replace = TRUE)
dummy_data_gender <- as.integer(factor(dummy_data_gender))
dummy_data_region <- sample(c("North", "East", "South", "West"), 100, replace = TRUE)
dummy_data_region <- as.integer(factor(dummy_data_region))
dummy_data_age <- tibble::enframe(dummy_data_age, "unit", "age") %>% select("age")
dummy_data_income <- tibble::enframe(dummy_data_income, "unit", "income") %>% select("income")
dummy_data_gender <- tibble::enframe(dummy_data_gender, "unit", "gender") %>% select("gender")
dummy_data_region <- tibble::enframe(dummy_data_region, "unit", "region") %>% select("region")
dummy_data_df <- bind_cols(dummy_data_age, dummy_data_income, dummy_data_gender, dummy_data_region)

#  dummy data counts, percentages
dummy_data_gender_pct <- dummy_data_df %>% 
  group_by(gender) %>% 
  summarise(n = n(),
            pct = 100 * (n/nrow(dummy_data_df)))
dummy_data_region_pct <- dummy_data_df %>% 
  group_by(region) %>% 
  summarise(n = n(),
            pct = 100 * (n/nrow(dummy_data_df)))


# create survey objects that represent weights
dummy_survey_unweighted <- svydesign(ids = ~1, 
                                     data = dummy_data_df, 
                                     weights = NULL)
gender_dist <- tibble(gender = c("1", "2"), Freq = nrow(dummy_data_df)*c(0.45, 0.55))
region_dist <- tibble(region = c("1", "2", "3", "4"), Freq = nrow(dummy_data_df)*c(0.166, 0.383, 0.212, 0.238))
dummy_gender_rake <- rake(design = dummy_survey_unweighted,
                          sample.margins = list(~gender, ~region),
                          population.margins = list(gender_dist, region_dist))

# bind gender weights to original data
dummy_data_gender_region_wts <- bind_cols(
  dummy_gender_rake[["variables"]], 
  data.frame(dummy_gender_rake[["prob"]])
  ) %>% 
  rename(gender_region_wt = dummy_gender_rake...prob...)

# use the ‘svymean’ and ‘svyby’ functions to produce outputs for the survey under different weights.

svymean(dummy_data_df,
        design = dummy_survey_unweighted)

svymean(dummy_data_df,
        design = dummy_gender_rake)

svyby(~age, ~region, 
      design = dummy_survey_unweighted, 
      FUN = svymean, 
      keep.names = FALSE)

svyby(~age, ~region,
      design = dummy_gender_rake,
      FUN = svymean,
      keep.names = FALSE)
