suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(survey)))
set.seed(123)

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

# create survey objects that represent weights
dummy_survey_unweighted <- svydesign(ids = ~1, 
                                     data = dummy_data_df, 
                                     weights = NULL)
gender_dist <- tibble(gender = c("1", "2"), Freq = nrow(dummy_data_df)*c(0.45, 0.55))
dummy_gender_rake <- rake(design = dummy_survey_unweighted,
                          sample.margins = list(~gender),
                          population.margins = list(gender_dist))

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
