suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(survey)))
set.seed(123)

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
dummy_survey_unweighted <- svydesign(ids = ~1, 
                                     data = dummy_data_df, 
                                     weights = NULL)
gender_dist <- tibble(gender = c("1", "2"), Freq = nrow(dummy_data_df)*c(0.45, 0.55))
dummy_gender_rake <- rake(design = dummy_survey_unweighted,
                          sample.margins = list(~gender),
                          population.margins = list(gender_dist))

gender_freq_comp <- data.frame(table(dummy_data_df$gender)) %>% 
  rename(gender = Var1) %>% 
  left_join(gender_dist, by = "gender", suffix = c("_input", "_pop"))

# bind demo weights to original data
input_demo_wts <- bind_cols(
  dummy_gender_rake[["variables"]], 
  data.frame(dummy_gender_rake[["prob"]])
) %>% 
  rename(demo_wt = dummy_gender_rake...prob...) %>% 
  arrange(desc(demo_wt)) %>% 
  mutate(income_wt = income / demo_wt)

svymean(dummy_data_df,
        design = dummy_survey_unweighted)
svymean(dummy_data_df,
        design = dummy_gender_rake)

mean_income_input <- dummy_data_df %>% 
  group_by(gender) %>% 
  summarize(income = mean(income))

mean_income_demo_wts <- input_demo_wts %>% 
  group_by(gender) %>% 
  summarize(income_wt = mean(income_wt))

mean_income_comp <- mean_income_input %>% 
  left_join(mean_income_demo_wts, by = "gender") %>% 
  mutate_at(vars(gender), ~as.character(.))

mean_income_gender_freq_comp <- gender_freq_comp %>% 
  left_join(mean_income_comp, by = "gender")

