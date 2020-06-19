suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(survey)))
set.seed(123)

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/WEIGHTING-DATA/master/INPUT-FILES/"
fileName_path   <- "unweighted-input.csv"

original_input <- suppressMessages(read_csv(url(
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
  NA, "northeast", "south", "midwest", "west")

gender_census <- tibble(cat = c("female", "male"),
                        Freq = nrow(original_input)*c(0.53, 0.47))
educ_census <- tibble(cat = c("no_HS", "HS_grad", "some_college", "BA_plus"),
                      Freq = nrow(original_input)*c(0.119, 0.263, 0.306, 0.311))
ethnic_census <- tibble(cat = c("hispanic", "asian", "black", "white", "other"),
                        Freq = nrow(original_input)*c(0.239, 0.048, 0.136, 0.521, .056))
region_census <- tibble(cat = c("northeast", "south", "midwest", "west"),
                        Freq = nrow(original_input)*c(0.166, 0.383, 0.212, 0.238))

# get demo counts of unweighted input
freq_demos_unweighted <- original_input %>%
  pivot_longer(age_range:clin_status, names_to = 'var', values_to = 'cat') %>%
  group_by(var, cat) %>%
  count(var, cat) %>%
  arrange(match(var, var_order), match(cat, cat_order)) %>%
  ungroup() %>%
  mutate(
    pct_samp = round(((n / nrow(original_input)) * 100), 1)
  ) %>%
  select(var, cat, n, pct_samp) %>% 
  full_join(region_census, by = "cat")

list_demos <- list(freq_demos_unweighted, gender_census, educ_census, 
                   ethnic_census, region_census)

# bind census demos
freq_demos_comp <- list_demos %>% 
  reduce(left_join, by = "cat") %>% 
  filter(!(var %in% c("age_range", "clin_status"))) %>% 
  unite(census_count, c(Freq.x, Freq.y, Freq.x.x, Freq.y.y), sep = "", remove = T) %>% 
  mutate_at(vars(census_count), ~as.integer(str_replace_all(., "NA", ""))) %>% 
  mutate(census_pct = 100 * round(census_count/nrow(original_input), 3),
         diff_pct = census_pct - pct_samp
  ) %>% 
  rename(input_count = n, input_pct = pct_samp) %>% 
  select(var, cat, input_count, census_count, input_pct, census_pct, diff_pct) %>% 
  arrange(desc(diff_pct))

rm(list = setdiff(ls(), ls(pattern = "input|comp|list")))

# rename vars in `_census` dfs
list_census <- list(list_demos[2:5],
                    c("gender", "educ", "ethnic", "region")) %>%
  pmap(~ ..1 %>%
         rename_at(vars(cat), ~str_replace(., "cat", !!..2))
  )

names(list_census) <- c("gender_census", "educ_census", "ethnic_census", "region_census")

list2env(list_census, envir=.GlobalEnv)


# create survey objects that represent weights
unweighted_survey_object <- svydesign(ids = ~1, 
                                      data = original_input, 
                                      weights = NULL)

# rake input data to create case-wise weights
rake_original_input <- rake(design = unweighted_survey_object,
                              sample.margins = list(~gender, ~educ, ~ethnic, ~region),
                              population.margins = list(gender_census, educ_census, 
                                                        ethnic_census, region_census))

# bind demo weights to original data
input_demo_wts <- bind_cols(
  rake_original_input[["variables"]],  
  data.frame(rake_original_input[["prob"]]), 
  data.frame(demo_wt= weights(rake_original_input))
) %>% 
  rename(samp_prob  = rake_original_input...prob...) %>% 
  mutate(ratio = samp_prob / demo_wt) %>% 
  select(ID:clin_status, samp_prob, demo_wt, ratio, everything()) %>% 
  arrange(desc(samp_prob))

# reality check, what demo cell is likely to have the weight that deviates farthest from 1?
#  female, no_HS, hispanic, northeast
# what demo cell is likely to have weight closest to one?
# female, HS grad, asian, south

case_high_wt <- input_demo_wts %>% 
  filter(
    gender == "female" &
      educ == "no_HS" &
      ethnic == "hispanic" &
      region == "northeast"
  ) %>% 
  sample_n(1)

case_1_wt <- input_demo_wts %>% 
  filter(
    gender == "female" &
      educ == "HS_grad" &
      ethnic == "asian" &
      region == "south"
  ) %>% 
  sample_n(1)

tail(input_demo_wts)

filter(input_demo_wts, between(samp_prob, .98, 1.02))

head(input_demo_wts)

ggplot(input_demo_wts, aes(demo_wt, samp_prob)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(x=1, y=1, color='purple', size = 3) + 
  scale_x_continuous(breaks = seq(0, 6, .5), minor_breaks = seq(0, 6, .1)) +
  scale_y_continuous(breaks = seq(0, 2.5, .5),
                     minor_breaks = seq(0, 2.5, .1)) +
  xlab("demographic weighting multiplier") +
  ylab("sampling probability") +
  annotate(
    "text",
    x = 1.5,
    y = 1.5,
    label = "Oversampled relative to census: sampling probability > 1",
    color = "red",
    hjust = 0
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 1.4,
    label = "Undersampled relative to census: sampling probability < 1",
    color = "darkgreen",
    hjust = 0
  ) +
annotate(
  "text",
  x = 1.1,
  y = 1.1,
  label = "samp prob = weight = 1: demo cell pct matches census pct",
  color = "purple",
  hjust = 0
)

# apply weights and calculate raw scores

unweighted_input <- input_demo_wts %>% 
  select(-c(samp_prob, ratio)) %>%
  rename_with(~ str_c("i", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"), "_uw"), 
    i01:i50) %>% 
  mutate(
    TOT_raw_unweight = rowSums(.[str_c("i", str_pad(
      as.character(1:50), 2, side = "left", pad = "0"), "_uw")])
  )


ID_weights <- unweighted_input %>% 
  select(ID, demo_wt)
    
weighted_input <- original_input %>%
  left_join(ID_weights, by = "ID") %>%
  relocate(demo_wt, .before = i01) %>%
  rename_with(~ str_c("i", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"
  ), "_w"),
  i01:i50) %>%
  mutate(TOT_raw_weight = rowSums(.[str_c("i", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"), "_w")]) * demo_wt)
