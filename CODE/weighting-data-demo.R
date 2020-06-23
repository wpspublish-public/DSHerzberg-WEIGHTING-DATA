suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(survey)))
set.seed(123)

var_order <- c("age", "age_range", "gender", "educ", "ethnic", "region", "clin_status")

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

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/WEIGHTING-DATA/master/INPUT-FILES/"
fileName_path   <- "unweighted-input.csv"

# read sample with non-census-matched demographics
original_input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

# read in census-matched data
fileName_path   <- "data-input-sim.csv"

census_match_input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

census_match_cell_counts <- census_match_input %>% 
  group_by(gender, educ, ethnic, region) %>% 
  summarize(n_census = n())

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

census_match_cat_count <- bind_rows(
  census_match_cat_count_gender, 
  census_match_cat_count_educ, 
  census_match_cat_count_ethnic, 
  census_match_cat_count_region, 
) %>% 
  arrange(match(demo_cat, cat_order)) %>% 
  mutate(across(
    c(demo_var),
    ~ case_when(
      lag(demo_var) != demo_var | is.na(lag(demo_var)) ~ demo_var,
      T ~ NA_character_
    )
  ))

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

rm(list = setdiff(ls(), ls(pattern = "input|comp|list|cell|count|order")))

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
) #+
  # stat_smooth(method='lm', formula = y ~ poly(x, 2))



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
  mutate(across(c(i01_w:i50_w),
                ~ . * demo_wt)) %>%
  mutate(TOT_raw_weight = rowSums(.[str_c("i", str_pad(as.character(1:50), 2, side = "left", pad = "0"), "_w")]))

ID_TOT_raw_weight <- weighted_input %>% 
  select(ID, TOT_raw_weight)

weight_unweight_comp <- unweighted_input %>%
  left_join(ID_TOT_raw_weight, by = "ID") %>%
  select(-(i01_uw:i50_uw)) %>%
  group_by(gender, educ, ethnic, region) %>%
  summarize(
    n_uw = n(),
    demo_wt = first(demo_wt),
    sum_TOT_unweight = sum(TOT_raw_unweight),
    sum_TOT_weight = round(sum(TOT_raw_weight), 0)
  ) %>% 
  arrange(desc(demo_wt)) %>% 
  full_join(census_match_cell_counts, by = c("gender", "educ", "ethnic", "region")) %>% 
  relocate(n_uw, .before = n_census)

unweighted_cat_count_gender <- input_demo_wts %>%
  group_by(gender) %>%
  summarize(n_input = n()) %>% 
  rename(demo_cat = gender) %>% 
  mutate(demo_var = "gender") %>% 
  relocate(demo_var, .before = demo_cat)

unweighted_cat_count_educ <- input_demo_wts %>%
  group_by(educ) %>%
  summarize(n_input = n()) %>% 
  rename(demo_cat = educ) %>% 
  mutate(demo_var = "educ") %>% 
  relocate(demo_var, .before = demo_cat)

unweighted_cat_count_ethnic <- input_demo_wts %>%
  group_by(ethnic) %>%
  summarize(n_input = n()) %>% 
  rename(demo_cat = ethnic) %>% 
  mutate(demo_var = "ethnic") %>% 
  relocate(demo_var, .before = demo_cat)

unweighted_cat_count_region <- input_demo_wts %>%
  group_by(region) %>%
  summarize(n_input = n()) %>% 
  rename(demo_cat = region) %>% 
  mutate(demo_var = "region") %>% 
  relocate(demo_var, .before = demo_cat)

unweighted_cat_count <- bind_rows(
  unweighted_cat_count_gender, 
  unweighted_cat_count_educ, 
  unweighted_cat_count_ethnic, 
  unweighted_cat_count_region, 
) %>% arrange(match(demo_cat, cat_order))%>% 
  mutate(across(
    c(demo_var),
    ~ case_when(
      lag(demo_var) != demo_var | is.na(lag(demo_var)) ~ demo_var,
      T ~ NA_character_
    )
  ))

unweighted_TOT_sum_gender <- unweighted_input %>%
  group_by(gender) %>%
  summarize(TOT_sum_input = sum(TOT_raw_unweight)) %>% 
  rename(demo_cat = gender) %>% 
  mutate(demo_var = "gender") %>% 
  relocate(demo_var, .before = demo_cat)

unweighted_TOT_sum_educ <- unweighted_input %>%
  group_by(educ) %>%
  summarize(TOT_sum_input = sum(TOT_raw_unweight)) %>% 
  rename(demo_cat = educ) %>% 
  mutate(demo_var = "educ") %>% 
  relocate(demo_var, .before = demo_cat)

unweighted_TOT_sum_ethnic <- unweighted_input %>%
  group_by(ethnic) %>%
  summarize(TOT_sum_input = sum(TOT_raw_unweight)) %>% 
  rename(demo_cat = ethnic) %>% 
  mutate(demo_var = "ethnic") %>% 
  relocate(demo_var, .before = demo_cat)

unweighted_TOT_sum_region <- unweighted_input %>%
  group_by(region) %>%
  summarize(TOT_sum_input = sum(TOT_raw_unweight)) %>% 
  rename(demo_cat = region) %>% 
  mutate(demo_var = "region") %>% 
  relocate(demo_var, .before = demo_cat)

unweighted_TOT_sum <- bind_rows(
  unweighted_TOT_sum_gender, 
  unweighted_TOT_sum_educ, 
  unweighted_TOT_sum_ethnic, 
  unweighted_TOT_sum_region, 
) %>% arrange(match(demo_cat, cat_order))%>% 
  mutate(across(
    c(demo_var),
    ~ case_when(
      lag(demo_var) != demo_var | is.na(lag(demo_var)) ~ demo_var,
      T ~ NA_character_
    )
  ))

weighted_TOT_sum_gender <- weighted_input %>%
  group_by(gender) %>%
  summarize(TOT_sum_census = round(sum(TOT_raw_weight))) %>% 
  rename(demo_cat = gender) %>% 
  mutate(demo_var = "gender") %>% 
  relocate(demo_var, .before = demo_cat)

weighted_TOT_sum_educ <- weighted_input %>%
  group_by(educ) %>%
  summarize(TOT_sum_census = round(sum(TOT_raw_weight))) %>% 
  rename(demo_cat = educ) %>% 
  mutate(demo_var = "educ") %>% 
  relocate(demo_var, .before = demo_cat)

weighted_TOT_sum_ethnic <- weighted_input %>%
  group_by(ethnic) %>%
  summarize(TOT_sum_census = round(sum(TOT_raw_weight))) %>% 
  rename(demo_cat = ethnic) %>% 
  mutate(demo_var = "ethnic") %>% 
  relocate(demo_var, .before = demo_cat)

weighted_TOT_sum_region <- weighted_input %>%
  group_by(region) %>%
  summarize(TOT_sum_census = round(sum(TOT_raw_weight))) %>% 
  rename(demo_cat = region) %>% 
  mutate(demo_var = "region") %>% 
  relocate(demo_var, .before = demo_cat)

weighted_TOT_sum <- bind_rows(
  weighted_TOT_sum_gender, 
  weighted_TOT_sum_educ, 
  weighted_TOT_sum_ethnic, 
  weighted_TOT_sum_region, 
) %>% arrange(match(demo_cat, cat_order)) %>% 
  mutate(across(
    c(demo_var),
    ~ case_when(
      lag(demo_var) != demo_var | is.na(lag(demo_var)) ~ demo_var,
      T ~ NA_character_
    )
  ))


list_comp <- list(census_match_cat_count, unweighted_cat_count,
                   weighted_TOT_sum, unweighted_TOT_sum)

TOT_sum_cat_count_comp <- list_comp %>%
  reduce(left_join, by = c("demo_var", "demo_cat")) %>%
  mutate(n_diff = n_input - n_census,
         sum_diff = TOT_sum_input - TOT_sum_census) %>%
  relocate(contains("demo"),
           n_input,
           n_census,
           TOT_sum_input,
           TOT_sum_census,
           n_diff,
           sum_diff) %>%
  mutate(demo_cat = factor(demo_cat, levels = demo_cat),)

plot_data <- TOT_sum_cat_count_comp %>%
  mutate(across(c(demo_var),
                ~ runner::fill_run(.)))

ggplot(plot_data, aes(n_diff, sum_diff)) +
  geom_point(aes(shape = demo_var, color = demo_cat), size = 2) +
  facet_wrap(~ demo_var) +
  xlab("Sample size diff: unweighted input vs. census target") +
  ylab("Sum TOT diff: unweighted input vs. census-weighted output") +
  title()
  geom_smooth(method = 'lm',
              se = FALSE,
              formula = y ~ x, 
              size = .3
              ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..rr.label.., sep = '*plain(\',\')~')),
    parse = TRUE
  )

  

