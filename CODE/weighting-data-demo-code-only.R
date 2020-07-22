suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(survey)))

set.seed(123)

var_order <- c("age", "age_range", "gender", "educ", "ethnic", "region", "clin_status")

var_order_census_match  <- c("gender", "educ", "ethnic", "region")

cat_order <- c(
  NA, "5", "6", "7", "8", "9", "10", "11", "12",
  NA, "5 to 8 yo", "9 to 12 yo", 
  NA, "male", "female",
  NA, "no_HS", "HS_grad", "some_college", "BA_plus",
  NA, "hispanic", "asian", "black", "white", "other",
  NA, "northeast", "south", "midwest", "west")

urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/WEIGHTING-DATA/master/INPUT-FILES/"
fileName_path   <- "unweighted-input.csv"

original_input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

fileName_path   <- "data-input-sim.csv"

census_match_input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

rm(list = ls(pattern = "_path"))

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

var_order_census_match %>%
  map(
    ~ census_match_cat_count %>%
      filter(var == all_of(.x)) %>%
      select(-var) %>%
      rename(!!.x := cat, Freq = n_census)
  ) %>%
  setNames(str_c(var_order_census_match, "_census")) %>%
  list2env(envir = .GlobalEnv)

unweighted_survey_object <- svydesign(ids = ~1, 
                                      data = original_input, 
                                      weights = NULL)

rake_original_input <- rake(design = unweighted_survey_object,
                              sample.margins = list(~gender, ~educ, ~ethnic, ~region),
                              population.margins = list(gender_census, educ_census, 
                                                        ethnic_census, region_census))

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

unweighted_output <- input_demo_wts %>% 
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
  unweighted_output,
  here(
    "OUTPUT-FILES/unweighted-data-for-analysis.csv" 
  ),
  na = ""
)

# Extract demo weights to create a weighted data set.
ID_weights <- unweighted_output %>% 
  select(ID, demo_wt)

# In the weighted data set, each item score has its case's weighting multiplier
# applied; item names have `_w` suffix.
weighted_output1 <- original_input %>%
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
  weighted_output,
  here(
    "OUTPUT-FILES/weighted-data-for-analysis.csv" 
  ),
  na = ""
)


# REALITY CHECK MATERIAL FOR DEMO/TEACHING ONLY ---------------------------

# reality check, what demo cell is likely to have the weight that deviates
# farthest from 1? female, no_HS, hispanic, northeast. Observe relationship
# between sampling probability (the magnitude of deviation between this
# category's count in the input sample and the census target for the same
# category) and demo weight (the multiplier applied to each case in this
# category so that its impact on the sample statistics is "as-if" it had been
# sampled at a rate that met the census target)

input_demo_wts %>% 
  filter(
    gender == "female" &
      educ == "no_HS" &
      ethnic == "hispanic" &
      region == "northeast"
  ) %>% 
  select(-(i01:i50)) %>% 
  sample_n(1)

# what demo cell is likely to have weight closest to one? female, HS grad,
# asian, south. See comment above re relationship between samp_prob and demo_wt.
# The closer these two values are to 1, the closer the input count was to the
# census target for this category).

input_demo_wts %>% 
  filter(
    gender == "female" &
      educ == "HS_grad" &
      ethnic == "asian" &
      region == "south"
  ) %>% 
  select(-(i01:i50)) %>% 
  sample_n(1)


# Now we can look at different sections of the input data with demo weights,
# which is sorted in descending order of sampling probability. The bottom (tail)
# of the data frame contains cases from categories that were under-sampled in
# the input, so they should have low samp_prob and high demo_wt.
tail(input_demo_wts) %>% 
  select(-(i01:i50))

# Here we look at a small slice of the data frame containing cases from cats
# whose counts closely approximated their census targets. Both samp_prob and
# demo_wt should be relatively close to 1.
filter(input_demo_wts, between(samp_prob, .98, 1.02)) %>% 
  select(-(i01:i50))

# The top (head) of the data frame contains cases from categories that were
# over-sampled in the input, so they should have high samp_prob and low demo_wt.
head(input_demo_wts) %>% 
  select(-(i01:i50))


# This plot shows the relationship between sampling probability and demo weight.
ggplot(input_demo_wts, aes(demo_wt, samp_prob)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(x=1, y=1, color='purple', size = 3) + 
  scale_x_continuous(breaks = seq(0, 8, .5), minor_breaks = seq(0, 8, .1)) +
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
  )

ID_weights <- unweighted_input %>% 
  select(ID, demo_wt)
    
# In the weighted data set, each item score has its case's weighting multiplier
# applied; item names have `_w` suffix.
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

# The next snippet yields a table that is a summary comparison of the weighted
# and unweighted data sets. Crossing all categories of the four demo vars of
# interest (gender, educ, ethnic, region) yields a total of 160 possible cells
# that could have cases in either the original input or census matched data. The
# table `weight_unweight_comp` has 146 rows, meaning that either the input data
# or the census matched data had at least 1 case in 146 of the 160 possible
# cells. Of these 146 cells, 127 had cases in the input data, and so demo
# weights and other summary statistics are only calculated for those rows.

# For each cell the table gives the count of cases present in the input and
# census matched samples. It also gives the summed total raw scores for that
# cell, for the weighted and unweighted versions of the input data. Because the
# table is sorted descending by demo_wt, you observe the expected relationship
# between the unweighted and weighted score sums.
# weight_unweight_comp <- unweighted_input %>%
#   left_join(ID_TOT_raw_weight, by = "ID") %>%
#   select(-(i01_uw:i50_uw)) %>%
#   group_by(gender, educ, ethnic, region) %>%
#   summarize(
#     n_uw = n(),
#     demo_wt = first(demo_wt),
#     sum_TOT_unweight = sum(TOT_raw_unweight),
#     sum_TOT_weight = round(sum(TOT_raw_weight), 0)
#   ) %>% 
#   arrange(desc(demo_wt)) %>% 
#   full_join(census_match_cell_counts, by = c("gender", "educ", "ethnic", "region")) %>% 
#   relocate(n_uw, .before = n_census)

# Gives the counts for the 15 categories (across the four demo vars) for the
# original input data.
unweighted_cat_count <- var_order_census_match %>%
  map_df(
    ~
      input_demo_wts %>%
      group_by(across(all_of(.x))) %>%
      summarize(n_input = n()) %>%
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x)) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))

# Gives the unweighted sum of total scores for the 15 categories (across the
# four demo vars) for the original input data.
unweighted_TOT_sum <- var_order_census_match %>%
  map_df(
    ~
      unweighted_input %>%
      group_by(across(all_of(.x))) %>%
      summarize(TOT_sum_input = sum(TOT_raw_unweight)) %>% 
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x)) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))

# Gives the weighted sum of total scores for the 15 categories (across the
# four demo vars) for the original input data.
weighted_TOT_sum <- var_order_census_match %>%
  map_df(
    ~
      weighted_input %>%
      group_by(across(all_of(.x))) %>%
      summarize(TOT_sum_weighted = round(sum(TOT_raw_weight))) %>% 
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x)) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))

list_comp <- list(census_match_cat_count, unweighted_cat_count,
                   weighted_TOT_sum, unweighted_TOT_sum)

TOT_sum_cat_count_comp <- list_comp %>%
  reduce(left_join, by = c("var", "cat")) %>%
  mutate(n_diff = n_input - n_census,
         sum_diff = TOT_sum_input - TOT_sum_weighted) %>%
  relocate(var,
           cat,
           n_input,
           n_census,
           TOT_sum_input,
           TOT_sum_weighted,
           n_diff,
           sum_diff) %>%
  mutate(cat = factor(cat, levels = cat),) %>% 
  mutate(across(
  c(var),
  ~ case_when(
    lag(var) != var | is.na(lag(var)) ~ var,
    T ~ NA_character_
  )
))

plot_data <- TOT_sum_cat_count_comp %>%
  mutate(across(c(var),
                ~ runner::fill_run(.)))

ggplot(plot_data, aes(n_diff, sum_diff)) +
  geom_point(aes(shape = var, color = cat), size = 3) +
  facet_wrap(~ var) +
  xlab("Sample size diff: unweighted input vs. census target") +
  ylab("Sum TOT diff: unweighted input vs. census-weighted output") +
  # title() +
  geom_smooth(method = 'lm',
              se = F,
              formula = y ~ x, 
              size = .3
              ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..rr.label.., sep = '*plain(\',\')~')),
    rr.digits = 5, 
    parse = TRUE
  )

  

