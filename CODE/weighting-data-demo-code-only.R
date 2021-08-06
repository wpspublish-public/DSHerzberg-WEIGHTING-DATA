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

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-WEIGHTING-DATA/master/INPUT-FILES/"
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
      mutate(var = all_of(.x),
             pct_census = n_census/10) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))

var_order_census_match %>%
  map(
    ~ census_match_cat_count %>%
      filter(var == all_of(.x)) %>%
      select(-var, -pct_census) %>%
      rename(!!.x := cat, Freq = n_census)
  ) %>%
  setNames(str_c(var_order_census_match, "_census")) %>%
  list2env(envir = .GlobalEnv) %>% 
  invisible(.)

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

rm(list = ls(pattern = "object|rake"))

unweighted_output <- input_demo_wts %>% 
  select(-c(samp_prob, ratio)) %>%
  rename_with(~ str_c("i", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"), "_uw"), 
    i01:i50) %>% 
  mutate(
    TOT_raw_unweight = rowSums(.[grep("*_uw", names(.))]
  )) %>%
  relocate(TOT_raw_unweight, .after = demo_wt)

write_csv(
  unweighted_output,
  here(
    "OUTPUT-FILES/unweighted-data-for-analysis.csv" 
  ),
  na = ""
)

weighted_output <- original_input %>%
  left_join(unweighted_output[c("ID", "demo_wt")], by = "ID") %>%
  rename_with(~ str_c("i", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"
  ), "_w"),
  i01:i50) %>%
  mutate(across(c(i01_w:i50_w),
                ~ . * demo_wt)) %>%
  mutate(
    TOT_raw_weight = rowSums(.[grep("*_w$", names(.))]
    )) %>%
  relocate(demo_wt, TOT_raw_weight, .before = i01_w)

write_csv(
  weighted_output,
  here(
    "OUTPUT-FILES/weighted-data-for-analysis.csv" 
  ),
  na = ""
)

# PROOF OF CONCEPT

cat_count_comp <-
  var_order_census_match %>%
  map_df(
    ~
      original_input %>%
      group_by(across(all_of(.x))) %>%
      summarize(n_input = n()) %>%
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x),
             pct_input = n_input / 10) %>%
      relocate(var, .before = cat)
  ) %>%
  arrange(match(cat, cat_order)) %>%
  bind_cols(census_match_cat_count[c("n_census", "pct_census")]) %>%
  mutate(pct_diff = pct_input - pct_census)

knitr::kable(cat_count_comp %>%
               mutate(across(var,
                             ~ case_when(
                               lag(.x) == .x ~ "",
                               T ~ .x
                             ))),
             caption = "Table 1: Comparison of input sample percentage to census target")

knitr::kable(
  weighted_output %>%
    filter(
      gender == "female" &
        educ == "no_HS" &
        ethnic == "hispanic" &
        region == "northeast"
    ) %>%
    select(-age_range,-clin_status,-(i01_w:i50_w),-TOT_raw_weight) %>%
    sample_n(1),
  digits = 2,
  caption = "Table 2: Demographic multiplier from under-sampled cell"
)

knitr::kable(
  weighted_output %>%
    filter(
      gender == "female" &
        educ == "HS_grad" &
        ethnic == "black" &
        region == "west"
    ) %>%
    select(-age_range,-clin_status,-(i01_w:i50_w),-TOT_raw_weight) %>%
    sample_n(1),
  digits = 2,
  caption = "Table 3: Demographic mupltiplier from accurately sampled cell"
)

knitr::kable(
  tail(input_demo_wts) %>%
    select(-age_range, -clin_status, -(i01:i50), -ratio),
  digits = 2,
  caption = "Table 4: Cases from under-sampled cells"
)

knitr::kable(
  filter(input_demo_wts, between(samp_prob, .98, 1.02)) %>%
    select(-age_range, -clin_status, -(i01:i50), -ratio),
  digits = 2,
  caption = "Table 5: Cases from accurately sampled cells"
)

knitr::kable(
  head(input_demo_wts) %>%
    select(-age_range, -clin_status, -(i01:i50), -ratio),
  digits = 2,
  caption = "Table 6: Cases from over-sampled cells"
)

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
  label = "samp prob = weight = 1: input cell pct matches census pct",
  color = "purple",
  hjust = 0
) 

unweighted_TOT_sum <- var_order_census_match %>%
  map_df(
    ~
      unweighted_output %>%
      group_by(across(all_of(.x))) %>%
      summarize(TOT_sum_input = sum(TOT_raw_unweight)) %>% 
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x)) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))

weighted_TOT_sum <- var_order_census_match %>%
  map_df(
    ~
      weighted_output %>%
      group_by(across(all_of(.x))) %>%
      summarize(TOT_sum_weighted = round(sum(TOT_raw_weight))) %>% 
      rename(cat = all_of(.x)) %>%
      mutate(var = all_of(.x)) %>%
      relocate(var, .before = cat)
  ) %>% 
  arrange(match(cat, cat_order))

list_comp <- list(census_match_cat_count[c("var", "cat", "n_census")], 
                  cat_count_comp[c("var", "cat", "n_input")],
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
  mutate(cat = factor(cat, levels = cat))

knitr::kable(TOT_sum_cat_count_comp %>%
               mutate(across(var,
                             ~ case_when(
                               lag(.x) == .x ~ "",
                               T ~ .x
                             ))),
             caption = "Table 7: Comparison of unweighted and weighted total scores")

plot_data <- TOT_sum_cat_count_comp %>%
  mutate(across(var,
                ~ runner::fill_run(.)))

ggplot(plot_data, aes(n_diff, sum_diff)) +
  geom_point(aes(shape = var, color = cat), size = 3) +
  facet_wrap( ~ var) +
  xlab("n_diff") +
  ylab("sum_diff") +
  guides(col = guide_legend(nrow = 11)) +
  geom_smooth(
    method = 'lm',
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

  

