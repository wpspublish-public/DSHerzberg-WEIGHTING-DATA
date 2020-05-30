suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(psych))

set.seed(123)

unweighted_input <-
  as_tibble(sim.poly.ideal(nvar = 50, n = 1000, cat = 4,)[["items"]]) %>%
  rename_all(~ str_c("i", str_pad(
    as.character(1:50), 2, side = "left", pad = "0"
  ))) %>%
  mutate(
    ID = 100001:101000,
    age = sample(c(5:12), 1000, replace = TRUE),
    age_range = case_when(age <= 8 ~ "5 to 8 yo",
                          T ~ "9 to 12 yo"),
    gender = sample(
      c("female", "male"),
      1000,
      replace = TRUE,
      prob = c(0.5, 0.5)
    ),
    educ = sample(
      c("no_HS", "HS_grad", "some_college", "BA_plus"),
      1000,
      replace = TRUE,
      prob = c(0.063, 0.247, 0.333, 0.357)
    ),
    ethnic = sample(
      c("hispanic", "asian", "black", "white", "other"),
      1000,
      replace = TRUE,
      prob = c(0.112, 0.058, 0.141, 0.646, .043)
    ),
    region = sample(
      c("northeast", "south", "midwest", "west"),
      1000,
      replace = TRUE,
      prob = c(0.081, 0.402, 0.257, 0.260)
    ),
    clin_status = sample(
      c("typ", "clin"),
      1000,
      replace = TRUE,
      prob = c(0.8, 0.2)
    )
  ) %>%
  select(ID:clin_status, i01:i50)

write_csv(unweighted_input,
          here("INPUT-FILES/unweighted-input.csv"),
          na = "")
