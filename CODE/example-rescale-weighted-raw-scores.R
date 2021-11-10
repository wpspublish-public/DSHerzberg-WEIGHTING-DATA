suppressMessages(suppressWarnings(library(tidyverse)))

# Read example data from TOD-E. The input is simplified: person ID, raw total
# score for segmentation (sege_sum), and weighted raw score (sege_sum_w). TOD-E
# segmentation has 25 pass-fail items, so the normative tables must provide a
# standard score corresponding to each of the possible raw score values (0-25).
# If you examine the distribution of sege_sum_w, you'll see that it comprises
# score values > 25. This is the rescaling problem: how do we transform
# sege_sum_w back to the 0-25 metric, so that it can be used with cNORM to
# generate normative lookup tables?

urlRemote_path  <- "https://raw.github.com/"
github_path <- "wpspublish/DSHerzberg-WEIGHTING-DATA/master/INPUT-FILES/"
fileName_path   <- "example-rescale-weighted-raw-scores-input.csv"

input <- suppressMessages(read.csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

rescale <- input %>% 
  mutate(
    sege_sum_w_rescale = 
    )
