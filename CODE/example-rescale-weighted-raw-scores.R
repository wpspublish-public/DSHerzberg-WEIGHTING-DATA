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

# Apply the rescaling transform to sege_sum_w. The expression below first
# transforms sege_sum_w to a POM (proportion of maximum) with:
#      sege_sum_w / max(sege_sum_w)
# we then rescale the POM back the metric of sege_sum (original raw score distribution) with:
#      * max(sege_sum)
input <- input %>% 
  mutate(
    sege_sum_w_rescale = round((sege_sum_w / max(sege_sum_w)) * max(sege_sum))
    )

# sege_sum_w_rescale is a variable that can be an input to cNORM to generate appropriate
# raw-to-standard score lookup tables.


# As we discussed in the meeting, my goal is to determine whether this rescaling
# transformation, using the POM method, does or does not distort the weighting
# adjustment embodied in sege_sum_w. I recognize this is a conceptually
# difficult question to answer, but I'm pushing up against the limits of my
# knowledge and comfort zone here. So any insight, visualizations, analysis you
# can provide on this question is appreciated. I'm not wedded to the POM
# transformation, it's just what I've come up with at the moment. If you can
# implement a linear transformation in a way that achieves the same goal, and
# you can demonstrate that it preserves the distributional properties of
# sege_sum_w better than the POM transformation, I'm happy to use your method.
