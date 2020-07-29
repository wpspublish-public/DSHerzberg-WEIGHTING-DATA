hux(weighted_output %>% 
      filter(
        gender == "female" &
          educ == "no_HS" &
          ethnic == "hispanic" &
          region == "northeast"
      ) %>% 
      select(-age_range, -clin_status, -(i01_w:i50_w), -TOT_raw_weight) %>% 
      sample_n(1)) %>% 
  set_number_format(everywhere, 1, NA) %>% 
  set_bottom_border(brdr(1, "solid", "blue"))

number_format(test)[, 1] <- NA


hux_format <- function(x) {
  hux(x) %>%
    set_number_format(everywhere, 1, NA)  %>%
    set_bottom_border(brdr(1, "solid", "blue")) %>%
    set_bottom_border(1, everywhere, brdr(2, "solid", "black")) %>%
    set_bold(1, everywhere) %>%
    set_background_color(evens, everywhere, "grey95")
}

temp <- head(input_demo_wts) %>% 
  select(-age_range, -clin_status, -(i01:i50), -ratio)

cat_count_comp %>% mutate(
  across(var,
    ~ case_when(
      lag(.x) == .x ~ NA_character_,
      T ~ .x
    )     
  )
) %>% 
  hux_format()
