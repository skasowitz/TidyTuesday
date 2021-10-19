library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2021, week = 43)
pumpkins <- tuesdata$pumpkins

glimpse(pumpkins)

pumpkins <- pumpkins %>%
  mutate(
    across(weight_lbs, parse_number),
    across(place, as.numeric),
    across(ott, as.numeric),
    across(est_weight, as.numeric),
    across(pct_chart, as.numeric)
  ) %>%
  drop_na(place) %>%
  separate(id, c("year", "type"), sep="-")


