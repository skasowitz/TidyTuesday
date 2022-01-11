# TidyTuesday 2022 Week 2

library(tidyverse)
library(lubridate)
library(geofacet)
library(camcorder)
library(ggokabeito)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

glimpse(colony)
skimr::skim(colony)
glimpse(stressor)

# rows with months == 2019 are all Q2 2019. Can correct that but there is no data in colony or stressor tables regardless
# makes more sense to filter out

col_data <- colony %>%
  filter(months != "2019") %>%
  mutate(quarter = case_when(
    months == "January-March" ~ "Q1",
    months == "April-June" ~ "Q2",
    months == "July-September" ~ "Q3",
    months == "October-December" ~ "Q4"
  )) %>%
  unite(yearquarter, c("year","quarter")) %>%
  mutate(date = yq(yearquarter))



stress_data <- stressor %>%
  filter(months != "2019") %>%
  mutate(quarter = case_when(
    months == "January-March" ~ "Q1",
    months == "April-June" ~ "Q2",
    months == "July-September" ~ "Q3",
    months == "October-December" ~ "Q4"
  )) %>%
  unite(yearquarter, c("year","quarter")) %>%
  mutate(date = yq(yearquarter))

stress_data <- stress_data %>%
  replace_na(list(stress_pct = 0)) %>%
  pivot_wider(names_from = stressor, values_from = stress_pct, values_fill = 0) %>%
  unite(id, c("state","yearquarter")) %>%
  rename(Diseases = Disesases)

data <- col_data %>%
  unite(id, c("state","yearquarter"), remove = FALSE) %>%
  left_join(stress_data %>%
              select(-months,-date)) %>%
  pivot_longer(c("Varroa mites":"Unknown"), names_to = "stressor", values_to = "stress_pct")

data$stressor <- factor(data$stressor, levels = c("Varroa mites", "Diseases", "Pesticides",
                                                  "Other pests/parasites", "Other", "Unknown"))

gg_record(dir = file.path(tempdir(), "recording_plot1"),
          device = "png",
          width = 8,
          height = 12)

p1 <- data %>%
  ggplot(aes(x = date, y = stress_pct)) +
  geom_area(aes(fill = stressor),
            color = "lightgrey",
            alpha = 0.6) +
  facet_geo(~ state, grid = "us_state_grid1") +
  scale_x_date(date_labels = "'%y",
               date_breaks = "1 year") +
  scale_fill_okabe_ito() +
  labs(title = "Bee Colony Stressors in the United States",
       x = "Year",
       y = "Colonies affected by stressor (%)",
       fill = "Stressor")


gg_playback(
  name = file.path(tempdir(), "recording_plot1", "p1_gif.gif"),
  first_image_duration = 10,
  last_image_duration = 20,
  frame_duration = 0.5
)
