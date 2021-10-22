library(tidyverse)
library(ggridges)
library(ggthemes)
library(tidytext)
library(geofacet)
theme_set(theme_tufte())


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
  separate(id, c("year", "type"), sep="-") %>%
  filter(!str_detect(country, ",")) %>% 
  mutate(type = fct_recode(type,
                         "Field Pumpkin" = "F",
                         "Giant Pumpkin" = "P",
                         "Giant Squash" = "S",
                         "Giant Watermelon" = "W",
                         "Long Gourd" = "L",
                         "Tomato" = "T"))
# Plot 1
pumpkins %>%
  ggplot(aes(y = type, x = weight_lbs, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", 
    calc_ecdf = TRUE,
    quantiles = 4,
    quantile_lines = TRUE,
    jittered_points = TRUE,
    position = position_raincloud(adjust_vlines = TRUE),
    point_size = 0.4,
    point_alpha = 0.2,
    vline_size = 0
  )

# Plot 2

pumpkins %>%
  ggplot(aes(weight_lbs, year)) +
  geom_boxplot(aes(fill = year), show.legend = F)

pumpkins %>%
  group_by(year, state_prov) %>%
  summarize(avg_weight = mean(weight_lbs, na.rm = T)) %>% 
  slice_max(avg_weight, n = 10) %>%
  ungroup() %>% 
  inner_join(pumpkins %>% 
               select(state_prov, country), by = "state_prov") %>% 
  distinct() %>% 
  mutate(state_prov = reorder_within(state_prov, avg_weight, year)) %>% 
  ggplot(aes(avg_weight, state_prov, fill = country)) +
  geom_col() +
  facet_wrap(. ~ year, scales = "free") +
  scale_y_reordered() + 
  theme(strip.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18)) +
  labs(x = "Average Pumpkin Weight",
       y = "",
       title = "Top 10 Yearly Average Pumpkin Weights per State/Province by Country")

# Plot 3

pumpkins %>%
  filter(country != "Unknown country") %>%
  group_by(year, type, country) %>%
  summarize(avg_weight = mean(weight_lbs, na.rm = T)) %>% 
  ungroup() %>% 
  complete(type, year, country, fill = list(avg_weight = 0)) %>%
  ggplot(aes(year, country, fill = avg_weight)) +
  geom_tile() +
  facet_wrap(~type, scales = "free") +
  scale_fill_gradient2(
    high = "green",
    low = "red",
    mid = "white",
    midpoint = 400
  )

# Plot 4
pumpkins %>%
  filter(country == "United States") %>%
  group_by(year, type, state_prov) %>%
  summarize(avg_weight = mean(weight_lbs, na.rm = T)) %>% 
  ungroup() %>%
  ggplot(aes(as.numeric(year), avg_weight, color = type)) +
  geom_line(size = 1) +
  facet_geo(~state_prov,
            label = "name",
            scales = "free") +
  scale_x_continuous(breaks = seq(2013,2021,3)) + 
  labs(x = "Year",
       y = "Average Weight",
       color = "Pumpkin Type",
       title = "State-wise Pumpkin Average Weight")

# A silly take
pumpkins_T100 <- tuesdata$pumpkins %>%
  filter(place != "EXH") %>% # Toss the exhibition pumps
  mutate(place = as.numeric(place), weight_lbs = parse_number(weight_lbs)) %>% 
  filter(place <= 100) %>% 
  drop_na(place) %>% 
  separate(id, into = c("year", "type")) %>% 
  filter(type != "T") %>% 
  filter(type != "L") %>%
  filter(type != "W")

ggplot(pumpkins_T100, aes(x = year, y = weight_lbs, colour = type)) +
  geom_jitter() +
  theme_dark()

# annapurani93's take

library(tidytuesdayR)
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2021-10-19')
data.frame(tuesdata$pumpkins)->pumpkin

pumpkin <- pumpkin[!grepl("Entries.", pumpkin$country),]


pumpkin <- pumpkin %>% 
  filter(place != "EXH") %>% 
  filter(place != "DMG") %>%
  mutate(weight_lbs = parse_number(weight_lbs)) %>% 
  separate(id, into = c("Year", "Type"), sep = "-") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Type= recode(Type,
                      "L"="Long Gourd (length in inches)",
                      "F"="Field Pumpkin (weight in pounds)",
                      "W"="Giant Watermelon (weight in pounds)",
                      "P"="Giant Pumpkin (weight in pounds)",
                      "S"="Giant Squash (weight in pounds)",
                      "T"="Tomato (weight in pounds)")) %>%
  filter(country != "Unknown country") %>% 
  drop_na(country) %>% 
  select(Year, Type, country, weight_lbs) %>%
  group_by(Year)

bySize <- pumpkin %>%
  group_by(Type) %>% 
  arrange(desc(weight_lbs), .by_group = TRUE) %>%
  slice_head(n = 100)

byCountry <- pumpkin %>%
  group_by(country) %>% 
  arrange(desc(weight_lbs), .by_group = TRUE) %>%
  slice_head(n = 100)
# jitterplots
bySize %>%
  ggplot(aes(Year, weight_lbs, colour = as.factor(Year))) +
  geom_jitter(alpha = 0.6, size = 1) +
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_y_continuous(limits=c(0,3000),breaks=c(0,500,1000,1500,2000,2500,3000))+
  scale_colour_manual(values=c("#F9C10E", "#FA4113","#BFDA7A","#5E32BA","#27C424","#BF200E",
                               "#F9804D","#F5D913","#FFF093"))+
  facet_wrap(~Type)+
  theme(plot.background = element_rect(fill="black"),
        panel.background=element_rect(fill="black"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour="white",face="bold"),
        axis.title = element_blank(),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",face="bold"),
        panel.grid.minor.x = element_line(colour="#C0C0C0",linetype = "dotted"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")+
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="THE GREAT PUMPKIN WEIGHOFF",
       subtitle = "The data visualization belows displays the top 100 pumpkins in each type, based on size, according to the Great Pumpkin Council",
       caption="Data from BigPumpkins.com | Design and analysis: @annapurani93" )+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5),
        plot.caption = element_text(colour = "white",size=8)) -> pumpkinplot2

byCountry %>%
  ggplot(aes(Year,weight_lbs,colour=as.factor(Type)))+
  geom_jitter(alpha=0.6, size=1)+
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_y_continuous(limits=c(0,3000),breaks=c(0,500,1000,1500,2000,2500,3000))+
  scale_colour_manual(values=c("#F9C10E", "#FA4113","#BFDA7A","#5E32BA","#27C424","#BF200E"))+
  facet_wrap(~country)+
  theme(plot.background = element_rect(fill="black"),
        panel.background=element_rect(fill="black"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour="white",face="bold"),
        axis.title = element_blank(),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",face="bold"),
        panel.grid.minor.x = element_line(colour="#C0C0C0",linetype = "dotted"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top",
        legend.background = element_rect("black"),
        legend.key = element_rect(fill="black"),
        legend.text = element_text(colour = "white", face="bold"))+
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="THE GREAT PUMPKIN WEIGHOFF",
       subtitle = "The data visualization belows displays the top 100 pumpkins in each country, based on size, according to the Great Pumpkin Council",
       caption="Data from BigPumpkins.com | Design and analysis: @annapurani93" )+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))+
  guides(color = guide_legend(override.aes = list(size = 4)))->pumpkinplot1
