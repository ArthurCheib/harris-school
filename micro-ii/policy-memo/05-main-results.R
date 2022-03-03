
## Loading the necessary packages and installing them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, wesanderson, ggthemes, scales, data.table, lubridate, hrbrthemes, geosphere, leaflet, leaflet.extras)

## Getting the dataset created with the loop
df_final <- data.table::fread(input = "micro-ii/policy-memo/data/Cameras-accidents-distance.csv") %>% 
  as_tibble()

## Getting the dataset containing all information on speed tickets cameras
df_cameras_position <- fread(input = "micro-ii/policy-memo/data/cameras-main_info.csv") %>% 
  as_tibble()

## Adding distance in yards to the dataset
df_final <- df_final %>% 
  mutate(yards_distance = distance*1.09361)

# threshold for the accidents
distance_threshold <- 218

## Amount of fatal/severes injures within 300yards radius from the cameras
df_final %>% 
  mutate(less_yards = case_when(distance < distance_threshold ~ 1,
                                TRUE ~ 0)) %>% 
  group_by(year(DATE), less_yards) %>% 
  summarize(total_accidents = n_distinct(CRASH_RECORD_ID)) %>% 
  ungroup() %>% 
  group_by(total_accidents) %>% 
  mutate(percent_accidents = total_accidents/sum(total_accidents)*100)

## Vector containing only the accidents within the radius
severe_fatal_radius <- df_final %>% 
  filter(yards_distance <= distance_threshold) %>% 
  pull(CRASH_RECORD_ID) %>% 
  unique()

### QUESTIONS ###

## 1) How much of the fatal/severe accidents in 2020 occurred within a 300 yards radius from cameras?

### Total fatal/severe accidents in Chicago in 2020
total_accidents_2020 <- df_final %>% 
  filter(year(DATE) == 2020) %>% 
  pull(CRASH_RECORD_ID) %>% 
  unique() %>% 
  length()

### Total fatal/severe accidents in Chicago in 2020 within the range of the cameras
accidents_range_cameras_20 <- df_final %>% 
  filter(year(DATE) == 2020 & distance <= distance_threshold) %>% 
  pull(CRASH_RECORD_ID) %>% 
  unique() %>% 
  length()

### Answer
percent_2020 <- round(accidents_range_cameras_20/total_accidents_2020*100, digits = 1)

## 2) How much of the fatal/severe accidents in 2021 occurred within a 300 yards radius from cameras?

### Total fatal/severe accidents in Chicago in 2020
total_accidents_2021 <- df_final %>% 
  filter(year(DATE) == 2021) %>% 
  pull(CRASH_RECORD_ID) %>% 
  unique() %>% 
  length()

### Total fatal/severe accidents in Chicago in 2020 within the range of the cameras
accidents_range_cameras_21 <- df_final %>% 
  filter(year(DATE) == 2021 & distance < distance_threshold) %>% 
  pull(CRASH_RECORD_ID) %>% 
  unique() %>% 
  length()

### Answer
percent_2021 <- round(accidents_range_cameras_21/total_accidents_2021*100, digits = 1)

## Graph
pal <- wes_palette("Zissou1", type = "discrete")[c(1,5)]

df_histogram <- tibble(year = c(2020, 2020, 2021, 2021),
                       accident_classification = rep(c("Inside camera's range", "Far from camera's range"), 2),
                       total_accidents = c(175, 2011-175, 179, 2173-179),
                       percent_accidents_near = c(percent_2020/100, (100-percent_2020)/100, percent_2021/100, (100-percent_2021)/100))

df_histogram %>% 
  ggplot(aes(x = year, y = percent_accidents_near, fill = accident_classification)) + 
  geom_bar(stat = "identity",
           position = "fill", width = 0.75) +
  scale_fill_manual(values = pal) +
  theme_light() +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  geom_text(aes(label = str_c(percent_accidents_near*100, "%")), 
            size = 4.5, colour = "white", fontface = "bold",
            position = position_stack(vjust = 0.5)) +
  labs(y = "", 
       fill = "Accident localization",
       x = "",
       title = "Accidents near cameras vs. far from them",
       subtitle = "Accidents with fatal or severe injuries within a 218 yards radius of a speed camera",
       caption = "Source: Chicago Data Portal") +
  scale_x_continuous(breaks = c(2020, 2021)) +
  theme(
    text = element_text('Avenir Next Condensed', size = 14),
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major = element_line('white', size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()) +
  theme(legend.position = "bottom")
    
