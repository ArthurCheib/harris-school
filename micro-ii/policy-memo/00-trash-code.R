# 2022-02-24 ------------------------------ First graph made out of the total traffic crashes dataset

## Loading the necessary packages and installing them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, wesanderson, ggthemes, scales, data.table, lubridate, hrbrthemes, geosphere, leaflet, leaflet.extras)

# CAR ACCIDENTS - TRAFFIC CRASHES

## Getting the data of people injured in the trafic by car crashes
df_car_crash <- data.table::fread(input = "micro-ii/policy-memo/data/Traffic_Crashes_-_Crashes.csv") %>%
  separate(col = CRASH_DATE, into = c("DATE", "TIME", "MORNING_NOON"), sep = " ") %>% 
  unite("TIME", TIME:MORNING_NOON, sep = " ") %>% 
  mutate(DATE = lubridate::mdy(DATE),
         DATE_TIME = lubridate::hms(TIME)) %>% 
  filter(year(DATE) >= 2018)

## Transforming the data before graph
df_line_graph2 <- df_car_crash %>%
  dplyr::select(CRASH_RECORD_ID, DATE, POSTED_SPEED_LIMIT, DAMAGE, INJURIES_TOTAL) %>% 
  group_by(year(DATE), month(DATE)) %>% 
  summarize(total_crashes = n_distinct(CRASH_RECORD_ID)) %>% 
  setNames(c("year", "month", "total_crashes")) %>% 
  unite("year_month", year:month) %>% 
  mutate(date = lubridate::ym(year_month)) %>% 
  dplyr::select(-year_month)


# 2022-02-24 ------------------------------

## 1) How many cameras in Chicago reported no fatal/severe injures within a 300yards radius in 2020?
fatal_severe_300yards <- df_final %>% 
  filter(yards_distance < accidents_threshold) %>% 
  pull(`CAMERA ID`) %>% 
  unique()

no_accidents_cameras <- df_final %>% 
  filter(!`CAMERA ID` %in% fatal_severe_300yards) %>% 
  count(`CAMERA ID`) %>% 
  pull(`CAMERA ID`)

print(no_accidents_cameras %>% unique %>% length())


# 2022-02-24 ------------------------------


## Line graph to check evolution of injuries in time
df_line_graph2  %>%
  ungroup() %>% 
  filter(date < ymd("2022-02-1")) %>%
  ggplot(aes(x = date, y = total_crashes)) +
  geom_line() +
  theme_ipsum() +
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y") +
  labs(title = "Traffic Crashes - car crashes",
       subtitle = "Monthly volume of car crashes - Chicago",
       x = "",
       y = "Total of car crashes") 


 # 2022-02-24 ------------------------------


## 5) How much of the fatal/severe accidents occurred within a 300 yards radius from prime cameras?

### Total fatal/severe accidents in Chicago in 2020
total_accidents_2020 

### Total fatal/severe accidents in Chicago in 2020 within the range of the prime cameras
accidents_range_prime <- df_final %>% 
  filter(year(DATE) == 2020 & `CAMERA ID` %in% prime_cameras & distance < distance_threshold) %>% 
  pull(CRASH_RECORD_ID) %>% 
  unique() %>% 
  length()

### Answer
round(accidents_range_prime/total_accidents_2020, digits = 2)

# 2022-02-24 ------------------------------

 