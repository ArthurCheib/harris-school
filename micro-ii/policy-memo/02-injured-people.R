# CAR ACCIDENTS - INJURED PEOPLE

## Loading the necessary packages and installing them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, wesanderson, ggthemes, scales, data.table, lubridate, hrbrthemes, geosphere, leaflet, leaflet.extras)

# Data source: https://data.cityofchicago.org/Transportation/Traffic-Crashes-People/u6pd-qa9d

## Getting the data of people injured in the traffic by car crashes
df_people_traffic <- data.table::fread(input = "micro-ii/policy-memo/data/Traffic_Crashes_-_People.csv") %>%
  separate(col = CRASH_DATE, into = c("DATE", "TIME", "MORNING_NOON"), sep = " ") %>% 
  unite("TIME", TIME:MORNING_NOON, sep = " ") %>% 
  mutate(DATE = lubridate::mdy(DATE),
         DATE_TIME = lubridate::hms(TIME)) %>% 
  filter(year(DATE) >= 2018)

## Types of accidents
types_of_accidents <- df_people_traffic %>% pull(INJURY_CLASSIFICATION) %>% unique()

## Transforming the data before graph
df_line_graph <- df_people_traffic %>%
  filter(INJURY_CLASSIFICATION %in% c(types_of_accidents[4], types_of_accidents[5])) %>% 
  select(PERSON_ID, DATE, VEHICLE_ID, DRIVERS_LICENSE_STATE, INJURY_CLASSIFICATION) %>% 
  group_by(year(DATE), month(DATE)) %>% 
  summarize(people_injured = n_distinct(PERSON_ID)) %>% 
  setNames(c("year", "month", "people_injured")) %>% 
  unite("year_month", year:month) %>% 
  mutate(date = lubridate::ym(year_month)) %>% 
  select(-year_month)

## Line graph to check evolution of injuries in time
df_line_graph  %>%
  ungroup() %>% 
  filter(date < ymd("2022-02-1")) %>%
  ggplot(aes(x = date, y = people_injured)) +
  geom_line() +
  theme_ipsum() +
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y") +
  labs(title = "Traffic Crashes - People",
       subtitle = "Monthly volume of people involved in a car crash (Fatal or severe injuries) - Chicago",
       x = "",
       y = "Total of injured people") +
  geom_vline(xintercept = as.numeric(df_line_graph$date[39]), linetype = 2, colour ="black") +
  annotate("text", x = df_line_graph$date[34], y = 300, label = "New Policy \n Implementation")


