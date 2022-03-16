# SPEED TICKETS - CAMERA

## Loading the necessary packages and installing them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, wesanderson, ggthemes, scales, data.table, lubridate, hrbrthemes)

## Getting the data of speed tickets in Chicago
speed_tickets <- data.table::fread(input = "micro-ii/policy-memo/data/Speed_Camera_Violations.csv") %>% 
  as_tibble() %>% 
  mutate(DATE = lubridate::mdy(`VIOLATION DATE`))

## Transforming the data before graphing
df_speed_tickets <- speed_tickets %>%
  mutate(DATE = lubridate::mdy(`VIOLATION DATE`)) %>% 
  filter(year(DATE) >= 2019) %>% 
  as_tibble() %>% 
  group_by(year(DATE), month(DATE)) %>% 
  summarize(total_violations = sum(VIOLATIONS)) %>% 
  setNames(c("year", "month", "total_violations")) %>% 
  unite("year_month", year:month, sep = "/") %>% 
  mutate(date = lubridate::ym(year_month)) %>% 
  select(-year_month)

## Getting the line for the graph
df_line_graph <- data.table::fread(input = "micro-ii/policy-memo/data/line-graph.csv")

## Checking the total violations that took place in Chicago - per year
df_speed_tickets %>%
  ungroup() %>% 
  filter(date < ymd("2022-02-1")) %>%
  mutate(total_violations = total_violations) %>% 
  group_by(year(date)) %>% 
  summarize(total_violations = sum(total_violations)) %>% 
  setNames(c("Year", "Total Violations"))

## Grapghing
df_speed_tickets %>%
  ungroup() %>% 
  filter(date < ymd("2022-02-1")) %>%
  mutate(total_violations = total_violations/1000) %>% 
  ggplot(aes(x = date, y = total_violations)) +
  geom_line(size = 0.65) +
  theme_ipsum() +
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y") +
  labs(title = "Speed Camera Violations",
       subtitle = "Monthly volume of traffic violations in Children's Safety Zones - Chicago",
       x = "",
       y = "Total of violations (in thousands)",
       caption = "Source: Chicago Data Portal") +
  geom_vline(xintercept = as.numeric(df_line_graph$date[38]), linetype = 2, colour ="black", size = 0.70) +
  annotate("text", x = df_line_graph$date[34], y = 320, label = "1st month after new policy: \n 326,083 tickets") +
  geom_hline(yintercept = 69.300, linetype = 2, colour = "blue", size = 0.70) +
  annotate("text", x = df_line_graph$date[44], y = 115,
           label = "Monthly average \n in 2019 and 2020: \n 69,300 tickets", colour = "blue") +
  geom_point(aes(x = ymd("2021-03-1"), y = 326.083), size = 4)

## Getting the numbers
df_speed_tickets %>%
  ungroup() %>% 
  filter(date < ymd("2022-02-1")) %>%
  mutate(total_violations = total_violations) %>% 
  group_by(year(date)) %>% 
  summarize(total_violations = mean(total_violations)) %>% 
  view
