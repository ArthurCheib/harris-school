
### Schedule for today

## 1) Go through the main joins functions that Prof. Austin cover on this week lecture on R
## 2) Explanation of function pivot_longer and pivot_wider (superseded version of spread/gather)
## 3) Quick exercises with separate/unite functions

# Always start with the libraries you plan to use in your script!
library(tidyverse)
library(gapminder)

# We will also need the data in the package nycflights13
install.packages("nycflights13")
library(nycflights13)

##### Joins #####

# Join = merge of datasets

# Typically you have many tables of data, and you must combine them to answer the questions that you're
# interested in. Joins allow you to do that, they will merge two (or more) datasets together.

# The types of joins that we care about are:
# Mutating joins - add new variables to one tibble from matching values in another
# Filtering joins - filter observations from one tibble based on whether or not they match a value in another

# Datasets are connected by 'keys', which are shared values across each tibble.
# You can use these keys to join different rows of data together.

# Join in R = VLOOKUP or HLOOKUP in Excel (for intuition/reference)

# Subset our data, keeping only the columns we care about
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

### Mutating joins
data(airlines)

# Join the full airline name onto flights2 by the carrier column
# A left join keeps all observations in the left tibble
airlines <- airlines %>% 
  setNames(c("ID", "name"))

result <- flights2 %>%
  left_join(airlines, by = c("origin" = "ID"))

flights2 <- flights2 %>% 
  filter(carrier !=  "9E")

# A right join keeps all observations in the right tibble
result3 <- flights2 %>%
  right_join(airlines, by = "carrier")

# A full join will keep all observations from both sides
# An inner join will keep only the observations that match for both

data("weather")

# By default, joins will search for shared columns between datasets
temp <- flights2 %>%
  left_join(weather)

data(planes)

# But often, it is better to join datasets explicitly by specifying their key values
flights2 %>%
  left_join(planes)

flights2 %>%
  left_join(planes, by = "tailnum")

# If two datasets share the same information but have different column names, you can specific how to join them with a named list:
flights2 %>%
  left_join(airports, by = c("dest" = "faa"))

### Filtering joins

# A semi join keeps all values in the left side that match the right
# An anti join drops all values in the left side that match the right

# Semi joins are an easy way to filter datasets using the results of some sort of summary
top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10) %>%
  mutate(origin = dest)

temp <- flights2 %>%
  left_join(top_dest, by = "dest")

# This will yield only flights to the top 10 destinations
temp1 <- flights %>%
  semi_join(top_dest, by = "dest")

temp2 <- flights %>%
  anti_join(top_dest, by = "dest")

## QUESTION: Filter flights to only show flights with planes that have flown at least 100 flights.

## QUESTION: Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns? We need to be careful here; are we selecting on the dependent variable?

flights2 %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n >= 100 & !is.na(tailnum))

worst_hours <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(dep_delay)) %>%
  slice(1:48)

weather %>%
  semi_join(
    worst_hours,
    by = c("origin", "year", "month", "day", "hour"
    )
  )

##### Gather/Spread #####

# pivot_longer = gather

flights2

# pivot_wider = spread

# Let's first load a built-in R dataset. This data starts in the wide format, there are columns that could be treated as observations
data("gapminder")

gapminder2 <- gapminder %>% 
  as_tibble() %>% 
  pivot_longer(names_to = "main_indicators", values_to = "values", 4:6)

# We can convert data from wide to long using pivot_wider
gapminder3 <- gapminder2 %>% 
  pivot_wider(names_from = main_indicators, values_from = values)



# And can convert from long back to wide using spread()
airquality_long %>%
  spread(key = type, value = measurement)

# Let's try doing the same for the PhD data available on Canvas
phds <- read_csv("phds.csv")

## QUESTION: How would we convert this PhD data from wide to long?
## CHALLENGE: Generate a new column(s) that contains the YoY percentage change for each PhD field.

## Cleaning example and answer to question 1
phds_long <- phds %>%
  gather(key = year, value = n_phds, `2008.0`:`2017.0`) %>%
  mutate(
    year = as.integer(year),
    n_phds = parse_number(n_phds)
  )

# Answer to challenge question
phds_long %>%
  group_by(field) %>%
  mutate(pct_change = (n_phds - lag(n_phds)) / lag(n_phds)) %>%
  arrange(field) %>%
  mutate(pct_change = replace_na(pct_change, 0))

phds_long %>%
  group_by(year) %>%
  summarize(mean_phds = sum(n_phds, na.rm = T)) %>%
  ggplot() +
  geom_line(aes(x = year, y = mean_phds))

##### Separate/Unite #####

# Unite() will combine the selected columns into a new column. For example, here I combine the year, month, and day of the flights dataset into a new column called date
united <- flights2 %>%
  unite("date", year, month, day)

# Separate() is the opposite of unite. It will divide a column into multiple columns, splitting on some separator that you specify.
united %>%
  separate(date, into = c("year", "month", "day"), sep = "_", remove = FALSE)

# QUESTION: Use unite(), mutate(), and as.Date() to convert the year, month, and day columns of flights2 into a date column


# mutate
# filter
# arrange
# group_by
# summarize
# left_join
# anti_join
# pivot_wider
# pivot_longer
# separate
# unite




