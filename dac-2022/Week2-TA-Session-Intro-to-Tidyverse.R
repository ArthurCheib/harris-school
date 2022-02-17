
### Schedule for today

## 1) Go through the five main functions that Prof. Austin cover on this week lecture on R + rexplanation of the pipe operator
## 2) Introduction to function "lag" - which can be pretty useful for comparisons
## 3) Some exercises with gapminder dataset

## Gapminder = a dataset containing "Life expectancy" + "Population" + "GDP per capita"

## GOAL 1 ##

## Opening tidyverse package into our current RStudio session
library(tidyverse)

## Download the "gapminder" dataset to our computer
install.packages("gapminder")

## Opening the gapminder package into our current RStudio session
library(gapminder)

## Creating an object called "dataset_gapminder" in our Environment that corresponds to the 'gapminder" dataset, 
## which is located inside the gapminder package
gap_dataset <- gapminder::gapminder


########## Syntax and Overview #########

## Today we will focus on the five main functions (also known as "verbs") of the dplyr packages 

# filter: pick rows matching criteria           - very common
# select: pick columns by name                  - common
# arrange: reorder rows                         - common
# mutate: add new variables                     - very common
# group_by: create groups using row values      - very common
# summarize: reduce variables to values         - very common


## But first, a note on a very important tidyverse syntax: THE PIPE OPERATOR ( %>% )

##### Pipe Operator (%>%)


## Definition: the pipe operator is a special operational function which allows
############## us to pass the result of one function/argument to the other one in sequence

## As you probably have notice in Prof. Austin, all five dplyr functions have *data frame* as their first argument
## Thus, the pipe will allow to use functions (or make transformations) in our dataset in a step-by-step way. Always
## using as input, the dataframe that some other functions treated and launched as an output. 

## Objective: do transformations in sequence, after telling R, only one time, upon which dataframe we're working on

## Let's start!

##### Filter #####

# Definition: filter() will return a subset of rows based on a logical condition(s)

# Filter the gapminder dataset for the year 1992
gapminder %>%
  filter(year == 1992)

# Filter for Belgium in 1997
gapminder %>%
  filter(country == "Belgium" & year == 1997)
  
#  ---- YOUR TURN ----

# How many countries had a Life Expectancy higher than 75 in 1977?
gapminder %>% 
  filter(lifeExp > 75 & year == 1977)

#  ----


##### Arrange #####

# Definition: arrange() is used to order data according to its values. You can order by multiple columns.
# The default is ascending order.

## Arrange the dataset by year
gapminder %>% 
  arrange(year)

## Order the dataset by year and then by continent
gapminder %>% 
  arrange(year, continent, desc(pop))


#  ---- YOUR TURN ----

# What were the countries with the highest population in 1957?
gap_dataset %>% 
  filter(year == 1957) %>% 
  arrange(desc(pop))

#  ----


##### Select #####

# Definition: select() will allow you to select columns from your tibble/dataframe.
# It is very versatile and includes a number of helper functions


## Get only the "country" column from the dataset
gapminder %>% 
  select(country)

## Get everything but the "country" column from the dataset
gapminder %>% 
  select(-country)

## Get only the first three columns of our dataset
gapminder %>% 
  select(1:3)

## Get me everything but last column of my dataset
gapminder %>% 
  select(-6)

## Combining two functions to do the same as we did above
gapminder %>% 
  select(-ncol(gapminder))

#  ---- YOUR TURN ----

# Get only the columns one, three and four of our dataset
gapminder %>% 
  select(1, 3, 4)

#  ----


##### Mutate #####

# Definition: mutate() creates new columns/variables and appends them to your dataframe.

# It uses the following syntax:
dataset %>% 
  mutate(new_column_name = value_of_the_new_column)

# Create a column that gives us population in millions:
gapminder %>% 
  mutate(pop_thousands = pop/1000000,
         pop_thousands_10 = pop_thousands*10)

#  ---- YOUR TURN ----

# Use mutate to change lifeExp to be in months (called the new column "lifeExpMonths")
gap_dataset %>% 
  mutate(lifeExpMonths = lifeExp/12)

# Use mutate to calculate the GDP of the countries (GDP = pop * gdpPercap) and create a new column transforming it into billions
gap_dataset %>% 
  mutate(gdp = pop * gdpPercap/1000000000)

#  ----

##### Group By / Summarize #####

# Definition: group_by() will create groups in the dataframe based on the distinct values in the columns you provide it.
# These groups will be "invisible" in the dataframe.
# You won't see them until you perform some sort of summarization operation on the dataframe.

## Group the data by continent
gapminder %>% 
  group_by(continent)

## Group the data by continent and year
gapminder %>% 
  group_by(continent, year)

## Remember: for group_by function the orders matters

# As said by Prof. Austin:
# group_by is almost always used with summarize().
# summarize() will apply some sort of summary function to your dataset or to the groups in your dataset.
# It MUST be used with functions that reduce a vector to a single number, think mean(), sum(), median().
# The syntax of summarize is the same as mutate():

## Getting the mean life expectancy for each of our continents inside the data frame
gapminder %>% 
  group_by(continent, year) %>% 
  summarize(avg_lifeExp = mean(lifeExp))

## Results are not meaningful - let's try it by year
gapminder %>% 
  group_by(continent, year) %>% 
  summarize(avg_lifeExp = mean(lifeExp))

## Let's be more specific - only for 1952 and 2007
gapminder %>% 
  filter(year %in% c(1952, 2007)) %>% 
  group_by(continent, year) %>% 
  summarize(avg_lifeExp = mean(lifeExp))

#  ---- YOUR TURN --- 

# What was the average(mean) gdpPercap for countries in the Americas continent in the years of 2002 and 2007
gapminder %>%
  filter(year %in% c(2002,2007)) %>%
  filter(continent == 'Americas') %>% 
  group_by(continent, year) %>%
  summarise(avg_gdpcap = mean(gdpPercap))


#  ----


## GOAL 2 ##

### Introduction to lag() function
# Definition: the lag function will "find" the previous value of our dataset
# In a column view, the lag function will "delayed" our data by a total of n rows (we got to choose n)
gapminder %>% 
  arrange(country, year) %>% 
  select(country, year, lifeExp) %>% 
  mutate(previous_lifeExp = lag(lifeExp),
         has_improved = case_when(lifeExp > previous_lifeExp ~ 1,
                                  TRUE ~ 0))


#  ---- YOUR TURN ----

## What was the change (=by how much) experienced by each country on life expectancy per group of five years (the dataset is divided in 5-5 years)? 
gapminder %>%
  arrange(country, year) %>%
  select(country, year, lifeExp) %>%
  mutate(prev_lifeExp = lag(lifeExp), 
         change_lifeExp = (lifeExp - prev_lifeExp)/lifeExp*100)


#  ----


## EXTRA ##
library(ggplot2)

# Create a scatter plot showing the change in mean life expectancy over time
gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp)) %>% 
  ggplot2::ggplot(aes(x = year, y = meanLifeExp)) +
  geom_point() +
  expand_limits(y = 0)

# Summarize mean gdp per capita within each continent within each year and call the new dataframe "gapminder_by_year_cont"
gapminder_by_year_cont <- gapminder %>%
  group_by(continent, year) %>%
  summarize(meanGdpPercap = mean(gdpPercap))

# Plot the change in mean gdp per capita in each continent over time
gapminder_by_year_cont %>% 
  ggplot(aes(x = year, y = meanGdpPercap, color = continent)) +
  geom_point(size = 6) +
  expand_limits(y = 0) +
  labs(title = "Mean gdp per capita - per continent",
       subtitle = "DAC-2022")






test <- gap_dataset %>%
  pull(pop, country)


test["Mongolia"]

test[c(1,2,3)]

  