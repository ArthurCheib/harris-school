## Getting packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, wesanderson, ggthemes, scales, allespaletti)

## Getting the data from Nicolas's TCC
url_file <- "https://raw.githubusercontent.com/ArthurCheib/harris-school/main/ap-ii/research-project/data/research-data.csv"

dataset <- read.csv(url_file) %>% 
  as_tibble()

### Getting the values of the boxplot
division <- summary(dataset$TotalEscolas)

division[[2]]
division[[3]]
division[[5]]

## Cleaning the dataset
colnames(dataset)[34] <- "sec_exclusiva"

dataset_graph <- dataset %>% 
  select(Ano, CODMUN, partido, invest, investAluno, IDEB, TotalEscolas, sec_exclusiva) %>% 
  filter(partido != 0) %>% 
  mutate(amount_schools = case_when(TotalEscolas <= division[[2]] ~ "2 ou menos",
                                    TotalEscolas > division[[2]] & TotalEscolas <= division[[3]] ~ "Entre 3 e 6",
                                    TotalEscolas > division[[3]] & TotalEscolas <= division[[5]] ~ "Entre 6 e 15",
                                    TRUE ~ "Acima de 15")) %>% 
  filter(investAluno <= 60000 & investAluno > 1200 & Ano == 2018) %>% 
  filter(!amount_schools %in% c("2 ou menos")) %>%
  filter(sec_exclusiva != 0)

## Making the 2nd graph - IDEB x PME
dataset_graph