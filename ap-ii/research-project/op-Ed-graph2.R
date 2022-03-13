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
  select(Ano, CODMUN, partido, invest, investAluno, IDEB, TotalEscolas, sec_exclusiva, PME) %>% 
  filter(partido != 0) %>% 
  mutate(amount_schools = case_when(TotalEscolas <= division[[2]] ~ "2 ou menos",
                                    TotalEscolas > division[[2]] & TotalEscolas <= division[[3]] ~ "Entre 3 e 6",
                                    TotalEscolas > division[[3]] & TotalEscolas <= division[[5]] ~ "Entre 6 e 15",
                                    TRUE ~ "Acima de 15")) %>% 
  filter(investAluno <= 60000 & investAluno > 1200) %>% 
  filter(!amount_schools %in% c("2 ou menos")) %>%
  filter(sec_exclusiva != 0)

## Making the 2nd graph - IDEB x PME
dataset_graph %>% 
  group_by(Ano, PME) %>% 
  summarize(avg_ideb = round(mean(IDEB), digits = 2)) %>% 
  ungroup() %>% 
  mutate(PME = factor(case_when(PME == 1 ~ "Adopted PME",
                         TRUE ~ "Hasn't adopted PME"),levels = c("Hasn't adopted PME", "Adopted PME"))) %>% 
  ggplot(aes(x = PME, y = avg_ideb, fill = PME)) +
  geom_col() +
  theme_wsj() +
  facet_wrap(~Ano) +
  scale_fill_manual(values = c("#9999CC", "#66CC99")) +
  scale_y_continuous(limits = c(0,6), breaks = c(seq(from = 0, to = 6, by = 1))) +
  labs(title ="IDEB variation according to the presence of PME",
       subtitle = "Standardized tests scores difference between the level of management capacity",
       y = "",
       fill = "",
       x = "",
       caption = "Source: Brazilian Government - MUNIC | Ministry of Education") +
  geom_text(aes(label = avg_ideb), vjust = -0.5, size = 5) +
  theme(legend.position = "none",
        plot.title = element_text(size = 16.5),
        plot.subtitle = element_text(size = 13.5),
        plot.caption = element_text(size = 12))
  

### Total municipalities implemented x not implemented
dataset_graph %>% 
  group_by(Ano, PME) %>% 
  summarize(total_implemented = n()) %>%
  mutate(perc_implemented = round(total_implemented/sum(total_implemented)*100, digits = 1)) %>% 
  ungroup() %>% 
  mutate(PME = factor(case_when(PME == 1 ~ "Adopted PME",
                                TRUE ~ "Hasn't adopted PME"),levels = c("Hasn't adopted PME", "Adopted PME"))) %>% 
  ggplot(aes(x = PME, y = perc_implemented, fill = PME)) +
  geom_col() +
  theme_wsj() +
  facet_wrap(~Ano) +
  scale_fill_manual(values = c("#9999CC", "#66CC99")) +
  scale_y_continuous(limits = c(0, 100), breaks = c(seq(from = 0, to = 100, by = 20))) +
  labs(title = "PME implementation in Brazilian Municipalities",
       subtitle = "Values for 2014 and 2018",
       y = "",
       fill = "",
       x = "",
       caption = "Source: Brazilian Government - MUNIC | Ministry of Education") +
  geom_text(aes(label = perc_implemented), vjust = -0.5, size = 5) +
  theme(legend.position = "none",
        plot.title = element_text(size = 16.5),
        plot.subtitle = element_text(size = 13.5),
        plot.caption = element_text(size = 12))

