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

## Making graph 1 (second attempt): investment X political parties
colnames(dataset)[34] <- "sec_exclusiva"

dataset %>% 
  select(Ano, CODMUN, partido, invest, investAluno, IDEB, TotalEscolas, sec_exclusiva) %>% 
  filter(partido != 0) %>% 
  mutate(amount_schools = case_when(TotalEscolas <= division[[2]] ~ "2 ou menos",
                                    TotalEscolas > division[[2]] & TotalEscolas <= division[[3]] ~ "Entre 3 e 6",
                                    TotalEscolas > division[[3]] & TotalEscolas <= division[[5]] ~ "Entre 6 e 15",
                                    TRUE ~ "Acima de 15")) %>% 
  filter(investAluno <= 60000 & investAluno > 1200 & Ano == 2018) %>% 
  filter(!amount_schools %in% c("2 ou menos")) %>%
  filter(sec_exclusiva != 0) %>% 
  mutate(partido = factor(case_when(partido == 1 ~ "Right-wing",
                             partido == 2 ~ "Centrist",
                             partido == 3 ~ "Left-wing"), levels = c("Left-wing", "Centrist", "Right-wing"))) %>% 
  group_by(partido) %>% 
  summarize(avg_investment = mean(investAluno)) %>% 
  ggplot(aes(x = partido, y = avg_investment/1000, fill = partido)) +
  geom_col() +
  theme_wsj() +
  scale_fill_tol("light") +
  scale_y_continuous(limits = c(0,30), breaks = c(seq(from = 0, to = 29, by = 5))) +
  labs(title ="Average Investment per Student in Brazilian municipalities",
       subtitle = "Amount invested in BRL currency (R$) - in thousands",
       y = "",
       fill = "",
       x = "",
       caption = "Source: Brazilian Government - TCE | MUNIC | Ministry of Education") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16.5),
        plot.subtitle = element_text(size = 13.5),
        plot.caption = element_text(size = 12)) +
  geom_text(aes(label = str_c("$", round(avg_investment/1000, digits = 0)), vjust = -0.5, size = 5))


## Data
dataset %>% 
  select(Ano, CODMUN, partido, invest, investAluno, IDEB, TotalEscolas, sec_exclusiva) %>% 
  filter(partido != 0) %>% 
  mutate(amount_schools = case_when(TotalEscolas <= division[[2]] ~ "2 ou menos",
                                    TotalEscolas > division[[2]] & TotalEscolas <= division[[3]] ~ "Entre 3 e 6",
                                    TotalEscolas > division[[3]] & TotalEscolas <= division[[5]] ~ "Entre 6 e 15",
                                    TRUE ~ "Acima de 15")) %>% 
  filter(investAluno <= 60000 & investAluno > 1200 & Ano == 2018) %>% 
  filter(!amount_schools %in% c("2 ou menos")) %>%
  filter(sec_exclusiva != 0) %>% 
  mutate(partido = factor(case_when(partido == 1 ~ "Right-wing",
                                    partido == 2 ~ "Centrist",
                                    partido == 3 ~ "Left-wing"), levels = c("Left-wing", "Centrist", "Right-wing"))) %>% 
  group_by(partido) %>% 
  summarize(avg_investment = mean(investAluno))
  
