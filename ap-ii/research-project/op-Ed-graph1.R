## Getting packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, wesanderson, ggthemes, scales, allespaletti)
  
## Getting the data from Nicolas's TCC
url_file <- "https://raw.githubusercontent.com/ArthurCheib/harris-school/main/ap-ii/research-project/data/research-data.csv"

dataset <- read.csv(url_file) %>% 
  as_tibble()

###
division <- summary(dataset$TotalEscolas)

division[[2]]
division[[3]]
division[[5]]

## Making graph 1: investment X political parties
dataset %>% 
  select(Ano, CODMUN, partido, invest, investAluno, IDEB, TotalEscolas) %>% 
  filter(partido != 0) %>% 
  mutate(amount_schools = case_when(TotalEscolas <= division[[2]] ~ "2 ou menos",
                                    TotalEscolas > division[[2]] & TotalEscolas <= division[[3]] ~ "Entre 3 e 6",
                                    TotalEscolas > division[[3]] & TotalEscolas <= division[[5]] ~ "Entre 6 e 15",
                                             TRUE ~ "Acima de 15")) %>% 
  filter(investAluno <= 60000 & investAluno > 1200 & Ano == 2018) %>% 
  filter(!amount_schools %in% c("2 ou menos")) %>%
  ggplot(aes(x = partido, y = investAluno)) +
  geom_point() +
  #facet_wrap(~amount_schools) +
  geom_smooth(method = "lm") +
  theme_light() +
  scale_x_continuous(breaks = c(1,2,3), labels = c("Right-wing", "Centrist", "Left-wing"))
  

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
  ggplot(aes(x = partido, y = avg_investment, fill = partido)) +
  geom_col() +
  theme_wsj() +
  scale_fill_tol("light") +
  scale_y_continuous(limits = c(0,25000), breaks = c(seq(from = 0, to = 25000, by = 5000))) +
  labs(title ="Average Investment per Student in Brazilian municipalities",
       subtitle = "Amount invested in BRA currency - R$",
       y = "",
       fill = "",
       x = "",
       caption = "Source: Brazilian Federal Government") +
  theme(legend.position = "none",
        plot.title = element_text(size = 14.5),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 12))

