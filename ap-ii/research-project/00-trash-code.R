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
  geom_smooth(method = "lm") +
  theme_light() +
  scale_x_continuous(breaks = c(1,2,3), labels = c("Right-wing", "Centrist", "Left-wing"))