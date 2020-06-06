library(ggrepel)
library(scales)
library(lubridate)
library(tidyverse)

#### Import Data ####
# GK Stats 2011-2019
gk_table <- read_csv('ASAkeepertable.csv') %>%
  select(first = First, last = Last, keeper = Keeper, team = Team, season = Season,
         min = Min, shots = Shots, goals = Goals, saves = Saves, header_pct = 'Header%',
         dist = Dist, xG, G_xG = 'G-xG', goals_per_shot = 'Goals/Shot', xG_per_shot = 'xG/Shot',
         G_xG_per_shot = 'G-xG/Shot', comp_k = 'Comp ($K)', plotnames)

# Salaries by team 2015-2019
salaries2015 <- read_csv('ASA_TeamSalaries2015.csv') %>% mutate(season = 2015)
salaries2016 <- read_csv('ASA_TeamSalaries2016.csv') %>% mutate(season = 2016)
salaries2017 <- read_csv('ASA_TeamSalaries2017.csv') %>% mutate(season = 2017)
salaries2018 <- read_csv('ASA_TeamSalaries2018.csv') %>% mutate(season = 2018)
salaries2019 <- read_csv('ASA_TeamSalaries2019.csv') %>% mutate(season = 2019)
salaries <- bind_rows(salaries2015, salaries2016, salaries2017, salaries2018, salaries2019) %>%
  filter(Team != 'NONE')
names(salaries) <- tolower(names(salaries))
salaries$n[2] <- 26

# League Table 2015-2019
table2015 <- read_csv('table2015.csv') %>% mutate(season = 2015)
table2016 <- read_csv('table2016.csv') %>% mutate(season = 2016)
table2017 <- read_csv('table2017.csv') %>% mutate(season = 2017)
table2018 <- read_csv('table2018.csv') %>% mutate(season = 2018)
table2019 <- read_csv('table2019.csv') %>% mutate(season = 2019)
table <- bind_rows(table2015, table2016, table2017, table2018, table2019)
names(table) <- tolower(names(table))
names(table)[2] <- 'team'

#### GK xG and compensation ####
# G-xG vs. compensation
left_join(gk_table, salaries %>% select(team, season, totalguar), by = c('team', 'season')) %>%
  filter(season %in% c(2015:2019)) %>%
  mutate(gk_comp_prc = 100*1000*comp_k/totalguar) %>%
  ggplot(aes(x = gk_comp_prc, y = -G_xG, color = factor(season))) +
  geom_point() +
  geom_text_repel(aes(label = ifelse(gk_comp_prc > 10, plotnames,
                               ifelse(-G_xG > 6 | -G_xG < -6, plotnames, NA))),
                  size = 3)+
  geom_hline(aes(yintercept = 0), alpha = .5, linetype = 'dashed', size = 1)+
  scale_x_log10() +
  theme_classic()+
  theme(axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'))+
  labs(title = 'xG Faced - Goals Allowed vs. GK Compensation Percentage',
       y = 'xG - G',
       x = 'GK compensation as a percentage of team spending',
       color = 'Season')


left_join(gk_table, salaries %>% select(team, season, totalguar), by = c('team', 'season')) %>%
  filter(season %in% c(2015:2019)) %>%
  filter(last != 'Howard') %>%
  mutate(gk_comp_prc = 100*1000*comp_k/totalguar) %>%
  ggplot(aes(x = gk_comp_prc, y = -G_xG, color = factor(season))) +
  geom_point() +
  geom_text_repel(aes(label = ifelse(gk_comp_prc > 6, plotnames,
                                     ifelse(-G_xG > 5 | -G_xG < -5, plotnames, NA))),
                  size = 3)+
  geom_hline(aes(yintercept = 0), alpha = .5, linetype = 'dashed', size = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'))+
  labs(title = 'xG Faced - Goals Allowed vs. GK Compensation Percentage',
       y = 'xG - G',
       x = 'GK compensation as a percentage of team spending',
       color = 'Season')

left_join(gk_table, salaries %>% select(team, season, totalguar), by = c('team', 'season')) %>%
  filter(season %in% c(2015:2019)) %>%
  filter(last != 'Howard') %>%
  filter(!is.na(min)) %>%
  mutate(gk_comp_prc = 100*1000*comp_k/totalguar,
         sig_mins = ifelse(min >= 2000, '2000+ minutes', '<2000 minutes')) %>%
  ggplot(aes(x = gk_comp_prc, y = -G_xG, color = factor(sig_mins))) +
  geom_point() +
  geom_hline(aes(yintercept = 0), alpha = .5, linetype = 'dashed', size = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'))+
  labs(title = 'xG Faced - Goals Allowed vs. GK Compensation Percentage',
       y = 'xG - G',
       x = 'GK compensation as a percentage of team spending',
       color = element_blank())


gk_table %>% filter(season %in% c(2015:2019)) %>%
  mutate(within_2g = ifelse(abs(G_xG) <= 2, 1, 0)) %>%
  summarize(sum = sum(within_2g)/n())

gk_table2 <- gk_table %>% filter(season %in% c(2015:2019)) %>%
  filter(!is.na(comp_k))
cor(gk_table2$comp_k, -gk_table2$G_xG)
    
    
gk_table %>%
  filter(season %in% c(2015:2019)) %>% filter(last != 'Howard') %>%
  ggplot(aes(x = comp_k, y = -G_xG, color = factor(season))) +
  geom_point() +
  geom_text_repel(aes(label = ifelse(comp_k > 475, plotnames,
                                     ifelse(-G_xG >5 | -G_xG < -6, plotnames,NA))),
                  size = 3)+
  geom_hline(aes(yintercept = 0), alpha = .5, linetype = 'dashed', size = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'))+
  labs(title = 'Goals Allowed - xG Faced vs. GK Compensation',
       y = 'G - xG',
       x = 'GK compensation (thousands of dollars)',
       color = 'Season')
