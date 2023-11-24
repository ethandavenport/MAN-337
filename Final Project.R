
library(tidyverse)
library(tidyr)

# read in data sets
epm23 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2023.csv")
epm22 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2022.csv")
epm21 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2021.csv")
epm20 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2020.csv")
epm19 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2019.csv")
epm18 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2018.csv")
epm17 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2017.csv")
epm16 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2016.csv")
epm15 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2015.csv")
epm14 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2014.csv")

epm = rbind(epm23, epm22, epm21, epm20, epm19, epm18, epm17, epm16, epm15, epm14)
  
#epm = epm_all %>%
  #pivot_wider(names_from = season,
              #values_from = team) %>%
  #select(-gp, -min, -mpg, -oepm, -depm, -epm, -ewins) %>%
  #group_by(name) %>%
  #summarize('2023' = 'hello')

# widen the data sets to show each player's path over their career
team_paths = epm %>%
  select(-gp, -min, -mpg, -oepm, -depm, -epm, -ewins) %>%
  spread(key = season, value = team)

epm_paths = epm %>%
  select(-gp, -min, -mpg, -oepm, -depm, -team, -ewins) %>%
  spread(key = season, value = epm)

oepm_paths = epm %>%
  select(-gp, -min, -mpg, -epm, -depm, -team, -ewins) %>%
  spread(key = season, value = oepm)

depm_paths = epm %>%
  select(-gp, -min, -mpg, -oepm, -epm, -team, -ewins) %>%
  spread(key = season, value = depm)

# filter these data sets to players who switched teams at some point
team_paths = team_paths %>%
  filter(`2014` != `2015` & !is.na(`2014`) & !is.na(`2015`) |
           `2015` != `2016` & !is.na(`2015`) & !is.na(`2016`) |
           `2016` != `2017` & !is.na(`2016`) & !is.na(`2017`) |
           `2017` != `2018` & !is.na(`2017`) & !is.na(`2018`) |
           `2018` != `2019` & !is.na(`2018`) & !is.na(`2019`) |
           `2019` != `2020` & !is.na(`2019`) & !is.na(`2020`) |
           `2020` != `2021` & !is.na(`2020`) & !is.na(`2021`) |
           `2021` != `2022` & !is.na(`2021`) & !is.na(`2022`) |
           `2022` != `2023` & !is.na(`2022`) & !is.na(`2023`))

epm_paths = epm_paths %>%
  filter(name %in% team_paths$name)

oepm_paths = oepm_paths %>%
  filter(name %in% team_paths$name)

depm_paths = depm_paths %>%
  filter(name %in% team_paths$name)

# write into CSVs for manipulation in python
write.csv(team_paths, "C:/Users/ethan/OneDrive/Fall 2023/MAN 337/team_paths.csv")
write.csv(epm_paths, "C:/Users/ethan/OneDrive/Fall 2023/MAN 337/epm_paths.csv")
write.csv(oepm_paths, "C:/Users/ethan/OneDrive/Fall 2023/MAN 337/oepm_paths.csv")
write.csv(depm_paths, "C:/Users/ethan/OneDrive/Fall 2023/MAN 337/depm_paths.csv")

# turn teams not in a relevant year into NA
team_paths = team_paths %>%
  mutate(tm14 = ifelse(`2014` == `2015`, NA, `2014`),
         tm15 = ifelse(`2014` == `2015` & `2015` == `2016`, NA, `2015`),
         tm16 = ifelse(`2015` == `2016` & `2016` == `2017`, NA, `2016`),
         tm17 = ifelse(`2016` == `2017` & `2017` == `2018`, NA, `2017`),
         tm18 = ifelse(`2017` == `2018` & `2018` == `2019`, NA, `2018`),
         tm19 = ifelse(`2018` == `2019` & `2019` == `2020`, NA, `2019`),
         tm20 = ifelse(`2019` == `2020` & `2020` == `2021`, NA, `2020`),
         tm21 = ifelse(`2020` == `2021` & `2021` == `2022`, NA, `2021`),
         tm22 = ifelse(`2021` == `2022` & `2022` == `2023`, NA, `2022`),
         tm23 = ifelse(`2022` == `2023`, NA, `2023`)) %>%
  select(-`2014`, -`2015`, -`2016`, -`2017`, -`2018`, -`2019`, -`2020`, -`2021`, -`2022`, -`2023`)

# combine files
paths = team_paths %>%
  left_join(epm_paths, by = c("nba_id", "name"), suffix = c("", ".e")) %>%
  left_join(oepm_paths, by = c("nba_id", "name"), suffix = c("", ".o")) %>%
  left_join(depm_paths, by = c("nba_id", "name"), suffix = c("", ".d"))



# ideal final data set columns: player, old team, various EPMs, new team, various EPMs

# adjust for age?
# compare offensive vs defensive EPMs
# compare team effect on negative vs positive players


