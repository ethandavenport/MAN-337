
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




# final data set: player, old team, various EPMs, new team, various EPMs

# adjust for age
# compare offensive vs defensive
# compare team effect on negative vs positive players


