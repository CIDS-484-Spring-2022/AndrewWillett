# Senior Capstone 

library(tidyverse)
library(readxl)
library(leaps)

regStats <- read_excel('RegularSeasonResults.xlsx')

regStats <- 
  regStats %>% 
  mutate(WLnum = WinLoss) %>% 
  mutate(WLnum = str_replace(WLnum, 'W', '1')) %>% 
  mutate(WLnum = str_replace(WLnum, 'L', '0')) %>% 
  mutate(WLnum = as.numeric(WLnum))

regStats %>% view()

regStats %>% 
  group_by(WinLoss,Season) %>% 
  mutate(FGM3Avg = mean(FGM3)) %>%
  ggplot(aes(Season, FGM3Avg, color = WinLoss)) +
  geom_line()

regStats %>% 
  group_by(Season) %>% 
  mutate(FGA3avg = mean(FGA3)) %>% 
  ggplot(aes(Season, FGA3avg)) +
  geom_line()

regStats %>% 
  group_by(Loc, Season) %>% 
  mutate(PFAvg = mean(PF)) %>% 
  ggplot(aes(Season, PFAvg, color = Loc)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE)

regStats %>% 
  group_by(WinLoss, Season) %>% 
  mutate(TOAvg = mean(TO)) %>% 
  ggplot(aes(Season, TOAvg, color = WinLoss)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

regStats %>% 
  group_by(TO) %>% 
  mutate(WLAvg = mean(WLnum)) %>% 
  ggplot(aes(TO, WLAvg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)


regStats %>% 
  glm(WLnum~FGM3, ., family = 'binomial')

regStats %>% 
  glm(WLnum~TO, ., family = 'binomial')

regStats %>% 
  glm(WLnum~Ast, ., family = 'binomial')

regStats %>% 
  glm(WLnum~OR, ., family = 'binomial')

summary(lm(WLnum~TO+Ast+FGM3, data = regStats))

with(summary(regsubsets(WLnum~., data = regStats)),
     data.frame(adjr2, cp, outmat))

with(summary(regsubsets(WLnum~Score+FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+
                          Ast+TO+Stl+Blk+PF, data = regStats)),
     data.frame(adjr2, cp, outmat))

regStats %>% 
  group_by(WinLoss, Season) %>% 
  mutate(fgaAvg = mean(FGA)) %>% 
  ggplot(aes(Season, fgaAvg, color = WinLoss)) +
  geom_point() +
  geom_smooth(method = lm, se =FALSE)

regStats %>% 
  group_by(FGA) %>% 
  mutate(WLAvg = mean(WLnum)) %>% 
  ggplot(aes(FGA, WLAvg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

regStats %>% 
  group_by(Loc) %>% 
  mutate(WLAvg = mean(WLnum))

regStats2018 <- regStats %>% 
  filter(Season == 2018)

with(summary(regsubsets(WLnum~Score+FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+
                          Ast+TO+Stl+Blk+PF, data = regStats2018)),
     data.frame(adjr2, cp, outmat))

regStats2018 %>% 
  group_by(FGA) %>% 
  mutate(WLAvg = mean(WLnum)) %>% 
  ggplot(aes(FGA, WLAvg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

regStats %>% 
  group_by(PF) %>% 
  mutate(WLAvg = mean(WLnum)) %>% 
  ggplot(aes(PF, WLAvg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

regStats2018 %>% 
  group_by(DR) %>% 
  mutate(WLAvg = mean(WLnum)) %>% 
  ggplot(aes(DR, WLAvg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

regStats %>% 
  group_by(FGM3) %>% 
  mutate(WLAvg = mean(WLnum)) %>% 
  ggplot(aes(FGM3, WLAvg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

regStats16_18 <- regStats %>% 
  filter(Season >= 2016)

with(summary(regsubsets(WLnum~Score+FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+
                          Ast+TO+Stl+Blk+PF, data = regStats16_18)),
     data.frame(adjr2, cp, outmat))

model16_18 <- lm(WLnum~Score+FGA+FTA+OR+DR+TO+Stl+Blk+PF,
                 data = regStats16_18)
summary(model16_18)

summary(lm(WLnum~Score+FGA+FTA+OR+DR+TO+Stl+Blk+PF+FGM3+FGM,
           data = regStats16_18))

mmStats <- read_excel('TourneymentResults.xlsx')

mmStats <- mmStats %>% 
  mutate(WLnum = WinLoss) %>% 
  mutate(WLnum = str_replace(WLnum, 'W', '1')) %>% 
  mutate(WLnum = str_replace(WLnum, 'L', '0')) %>% 
  mutate(WLnum = as.numeric(WLnum))

mmStats %>% 
  group_by(FGA) %>% 
  mutate(WLAvg = mean(WLnum)) %>% 
  ggplot(aes(FGA, WLAvg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

tMod <- lm(WLnum~Score+FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+Ast+TO+Stl+Blk+PF,
           data = mmStats)
summary(tMod)














