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

mmStats %>% view()

mmStats %>% 
  group_by(FGA) %>% 
  mutate(WLAvg = mean(WLnum)) %>% 
  ggplot(aes(FGA, WLAvg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

tMod <- lm(WLnum~Score+FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+Ast+TO+Stl+Blk+PF,
           data = mmStats)
summary(tMod)


torSeed <- read_excel('NCAATourneySeeds.xlsx')

mmStats <- mmStats %>% 
  inner_join(torSeed) %>% 
  mutate(Seed = str_extract(Seed, "[0-9]+")) %>% 
  mutate(Seed = as.numeric(Seed))

tMod <- lm(WLnum~Score+FGM+FGA+FGM3+FGA3+FTA+OR+DR+Ast+TO+Stl+Blk+PF+Seed,
           data = mmStats)
summary(tMod)

gonz <- data.frame("Score"= 87.8, "FGM"= 32.9, "FGA"= 62.4, "FGM3"= 8.4, 
                  "FGA3"= 22.1, "FTA"= 18.6, "OR"= 9.4, "DR"= 32.1, "Ast"= 18.2,
                  "TO"= 11.8, "Stl"= 6.7, "Blk"= 5.9, "PF"= 15.4, "Seed"= 1)
predict(tMod, gonz, interval = 'predict')

gast <- data.frame("Score"= 70.6, "FGM"= 25, "FGA"= 62, "FGM3"= 7.7, 
                   "FGA3"= 23.4, "FTA"= 18.1, "OR"= 13.4, "DR"= 24, "Ast"= 13.5,
                   "TO"= 11.9, "Stl"= 8.9, "Blk"= 4.5, "PF"= 15.8, "Seed"= 16)
predict(tMod, gast, interval = 'predict')

gast <- data.frame("Score"= 70.6, "FGM"= 25, "FGA"= 62, "FGM3"= 7.7, 
                   "FGA3"= 23.4, "FTA"= 18.1, "OR"= 13.4, "DR"= 24, "Ast"= 13.5,
                   "TO"= 11.9, "Stl"= 8.9, "Blk"= 4.5, "PF"= 15.8, "Seed"= 16)
predict(tMod, gast, interval = 'predict')

ariz <- data.frame("Score"= 84.6, "FGM"= 30.4, "FGA"= 61.4, "FGM3"= 7.8, 
                   "FGA3"= 21.9, "FTA"= 21.6, "OR"= 11.4, "DR"= 29.9, "Ast"= 19.9,
                   "TO"= 6.7, "Stl"= 6.7, "Blk"= 5.7, "PF"= 16.5, "Seed"= 1)
predict(tMod, ariz, interval = 'predict')

ariz <- data.frame("Score"= 84.6, "FGM"= 30.4, "FGA"= 61.4, "FGM3"= 7.8, 
                   "FGA3"= 21.9, "FTA"= 21.6, "OR"= 11.4, "DR"= 29.9, "Ast"= 19.9,
                   "TO"= 6.7, "Stl"= 6.7, "Blk"= 5.7, "PF"= 16.5, "Seed"= 1)
predict(sPrediction, ariz, interval = 'predict')

regStats16_18 <- 
  regStats %>% 
  filter(Season >= 2016)

mod16_18 <- lm(WLnum~Score+FGM+FGA+FGM3+FGA3+FTA+OR+DR+Ast+TO+Stl+Blk+PF,
               data = regStats16_18)
summary(mod16_18)

with(summary(regsubsets(WLnum~Score+FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+
                          Ast+TO+Stl+Blk+PF, data = regStats16_18)),
     data.frame(adjr2, cp, outmat))

regStats03_05 <- 
  regStats %>% 
  filter(Season <= 2005)

mod03_05 <- lm(WLnum~Score+FGM+FGA+FGM3+FGA3+FTA+OR+DR+Ast+TO+Stl+Blk+PF,
               data = regStats03_05)
summary(mod03_05)

with(summary(regsubsets(WLnum~Score+FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+
                          Ast+TO+Stl+Blk+PF, data = regStats03_05)),
     data.frame(adjr2, cp, outmat))

sPrediction <- lm(WLnum~Score+FGM+FGA+FGM3+FGA3+FTA+OR+DR+Ast+TO+Stl+Blk+PF,
                  data = regStats)
summary(sPrediction)

tMod <- lm(WLnum~Score+FGM+FGA+FGM3+FGA3+FTA+OR+DR+Ast+TO+Stl+Blk+PF,
           data = mmStats)
summary(tMod)

with(summary(regsubsets(WLnum~Score+FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+
                          Ast+TO+Stl+Blk+PF, data = regStats)),
     data.frame(adjr2, cp, outmat))

with(summary(regsubsets(WLnum~Score+FGM+FGA+FGM3+FGA3+FTM+FTA+OR+DR+
                          Ast+TO+Stl+Blk+PF, data = mmStats)),
     data.frame(adjr2, cp, outmat))




mmStats %>% 
  group_by(Ast) %>% 
  mutate(avgWL = mean(WLnum)) %>% 
  ggplot(aes(Ast, avgWL)) +
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  xlab("Assists") + ylab("Average Win %")

mmStats %>% 
  group_by(FGM) %>% 
  mutate(avgWL = mean(WLnum)) %>% 
  ggplot(aes(FGM, avgWL)) +
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  xlab("Field Goals Made") + ylab("Average Win %")

regStats %>% 
  group_by(Ast) %>% 
  mutate(avgWL = mean(WLnum)) %>% 
  ggplot(aes(Ast, avgWL)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  xlab("Assists") + ylab("Win %")









