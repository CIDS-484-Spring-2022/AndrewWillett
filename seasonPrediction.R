#Season Record Prediction

sPrediction <- lm(WLnum~Score+FGM+FGA+FGM3+FGA3+FTA+OR+DR+Ast+TO+Stl+Blk+PF,
                  data = regStats)
summary(sPrediction)


gonz <- data.frame("Score"= 87.8, "FGM"= 32.9, "FGA"= 62.4, "FGM3"= 8.4, 
                   "FGA3"= 22.1, "FTA"= 18.6, "OR"= 9.4, "DR"= 32.1, "Ast"= 18.2,
                   "TO"= 11.8, "Stl"= 6.7, "Blk"= 5.9, "PF"= 15.4, "Seed"= 1)
predict(sPrediction, gonz, interval = 'predict')

gast <- data.frame("Score"= 70.6, "FGM"= 25, "FGA"= 62, "FGM3"= 7.7, 
                   "FGA3"= 23.4, "FTA"= 18.1, "OR"= 13.4, "DR"= 24, "Ast"= 13.5,
                   "TO"= 11.9, "Stl"= 8.9, "Blk"= 4.5, "PF"= 15.8, "Seed"= 16)
predict(sPrediction, gast, interval = 'predict')

bsu <- data.frame("Score"= 68.1, "FGM"= 24.5, "FGA"= 54.6, "FGM3"= 7.4, 
                  "FGA3"= 21.2, "FTA"= 19.1, "OR"= 10.2, "DR"= 24.8, "Ast"= 11.5,
                  "TO"= 12.1, "Stl"= 5.9, "Blk"= 3.5, "PF"= 15.1, "Seed"= 8)
predict(sPrediction, bsu, interval = 'predict')

mem <- data.frame("Score"= 75.4, "FGM"= 26.9, "FGA"= 57.2, "FGM3"= 6.3, 
                  "FGA3"= 17.6, "FTA"= 22, "OR"= 12.5, "DR"= 26.3, "Ast"= 16.1,
                  "TO"= 16.4, "Stl"= 8.7, "Blk"= 5.7, "PF"= 18.7, "Seed"= 9)
predict(sPrediction, mem, interval = 'predict')

conn <- data.frame("Score"= 75.2, "FGM"= 26.8, "FGA"= 61.6, "FGM3"= 7.4, 
                   "FGA3"= 21.1, "FTA"= 18.8, "OR"= 14, "DR"= 26.9, "Ast"= 14,
                   "TO"= 11.9, "Stl"= 6, "Blk"= 6.5, "PF"= 16.8, "Seed"= 5)
predict(sPrediction, conn, interval = 'predict')

nmsu <- data.frame("Score"= 73.3, "FGM"= 25.9, "FGA"= 56.6, "FGM3"= 7.8, 
                   "FGA3"= 24, "FTA"= 19.7, "OR"= 11.1, "DR"= 27.3, "Ast"= 13.9,
                   "TO"= 14, "Stl"= 5.1, "Blk"= 4.2, "PF"= 15.9, "Seed"= 12)
predict(sPrediction, nmsu, interval = 'predict')

ark <- data.frame("Score"= 76.9, "FGM"= 26.6, "FGA"= 60.6, "FGM3"= 6.4, 
                  "FGA3"= 20.8, "FTA"= 23, "OR"= 11.2, "DR"= 26.9, "Ast"= 14.1,
                  "TO"= 12.6, "Stl"= 7.7, "Blk"= 4.2, "PF"= 16.8, "Seed"= 4)
predict(sPrediction, ark, interval = 'predict')

uvm <- data.frame("Score"= 74.6, "FGM"= 27.6, "FGA"= 55.9, "FGM3"= 8.8, 
                  "FGA3"= 24, "FTA"= 14.8, "OR"= 7.3, "DR"= 27.8, "Ast"= 15.2,
                  "TO"= 9.7, "Stl"= 5.2, "Blk"= 2.5, "PF"= 13.8, "Seed"= 13)
predict(sPrediction, uvm, interval = 'predict')

ala <- data.frame("Score"= 80, "FGM"= 27.7, "FGA"= 62.8, "FGM3"= 9.3, 
                  "FGA3"= 30.1, "FTA"= 21.1, "OR"= 13.5, "DR"= 26.4, "Ast"= 14.6,
                  "TO"= 14.6, "Stl"= 7, "Blk"= 4.7, "PF"= 18.6, "Seed"= 6)
predict(sPrediction, ala, interval = 'predict')

ttu <- data.frame("Score"= 71.7, "FGM"= 25.9, "FGA"= 55.2, "FGM3"= 6, 
                  "FGA3"= 19, "FTA"= 19.9, "OR"= 10.6, "DR"= 25.9, "Ast"= 13.6,
                  "TO"= 16.2, "Stl"= 8.2, "Blk"= 3.5, "PF"= 16.2, "Seed"= 3)
predict(sPrediction, ttu, interval = 'predict')

mtst <- data.frame("Score"= 77, "FGM"= 26.5, "FGA"= 55.8, "FGM3"= 7.6, 
                   "FGA3"= 20.7, "FTA"= 21.6, "OR"= 8.7, "DR"= 26.8, "Ast"= 13.2,
                   "TO"= 12.8, "Stl"= 5.2, "Blk"= 3.7, "PF"= 18.1, "Seed"= 14)
predict(sPrediction, mtst, interval = 'predict')

msu <- data.frame("Score"= 72.1, "FGM"= 26, "FGA"= 56.9, "FGM3"= 7.2, 
                  "FGA3"= 19, "FTA"= 17.3, "OR"= 10, "DR"= 27.1, "Ast"= 15.8,
                  "TO"= 13.1, "Stl"= 5.8, "Blk"= 5.1, "PF"= 16.9, "Seed"= 7)
predict(sPrediction, msu, interval = 'predict')

dav <- data.frame("Score"= 75.6, "FGM"= 26.7, "FGA"= 55.4, "FGM3"= 8.8, 
                  "FGA3"= 22.8, "FTA"= 17.7, "OR"= 7.4, "DR"= 26.8, "Ast"= 14.7,
                  "TO"= 9.8, "Stl"= 4.7, "Blk"= 2.6, "PF"= 14.7, "Seed"= 10)
predict(sPrediction, dav, interval = 'predict')

duke <- data.frame("Score"= 80.2, "FGM"= 29.7, "FGA"= 60.7, "FGM3"= 8.2, 
                   "FGA3"= 22.2, "FTA"= 17.4, "OR"= 10.7, "DR"= 27.6, "Ast"= 16.9,
                   "TO"= 10.2, "Stl"= 6.5, "Blk"= 5.5, "PF"= 13.6, "Seed"= 2)
predict(sPrediction, duke, interval = 'predict')

csuf <- data.frame("Score"= 70.3, "FGM"= 24.8, "FGA"= 55.4, "FGM3"= 5.5, 
                   "FGA3"= 16.7, "FTA"= 19.9, "OR"= 9.7, "DR"= 23.9, "Ast"= 11.3,
                   "TO"= 12.3, "Stl"= 7.3, "Blk"= 2, "PF"= 16, "Seed"= 15)
predict(sPrediction, csuf, interval = 'predict')

################

bay <- data.frame("Score"= 76.5, "FGM"= 28.2, "FGA"= 60.9, "FGM3"= 7.9, 
                  "FGA3"= 23, "FTA"= 17.4, "OR"= 12.8, "DR"= 24.4, "Ast"= 15.8,
                  "TO"= 12.5, "Stl"= 8.8, "Blk"= 3.4, "PF"= 15.8, "Seed"= 1)
predict(sPrediction, bay, interval = 'predict')

norf <- data.frame("Score"= 75.1, "FGM"= 26.1, "FGA"= 57, "FGM3"= 7, 
                   "FGA3"= 20.2, "FTA"= 15, "OR"= 10.1, "DR"= 28.6, "Ast"= 12.6,
                   "TO"= 14, "Stl"= 6.7, "Blk"= 3.7, "PF"= 17.7, "Seed"= 16)
predict(sPrediction, norf, interval = 'predict')

unc <- data.frame("Score"= 77.5, "FGM"= 27.6, "FGA"= 61, "FGM3"= 8.3, 
                  "FGA3"= 22.9, "FTA"= 18.1, "OR"= 10.7, "DR"= 29.1, "Ast"= 14.8,
                  "TO"= 11.7, "Stl"= 5.4, "Blk"= 3.8, "PF"= 14.4, "Seed"= 8)
predict(sPrediction, unc, interval = 'predict')

marq <- data.frame("Score"= 74.4, "FGM"= 26.9, "FGA"= 59.4, "FGM3"= 8.6, 
                   "FGA3"= 24.8, "FTA"= 15.9, "OR"= 7.7, "DR"= 27, "Ast"= 16,
                   "TO"= 12.5, "Stl"= 7.9, "Blk"= 5.1, "PF"= 17.5, "Seed"= 9)
predict(sPrediction, marq, interval = 'predict')

smc <- data.frame("Score"= 69.8, "FGM"= 26.2, "FGA"= 56.4, "FGM3"= 7.2, 
                  "FGA3"= 20.4, "FTA"= 13.5, "OR"= 8.6, "DR"= 25.2, "Ast"= 13.4,
                  "TO"= 11.3, "Stl"= 7, "Blk"= 2.6, "PF"= 15.6, "Seed"= 5)
predict(sPrediction, smc, interval = 'predict')

iu <- data.frame("Score"= 71.3, "FGM"= 26, "FGA"= 56.9, "FGM3"= 6, 
                 "FGA3"= 17.9, "FTA"= 19.1, "OR"= 9.2, "DR"= 27.2, "Ast"= 14.8,
                 "TO"= 11.8, "Stl"= 5.6, "Blk"= 4.9, "PF"= 17.4, "Seed"= 12)
predict(sPrediction, iu, interval = 'predict')

ucla <- data.frame("Score"= 76.4, "FGM"= 27.9, "FGA"= 62.1, "FGM3"= 6.9, 
                   "FGA3"= 19.6, "FTA"= 18.5, "OR"= 11.6, "DR"= 25.5, "Ast"= 14,
                   "TO"= 9.2, "Stl"= 7, "Blk"= 3.3, "PF"= 16.3, "Seed"= 4)
predict(sPrediction, ucla, interval = 'predict')

akr <- data.frame("Score"= 71.2, "FGM"= 24.4, "FGA"= 52.9, "FGM3"= 7.9, 
                  "FGA3"= 22.2, "FTA"= 21, "OR"= 9.9, "DR"= 25.1, "Ast"= 11.9,
                  "TO"= 11.5, "Stl"= 5.8, "Blk"= 3.3, "PF"= 15.8, "Seed"= 13)
predict(sPrediction, akr, interval = 'predict')

tex <- data.frame("Score"= 68.3, "FGM"= 24.3, "FGA"= 55, "FGM3"= 6.4, 
                  "FGA3"= 19.8, "FTA"= 13.3, "OR"= 10.4, "DR"= 23.2, "Ast"= 13,
                  "TO"= 11.8, "Stl"= 7.7, "Blk"= 3.4, "PF"= 17.4, "Seed"= 6)
predict(sPrediction, tex, interval = 'predict')

vt <- data.frame("Score"= 70.7, "FGM"= 26, "FGA"= 55.3, "FGM3"= 9, 
                 "FGA3"= 22.9, "FTA"= 13.1, "OR"= 8.6, "DR"= 23.6, "Ast"= 14.4,
                 "TO"= 10.8, "Stl"= 5.4, "Blk"= 3.2, "PF"= 14.7, "Seed"= 11)
predict(sPrediction, vt, interval = 'predict')

pur <- data.frame("Score"= 79.8, "FGM"= 28.2, "FGA"= 57.2, "FGM3"= 8.7, 
                  "FGA3"= 8.7, "FTA"= 22.4, "OR"= 11.2, "DR"= 27.4, "Ast"= 16.6,
                  "TO"= 11.7, "Stl"= 4.6, "Blk"= 3.5, "PF"= 14.4, "Seed"= 3)
predict(sPrediction, pur, interval = 'predict')

yale <- data.frame("Score"= 72.3, "FGM"= 25.7, "FGA"= 57.9, "FGM3"= 6.9, 
                   "FGA3"= 21, "FTA"= 18.9, "OR"= 8.9, "DR"= 27.1, "Ast"= 11.9,
                   "TO"= 12.9, "Stl"= 6.2, "Blk"= 3.2, "PF"= 17.4, "Seed"= 14)
predict(sPrediction, yale, interval = 'predict')

mur <- data.frame("Score"= 79.3, "FGM"= 29, "FGA"= 60.6, "FGM3"= 8.1, 
                  "FGA3"= 22.8, "FTA"= 19, "OR"= 12.5, "DR"= 26.7, "Ast"= 14.7,
                  "TO"= 11.4, "Stl"= 7.9, "Blk"= 3.1, "PF"= 15, "Seed"= 7)
predict(sPrediction, mur, interval = 'predict')

sf <- data.frame("Score"= 77.1, "FGM"= 27.6, "FGA"= 60.5, "FGM3"= 9.6, 
                 "FGA3"= 27.2, "FTA"= 17.5, "OR"= 10.7, "DR"= 27.3, "Ast"= 13.1,
                 "TO"= 12.9, "Stl"= 7.7, "Blk"= 4.6, "PF"= 17.9, "Seed"= 10)
predict(sPrediction, sf, interval = 'predict')

uk <- data.frame("Score"= 79.5, "FGM"= 30.4, "FGA"= 62.8, "FGM3"= 6.2, 
                 "FGA3"= 17.6, "FTA"= 17.1, "OR"= 12.9, "DR"= 27.3, "Ast"= 16.1,
                 "TO"= 11.5, "Stl"= 6.8, "Blk"= 4.2, "PF"= 14.2, "Seed"= 2)
predict(sPrediction, uk, interval = 'predict')

spu <- data.frame("Score"= 66.9, "FGM"= 23.6, "FGA"= 54.7, "FGM3"= 5.9, 
                  "FGA3"= 16.6, "FTA"= 20.3, "OR"= 11, "DR"= 25.2, "Ast"= 12.2,
                  "TO"= 13.9, "Stl"= 7.3, "Blk"= 4.9, "PF"= 19.5, "Seed"= 15)
predict(sPrediction, spu, interval = 'predict')

############################

hall <- data.frame("Score"= 66.9, "FGM"= 23.6, "FGA"= 54.7, "FGM3"= 5.9, 
                   "FGA3"= 16.6, "FTA"= 20.3, "OR"= 11, "DR"= 25.2, "Ast"= 12.2,
                   "TO"= 13.9, "Stl"= 7.3, "Blk"= 4.9, "PF"= 19.5, "Seed"= 8)
predict(sPrediction, hall, interval = 'predict')

tcu <- data.frame("Score"= 68.1, "FGM"= 25.3, "FGA"= 57.4, "FGM3"= 5.6, 
                  "FGA3"= 18.3, "FTA"= 17.9, "OR"= 12.9, "DR"= 25.8, "Ast"= 13.6,
                  "TO"= 14.5, "Stl"= 6.3, "Blk"= 4.2, "PF"= 15.3, "Seed"= 9)
predict(sPrediction, tcu, interval = 'predict')

hou <- data.frame("Score"= 75.8, "FGM"= 28.1, "FGA"= 60, "FGM3"= 8, 
                  "FGA3"= 23.5, "FTA"= 17.2, "OR"= 13.4, "DR"= 25.9, "Ast"= 16.7,
                  "TO"= 11.3, "Stl"= 8.2, "Blk"= 5.2, "PF"= 17.4, "Seed"= 5)
predict(sPrediction, hou, interval = 'predict')

uab <- data.frame("Score"= 80.7, "FGM"= 29.7, "FGA"= 63.4, "FGM3"= 8, 
                  "FGA3"= 21, "FTA"= 18.1, "OR"= 12.2, "DR"= 26.2, "Ast"= 13,
                  "TO"= 11.5, "Stl"= 9.6, "Blk"= 4.3, "PF"= 16.6, "Seed"= 12)
predict(sPrediction, uab, interval = 'predict')

ill <- data.frame("Score"= 75.8, "FGM"= 26.7, "FGA"= 58.9, "FGM3"= 9.2, 
                  "FGA3"= 25.1, "FTA"= 18.6, "OR"= 11.6, "DR"= 27, "Ast"= 15.4,
                  "TO"= 12.2, "Stl"= 5.2, "Blk"= 3, "PF"= 16.4, "Seed"= 4)
predict(sPrediction, ill, interval = 'predict')

utc <- data.frame("Score"= 74.8, "FGM"= 27.5, "FGA"= 58.6, "FGM3"= 8, 
                  "FGA3"= 23.1, "FTA"= 15.7, "OR"= 10.6, "DR"= 25.3, "Ast"= 13,
                  "TO"= 11, "Stl"= 7.2, "Blk"= 2.2, "PF"= 15.5, "Seed"= 13)
predict(sPrediction, utc, interval = 'predict')

csu <- data.frame("Score"= 73.7, "FGM"= 26.6, "FGA"= 54.8, "FGM3"= 7.6, 
                  "FGA3"= 21.3, "FTA"= 16.7, "OR"= 6.6, "DR"= 25.3, "Ast"= 14.2,
                  "TO"= 10.1, "Stl"= 6.3, "Blk"= 3.2, "PF"= 14.9, "Seed"= 6)
predict(sPrediction, csu, interval = 'predict')

mich <- data.frame("Score"= 73, "FGM"= 27.1, "FGA"= 57.9, "FGM3"= 6.4, 
                   "FGA3"= 18.7, "FTA"= 16.7, "OR"= 10, "DR"= 25.5, "Ast"= 14.2,
                   "TO"= 11.5, "Stl"= 4.8, "Blk"= 3.1, "PF"= 15.5, "Seed"= 11)
predict(sPrediction, mich, interval = 'predict')

tenn <- data.frame("Score"= 73.2, "FGM"= 25.9, "FGA"= 60.2, "FGM3"= 8.8, 
                   "FGA3"= 24.4, "FTA"= 17.8, "OR"= 11.9, "DR"= 25.2, "Ast"= 16.1,
                   "TO"= 12.4, "Stl"= 9.3, "Blk"= 4.4, "PF"= 17, "Seed"= 3)
predict(sPrediction, tenn, interval = 'predict')

long <- data.frame("Score"= 76.3, "FGM"= 26.7, "FGA"= 58.8, "FGM3"= 8.2, 
                   "FGA3"= 21.6, "FTA"= 20.5, "OR"= 12, "DR"= 25.3, "Ast"= 14,
                   "TO"= 12, "Stl"= 7.8, "Blk"= 2.4, "PF"= 15.4, "Seed"= 14)
predict(sPrediction, long, interval = 'predict')

osu <- data.frame("Score"= 73.8, "FGM"= 25.9, "FGA"= 54.7, "FGM3"= 8.1, 
                  "FGA3"= 21.7, "FTA"= 18.3, "OR"= 8.8, "DR"= 25.5, "Ast"= 12.8,
                  "TO"= 11, "Stl"= 4.4, "Blk"= 4.8, "PF"= 16.4, "Seed"= 7)
predict(sPrediction, osu, interval = 'predict')

luc <- data.frame("Score"= 73.8, "FGM"= 26.3, "FGA"= 54.3, "FGM3"= 8.9, 
                  "FGA3"= 23.3, "FTA"= 17.3, "OR"= 7.7, "DR"= 26.3, "Ast"= 15,
                  "TO"= 12.2, "Stl"= 6.9, "Blk"= 2.4, "PF"= 17.2, "Seed"= 10)
predict(sPrediction, luc, interval = 'predict')

vill <- data.frame("Score"= 72.6, "FGM"= 24.6, "FGA"= 56.3, "FGM3"= 9.3, 
                   "FGA3"= 25.9, "FTA"= 17.2, "OR"= 10.2, "DR"= 24.7, "Ast"= 12.1,
                   "TO"= 10, "Stl"= 6.2, "Blk"= 2.2, "PF"= 15.2, "Seed"= 2)
predict(sPrediction, vill, interval = 'predict')

del <- data.frame("Score"= 73.8, "FGM"= 26.1, "FGA"= 55.6, "FGM3"= 7.6, 
                  "FGA3"= 21.5, "FTA"= 18.9, "OR"= 8.6, "DR"= 24.2, "Ast"= 12.7,
                  "TO"= 12.6, "Stl"= 6.6, "Blk"= 3.7, "PF"= 15.6, "Seed"= 15)
predict(sPrediction, del, interval = 'predict')

#########################################

ku <- data.frame("Score"= 78.6, "FGM"= 28.6, "FGA"= 59.6, "FGM3"= 7.1, 
                 "FGA3"= 20.1, "FTA"= 19.5, "OR"= 11.1, "DR"= 26.3, "Ast"= 15.4,
                 "TO"= 12.5, "Stl"= 6.4, "Blk"= 4.1, "PF"= 15.9, "Seed"= 1)
predict(sPrediction, ku, interval = 'predict')

txso <- data.frame("Score"= 69.5, "FGM"= 25.3, "FGA"= 57.9, "FGM3"= 5.7, 
                   "FGA3"= 18, "FTA"= 19.5, "OR"= 12, "DR"= 27.7, "Ast"= 10.7,
                   "TO"= 14.9, "Stl"= 5.5, "Blk"= 5.1, "PF"= 17.8, "Seed"= 16)
predict(sPrediction, txso, interval = 'predict')

sdsu <- data.frame("Score"= 65.3, "FGM"= 23.7, "FGA"= 54.7, "FGM3"= 6, 
                   "FGA3"= 16.9, "FTA"= 17.1, "OR"= 9.8, "DR"= 25.3, "Ast"= 11.9,
                   "TO"= 12.7, "Stl"= 7.5, "Blk"= 4.7, "PF"= 17.4, "Seed"= 8)
predict(sPrediction, sdsu, interval = 'predict')

crei <- data.frame("Score"= 69, "FGM"= 25.8, "FGA"= 57.2, "FGM3"= 6.7, 
                   "FGA3"= 21.8, "FTA"= 14.8, "OR"= 9.6, "DR"= 28.6, "Ast"= 13.2,
                   "TO"= 14.1, "Stl"= 5.4, "Blk"= 4.4, "PF"= 13.4, "Seed"= 9)
predict(sPrediction, crei, interval = 'predict')

iowa <- data.frame("Score"= 83.8, "FGM"= 29.9, "FGA"= 64.5, "FGM3"= 9.3, 
                   "FGA3"= 25.2, "FTA"= 19.6, "OR"= 11.8, "DR"= 25.4, "Ast"= 16.1,
                   "TO"= 9.2, "Stl"= 7.4, "Blk"= 4.4, "PF"= 16.1, "Seed"= 5)
predict(sPrediction, iowa, interval = 'predict')

rich <- data.frame("Score"= 71.7, "FGM"= 25.4, "FGA"= 57.2, "FGM3"= 8.1, 
                   "FGA3"= 24.1, "FTA"= 17.7, "OR"= 7.5, "DR"= 24.8, "Ast"= 14.7,
                   "TO"= 9.8, "Stl"= 7.9, "Blk"= 2.3, "PF"= 13.9, "Seed"= 12)
predict(sPrediction, rich, interval = 'predict')

prov <- data.frame("Score"= 71.8, "FGM"= 24.4, "FGA"= 55.9, "FGM3"= 7.3, 
                   "FGA3"= 21.3, "FTA"= 21.5, "OR"= 10.5, "DR"= 26.9, "Ast"= 13.2,
                   "TO"= 11.7, "Stl"= 5, "Blk"= 3.8, "PF"= 16, "Seed"= 4)
predict(sPrediction, prov, interval = 'predict')

sdst <- data.frame("Score"= 86.7, "FGM"= 31.1, "FGA"= 59.2, "FGM3"= 9.4, 
                   "FGA3"= 20.9, "FTA"= 20, "OR"= 7.4, "DR"= 28.1, "Ast"= 15.2,
                   "TO"= 11, "Stl"= 6.1, "Blk"= 2.5, "PF"= 15.4, "Seed"= 13)
predict(sPrediction, sdst, interval = 'predict')

lsu <- data.frame("Score"= 73.1, "FGM"= 26.1, "FGA"= 59.4, "FGM3"= 6.5, 
                  "FGA3"= 20.5, "FTA"= 19.6, "OR"= 12.1, "DR"= 24.8, "Ast"= 12.7,
                  "TO"= 14.5, "Stl"= 11.1, "Blk"= 4.4, "PF"= 19.3, "Seed"= 6)
predict(sPrediction, lsu, interval = 'predict')

isu <- data.frame("Score"= 66.5, "FGM"= 24.5, "FGA"= 55.8, "FGM3"= 6.7, 
                  "FGA3"= 20.9, "FTA"= 15.7, "OR"= 9.4, "DR"= 22.6, "Ast"= 14.7,
                  "TO"= 13.8, "Stl"= 8.4, "Blk"= 3.1, "PF"= 18, "Seed"= 11)
predict(sPrediction, isu, interval = 'predict')

wisc <- data.frame("Score"= 70.7, "FGM"= 25, "FGA"= 58.8, "FGM3"= 6.6, 
                   "FGA3"= 21.2, "FTA"= 19, "OR"= 9.3, "DR"= 25.6, "Ast"= 11.1,
                   "TO"= 8.5, "Stl"= 5.3, "Blk"= 2.8, "PF"= 17, "Seed"= 3)
predict(sPrediction, wisc, interval = 'predict')

colg <- data.frame("Score"= 76.1, "FGM"= 27.7, "FGA"= 58.1, "FGM3"= 9.9, 
                   "FGA3"= 24.6, "FTA"= 15.4, "OR"= 8.8, "DR"= 27.4, "Ast"= 17.2,
                   "TO"= 11.3, "Stl"= 6.2, "Blk"= 3.7, "PF"= 14.5, "Seed"= 14)
predict(sPrediction, colg, interval = 'predict')

usc <- data.frame("Score"= 72.6, "FGM"= 26.8, "FGA"= 59.2, "FGM3"= 6.7, 
                  "FGA3"= 18.9, "FTA"= 18.3, "OR"= 11.9, "DR"= 28.2, "Ast"= 14.2,
                  "TO"= 12, "Stl"= 5.1, "Blk"= 4.5, "PF"= 15.6, "Seed"= 7)
predict(sPrediction, usc, interval = 'predict')

aub <- data.frame("Score"= 78.7, "FGM"= 27.9, "FGA"= 63.6, "FGM3"= 8.1, 
                  "FGA3"= 25.4, "FTA"= 20.1, "OR"= 12.5, "DR"= 27.5, "Ast"= 14.6,
                  "TO"= 12.1, "Stl"= 8.8, "Blk"= 7.9, "PF"= 18.5, "Seed"= 2)
predict(sPrediction, aub, interval = 'predict')

jvst <- data.frame("Score"= 78.7, "FGM"= 27.9, "FGA"= 63.6, "FGM3"= 8.1, 
                   "FGA3"= 25.4, "FTA"= 20.1, "OR"= 12.5, "DR"= 27.5, "Ast"= 14.6,
                   "TO"= 12.1, "Stl"= 8.8, "Blk"= 7.9, "PF"= 18.5, "Seed"= 15)
predict(sPrediction, jvst, interval = 'predict')
