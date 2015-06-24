

rm(list = ls())
setwd("~/Documents/SeniorProject")

#### DATA PREPARATION#####################################
season = read.csv("seasonStats.csv",header = TRUE)
game = read.csv("gameStats.csv",header = TRUE)

data = merge(season,game,by = "home")

#example of a model that wouldnt make sense
fit = glm(as.factor(win) ~ FG + awayFG,data = data,family = "binomial")
d2 = data
colnames(d2)[3] = "awayFG"
colnames(d2)[37] = "FG"
predict(fit,d2[1,],type = "response")





final = data.frame(data$win,data$home,data$away,data$gameInSeries,data$wonPrevious)
names = c("win","home","away","gameInSeries","wonPrevious")
colnames(final) = names



#create variables
final$seed = data$homeSeed - data$awaySeed
final$SRS = data$SRS - data$awaySRS
final$PACE = data$Pace - data$awayPace
final$Offrtg = data$Offrtg - data$awayoffrtg
final$Defrtg = data$Defrtg - data$awaydefrtg
final$seasonWins = data$homeSWins - data$awaySWins
final$MP = data$MP - data$awayMP
final$FG = data$FG - data$awayFG
final$FGA = data$FGA - data$awayFGA
final$FG. = data$FG. - data$awayFG.
final$X3P = data$X3P - data$away3P
final$X3PA = data$X3PA - data$away3PA
final$X3P. = data$X3P. - data$away3P.
final$X2P = data$X2P - data$away2P
final$X2PA = data$X2PA - data$away2PA
final$X2P. = data$X2P. - data$away2P.
final$FT = data$FT - data$awayFT
final$FTA = data$FTA - data$awayFTA
final$FT. = data$FT. - data$awayFT.
final$ORB = data$ORB - data$awayORB
final$DRB = data$DRB - data$awayDRB
final$TRB = data$TRB - data$awayTRB
final$AST = data$AST - data$awayAST
final$STL = data$STL - data$awaySTL
final$BLK = data$BLK - data$awayBLK
final$TOV = data$TOV - data$awayTOV
final$PF = data$PF - data$awayPF
final$PTS = data$PTS - data$awayPTS




#feature plot (standardized)
library(caret)
nd = final
nd$win = ifelse(nd$win ==1,"win","loss")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = scale(nd[, 4:33]),
            y = as.factor(nd$win),
            plot = "box",
            layout = c(5, 6))

x = data.frame(biserial(final[,4:33],final$win))
x = t(x)

aggregate(win ~ seed,data = final,FUN = mean)

#game 1 data frame
game1 = final[final$gameInSeries == 1,]
game1 = game1[,-5]

#other games data frame
gameOther = final[final$gameInSeries != 1,]

prop.table(table(game1$win))
#game 1 home team wins 72.05% of the time

prop.table(table(gameOther$win))
#all other games home team wins 59.8% of the time




######LOGISTIC REGRESSION GAME 1 #####################################
library(MASS)
library(caret)
mod1 = game1[,-c(2,3)]
temp = glm(as.factor(win) ~ . - X2P - X2P.,data = mod1, family = "binomial")
s = stepAIC(temp,direction = "both")
s$anova
 
game1Fit = glm(as.factor(win) ~ TRB + PF,data = mod1, family = "binomial")
summary(game1Fit)

pred = predict(game1Fit,data.frame(TRB = -4, PF = 3))



#### LOGISTIC REGRESSION MODEL ALL OTHER GAMES #####################################
library(MASS)
modO = gameOther[,-c(2,3)]
t = createDataPartition(y = modO$win,p = .85,times = 1,list = FALSE)
training = modO[t,]
test = modO[-t,]
fit = glm(as.factor(win)~.,data = training,family = "binomial")
step = stepAIC(fit,direction = "both")
step$anova

otherGamesMod = glm(as.factor(win) ~ seasonWins + FG. + X3P + X3PA + X2P + X2P. + 
                      FT + FT. + ORB + DRB + TRB + BLK + PF + PTS + wonPrevious,family = "binomial",data = modO)
summary(otherGamesMod)

otherGamesMod2 = glm(as.factor(win) ~ PACE + FGA + X3P + X3PA + X2P + X2P. + FTA + 
                       FT. + DRB + STL + PTS,family = "binomial",data = training)



#####SIMULATION AND PREDICTION #####################################
library(caret)
t = createDataPartition(y = modO$win,p = .75,times = 1,list = FALSE)
training = modO[t,]
test = modO[-t,]

trainMod = glm(as.factor(win) ~ seasonWins + FG. + X3P + X3PA + X2P + X2P. + 
                      FT + FT. + ORB + DRB + TRB + BLK + PF + PTS + wonPrevious,family = "binomial",data = training)


#accuracy check
p = predict(otherGamesMod2,test,type = "response")
right = c()
m = cbind(p,test)
m$resp = 0
for(i in 1:nrow(m)){
  if(m$p[i] > .55){
    m$resp[i] = 1
  } else{
    m$resp[i] = 0
  }
  if(m$resp[i] == m$win[i]){
    right = c(right,"right")
  } else{
    right = c(right,"wrong")
  }
}
prop.table(table(right))
table(m$resp,m$win)

summary(bestFit)


### SIMULATION  with logistic regression##############################################

#Game 1 Stats (without wonPrevious) 

g1team1stats = data.frame(seed = 1, SRS = 1 ,DEFRTG = 3 ,seasonWins = 62, FGA = 83.5, X3PA = 21.4,
                        X2P = 32,X2P. = .517 ,FT = 15.7,ORB = 9.3,
                        DRB = 34,TRB = 43.3,PF = 18.2)
g1team2stats = data.frame(seed = 2, SRS = 7, DEFRTG = 11,seasonWins = 54, FGA = 76.5, X3PA = 22.3,
                          X2P = 32,X2P. = .558,FT = 17.5,ORB = 7.6,
                          DRB = 29.2,TRB = 36.9,PF = 19.5)
teamAg1 = g1team1stats - g1team2stats
teamBg1 = g1team2stats - g1team1stats


#Game Other stats(with wonPrevious),
#if won
goteam1statsW = data.frame(seasonWins = 62,FG. = .486,X3P = 8.5, X3PA = 21.4,
                          X2P = 32,X2P. = .517,FT = 15.7,FT. = .785,ORB = 9.3,
                          DRB = 34,TRB = 43.3,BLK = 5.1,PF = 18.2,PTS = 105.2,wonPrevious = 1)
goteam2statsL = data.frame(seasonWins = 54,FG. = .501,X3P = 8.1, X3PA = 22.3,
                          X2P = 30.2,X2P. = .558,FT = 17.5,FT. = .76,ORB = 7.6,
                          DRB = 29.2,TRB = 36.9,BLK = 4.5,PF = 19.5,PTS = 102.2,wonPrevious = 0)
teamAgoW = goteam1statsW - goteam2statsL
teamBgoL = goteam2statsL - goteam1statsW

#if lost
goteam1statsL = data.frame(seasonWins = 62,FG. = .486,X3P = 8.5, X3PA = 21.4,
                          X2P = 32,X2P. = .517,FT = 15.7,FT. = .785,ORB = 9.3,
                          DRB = 34,TRB = 43.3,BLK = 5.1,PF = 18.2,PTS = 105.2,wonPrevious = 0)
goteam2statsW = data.frame(seasonWins = 54,FG. = .501,X3P = 8.1, X3PA = 22.3,
                          X2P = 30.2,X2P. = .558,FT = 17.5,FT. = .76,ORB = 7.6,
                          DRB = 29.2,TRB = 36.9,BLK = 4.5,PF = 19.5,PTS = 102.2,wonPrevious = 1)
teamAgoL = goteam1statsL - goteam2statsW
teamBgoW = goteam2statsW - goteam1statsL



############PREDICTIONS


### GAME 1 PREDICTIONS

predHomeAg1 = predict(game1Fit,teamAg1,type = "response")
predAwayAg1 = 1 - predHomeAg1

predHomeBg1 = predict(game1Fit,teamBg1,type = "response")
predAwayBg1 = 1 - predHomeBg1

#only run if using rf for round 1
p1 = predict(tree,teamAg1,type = "prob")
predHomeAg1 = p[2]
predAwayAg1 = 1 - predHomeAg1

p2 = predict(tree,teamBg1,type = "prob")
predHomeBg1 = p2[2]
predAwayBg1 = 1 - predHomeBg1


### ALL OTHER GAMES

#if team 1 wins prev game
predHomeAgoW = predict(otherGamesMod,teamAgoW,type = "response")
predAwayAgoW = 1 - predHomeAgoW

#if team 1 loses prev game
predHomeAgoL = predict(otherGamesMod,teamAgoL,type = "response")
predAwayAgoL = 1 - predHomeAgoL


#if team 2 wins prev game
predHomeBgoW = predict(otherGamesMod,teamBgoW,type = "response")
predAwayBgoW = 1 - predHomeBgoW

#if team 2 loses prev game
predHomeBgoL = predict(otherGamesMod,teamBgoL,type = "response")
predAwayBgoL = 1 - predHomeBgoL



#find probability of winning 7 game series

teamHCwins = c()
games7 = c()
for(i in 1:1000){
#game 1  
games7[1] = sample(c(1,0),1,replace = TRUE,c(predHomeAg1,predAwayAg1))
#game 2
if(games7[1] == 1){
  games7[2] = sample(c(1,0),1,replace = TRUE,c(predHomeAgoW,predAwayAgoW))
} else{
  games7[2] = sample(c(1,0),1,replace = TRUE,c(predHomeAgoL,predAwayAgoL))
}
#game 3
if(games7[2] == 1){
games7[3] = sample(c(0,1),1,replace = TRUE,c(predHomeBgoW,predAwayBgoW))
} else{
games7[3] = sample(c(0,1),1,replace = TRUE,c(predHomeBgoL,predAwayBgoL))
}
#game 4
if(games7[3] == 1){
  games7[4] = sample(c(0,1),1,replace = TRUE,c(predHomeBgoW,predAwayBgoW))
} else{
  games7[4] = sample(c(0,1),1,replace = TRUE,c(predHomeBgoL,predAwayBgoL))
}
#game 5
if(games7[4] == 1){
  games7[5] = sample(c(1,0),1,replace = TRUE,c(predHomeAgoW,predAwayAgoW))
} else{
  games7[5] = sample(c(1,0),1,replace = TRUE,c(predHomeAgoL,predAwayAgoL))
}
#game 6
if(games7[5] == 1){
  games7[6] = sample(c(0,1),1,replace = TRUE,c(predHomeBgoW,predAwayBgoW))
} else{
  games7[6] = sample(c(0,1),1,replace = TRUE,c(predHomeBgoL,predAwayBgoL))
}
#game 7
if(games7[6] == 1){
  games7[7] = sample(c(1,0),1,replace = TRUE,c(predHomeAgoW,predAwayAgoW))
} else{
  games7[7] = sample(c(1,0),1,replace = TRUE,c(predHomeAgoL,predAwayAgoL))
}

if(sum(games7) >= 4){
  teamHCwins=c(teamHCwins,1)
} else{
  teamHCwins = c(teamHCwins,0)
}
}

prop.table(table(teamHCwins))

hist(teamHCwins)

##############RANDOM FORESTS##########
###############both kind of models
library(caret)
library(rpart)
library(randomForest)
library(ggplot2)

#create training/test
t = createDataPartition(y = mod1$win,p = .85,times = 1,list = FALSE)
training = modO[t,]
test = modO[-t,]


#game 1
tree = randomForest(as.factor(win) ~ seed + SRS + DEFRTG + seasonWins + FGA + X3PA + 
                      X2P + X2P. + FT  + DRB + TRB + PF,data = mod1,importance = TRUE)
tree = randomForest(as.factor(win) ~ seed + SRS + DEFRTG + seasonWins + FGA + X3PA + 
                      X2P + X2P. + FT  + DRB + TRB + PF,data = training,importance = TRUE)
importance(tree)
#find importance
imp = data.frame(importance(tree))
rowNames = row.names(imp)
imp = cbind(rowNames,imp)
newimp = imp[order(imp$MeanDecreaseAccuracy),]
newimp$rowNames = as.character(newimp$rowNames)
newimp$rowNames = factor(newimp$rowNames,levels = unique(rowNames))
ggplot(data = newimp,aes(y = reorder(rowNames,MeanDecreaseAccuracy),x = MeanDecreaseAccuracy)) + 
      geom_point() + ggtitle("Mean Decrease Accuracy per predictor") + ylab("variable") + theme_set(theme_grey(base_size = 17)) 

tree = randomForest(as.factor(win) ~ .,data = mod1,importance = TRUE)
varImpPlot(tree,main = "Variable Importance")
tree

################
mod = final[,-c(2,3)]
write.csv(mod,"predictDF.csv")
library(randomForest)
library(caret)
t = createDataPartition(y = mod$win,p = .75,times = 1,list = FALSE)
training = mod[t,]
test = mod[-t,]

mod$win = ifelse(mod$win == 1,"win","loss")
mod$win = as.factor(mod$win)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10) 
rf = train(win~. - gameInSeries - wonPrevious,data = mod,method = "rf",trControl = ctrl)
varImp(rf)
plot(rf)
rf
fm = rf$finalModel
fm



#simulation using random forest

#put in info for team 1 and 2 into data frame
predictD = read.csv("predictDF.csv",header = TRUE)

teamAstats = predictD[1,-1]
teamBstats = predictD[2,-1]
teamAhome = teamAstats - teamBstats
teamBhome = teamBstats - teamAstats

#if team with better record is home
probAHome = predict(fm,teamAhome,type = "prob")


#if other team is at home
probBHome = predict(fm,teamBhome,type = "prob")


#find probability of winning 7 game series

teamHCwins = c()
games7 = c()
for(i in 1:1000){
  
  #game 1  
  games7[1] = sample(c(1,0),1,replace = TRUE,c(probAHome[2],probAHome[1]))
  
  #game 2
  games7[2] = sample(c(1,0),1,replace = TRUE,c(probAHome[2],probAHome[1]))
  
  #game 3
  games7[3] = sample(c(0,1),1,replace = TRUE,c(probBHome[2],probBHome[1]))
  
  #game 4
  games7[4] = sample(c(0,1),1,replace = TRUE,c(probBHome[2],probBHome[1]))
 
  #game 5
  games7[5] = sample(c(1,0),1,replace = TRUE,c(probAHome[2],probAHome[1]))
  
  #game 6
  games7[6] = sample(c(0,1),1,replace = TRUE,c(probBHome[2],probBHome[1]))
  
  #game 7
  games7[7] = sample(c(1,0),1,replace = TRUE,c(probAHome[2],probAHome[1]))
  
  if(sum(games7) >= 4){
    teamHCwins=c(teamHCwins,1)
  } else{
    teamHCwins = c(teamHCwins,0)
  }
}

prop.table(table(teamHCwins))
#sim results
#warriors - pelicans 93.6 for warriors
#cavs -  celtics 86.1 cavaliers
#rockets -mavs 49 for rockets
#clippers - spurs 48 clippers
#memphis - blazers 59.8 memphis
#hawks - nets 97.5 hawks
#bulls - bucks 57.5 bulls
#raptors - wizards 64.3 raptors




hist(teamHCwins)























confusionMatrix(predict(fm,mod[1:5,]),mod$win[1:5])





mode(training$win)
rf = randomForest(as.factor(win) ~. - gameInSeries - seasonWins, importance = TRUE,data = mod)
plot(rf)
varImpPlot(rf,main = "Variable Importance")
importance(rf)
print(rf)

othergamesTree
p = predict(tree,test,response = "prob")
test = cbind(p,test)
table(test$win,p)
prop.table(table(test$win,p))



########### PLS total model
library(randomForest)
library(pROC)
library(caret)
library(rattle)


df = final[,-c(2,3)]

t = createDataPartition(y = df$win,p = .85,times = 1,list = FALSE)
training = df[t,]
test = df[-t,]

tree = rpart(as.factor(win)~.,data = training)


fancyRpartPlot(tree)

df$win = ifelse(df$win == 1,"win","loss")
control = trainControl(method = "repeatedcv",number = 10,
                       repeats = 10,classProbs = TRUE)
pls = train(as.factor(win)~.,method = "pls",data = training,trControl = control,prox = TRUE,allowParallel = TRUE)
pls
plot(pls)

pred = predict(pls,test)
confusionMatrix(pred,test$win)

table(predict)


