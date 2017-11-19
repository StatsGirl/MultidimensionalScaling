# CJ project HW3
#due 11.3.2017

library(Hmisc)
library(MASS)
#Question1:
getwd() #get working directory
dir<-setwd('/Users/breyawalker/Desktop/CJ') #set new working directory

df = read.csv("CJProject.csv", header = TRUE)


#80% 20% out (218 in 55 out)
train=sample(218,55)

fit.glm <- glm(formula= Bidded ~  Yr.blt + Total.Area, data = df, family = "binomial", subset = train)
summary(fit.glm)


probs2 <- predict(fit.glm, newdata=df[-train], type = "response")
pred.glm <- rep("0", length(probs2))
pred.glm[probs2 > 0.5] <- "1"

mean(pred.glm != df[-train, ]$Bidded)

#generated dataset

mysample <- data.frame(df[sample(1:nrow(df), 200,
                          replace=FALSE),])
dim(mysample)

#Question4

train=sample(160,40)

fit.glm <- glm(formula= Bidded ~  Yr.blt + Total.Area, data = mysample, family = "binomial", subset = train)
summary(fit.glm)


probs2 <- predict(fit.glm, newdata=df[-train], type = "response")
pred.glm <- rep("0", length(probs2))
pred.glm[probs2 > 0.5] <- "1"

mean(pred.glm != df[-train, ]$Bidded)



