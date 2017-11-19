#CJ Data Science HW->2
#Due 10.24.17
library(Hmisc)
library(MASS)
#Question1:
getwd() #get working directory
dir<-setwd('/Users/breyawalker/Desktop/CJ') #set new working directory

df = read.csv("CJProject.csv", header = TRUE) #create new df of tax sale data

summary(df)
my_num_data <- df[, sapply(df, is.numeric)]


cor(df$Stories,df$Zillow.Value.estimate,use = "pairwise.complete.obs")
plot(df$Stories,df$Zillow.Value.estimate,main="Scatterplot: Stories in house by estimated home value", xlab="Number of stories", ylab="Estimated Home Value")

cor(df$Zillow.Value.estimate,df$Zillow.Rent.estimate,use = "pairwise.complete.obs")
plot(df$Zillow.Value.estimate,df$Zillow.Rent.estimate,main="Scatterplot: Estimated house value by rent estimate",xlab="Estimated Value", ylab="Estimated Rent value")

boxplot(df$Acres~df$Redemption.Period,main="Redemption period associated with the Acres of land", 
        xlab="Redemption Period", ylab="Acres")

boxplot(df$Minimum.bid~df$Beds, main="Number of bedrooms associated with the Minimum bid", xlab= "Bedrom of bathroom", ylab="Minimum bid")

boxplot(df$Minimum.bid~df$Bidded, main="Minimum of bid value by bidded status", xlab="Bidded", ylab="Minimum bid")

cor(df[sapply(df,is.numeric)], method= "pearson", use="complete.obs") #only use numeric columns in df for correlation
#Question2: 
 
fit.glm4<-glm(df$Bidded~df$Minimum.bid+df$Beds+df$Bath+df$Total.Area+df$Yr.blt,family=binomial())
summary(fit.glm4)

summary(df$Minimum.bid)
summary(df$Beds)
summary(df$Bath)
summary(df$Total.Area)
summary(df$Yr.blt)

#Question3:
fit.glm4<-glm(df$Bidded~df$Minimum.bid+df$Beds+df$Bath+df$Total.Area+df$Yr.blt,family=binomial())
summary(fit.glm4)

#Question4:
library(rcompanion)
nagelkerke(fit.glm4)

#Question5:
fit.glm5<-glm(df$Minimum.bid~df$Zillow.Value.estimate+df$Acres+df$Beds+df$Bath+df$Total.Area+df$Yr.blt, data=df)
summary(fit.glm5)


