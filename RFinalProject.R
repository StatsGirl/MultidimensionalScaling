#Author: Breya Walker
#Date: 11.26.2017
#Purpose: The purpose of this R script is to (1) summary pollution data (2) find significant variables
#associated with the following outcome vars (the outcome vars will be grouped together): Diagnosis outcome, White Blood cell count, IG G (i.e., protects against infection), IG A (i.e., found in lining of respistory tracks), IG M (i.e., 1st to respond to infection) counts, Lymphocytes (LYM) percent,
#T Lymphocytes, CD4 CD8 ratio (< than 2 = illness), CD3_CD_16_56_ (# shown to corr with tumor burden in patients), aspartate aminotransferase (AST) test (high levels =sick), MP_IgG_serum, MP_IgM_serum (some form of indication of IG M response in combination with white blood cells response to sickness),
#(3) similar outcome vars will be grouped together (4) use of cross validation to test model for accuracy 
#Decision tree with pruning will be applied to find diagnosis outcome based on IV vars
library (glmnet)
library(mice)
install.packages("VIM")
install.packages("imputeMissings")
install.packages("rpart")
library(imputeMissings)
library(VIM)
library(rcompanion)
library(pls)
library(earth)
library(ipred)

dir<-setwd('/Users/breyawalker/Desktop/takehomeproject')
Air<-read.csv('RealData.csv')

#replace missing with NA
for(i in 1:ncol(Air))
{

  Air[i][Air[i]=="missing"]<-NA
  
}

#male=2 female=1
for (i in 1:ncol(Air)){
  if (sapply(Air[i],is.factor)){
    Air[i]<-as.numeric(unlist(Air[i]))
  } 
}
#generate summary count tables for Air dataframe
summary(Air)

#see the pattern of missing values
md.pattern(Air)
aggr_plot <- aggr(Air, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Air), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
summary(aggr_plot)

#impute missing values with median value of column
for(i in 1:ncol(Air)){
  Values<-compute(Air[i],method="median/mode")
  Air[i]<-impute(Air[i],object=Values)
  
}
#check summary of newly imputed values to double check imputation
summary(Air)

aggr_plot <- aggr(Air, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Air), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#run the lasso regression for the following outcome variables:
#White Blood cell count,Lymphocytes (LYM) percent,
#T Lymphocytes, CD4 CD8 ratio (< than 2 = illness), CD3_CD_16_56_ (# shown to corr with tumor burden in patients), aspartate aminotransferase (AST) test (high levels =sick), MP_IgG_serum, MP_IgM_serum (some form of indication of IG M response in combination with white blood cells response to sickness),

DVs<-(Air[,41:62]) #all of the DVs excluding DX
Genderfactors=factor(Air$Gender) #create factors for Gender
DummiesG=model.matrix(~Genderfactors) #males=1 females=0
Air$Gender<-DummiesG[,2]
IVs<-data.frame(Air[,3:39])#all IVs
#DX<-factor(Air$Diagnosis)#create factors for DX
#DXFacts=model.matrix(~DX)

#correlation between numeric IVs to get a better sense of what variables group together for analyses
coe<-cor(DVs)
zdf <- as.data.frame(as.table(coe))
subset(zdf, abs(Freq) > 0.4)

#The following Dependent variables are grouped together based strong correlations:
#L_percent+GRN_percent (when both increase indicative of infection)
#IGA+IGM
#MP_IGM_serum+IgM
#CD3.CD4+CD3
#CD4CD8 ratio+CD3.CD4 (with a normal CD4CD8 ratio being 0.9 to 6)
##alpha_HBDH+LDH

#Fit the lasso regression to find significant IVs
train=sample(5487,1646)
test=(-train)
x <- as.formula(paste("Air$L_percent+Air$GRN_percent~", paste(names(Air)[3:39],collapse="+")))
x2<-data.frame(Air[3:39])
x1<-model.matrix(x, data=Air)
y1<-Air$L_percent+Air$GRN_percent #lympophycte variables together

y1.train<-y1[train]
y1.test<-y1[test]
fitE<-glmnet(x1[train,],y1[train],alpha=1) #Variables that predict white blood cell outcomes

plot(fitE, xvar="lambda")

#Cross Validation glmnet
cv.glmmod <- cv.glmnet(x1[train,],y1[train],alpha=1,nfolds=5, type.measure="mse")
plot(cv.glmmod)

set.seed(123456)
(best.lambda <- cv.glmmod$lambda.min)
lasso.pred=predict(cv.glmmod,s=best.lambda ,newx=x1[test,])
mean((lasso.pred-y1.test)^2)
coef(cv.glmmod, s = "lambda.min")

out=glmnet(x1,y1,alpha=1,lambda=best.lambda)
lasso.coef=predict(out,type="coefficients",s=best.lambda)[1:39,]
lasso.coef

#We can see that Age, Gender, PM10_0, O3_0, CO_1, PM10_3 are important for predicting L percent and GRN Percent in patients
#So the lasso model with λ chosen by cross-validation contains only 6 variables.
#inferential statistic using the 6 vars above

plot(Air$GRN_percent,Air$L_percent)
qqplot(Air$GRN_percent,Air$L_percent)
fit.lm<-lm(Air$L_percent+Air$GRN_percent~Air$Gender+Air$Age+Air$PM10_0+Air$O3_0+Air$CO_1+Air$PM10_3, data=Air)
summary(fit.lm)
AIC(fit.lm)
nagelkerke(fit.lm)

#Predict outcome variables of  IG A (i.e., found in lining of respistory tracks), IG M (i.e., 1st to respond to infection) counts
#Fit the lasso regression
train=sample(5487,1646)
test=(-train)
x <- as.formula(paste("Air$IgA+Air$IgM~", paste(names(Air)[3:39],collapse="+")))
x2<-model.matrix(x, data=Air)
y2<-Air$IgA+Air$IgM
y2.train<-y2[train]
y2.test<-y2[test]
fitE<-glmnet(x2[train,],y2[train],alpha=1) #Variables that predict white blood cell outcomes

plot(fitE, xvar="lambda")

#Cross Validation glmnet
cv.glmmod2 <- cv.glmnet(x2[train,],y2[train],alpha=1,nfolds=20, type.measure="mse")
plot(cv.glmmod2)

set.seed(123456)
(best.lambda <- cv.glmmod2$lambda.min)
lasso.pred=predict(cv.glmmod2,s=best.lambda ,newx=x2[test,])
mean((lasso.pred-y2.test)^2)
coef(cv.glmmod2, s = "lambda.min")

out=glmnet(x2,y2,alpha=1,lambda=best.lambda)
lasso.coef=predict(out,type="coefficients",s=best.lambda)[1:39,]
lasso.coef
#Gender, age AQI_0, NO2_0, SO2_0,NO2_1, SO2_0, NO2_1, SO2_1, NO2_3, AQI_4, PM2.5_4, NO2_4, SO2_4 are significant vars to include ofr IgA and IgM
plot(Air$IgA,Air$IgM)
cor(Air$IgA,Air$IgM)
qqplot(Air$IgA,Air$IgM)
fit2.lm<-lm(Air$IgA+Air$IgM~Air$Gender+Air$Age+Air$AQI_0+Air$AQI_4+Air$NO2_0+Air$NO2_1+Air$NO2_3+Air$NO2_4+Air$SO2_0+Air$SO2_1+Air$SO2_4+Air$PM2.5_4,data=Air)
summary(fit2.lm)
AIC(fit2.lm)
nagelkerke(fit2.lm)

#MP_IGM_serum+IgM
train=sample(5487,1646)
test=(-train)
x <- as.formula(paste("Air$MP_IgM_serum+Air$IgM~", paste(names(Air)[3:39],collapse="+")))
x3<-model.matrix(x, data=Air)
y3<-Air$MP_IgM_serum+Air$IgM
y3.train<-y3[train]
y3.test<-y3[test]
fitE<-glmnet(x3[train,],y3[train],alpha=1) #Variables that predict white blood cell outcomes

plot(fitE, xvar="lambda")

#Cross Validation glmnet
cv.glmmod3 <- cv.glmnet(x3[train,],y3[train],alpha=1,nfolds=20, type.measure="mse")
plot(cv.glmmod3)

set.seed(123456)
(best.lambda <- cv.glmmod3$lambda.min)
lasso.pred=predict(cv.glmmod3,s=best.lambda ,newx=x3[test,])
mean((lasso.pred-y3.test)^2)
coef(cv.glmmod3, s = "lambda.min")

out=glmnet(x3,y3,alpha=1,lambda=best.lambda)
lasso.coef=predict(out,type="coefficients",s=best.lambda)[1:39,]
lasso.coef

#Gender, Age,SO2_0, SO2_1,CO_4, O3_1,PM2.5_0,PM2.5_4, PM10_0
plot(Air$MP_IgM_serum,Air$IgM)
cor(Air$MP_IgM_serum,Air$IgM)
qqplot(Air$MP_IgM_serum,Air$IgM)
c<-c(1,2,7,14,36,16,4,5,32)
xnams3<-colnames(IVs[,c]) #select the significant 9 vars above
for (i in 1:9)
{
  xnams3[i]<-paste("Air$",xnams3[i])
}
formula3<-as.formula(paste("Air$MP_IgM_serum+Air$IgM~",paste(xnams3,collapse="+")))
fit3.glm<-glm(formula3,data=Air,family="gaussian") #use glm because non linear due to qqplot
summary(fit3.glm)
AIC(fit3.glm)
nagelkerke(fit3.glm)

#CD3.CD4+CD3
train=sample(5487,1646)
test=(-train)
x <- as.formula(paste("Air$CD3.CD4.+Air$CD3.~", paste(names(Air)[3:39],collapse="+")))
x4<-model.matrix(x, data=Air)
y4<-Air$CD3.CD4.+Air$CD3.
y4.train<-y4[train]
y4.test<-y4[test] 
fitE<-glmnet(x4[train,],y4[train],alpha=1) #Variables that predict white blood cell outcomes

plot(fitE, xvar="lambda")

#Cross Validation glmnet
cv.glmmod4 <- cv.glmnet(x4[train,],y4[train],alpha=1,nfolds=20, type.measure="mse")
plot(cv.glmmod4)

set.seed(123456)
(best.lambda <- cv.glmmod4$lambda.min)
lasso.pred=predict(cv.glmmod4,s=best.lambda ,newx=x4[test,])
mean((lasso.pred-y4.test)^2)
coef(cv.glmmod4, s = "lambda.min")

out=glmnet(x4,y4,alpha=1,lambda=best.lambda)
lasso.coef=predict(out,type="coefficients",s=best.lambda)[1:39,]
lasso.coef

#O3_0, AQI_1, Gender, NO2_, SO2_2, CO_4, Age, PM2.5_4, AQI_0, CO_0, PM2.5_2, PM10_4sigificant vars to include for MP_IgM and IgM
plot(Air$CD3.CD4.,Air$CD3.)
cor(Air$CD3.CD4.,Air$CD3.)
qqplot(Air$CD3.CD4.,Air$CD3.)
fit4.lm<-lm(Air$CD3.CD4.+Air$CD3.~Air$O3_0+Air$AQI_1+Air$Gender+Air$NO2_0+Air$SO2_2+Air$CO_4+Air$Age, Air$PM2.5_4+Air$AQI_0+Air$CO_0+Air$PM2.5_2+Air$PM10_4,data=Air)
summary(fit4.lm)
AIC(fit4.lm)
nagelkerke(fit4.lm)

#CD4CD8 ratio+CD3.CD4 (with a normal CD4CD8 ratio being 0.9 to 6)
train=sample(5487,1646)
test=(-train)
x <- as.formula(paste("Air$CD4.CD8+Air$CD3.CD4.~", paste(names(Air)[3:39],collapse="+")))
x5<-model.matrix(x, data=Air)
y5<-Air$CD4.CD8+Air$CD3.CD4.
y5.train<-y5[train]
y5.test<-y5[test] 
fitE<-glmnet(x5[train,],y5[train],alpha=1) #Variables that predict white blood cell outcomes

plot(fitE, xvar="lambda")

#Cross Validation glmnet
cv.glmmod5 <- cv.glmnet(x5[train,],y5[train],alpha=1,nfolds=20, type.measure="mse")
plot(cv.glmmod5)

set.seed(123456)
(best.lambda <- cv.glmmod5$lambda.min)
lasso.pred=predict(cv.glmmod5,s=best.lambda ,newx=x5[test,])
mean((lasso.pred-y5.test)^2)
coef(cv.glmmod5, s = "lambda.min")

out=glmnet(x5,y5,alpha=1,lambda=best.lambda)
lasso.coef=predict(out,type="coefficients",s=best.lambda)[1:39,]
lasso.coef

#O#_0, AQI_1, NO@_2, Gender, NO2_0, Age, PM10_1,O3_4, CO_0, PM2.5_2, PM10_4
plot(Air$CD4.CD8,Air$CD3.CD4.)
cor(Air$CD4.CD8,Air$CD3.CD4.)
qqplot(Air$CD4.CD8,Air$CD3.CD4.)
fit5.glm<-glm(Air$CD4.CD8+Air$CD3.CD4.~Air$O3_0+Air$AQI_1+Air$NO2_2+Air$Gender+Air$NO2_0+Air$Age+Air$PM10_1+Air$O3_4+Air$CO_0+Air$PM2.5_2+Air$PM10_4,data=Air)
summary(fit5.glm)
AIC(fit5.glm)
nagelkerke(fit5.glm)

##alpha_HBDH+LDH
train=sample(5487,1646)
test=(-train)
x <- as.formula(paste("Air$alpha_HBDH+Air$LDH~", paste(names(Air)[3:39],collapse="+")))
x6<-model.matrix(x, data=Air)
y6<-Air$alpha_HBDH+Air$LDH
y6.train<-y6[train]
y6.test<-y6[test] 
fitE<-glmnet(x6[train,],y6[train],alpha=1) #Variables that predict white blood cell outcomes

plot(fitE, xvar="lambda")

#Cross Validation glmnet
cv.glmmod6 <- cv.glmnet(x6[train,],y6[train],alpha=1,nfolds=20, type.measure="mse")
plot(cv.glmmod6)

set.seed(123456)
(best.lambda <- cv.glmmod6$lambda.min)
lasso.pred=predict(cv.glmmod6,s=best.lambda ,newx=x6[test,])
mean((lasso.pred-y6.test)^2)
coef(cv.glmmod6, s = "lambda.min")

out=glmnet(x6,y6,alpha=1,lambda=best.lambda)
lasso.coef=predict(out,type="coefficients",s=best.lambda)[1:39,]
lasso.coef

#PM10_0, NO2_0, CO_4, Gender, SO2_0, O#_4, Age, SO2_1, PM2.5_4, O3_0, CO_2
plot(Air$alpha_HBDH,Air$LDH)
cor(Air$alpha_HBDH,Air$LDH)
qqplot(Air$alpha_HBDH,Air$LDH)
fit6.lm<-lm(Air$alpha_HBDH+Air$LDH~Air$PM10_0+Air$NO2_0+Air$CO_4+Air$Gender+Air$SO2_0+Air$O3_4+Air$Age+Air$SO2_1+Air$PM2.5_4+Air$O3_0+Air$CO_2,data=Air)
summary(fit6.lm)
AIC(fit6.lm)
nagelkerke(fit6.lm)

#MARS for all 6 pairs of IVs
#y1,y2, y3, y4, y5, y6
#y1: L_percent+GRN_percent (when both increase indicative of infection)
x<-Air[,3:39]
marsFit<-earth(x,y1)
marsFit
summary(marsFit)
varImp(marsFit)

#y2: IGA+IGM
x<-Air[,3:39]
marsFit2<-earth(x,y2)
marsFit2
summary(marsFit2)
varImp(marsFit2)

#y3: MP_IGM_serum+IgM
x<-Air[,3:39]
marsFit3<-earth(x,y3)
marsFit3
summary(marsFit3)
varImp(marsFit3)

#y4: CD3.CD4+CD3
x<-Air[,3:39]
marsFit4<-earth(x,y4)
marsFit4
summary(marsFit4)
varImp(marsFit4)

#y5: CD4CD8 ratio+CD3.CD4 (with a normal CD4CD8 ratio being 0.9 to 6)
x<-Air[,3:39]
marsFit5<-earth(x,y5)
marsFit5
summary(marsFit5)
varImp(marsFit5)

#y6: alpha_HBDH+LDH
x<-Air[,3:39]
marsFit6<-earth(x,y6)
marsFit6
summary(marsFit6)
varImp(marsFit6)

#DX Naives Bayes with pruning
DXfactors<-factor(Air$Diagnosis)
DXfactorsDF<-data.frame(DXfactors)
DXlist<-list(DXfactors)
# fit model
fit <- bagging(DXfactors~., data=IVs, nbagg=100, coob=TRUE,subset=train)
# summarize the fit
summary(fit)
print(fit)
