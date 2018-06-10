data= read.csv(file="C:/Users/shash/Documents/R for Stats/Mini Projects/Miniproject 6/crime.csv") #read data from csv file
data #preview data
str(data) #data stats
#'data.frame':	50 obs. of  9 variables:
# $ state        : Factor w/ 50 levels "Alabama","Alaska",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ murder.rate  : num  7.4 4.3 7 6.3 6.1 3.1 2.9 3.2 5.6 8 ...
# $ poverty      : num  14.7 8.4 13.5 15.8 14 8.5 7.7 9.9 12 12.5 ...
# $ high.school  : num  77.5 90.4 85.1 81.7 81.2 89.7 88.2 86.1 84 82.6 ...
# $ college      : num  20.4 28.1 24.6 18.4 27.5 34.6 31.6 24 22.8 23.1 ...
# $ single.parent: num  26 23.2 23.5 24.7 21.8 20.8 22.9 25.6 26.5 25.5 ...
# $ unemployed   : num  4.6 6.6 3.9 4.4 4.9 2.7 2.3 4 3.6 3.7 ...
# $ metropolitan : num  70.2 41.6 87.9 49 96.7 84 95.6 81.4 93 69.1 ...
# $ region       : Factor w/ 4 levels "North Central",..: 3 4 4 3 4 4 2 3 3 3 ...
#attaching the variables in memory
attach(data)
#using pairs to understand relationship between murder rate and the predictors
pairs(data[2:8])



fit.modelfull<-lm(data$murder.rate~data$poverty+data$high.school+data$college+data$single.parent+data$unemployed+data$metropolitan+data$region)
summary(fit.modelfull)




#Fitting murder rate with poverty predictor
fitm.p <- lm(data$murder.rate ~ data$poverty)
summary(fitm.p)


#Fitting murder rate with high school predictor
fitm.hs <- lm(data$murder.rate ~ data$high.school)
summary(fitm.hs)


#Fitting murder rate with college predictor
fitm.c <- lm(data$murder.rate ~ data$college)
summary(fitm.c)

#Fitting murder rate with single parent predictor
fimt.sp <- lm(data$murder.rate ~ data$single.parent)
summary(fitm.sp)

#Fitting murder rate with unemployed predictor
fitm.u <- lm(data$murder.rate ~ data$unemployed)
summary(fitm.u)

#Fitting murder rate with metropolitan predictor
fitm.m <- lm(data$murder.rate ~ data$metropolitan)
summary(fitm.m)

#box plot to analyze region against murder rate
plot(data$murder.rate ~ data$region) 
#checking most prequent qualitative predictor
table(data$region)
#Fitting murder rate with region predictor
fitm.r <- lm(data$murder.rate ~ data$region)
summary(fit.mr)

#full model with all predictors
fit.modelfull<-lm(data$murder.rate~data$poverty+data$high.school+data$college+data$single.parent+data$unemployed+data$metropolitan+data$region)
anova(fit.modelfull)
summary(fit.modelfull)

#Comparing qqnorm plots before and after transformation
par(mfrow=c(2,2))
#Residual plot of the full model without tranformation
plot(fitted(fit.modelfull),resid(fit.modelfull))
abline(h=0)
#QQplot of residuals of the full model without tranformation
qqnorm(resid(fit.modelfull))
qqline(resid(fit.modelfull))

#applying log transformation and checking the residual plot and testing the normality assumption
fit.fulllog<-update(fit.modelfull,log(data$murder.rate) ~ .)
#Residual plot of the full model with log tranformation
plot(fitted(fit.fulllog),resid(fit.fulllog))
abline(h=0)
#QQplot of residuals of the full model with log tranformation
qqnorm(resid(fit.fulllog))
qqline(resid(fit.fulllog))

#checking the transformed full model
summary(fit.fulllog)

#Forward Method of getting a reduced Model
#First model with just single parent
fit.1f<-lm(murder.rate~single.parent)
summary(fit.1f)
#adding high school and single parent
fit.2f<-lm(data$murder.rate~data$single.parent+data$high.school)
summary(fit.2f)
#adding high school, single parent and Region
fit.3f<-lm(data$murder.rate~data$single.parent+data$high.school+data$region)
summary(fit.3f)
#adding high school, single parent,Region and Poverty
fit.4f<-lm(data$murder.rate~data$single.parent+data$high.school+data$region+data$poverty)
summary(fit.4f)
#adding high school, single parent,Region and unemployed
fit.5f<-lm(data$murder.rate~data$single.parent+data$high.school+data$region+data$unemployed)
summary(fit.5f)
#adding high school, single parent,Region and metropolitan
fit.6f<-lm(data$murder.rate~data$single.parent+data$high.school+data$region+data$metropolitan)
summary(fit.6f)
#adding high school, single parent,Region,Poverty and college
fit.7f<-lm(data$murder.rate~data$single.parent+data$high.school+data$region+data$metropolitan+data$college)
summary(fit.7f)

#comparing full model against reduced model with high school, single parent,Region and metropolitan
anova(fit.modelfull,fit.6f)

#Backward Method of getting a reduced Model
#removing poverty from full model
fit.1b<-update(fit.modelfull, . ~ . - poverty)
summary(fit.1b)
#removing unemployed from fit.1b
fit.2b<-update(fit.1b, . ~ . - unemployed)
summary(fit.2b)
#removing college from fit.2b
fit.3b<-update(fit.2b, . ~ . - college)
summary(fit.3b)
#removing high school from fit.3b
fit.4b<-update(fit.3b, . ~ . - high.school)
summary(fit.4b)
#removing region from fit.3b
fit.5b<-update(fit.3b, . ~ . - region)
summary(fit.5b)
#ANOVA analysis between full and reduced model being used forward - fit.3b
anova(fit.3b,fit.modelfull)

par(mfrow=c(1,2))
# Residual plot and checking for normality assumption for the reduced model
plot(fitted(fit.3b),resid(fit.3b))
abline(h=0)
#QQplot
qqnorm(resid(fit.3b))
qqline(resid(fit.3b))

#Using StepAIC function to verify and compare the reduced model obtained above
#using R package MASS
library(MASS)

#Forward selection
prog.lm.forstep=stepAIC(fit.modelfull, scope=list(lower=~1,upper=~poverty+high.school+college+single.parent+unemployed+metropolitan+region),direction="forward") 
#Backward selection same as (fit.modelfull,trace=0) as the scope argument is missing
prog.lm.backstep=stepAIC(fit.modelfull, scope=list(lower=~1,upper=~poverty+high.school+college+single.parent+unemployed+metropolitan+region),direction="backward") 
#Both
prog.lm.bothstep=stepAIC(fit.modelfull, scope=list(lower=~1,upper=~poverty+high.school+college+single.parent+unemployed+metropolitan+region),direction="both") 
summary(fit.3b)

#Prediction using the model
MRPrediction=4.19460-(0.12879*mean(high.school))+ (0.42150*mean(single.parent))+(0.03325*mean(metropolitan))-(2.34448*0)-(0.04464*1)-(-0.30106*0)
MRPrediction



