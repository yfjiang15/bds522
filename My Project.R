library(dplyr)
library(ggplot2)
library(latexpdf)
setwd("~/Documents/OneDrive/Documents/UPenn MBDS 18Fall/BDS 522/Project")
data <- read.csv("Data_2015.csv")
attach(data)

shapiro.test(AveWage)
shapiro.test(PropertyValue)
Shapiro.test(Immigration)

hist(Population, breaks = 40)
hist(AveWage, breaks=20)
hist(Immigration,breaks = 30)

summary(lm(Immigration ~ NonE))
t.test(Immigration,NonE)

# single regressions
summary(lm(Immigration ~ PropertyValue))
summary(lm(Immigration ~ Insured))
summary(lm(Immigration ~ AveWage))
summary(lm(Immigration ~ factor(FoodDanger)))

par(mfrow=c(1,3))
plot(AveWage,Immigration, main = "Averate Wage")
plot(PropertyValue,Immigration, main = "Property Value")
plot(Insured,Immigration, main = "Health Insurace Coverage")

#multiple regressions
summary(lm(Immigration ~ AveWage + PropertyValue + Insured + factor(FoodDanger)))

summary(lm(Immigration ~ PropertyValue + Insured + factor(FoodDanger)))


ggplot(data, aes(x = Insured, y = Immigration, color = FoodDanger)) + geom_smooth(method = "lm") + geom_point(size = 1, shape = 2)

plot(Immigration,AveWage)
par(new=TRUE)
plot(Immigration,PropertyValue)

ggplot(data,aes(x=AveWage,y=Immigration)) + line(method = "lm")+labs("Averate Wage") 

