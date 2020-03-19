#1) Calories_consumed-> predict weight gained using calories consumed

calories_consumed<-read.csv(file.choose())
View(calories_consumed)
attach(calories_consumed)

#Exploratory Data Analysis
summary(calories_consumed)
#EDA 1st moment of business decision
mean(Weight.gained..grams.)
mean(Calories.Consumed)
median(Weight.gained..grams.)
median(Calories.Consumed)
#EDA 2nd moment of business decision
sd(Weight.gained..grams.)
sd(Calories.Consumed)
var(Weight.gained..grams.)
var(Calories.Consumed)
#EDA 3rd and 4th moment of business decision
library(moments)
skewness(Weight.gained..grams.) #POSTIVE Skewness
skewness(Calories.Consumed)#POSTIVE Skewness
kurtosis(Weight.gained..grams.)#POSTIVE kurtosis
kurtosis(Calories.Consumed)#POSTIVE kurtosis

#graphical representation
boxplot(Weight.gained..grams.,horizontal = T)
boxplot(Calories.Consumed, horizontal= T)
hist(Weight.gained..grams.)
hist(Calories.Consumed)

#Normality Check
qqnorm(Weight.gained..grams.)
qqline(Weight.gained..grams.)
qqnorm(Calories.Consumed)
qqline(Calories.Consumed)

#Scatter Plot
plot(Weight.gained..grams.,Calories.Consumed,xlab='Weight gained(grams)', ylab= 'Calories Consumed', pch=20)
cor(Weight.gained..grams.,Calories.Consumed)
#r=0.94----strong correlation


#Simple Regresson Model
reg<-lm(Calories.Consumed~Weight.gained..grams.,data=calories_consumed)
summary(reg)
reg$coefficients
reg$residualsActual_calories_consumed
Predicted_calories_consumed<-reg$fitted.values
Predicted_calories_consumed
Actual_calories_consumed<-calories_consumed$Calories.Consumed
Actual_calories_consumed
confint(reg,level=0.95)
predict(reg, interval = "predict")
predict(reg, interval = "confidence")

# R_squared =0.89 so no further transforations are required
RMSE<-sqrt(mean(reg$residuals^2))

# Visualising the results
library(ggplot2)
ggplot() +
  geom_point(aes(x = Weight.gained..grams., y = Calories.Consumed),
             colour = 'red') +
  geom_line(aes(x = Weight.gained..grams., y = predict(reg, data = calories_consumed )),
            colour = 'blue') +
  ggtitle('Calories consumed vs Weight gained in gms') +
  xlab('Weight gained in gms') +
  ylab('Calories Consumed')

