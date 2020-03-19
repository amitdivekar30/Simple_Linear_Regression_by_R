#4) Salary_hike -> Build a prediction model for Salary_hike

Salary_Data<-read.csv(file.choose())
View(Salary_Data)
attach(Salary_Data)

#Exploratory Data Analysis
summary(Salary_Data)
#EDA 1st moment of business decision
mean(YearsExperience)
mean(Salary)
median(YearsExperience)
median(Salary)
#EDA 2nd moment of business decision
sd(YearsExperience)
sd(Salary)
var(YearsExperience)
var(Salary)
#EDA 3rd and 4th moment of business decision
library(moments)
skewness(YearsExperience) #POSTIVE Skewness
skewness(Salary)#POSTIVE Skewness
kurtosis(YearsExperience)#POSTIVE kurtosis
kurtosis(Salary)#POSTIVE kurtosis

#graphical representation
boxplot(YearsExperience,horizontal = T)
boxplot(Salary, horizontal= T)
hist(YearsExperience)
hist(Salary)

#Normality Check
qqnorm(YearsExperience)
qqline(YearsExperience)
qqnorm(Salary)
qqline(Salary)


#Scatter Plot
plot(YearsExperience,Salary,xlab= 'Years of Experience', ylab='Salary', pch=20)
cor(YearsExperience,Salary)
#r=0.98

#Simple Regresson Model
reg3<-lm(Salary~YearsExperience,data=Salary_Data)
summary(reg3)
reg3$coefficients
reg3$residuals
Predicted_Salary<-reg3$fitted.values
Predicted_Salary
Actual_Salary<-Salary_Data$Salary
Actual_Salary
confint(reg3,level=0.95)
predict(reg3, interval = "predict")
predict(reg3, interval = "confidence")

#R squared= 0.957

#Existing model shows strong as R squared =0.957

#RMSE
RMSE<-sqrt(mean(reg3$residuals^2))
RMSE

