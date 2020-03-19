#3) Emp_data -> Build a prediction model for Churn_out_rate

emp_data<-read.csv(file.choose())
View(emp_data)
attach(emp_data)

#Exploratory Data Analysis
summary(emp_data)
#EDA 1st moment of business decision
mean(Salary_hike)
mean(Churn_out_rate)
median(Salary_hike)
median(Churn_out_rate)
#EDA 2nd moment of business decision
sd(Salary_hike)
sd(Churn_out_rate)
var(Salary_hike)
var(Churn_out_rate)
#EDA 3rd and 4th moment of business decision
library(moments)
skewness(Salary_hike) #POSTIVE Skewness
skewness(Churn_out_rate)#POSTIVE Skewness
kurtosis(Salary_hike)#POSTIVE kurtosis
kurtosis(Churn_out_rate)#POSTIVE kurtosis

#graphical representation
boxplot(Salary_hike,horizontal = T)
boxplot(Churn_out_rate, horizontal= T)
hist(Salary_hike)
hist(Churn_out_rate)

#Normality Check
qqnorm(Salary_hike)
qqline(Salary_hike)
qqnorm(Churn_out_rate)
qqline(Churn_out_rate)

#Scatter Plot
plot(Salary_hike,Churn_out_rate,xlab= 'Salary hike', ylab='Churn out Rate', pch=20)
cor(Salary_hike,Churn_out_rate) #r=-0.91

#Simple Regresson Model
reg2<-lm(Churn_out_rate~Salary_hike,data=emp_data)
summary(reg2)
reg2$coefficients
reg2$residuals
Predicted_Churn_out_rate<-reg2$fitted.values
Predicted_Churn_out_rate
Actual_Churn_out_rate<-emp_data$Churn_out_rate
Actual_Churn_out_rate
confint(reg2,level=0.95)
predict(reg2, interval = "predict")
predict(reg2, interval = "confidence")

#R squared= 0.83

#log transformation
reg2_log<-lm(Churn_out_rate~log(Salary_hike), data=emp_data)
summary(reg2_log) #R_squared=0.85, improved from last model
confint(reg2_log,level=0.95)
predict(reg2_log, interval = "predict")
predict(reg2_log, interval = "confidence")

#expotential Transformation
reg2_exp<-lm(log(Churn_out_rate)~Salary_hike, data=emp_data)
summary(reg2_exp) #R_squared=0.83, reduced from last model
confint(reg2_exp,level=0.95)
predict(reg2_exp, interval = "predict")
Predicted_Delivery_time1<-exp(reg2_exp$fitted.values)#exp(logy)=exp(b0+b1x)-----y=exp(b0+b1x)
Predicted_Delivery_time1

#logarithmic transformation gives good R squared value
RMSE<-sqrt(mean(reg2_log$residuals^2))
RMSE
