#2) Delivery_time -> Predict delivery time using sorting time 

delivery_Time<-read.csv(file.choose())
View(delivery_Time)
attach(delivery_Time)

#Exploratory Data Analysis
summary(delivery_Time)

#EDA 1st moment of business decision
mean(Sorting.Time)
mean(Delivery.Time)
median(Sorting.Time)
median(Delivery.Time)
#EDA 2nd moment of business decision
sd(Sorting.Time)
sd(Delivery.Time)
var(Sorting.Time)
var(Delivery.Time)
#EDA 3rd and 4th moment of business decision
library(moments)
skewness(Sorting.Time) #POSTIVE Skewness
skewness(Delivery.Time)#POSTIVE Skewness
kurtosis(Sorting.Time)#POSTIVE kurtosis
kurtosis(Delivery.Time)#POSTIVE kurtosis

#graphical representation
boxplot(Delivery.Time,horizontal = T)
boxplot(Sorting.Time, horizontal= T)
hist(Delivery.Time)
hist(Sorting.Time)

#Normality Check
qqnorm(Sorting.Time)
qqline(Sorting.Time)
qqnorm(Delivery.Time)
qqline(Delivery.Time)

#Scatter Plot
plot(Sorting.Time,Delivery.Time,xlab= 'Sorting Time', ylab='Delivery Time', pch=20)
cor(Sorting.Time,Delivery.Time) # r=0.82 moderarte correlation

#Simple Regresson Model
reg1<-lm(Delivery.Time~Sorting.Time,data=delivery_Time)
summary(reg1)
reg1$coefficients
reg1$residuals
Predicted_Delivery_time<-reg1$fitted.values
Predicted_Delivery_time
Actual_Delivery_time<-delivery_Time$Delivery.Time
Actual_Delivery_time
confint(reg1,level=0.95)
predict(reg1, interval = "predict")

#using transformations since R_squared =0.68

#logarithmic transformation on x
reg1_log<-lm(Delivery.Time~log(Sorting.Time), data=delivery_Time)
summary(reg1_log) #R_squared=0.69, improved from last model
confint(reg1_log,level=0.95)
predict(reg1_log, interval = "predict")

#expotential Transformation
reg1_exp<-lm(log(Delivery.Time)~Sorting.Time, data=delivery_Time)
summary(reg1_exp) #R_squared=0.71, improved from last model
confint(reg1_exp,level=0.95)
predict(reg1_exp, interval = "predict")

#reciprocal transformation
rec_sorting_time<-1/Sorting.Time
delivery_Time_new<-data.frame(delivery_Time,rec_sorting_time)
attach(delivery_Time_new)
reg1_rec<-lm(Delivery.Time~rec_sorting_time, data=delivery_Time_new)
summary(reg1_rec) #R_squared=0.64, reduced from last model
confint(reg1_rec,level=0.95)
predict(reg1_rec, interval = "predict")

#square root transformation
reg1_sqrt<-lm(Delivery.Time~sqrt(Sorting.Time), data=delivery_Time)
summary(reg1_sqrt) #R_squared=0.69, improved from last model
confint(reg1_sqrt,level=0.95)
predict(reg1_sqrt, interval = "predict")

# 1/sqrt transformation
rec_sqrt_sorting_time<-1/sqrt(Sorting.Time)
delivery_Time_new<-data.frame(delivery_Time,rec_sqrt_sorting_time)
attach(delivery_Time_new)
reg1_rec_sqrt<-lm(Delivery.Time~rec_sqrt_sorting_time, data=delivery_Time_new)
summary(reg1_rec_sqrt) #R_squared=0.68, decreased from last model
confint(reg1_rec_sqrt,level=0.95)
predict(reg1_rec_sqrt, interval = "predict")

#sqr transformation
reg1_sqr<-lm(Delivery.Time~(Sorting.Time^2), data=delivery_Time)
summary(reg1_sqr) #R_squared=0.68, decreased from last model
confint(reg1_sqr,level=0.95)
predict(reg1_sqr, interval = "predict")

# log sqrt transformation
reg1_log_sqrt<-lm(log(Delivery.Time)~sqrt(Sorting.Time), data=delivery_Time)
summary(reg1_log_sqrt) #R_squared =0.75 improved than previous models
confint(reg1_log_sqrt,level=0.95)
predict(reg1_log_sqrt, interval = "predict")
predict(reg1_log_sqrt, interval = "confidence")

#sqrt log transformation
reg1_sqrt_log<-lm(sqrt(Delivery.Time)~log(Sorting.Time), data=delivery_Time)
summary(reg1_sqrt_log) #R_squared=0.74


#from all above models 
# reg1_log_sqrt has maximum R_squared value
Predicted_Delivery_time1<-exp(reg1_log_sqrt$fitted.values)#exp(logy)=exp(b0+b1x)-----y=exp(b0+b1x)
Predicted_Delivery_time1
Actual_Delivery_time<-Delivery.Time
Actual_Delivery_time

#RMSE
error<-Actual_Delivery_time-Predicted_Delivery_time1
RMSE<-sqrt(mean(error^2))
RMSE

