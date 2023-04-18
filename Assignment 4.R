#Assignment 4 Regression Methods
par(mfrow=c(1,1))
data<-read.table("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Regression Methods/mpg_data.txt",header = T)
colnames(data)=c("MPH","MPG")
attach(data)
plot(MPH,MPG,main="Speed vs Fuel Efficiency")
#1 There appears to be a strong polynomial relationship among the variables
#2 It seems the variance might not be constant throughout the data. It looks like at the height of the curve
# the variance increases
plot(MPH,1/(MPG),main="Speed vs Fuel Efficiency")
MPG.t<-1/(MPG)

mod2 = lm(MPG.t~MPH+I(MPH^2))
mod3 = lm(MPG.t~MPH+I(MPH^2)+I(MPH^3))
mod4 = lm(MPG.t~MPH+I(MPH^2)+I(MPH^3)+I(MPH^4))
mod5 = lm(MPG.t~MPH+I(MPH^2)+I(MPH^3)+I(MPH^4)+I(MPH^5))
summ2 = summary(mod2)
summ3 = summary(mod3)
summ4 = summary(mod4)
summ5 = summary(mod5)
summ2 # R^2 = 0.856, p values are significant
summ3 # R^2 = 0.8573, ^3 p value isn't significant
summ4 # R^2 = 0.9654, p values are significant
summ5 # R^2 = 0.9654, ^3, ^4, ^5 p values aren't significant
#3 It looks like summ4 is our best bet, as all variables are significant and it has the highest R^2 of the models
# MPG.t = -3.872(10^-3)*(MPH)+1.094(10^-4)*(MPH^2)-1.292(10^-6)*(MPH^3)+5.412(10^-9)*(MPH^4)+7.551(10^-2)
# MPG = 1/(MPG.t)
#4 Below is the regression line
plot(MPH,MPG,main="Speed vs Fuel Efficiency")
sort.MPH=sort(data$MPH)
mod4.lines=predict(mod4, data.frame(MPH=sort.MPH),interval = "confidence",level=0.9)
lines(sort.MPH,1/mod4.lines[,1],col='blue')
#5 There is a statistically significant relationship between MPH and MPG, I can tell because the p-value of the model
# is <2.2e-16
sort.data=data[order(data$MPH),]
sort.data
plot(1/mod4.lines[,1],sort.data$MPG)
cor(1/mod4.lines[,1],sort.data$MPG)
#6 The coefficient of determination is 97.65004%
which(1/mod4.lines[,1]==max(1/mod4.lines[,1]))
sort.MPH[118]
#7 38.07686 MPG is the most fuel efficient rate at the 118th index.
# When looking up the index, we see that at 85.70451 MPH this fuel efficiency peaks
#8 Below is the code to overlay the 90% confidence bands for mean MPG
plot(MPH,MPG,main="Speed vs Fuel Efficiency")
lines(sort.MPH,1/mod4.lines[,1],col='blue')
lines(sort.MPH,1/mod4.lines[,2],lty='dashed',col='red')
lines(sort.MPH,1/mod4.lines[,3],lty='dashed',col='red')
