#Regression Methods Assignment 2

hubble_data<-read.table("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Regression Methods/hubble_data.txt")
colnames(hubble_data)<-c("distance","recession_velocity")
hubble_data<-hubble_data[2:25,]
hubble_data #1 below
plot(hubble_data$distance,hubble_data$recession_velocity,xlab="distance (megaparsecs)",ylab="recession velocity (km/s)",main="Hubble Data")
#2 Yes, it would be appropriate. From an eye inspection, it looks as if there is a positive linear relationship between distance and recession velocity.
hubble_data$distance<-as.numeric(as.character(hubble_data$distance))
hubble_model<-lm(recession_velocity~distance,data=hubble_data)
hubble_model #3 As the distance increases, the recession velocity increases at a rate of 454.16 km/s
summary(hubble_model) #4 The data suggests there is a statistically significant relationship between the two variable. The p-value of the explanatory variable is 4.48e-06, below alpha=0.001
#5 Distance explains recession velocity 60.64% of the time, our R^2 value.
predict(hubble_model,data.frame(distance=0.77),interval = "predict",level = 0.95)
#6 An estimate of Andromeda's recession velocity is 308.9184 km/s, and I'm 95% certain it's actual recession velocity is between (-184.5624, 802.3992)
