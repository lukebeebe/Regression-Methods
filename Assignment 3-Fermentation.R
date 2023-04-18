#Regression Methods Assignment 3
#Luke Beebe

model<-read.table("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Regression Methods/modeling_data.txt", header = T)
train<-read.table("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Regression Methods/validation_data.txt", header = T)

plot(model$Temperature,model$Alcohol_Percentage, main="Fermentation Results") #1 there is an outlier in the dataset, otherwise seems negatively correlated linear relationship
model<-model[-186,] #remove outlier, look at data
plot(model$Temperature,model$Alcohol_Percentage, main="Fermentation Results")
#The data's variance seems to be predicted by the explanatory variable-Heteroscedastic data
#confidence/prediction intervals/hypothesis tests are no longer valid, unless we fix it
plot((model$Temperature-50)^3,1/(model$Alcohol_Percentage), main="Fermentation Results")
#This seemed to do the trick, variation looks constant throughout the data. Had to play around with the transformations for a little.
original_lm<-lm(Alcohol_Percentage~Temperature,data=model) #Parameter estimates are 26.174-0.358t, then we have to transform it
original_lm
summary(original_lm) #clear relation between the variable, R^2 is also pretty high a 86.82
plot(model$Temperature,model$Alcohol_Percentage, main="Fermentation Results") #graph from #1
abline(original_lm,col='red') #regression line

alc_predictions<-predict(original_lm,data.frame(Temperature=train$Temperature)) #predicts values using lm
plot(alc_predictions,train$Alcohol_Percentage,main="SLM Predicted vs Actual") #plots predicted vs actual for train data
cor(alc_predictions,train$Alcohol_Percentage) #correlation is 91.70591%

transformed_df<-data.frame((model$Temperature-50)^3,1/(model$Alcohol_Percentage))
colnames(transformed_df)<-c("transformed_temp", "transformed_alc")
transformed_lm<-lm(transformed_alc~transformed_temp,data=transformed_df)
transformed_lm #alcohol percentage = 1/(1.503*10^(-1)+((5.926*10^(-5))((temp-50)^3)))
summary(transformed_lm) #R^2 is actually better
sort_temp<-sort(model$Temperature)
transformed_predict<-predict(transformed_lm, data.frame(transformed_temp=(sort_temp-50)^3),interval = 'prediction')

plot(model$Temperature,model$Alcohol_Percentage, main="Fermentation Results")
abline(original_lm,col='red')
lines(sort_temp,(1/transformed_predict[,1]),col='blue')
lines(sort_temp,(1/transformed_predict[,2]),lty='dashed',col='blue')
lines(sort_temp,(1/transformed_predict[,3]),lty='dashed',col='blue')
transformed_temp_train=(train$Temperature-50)^3
transform_alc_pred<-predict(transformed_lm,data.frame(transformed_temp=transformed_temp_train))
transform_alc_pred
plot(1/transform_alc_pred,train$Alcohol_Percentage, main="Transformed Predicted vs Actual")
cor(1/transform_alc_pred,train$Alcohol_Percentage) #it's less correlated than our linear model
