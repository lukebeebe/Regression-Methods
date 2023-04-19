# Assignment 5, Diamonds

par(mfrow=c(1,1))
data<-read.table("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Regression Methods/diamonds_data.txt",header = T)
attach(data)
data.lm<-lm(PRICE~CLARITY)
summary(data.lm) #1 price=2694.8+2362.3(VS1)+3163.4(VS2)+2872.9(VVS1)+2661.8(VVS2)
#2 IF=2694.8 VS1=5057.1 VS2=5858.2 VVS1=5567.7 VVS2=5356.6
#3 There is a statistically significant relationship between diamond price and clarity rating.
 # The method I used was the p-value which checks the relationship between the response and
 # explanatory variable.
pairwise.t.test(PRICE,CLARITY,p.adjust.method="bonferroni")
#4 Difference between IF & VS1, VS2, VVS1, VVS2 are all statistically significant.
 # I used the bonferroni method to compare groups while controlling the family-wise error rate.
 # which is new.alpha=old.alpha/(num.groups). So, old.alpha=0.05 would be new.alpha=0.01 with 5 groups
 # Also, family-wise error rate is calculated as 1-(1-a)^n<=0.1 n=num of groups, a=alpha
 # 1-(1-0.02)^5 = 0.096
 # So, we should use a=0.02 for our family-wise error rate to be at most 0.1
 # Luckily, question 5 asks for a=0.01, so we're covered as the error rate at a=0.01 is 0.049<=0.1
 # which also ensures the family-wise error rate is at most 0.1
data.aov<-aov(PRICE~CLARITY)
data.tukey<-TukeyHSD(data.aov,conf.level=0.99)
data.tukey
plot(data.tukey,las=2)
#5 Six of these intervals contain zero, all of which have a p-value of 1.0 from question 4. Meaning the difference in
 # mean levels of clarity between those groups are not statistically significant.