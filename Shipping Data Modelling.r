#Q1a
#prepare variable to create bar chart by reordering levels
dataset$Customer_rating <- ordered(dataset$Customer_rating, levels = c(1,2,3,4,5)) 

#create and store frequency table 
freq<-table(dataset$Customer_rating) 

#Using % Relative Frequency
#solve for % relative frequency and round to 2 decimal points
RFrequency<-round(100*freq/sum(freq),2) 

#plot the bar chart for customer rating
barplot(RFrequency,ylim=c(0, 30),main="Bar Chart for the Customer Rating of Shipments",ylab="% Relative Frequency",xlab="Customer Rating", col="blue" ) 


#Q1b
#summarizing frequency in correct format for plot
counts <- table(dataset$Gender, dataset$Customer_rating) 


#Q1c
#side by side barplot where y-axis is frequency
Bar<-barplot(counts, main="Customer Rating by Female vs. Male",
              xlab="Customer Rating",ylab="Frequency", col=c("lightblue","red"), 
              ylim=c(0, 80), legend = rownames(counts), beside=TRUE)
text(x =Bar , y = counts, label = counts, pos = 1.5, cex = 0.8, col = "black")


#Q1d
#Make Customer rating binary where
#Rating=1 if customer rating is values 4-5 considered a Excellent rating and
#Rating=0 if customer rating is values 1-3 considered a Poor rating
dataset$Rating<-ifelse(dataset$Customer_rating < 4,0,1)
dataset$Rating<-as.factor(dataset$Rating) 
table(dataset$Rating)


#Q1e
rating.count <- table(dataset$Reached_on_Time, dataset$Rating) 
#solve for relative frequency and round to 2 decimal points
RFrequency1<-round(rating.count/sum(rating.count),2) 
#printing out to see the values
RFrequency1 


#Q1g
#Binomial RLF for Binomial(40,theta)
n <- 495
y <- 48+48+59+38    #number of excellent rating

thetahat <- y/n    #maximum likelihood estimate of theta

s<-(thetahat*(1-thetahat)/n)^0.5 #standard error of estimator

# interval of values for plotting relative likelihood function
th<-seq(max(0,thetahat-4*s), min(1,thetahat+4*s),0.001)

# create function to calculate Binomial relative likelihood function
BinRLF <- function(x,y,n,thetahat) {(x/thetahat)^y*(1-x)^(n-y)/(1-thetahat)^(n-y)}
#x is the various theta values at whih to evaluate the function
#y is the oberved data, n is the known parameter, thetahat is the MLE

# plot relative likelihood function
plot(th, BinRLF(th,y,n,thetahat),xlab=expression(theta),ylab=expression(paste("R(",theta,")")),type="l",lwd=2)

# draw a horizontal line at 0.15
abline(a=0.15,b=0,col="darkred",lwd=3)
title(main="Binomial Relative Likelihood Function")

#find 15% likelihood interval using uniroot
uniroot(function(x) BinRLF(x,y,n,thetahat)-0.15,lower = 0.33, upper = 0.37)$root
uniroot(function(x) BinRLF(x,y,n,thetahat)-0.15,lower = 0.4, upper = 0.45)$root


#Q2b
library("moments")
x <- dataset$Discount_offered
#five number summary
fivenum(x) 

#find IQR
10-4

#find Range
64-1

#find mean
mean<-mean(x) 
mean

#find standard deviation
sd(x) 

#find skewness
skewness(x)


#Q2c
#Exponential RLF for Exponential(theta)
n <- 500

thetahat <- mean    #maximum likelihood estimate of theta

s<-thetahat/n^0.5 #standard error of estimator

# interval of values for plotting relative likelihood function
th<- seq(max(0,thetahat-4*s), thetahat+4*s,0.001)

# create function to calculate Binomial relative likelihood function
ExpRLF <- function(x,y,n,thetahat) {(thetahat/x)^n*exp(n*(1-thetahat/x))}
#x is the various theta values at whih to evaluate the function
#y is the oberved data, n is the known parameter, thetahat is the MLE

# plot relative likelihood function
plot(th, ExpRLF(th,y,n,thetahat),xlab=expression(theta),ylab=expression(paste("R(",theta,")")),type="l",lwd=2)

# draw a horizontal line at 0.15
abline(a=0.15,b=0,col="darkred",lwd=3)
title(main="Exponential Relative Likelihood Function")

#find 15% likelihood interval using uniroot
uniroot(function(x) ExpRLF(x,y,n,thetahat)-0.15,lower = 10.5, upper = 11)$root
uniroot(function(x) ExpRLF(x,y,n,thetahat)-0.15,lower = 12.5, upper = 13)$root


#Q2d
#find a
a <- qnorm((1+0.9)/2)

#find lower bound
mean-a*mean/sqrt(n)

#find upper bound
mean+a*mean/sqrt(n)



#Q3b
library("moments")
x <- dataset$Customer_care_calls
#five number summary
fivenum(x) 

#find IQR
5-3

#find Range
7-0

#find mean
mean<-mean(x) 
mean

#find standard deviation
sd(x) 

#find skewness
skewness(x)


#Q3c
#create and store observed frequency table
table(dataset$Customer_care_calls) 

#find the observed frequency of greater or equal to 5
102+40+9

#find the total of observed frequency
7+10+28+141+163+151

#find the expected frequency when the number of customer care calls is 0
dpois(0, mean)*n

#find the expected frequency when the number of customer care calls is 1
dpois(1, mean)*n

#find the expected frequency when the number of customer care calls is 2
dpois(2, mean)*n

#find the expected frequency when the number of customer care calls is 3
dpois(3, mean)*n

#find the expected frequency when the number of customer care calls is 4
dpois(4, mean)*n

#find the expected frequency when the number of customer care calls >= 5
(1-ppois(4, mean))*n

#find the total of expected frequency
dpois(0, mean)*n+dpois(1, mean)*n+dpois(2, mean)*n+dpois(3, mean)*n+dpois(4, mean)*n+(1-ppois(4, mean))*n


#Q4b
#Select all Ship shipments
Shipped<-subset(dataset, Mode_of_Shipment=="Ship")

library("moments")
x <- Shipped$Weight_in_gms
#five number summary
fivenum(x) 

#find IQR
5101.5-1858.5

#find Range
5992.0-1001.0

#find mean
mean<-mean(x) 
mean

#find standard deviation
s <- sd(x) 
s

#find skewness
skewness(x)

#find kurtosis
kurtosis(x)


#Q4c
#Plot qqplot for Weight in grams
qqnorm(Shipped$Weight_in_gms, col="darkblue",
       main="Normal qqplot for Weight in Grams of Ship Shipments")
qqline(Shipped$Weight_in_gms,lwd="3", 
       col="darkred")

#Q4e
# let n be the number of Ship shipment
n <- dim(Shipped)[1]

#find b
b <- qt((1+0.95)/2, n-1)

#find lower bound of 95% confidence interval for mu_1
mean - b*s/sqrt(n)

#find upper bound of 95% confidence interval for mu_1
mean + b*s/sqrt(n)

#find c
c<-qchisq((1-0.9)/2, n-1)

#find d
d<-qchisq((1+0.9)/2, n-1)

#find lower bound of 90% confidence interval for sigma_1
sqrt((n-1)*s^2/d)

#find upper bound of 90% confidence interval for sigma_1
sqrt((n-1)*s^2/c)


#Q4f
#Select all Road shipments
Road<-subset(dataset, Mode_of_Shipment=="Road")

library("moments")
wt.road <- Road$Weight_in_gms

#five number summary
fivenum(wt.road) 

#find IQR
5012 - 1806

#find Range
6000-1017

#find mean
mean<-mean(wt.road) 
mean

#find standard deviation
s <- sd(wt.road) 
s

#find skewness
skewness(wt.road)

#find kurtosis
kurtosis(wt.road)


#Q4g
#Plot qqplot for Weight in grams of Road Shipments
qqnorm(Road$Weight_in_gms, col="darkblue",
       main="Normal qqplot for Weight in Grams of Road Shipments")
qqline(Road$Weight_in_gms,lwd="3", 
       col="darkred")


#Q4i
# let n be the number of Road shipment
n <- dim(Road)[1] #take the first result as n 

#find b
b <- qt((1+0.95)/2, n-1)

#find lower bound of 95% confidence interval for mu_3
mean - b*s/sqrt(n)

#find upper bound of 95% confidence interval for mu_3
mean + b*s/sqrt(n)

#find c
c<-qchisq((1-0.9)/2, n-1)

#find d
d<-qchisq((1+0.9)/2, n-1)

#find lower bound of 90% confidence interval for sigma_3
sqrt((n-1)*s^2/d)

#find upper bound of 90% confidence interval for sigma_3
sqrt((n-1)*s^2/c)


#Q4j
boxplot(formula = Weight_in_gms ~ Mode_of_Shipment, 
        data = dataset,
        outline=TRUE, 
        frame=T, 
        col="seashell",
        xlab="Mode of Shipment",
        ylab="Weight of Shipment (g)",
        main="Boxplot for Weight of Shipment vs Mode",
        rm.NA=TRUE, #ignore missing values
        cex.axis=1.15,
        cex.lab=1.5)


#Q5b
#find number shipments that arrived on time
dataset$ROT<-ifelse(dataset$Reached_on_Time==0,0,1)
dataset$ROT<-as.factor(dataset$ROT) 
table(dataset$ROT)

#find maximum likelihood estimate
thetahat<-297/500
thetahat


#Q5c
#set n
n<- 500

#find a
a <- qnorm((1+0.95)/2)

#find lower bound of 95% confidence interval
thetahat - a*(thetahat*(1-thetahat)/n)^0.5

#find upper bound of 95% confidence interval
thetahat + a*(thetahat*(1-thetahat)/n)^0.5


#Q5d
1-pbinom(22,50,thetahat)


#Q5e
#Select all high importance product shipments
Important<-subset(dataset, Product_importance=="high")

#Find the number of high importance product
hi<-dim(Important)[1]

#Find the number of high importance product that arrived on time
hi.on.time<-dim(subset(Important, Reached_on_Time==1))[1]

#find the proportion
hi.on.time/hi


#Q5f
#prepare variable to create bar chart by reordering levels
dataset$Product_importance <- ordered(dataset$Product_importance, levels = c("low", "medium", "high")) 

#summarizing frequency in correct format for plot
counts <- table(dataset$Reached_on_Time, dataset$Product_importance) 

#solve for % relative frequency and round to 2 decimal points
RFrequency1<-round(counts/sum(counts),2) 

#print out to see the values
RFrequency1


#Q5g

#side by side barplot where y-axis is relative frequency
Bar1<-barplot(RFrequency1, main="Proportion of Products Delivered on Time vs Importance",
             xlab="Product Importance",ylab="Relative Frequency",
             ylim=c(0,0.3),col=c("lightblue","red"),
             legend = rownames(counts), beside=TRUE)
text(x =Bar1 , y = RFrequency1, label = RFrequency1, pos = 1.5, cex = 0.8, col = "black")
