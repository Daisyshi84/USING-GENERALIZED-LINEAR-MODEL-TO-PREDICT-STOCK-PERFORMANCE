---
title: "Using Generalized Linear Model To Predict Stock Performance"
author: "Minchan Shi"
date: "12/10/2020"
output:
  word_document: default
  pdf_document: default
  
---

# Abstract

In this project, I will use multiple regression and logistic regression to predict the price of the stock that I am interested in. In the first phase,Multiple Regression Analysis is applied to define the variables which have a strong relationship with the output. Then logistic regression is designed to predict whether stock prices will rise or fall the next day, as well as the trends and performance based on historical data.  The corresponding trading strategy is made accordingly.


# Introduction

Predicting stock price trends and deciphering confusing market data has long been an attractive topic for investors and researchers alike. Predicting how the stock market will change is also historically one of the most difficult things to do. Among the popular methods that have been adopted, regression techniques are very popular because it can identify stock trends from large amounts of data that capture underlying stock price dynamics. In this project, I apply the regression method to stock price trend forecasting. According to market efficiency theory, the U.S. stock market is a semi-strong efficient market, which means that all public information calculates the current price of the stock. This means that both basic and technical analysis can be used for better short-term returns (a day or a week). I want to build a model to predict the probability that the stock price will go up or down the next day. In fact, in my original analysis, the prediction accuracy was low the next day, which showed that the stock market could not judge the short-term trend through simple analysis. However, when I tried to predict long-term stock price trends, the model was more accurate. Based on my predictions, I built a stock-trading strategy that significantly outperformed the stocks themselves.


# Data Description

The dataset is from yahoo finance historical database. In this project, I am interested in the daily price of VanEck Vectors Gold Miners ETF (GDX) from 2015 to 2020. All the data is obtained from <”https://finance.yahoo.com/quote/GDX/performance?p=GDX”>.  This data set includes  1259 sample size and 9 variables  consisting of  "Date"  ,  "Open" ,  "High"   ,   "Low"  ,  "Close"  ,   "Adj.Close", "Volume" . "Nextday", "Profit_In_Percentage", "Month" ,"Year" .  "Date" is the Date on which the stock is traded. "Open" is the opening price of the stock we observe at the beginning of the market each morning. A "High" is the highest price seen in a day's trading. "Low" is the lowest price seen in a day's trading, and" adj. Close" is the price at the end of the day. "Volume" is the Volume of stock traded that day.In addition, I created one extra variable “Nextday” using the daily labeling as follows: label "H" if the closing price is higher than open price on the next day, Otherwise label "N". “Profit_In_Percentage” refers to the percentage of earnings on the day and is calculated as (Close-Open)/Close*100. "Month" ,"Year" refers to the traded date in the month and year.


```{r, echo=FALSE, warning=FALSE}
library(tidyverse)
library(lubridate)
library(ggplot2) 
library(gapminder)
library(dplyr)
library(moments) # For skewness() and kurtosis()
library(car)
library(quantmod)

# read data from online 
GDX<- getSymbols("GDX", src = "yahoo", from = "2015-01-01", to = "2020-10-28", auto.assign = FALSE)
summary(dailyReturn(GDX))
summary(weeklyReturn(GDX))
summary(monthlyReturn(GDX))
summary(quarterlyReturn(GDX))
yearlyReturn(GDX)

ggplot(dailyReturn(GDX),aes(x=index(GDX),y=dailyReturn(GDX)))+geom_line(color = "deepskyblue4")+
  labs(xlab='Year',ylab='Return',title='Daily Return From 2015 to 2020')


ggplot(quarterlyReturn(GDX),aes(x=index(quarterlyReturn(GDX)),y=quarterlyReturn(GDX)))+geom_line(color = "deepskyblue4")+
  labs(xlab='Year',ylab='Return',title='Quarterly Return From 2015 to 2020')


ggplot(yearlyReturn(GDX),aes(x=index(yearlyReturn(GDX)),y=yearlyReturn(GDX)))+geom_line(color = "deepskyblue4")+
  labs(xlab='Year',ylab='Return',title='Yearly Return From 2015 to 2020')

# read data 
GDX<- read.csv( "/Users/daisyshi/Downloads/GDX (1).csv", header = TRUE)


# add another variable "nextday"  
for (i in GDX){
  i<-1:nrow(GDX)
  n<-GDX[i,5]
  nn<- GDX[i+1,2]
  Nextday<- GDX[i,5]-GDX[i+1,2] 
  Nextday<- ifelse(Nextday>0,"Increase","Decrease")
}


GDX<-cbind(GDX,Nextday)
summary(GDX)

GDX<- GDX %>%
  mutate(Profit_In_Percentage= round(((GDX$Close-GDX$Open)/GDX$Close * 100),digits=2))  

str(GDX)
cor(GDX[,c(-1,-8)])
pairs(GDX[,c(-1,-8)])
hist(GDX$Close)
dim(GDX)
round(skewness(GDX$Close),2)
kurtosis(GDX$Clos,na.rm=TRUE)

# to build a function and list all of the descripticve statistics 

x<-function(x){
  skewness<- round(skewness(x),2)
  kurtosis<- round(kurtosis(x,na.rm=TRUE))
  sd<- round(sd(x),2)
  min<- round(min(x),2)
  mean<-round(mean(x),2)
  median<-round(median(x),2)
  max<-round(max(x),2)
  return(c(skewness=skewness,kurtosis=kurtosis,sd=sd,min=min,mean=mean,median=median,max=max))
}

# Descriptive Statistics
GDX %>% select(-Date,-Nextday) %>% map(x)

# This code is much easier to check the descripticve statistics 
GDX %>% split(.$Nextday) %>% map(describe)

count <- GDX %>%
  filter(!is.na(Nextday))%>%
  count(Nextday,sort = TRUE)

 


ggplot(count,aes(Nextday,n,fill=n))+geom_bar(stat='identity',width = 0.2,alpha=0.8, show.legend = FALSE) +
  theme(text = element_text(size=10)) +
  labs(x=NULL,y="Count",
  title="Decrease & Increase From 2015-2020(Days)") 

GDX<- GDX %>%
  filter(!is.na(Nextday)) %>%
  mutate(Year=year(Date),Month=month(Date))


#KS tests
ks.test(GDX$Close, "pnorm")

#Shapiro-Wilks test
shapiro.test(GDX$Close)
 
ggplot(GDX,aes(Date,Close,color=Nextday)) + geom_point() +facet_wrap(~Year)+
  labs(title="2015~2020 Historical Trend",y="Price",x="Timeline 2015-2020") 

ggplot(GDX,aes(Month,Close,color=Year)) + geom_histogram(stat = 'identity',width=0.5)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  labs(title='Monthly summary - Closing price')

fit_lm <- lm(Close~ Open + High + Low +Volume, data = GDX)
summary(fit_lm)
fit_lm1 <- lm(Close~ Open + High + Low, data = GDX)
summary(fit_lm1)
fit_lm2 <- lm(Close~ Open + Volume , data = GDX)
summary(fit_lm2)

anova(fit_lm,fit_lm1)
anova(fit_lm,fit_lm2)
anova(fit_lm1,fit_lm2)

BIC(fit_lm)
BIC(fit_lm1)
BIC(fit_lm2)
library(moments)
library(MASS)
library(lmtest) 
 
acf(fit_lm$residuals)
dwtest(fit_lm)
bptest(fit_lm)

#Run a best subset regression with exhaustive search. Select the subset size by BIC. Present the selected explanatory variables along with their coefficients.
library("leaps")
fit<- regsubsets(Close~. -High -Low, data= GDX,method = "exhaustive",really.big=T)
fit.summary<- summary(fit)
which.min(fit.summary$bic)
 
AIC(fit_lm)
AIC(fit_lm1)
AIC(fit_lm2)


vif(fit_lm)
vif(fit_lm1)
vif(fit_lm2)
library(car)  # for outlierTest
library(MASS)
library(broom)
library(dplyr)
library(ISLR)

plot(fit_lm2)
outlierTest(fit_lm2)
qqPlot(fit_lm1)
leveragePlots(fit_lm2)
set.seed(1)

train <- sample(nrow(GDX),nrow(GDX)*.50)
train <- GDX[train,]
test<- GDX[-train]

new_model<- lm(Close ~ . -Date , data=GDX,subset = train)
summary(new_model)

reduced_model<- lm(Close ~ Open + Volume, data=GDX,subset = train)
summary(reduced_model)
leveragePlots(reduced_model)

anova(new_model,reduced_model)
BIC(new_model)
BIC(reduced_model)
vif(new_model)
prediction<- predict(reduced_model,newdata=test)
predict_value<- cbind(GDX,prediction)
coef<- coef(new_model)
mse<- mean((prediction-predict_value$Close)^2)

estimated_error_variance<-deviance(new_model)/df.residual(new_model)


two<- predict_value %>%
filter(Year==2020)
 

ggplot(two,aes(as.numeric(Date)))+
  geom_line(aes(y = Close, colour = "Close"))  +
    geom_line(aes(y = prediction, colour = "prediction"))+
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  ggtitle("The Prediction Price of Stock  vs.  Actual Closing Price")+
  labs(x="Date",y="Price")

predict_value %>%
  ggplot(aes(prediction,Close)) + geom_point(aes(size=Volume),color = "midnightblue", alpha = 0.7) + geom_smooth(method="lm")+labs(title='Prediction Price')


glm.model<- glm(Nextday ~ Open + Low + Profit_In_Percentage, data=GDX,subset = train,family = 'binomial')
prob <- predict(glm.model,newdata=test,type = "response")
predict = rep("Decrease", length(prob))
predict[prob > 0.5] <- "Increase"
table(predict,test$Nextday)
mean(predict==test$Nextday)


library(plyr)
library(dplyr)
 
library(DT)
library(psych)
library(purrr)
library(pscl)
library(ROCR)


#evaluate model 
pred <- prediction(prob, train$Nextday)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)


```
# Methods and Data Diagnostics 

The question I want to analyze is: what is the closing price of the stock the next day? Short-term predictions of whether it will go "up" or "down" tomorrow. Long-term forecasting to understand what time should buy and sell. In the field of big data, data visualization tools and technologies are critical to analyzing massive amounts of information and making data-driven decisions.In this analysis, I will use data visualization plots to check and understand data central trends, outliers, and stock prices trends over the years.  Meanwhile, I will focus on compare AIC,BIC and R squared to varify the fit of the model.   In addition, I would like to do more analysis on classification, I used Logistic Regression to predict whether the stock would go 'up' or 'down' for the next day.

To formally assess the normality of response variable of the study, Shapiro-Wilks test as well as the Kolmogorov-Smirnov test are preformed. Formal Hypotheses are as follows:

K-S test: 
H0: The data comes from a normal distribution 
HA: At least one value is non-normal

Shapiro-Wilks test: 
H0: The data comes from a normal distribution.
HA: The data does not come from a normal distribution. 

Both test indicate siginificant p-value, we have the reason to believe that the dataset we have is not normally distributed. Also, The skewness 0.07943889 and kurtosis 3.213588 indicate that the dataset is not normally distributed. However, given the information from the QQ plot and the large sample size `r nrow(GDX)`,it is deemed appropriate for conducting the analysis as it confirms the model assumption of a close to normal distribution. Analysis of Variance and outlier test was also performed. The dataset does not have any empty values, but some outlier appears. According to correlation plot we can easily see the correlation between variables are very high, also values of VIF that exceed 10 are regarded as indicating multicollinearity, so in the regression analysis, several variables with high correlation are dropped. AIC and BIC are both used for choosing best predictor subsets in regression. The smallest BIC indicate 'fit_lm1' model is the best fit, and  smallest AIC indicate 'fit_lm' model is the best fit.  

By establishing the analysis and comparison of three linear regression models, the closing price of the stock in the final model is taken as the corresponding, and the opening price and trading volume are used as the predictors. Dataset is devided into two parts for training and testing respectively, 50% of which was used for training data, and the other 50% was used for testing the output accuracy. The outlierTest indicate that there are few points are extrem large, I will simply keep these points in the analysis to get better understanding of the real situation.  An analysis of variance hypothesis test that compares the quality of fit for the linear model stored in the respective variables 'fit_lm','fit_lm1' and 'fit_lm2'. The calculated F-Statistic and associated p-value from the generated ANOVA table indicates that the multiple linear regression model “lm(Close~ Open + Volume , data = GDX)” is better at predicting 'Close' versus the more variables in multiple linear regression model. 

# Main Results

From Generalized Linear Model we can see that the closing price of a stock can be predicted by establishing a regression model,however,the real value is far away from the model result. Around 25% error rate in the regression model compare with the real closing price.  Model coefficient Intercept is 3.693318e-01, coefficients of open is 9.883882e-01,coefficients of Volume  is -2.409114e-09. An increase of one unit in 'open' is associated with an increase of 9.883882e-01 in 'close', also,an increase of one unit in 'Volume' is associated with an increase of -2.409114e-09 in 'close'.The estimated error variance is 0.196588.The R-squared value of the model is 0.9921, implying that the equation explains 99.2 percent of the variation of the future stock market price.

I used the model generated on the training set to predict outcomes for our validation set and calculated the test error rate, or the percentage of time the model misclassified an outcome as compared to the observed results. In general,linear regression analysis does not seem to be suitable for predicting stock prices.

I used logistic regression to estimate that the model correctly predicted that the market opening price order to go up on 13 days and that it order to go down on 669 days, for a total of 669 + 13 = 682 correct predictions. Among them, the error rate reaches 1-0.5421304=0.4578696, which means the error rate of logistic regression is about 46%. According to the observation, we can know that the Generalized Linear Model is not very accurate in predicting stocks.

The bar chart display the total days of Decrease and Increase from 2015 June until 2020 June during the five years period.

# Conclusion 

Based on the the QQ plot and R square, it indicate the model is good fit. However, using multiple liner regression we got around 25% error rate, and by using logistic regression, we get around 46% error rate, meaning using Generalized linear model to analyze stock trend is not accurate enough. 

From 2015 to 2020, the daily average rise and fall of the stock was around -0.23% to 0.18%, The average day trades without any profit or loss. In addition, a chart of the total number of daily gains and losses from 2015 to 2020 shows that short-term investing in this stock is not an ideal strategy.  By using the method of logistic regression analysis, we predict that the opening price of this stock is likely to show a downward trend every day, but the difference is not obvious. So this stock is not suitable for daily trading operations. 

The stock has made very good returns in 2016(more than 52% yearly return) and 2019 - 2020(more than 30% yearly return). It is on a long-term uptrend. Although the yearly returns sheet indicate the average returns between 2013-2020 only 4%. But based on its long-term performance, we can tell that it is suitable for long-term holding. I would finally include this stock in my investment portfolio due to a low-risk, low-return strategy that can be used to balance my investment allocation risk.


