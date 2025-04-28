################################################################################
### Fall 2024
################################################################################
library(dbplyr)

#read file into prices
prices = read.csv("usedsuvprices.csv")
prices$suv_price = prices$price
attach(prices)

#Q1 
#data frame for quantitative variables
dfprice = data.frame(fueleconomy,kilometers,suv_price)
#getting number of rows of data frame
n = nrow(prices)
plot(kilometers,suv_price, axes=TRUE, frame.plot=TRUE, pch = 16,
     xlab='Kilometers driven', ylab='SUV Price',
     main = "Scatterplot of Used SUV Price vs Kilometers Driven",
     col = "orange")
#find the correlation between quantitative variables
cor(dfprice)
#calculating coefficient of correlation
(r = cor(kilometers,suv_price))
#doing the correlation test
cor.test(kilometers,suv_price,alternative = "two.sided",
         conf.level = 0.95)


#Q2) Perform a SLR 
#################################################################
# run single linear regression
(m = lm(suv_price ~ kilometers))
abline(m)
#summarizing the model
summary(m)
# generating the residual plot 
resid = resid(m)
plot( fitted(m),resid, axes=TRUE, frame.plot=TRUE, pch = 16,
      xlab='Fitted SUV Price', ylab='Residuals',
      main = "SLR: Scatterplot of Residuals vs. Fitted used SUV Price",
      col = "cyan")
abline(a =0 ,b=0, col = "black")
#histogram of residuals
hist(resid,main = "Histogram of Residuals", col = "cyan",xlab = "Residuals")
#####################################################################
#Since the variance is increasing in the residual plot - use ln
# run single linear regression

(m1 = lm(log(suv_price) ~ kilometers))
#summarizing the model
summary(m1)

# generating the residual plot 
resid = resid(m1)
plot( fitted(m1),resid, axes=TRUE, frame.plot=TRUE, pch = 16,
      xlab='Natural Log Fitted SUV Price', ylab='Residuals',
      main = "Scatterplot of Residuals vs Natural Log of Fitted SUV Price",
      col = "cyan")
abline(a =0 ,b=0, col = "black")
#histogram of residuals
hist(resid,main = "Histogram of Residuals(After Applying Transformation)",
     col = "cyan",xlab = "Residuals")

# find the cooks distance 
k = 1
cooks.dist = cooks.distance(m1)
cooks.dist[which(cooks.dist > (4/(n-k-1)))]
cd_value = cooks.dist [which(cooks.dist > (4/(n-k-1)))]
cd_fitted =fitted(m1)[which(cooks.dist > (4/(n-k-1)))]
cd_kilometers = dfprice[which(cooks.dist > (4/(n-k-1))),]$kilometers
cd_price = dfprice[which(cooks.dist > (4/(n-k-1))),]$suv_price
cd_fitted_price = exp(cd_fitted)
cd_data = data.frame(cd_value,cd_fitted,cd_kilometers,cd_fitted_price,cd_price)
print(head(cd_data[order(cd_data$cd_value, decreasing = TRUE), ] )  )

# Q3) Calculate the least squares regression equation
# that predicts SUV price from  kilometers , brand and fuel_consumption

#fitting the model
#dummy varible for brand
prices$dbrand = ifelse(brand == "Toyota",1,0)
(m2 = lm(log(suv_price) ~  kilometers + fueleconomy + prices$dbrand ))
#getting summary statistics
summary(m2)
#getting F critical value at alpha = 0.05(Right Tailed)-df1 = 3 and df2 = 98
(fcv = qf(.95,df1 = 3, df2 = 1157))
#getting F global from summary function

confint(m2, level = 0.95)
# generating the residual plot 
resid = resid(m2)
plot( exp(fitted(m2)),resid, axes=TRUE, frame.plot=TRUE, pch = 16,
      xlab='Fitted SUV Price', ylab='Residuals',
      main = "MLR Scatterplot of Residuals vs.Fitted SUV Price",
      col = "green")
axis(1, at=seq(0, 18 , 1)) #x axis
abline(a =0 ,b=0, col = "black")
#histogram of residuals
hist(resid,main = "MLR: Histogram of residuals")




###########################################################################
#ANOVA and ANCOVA
###########################################################################


#factor fueltype column if not a factor
is.factor(prices$fueltype)
factor(prices$fueltype)

# How many students are in each group? 
n = nrow(prices)
#no of types of fuel
k=4
# no of rows  by fuel type 
(scnt= aggregate(prices$suv_price, by = list(prices$fueltype),FUN = length))
# Summarize the data relating to SUV price by fuel type
# student group (separately).
# Numerical Summary by iq 
aggregate(prices$suv_price,by= list(fueltype),summary)
aggregate(prices$suv_price,by= list(fueltype),sd)

# Create a Boxplot 
boxplot(prices$suv_price ~ prices$fueltype, data=prices, 
        main="BoxPlot: Used SUV price by Fuel Type", 
        xlab="Fuel Type", ylab="used SUV Price") #, ylim=c(20, 60))



#Q2) Do the test scores vary by student group?  
# Perform a one way ANOVA using the aov or Anova function in R to assess
# Use a significance level of α=0.05
(m<- aov(prices$suv_price~prices$fueltype , data=prices))
summary(m)

#finding F critical value for df1 = 2 ,and df2 = 42 for alpha = 0.05 right tail
(FCV = qf(0.95,k-1,n-k))

#Tukeys Test for pairwise comparison
TukeyHSD(m)



#Q5 - Redo the anova model for age adjusting for age
#pass the model to anova function

library(car)
# ANCOVA
# Now we run ANOVA with adjusting for age 
Anova(lm(prices$suv_price~prices$fueltype + prices$fueleconomy 
        ), type=3)


# calculating the lsmeans 
# install.packages("emmeans")
library(emmeans)
my.model<-lm(prices$suv_price~prices$fueltype + prices$kilometers,  data = prices)
emm_options(contrasts=c("contr.treatment", "contr.poly"))
emmeans(my.model, specs = "fueltype")

# no p value adjustment 
emmeans(my.model, specs = "fueltype" , contr = "pairwise",  adjust="none")
# #P value adjustment: tukey method
# emmeans(my.model, specs = "fueltype" , contr = "pairwise",  adjust="tukey")














