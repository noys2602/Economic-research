# general setup
rm(list=ls()) # del all objects and functions
gc() #cleans memory
options(scipen=999) # tell R not to use Scientific notation
options(digits = 5) # controls how many digits are printed by default



path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)



#############
# question 1
#############

# call csv file
database<-read.csv("ceosal1.csv")

database$log_salary<-log(database$salary)
database$log_sales<-log(database$sales)

model1<-lm(salary~sales, data=database)
summary(model1)
dy_dx<-model1$coefficients[2]

model2<-lm(log_salary~sales, data=database)
summary(model2)
dlogy_dx<-model2$coefficients[2]

model3<-lm(log_salary~log_sales, data=database)
summary(model3)
dlogy_dlogx<-model3$coefficients[2]

#Calculating Means
mean_y<-mean(database$salary)
mean_x<-mean(database$sales)

#Alternative commands:
#mean_y<-aggregate(salary~1, data=database, mean)
#mean_x<-aggregate(sales~1, data=database, mean)

#Calculating max point, the max x's are separate for each model, y max is the same
#max_y<-aggregate(salary~1, data=database, max)
max_y <- max(database$salary)
max_logy<-aggregate(log_salary~1, data=database, max)

max_x_model1<-(max_y-model1$coefficients[1])/model1$coefficients[2]
max_x_model2<-(max_logy-model2$coefficients[1])/model2$coefficients[2]

#Calculating Elasticity at mean point
elasticity1<-dy_dx*(mean_x/mean_y)
elasticity1
elasticity2<-dlogy_dx*mean_x
elasticity2
elasticity3<-dlogy_dlogx
elasticity3

#Calculating Elasticity at max point
elasticity1_max<-dy_dx*(max_x_model1/max_y)
elasticity1_max
elasticity2_max<-dlogy_dx*max_x_model2
elasticity2_max
elasticity3_max<-dlogy_dlogx
elasticity3_max

#############
# question 2
#############

database$salary_nis<-database$salary*3.5
database$salary_500<-database$salary+500
database$lsalary_nis<-log(database$salary_nis)
database$lsalary_500<-log(database$salary_500)

model1_base<-lm(salary~sales, data=database)
summary(model1_base)
model2_base<-lm(salary_nis~sales, data=database)
summary(model2_base)
model3_base<-lm(salary_500~sales, data=database)
summary(model3_base)

model1_loglevel<-lm(lsalary~sales, data=database)
summary(model1_loglevel)
model2_loglevel<-lm(lsalary_nis~sales, data=database)
summary(model2_loglevel)
model3_loglevel<-lm(lsalary_500~sales, data=database)
summary(model3_loglevel)

model1_loglog<-lm(lsalary~lsales, data=database)
summary(model1_loglog)
model2_loglog<-lm(lsalary_nis~lsales, data=database)
summary(model2_loglog)
model3_loglog<-lm(lsalary_500~lsales, data=database)
summary(model3_loglog)

#############
# question 3
#############

# define lists for conditioning
rm(list=ls())
database<-read.csv("income_survey_2011.csv")
age_list <- c("25-34", "35-44", "45-54")
workstat_list <- c("salaried worker", "self-employed", "self-employed and salaried worker")

# restrict the sample
mydata<-subset(database, (age %in% age_list &  workstat %in% workstat_list & weekwhrs>0))

#recode the 50+ years of schooling values
mydata$schooly[mydata$schooly > 50] <- NA

# create a new variable = ln hourly wage

mydata$incsal_week <- mydata$incsal / mydata$mwweeks # from month to weeks
mydata$incsal_hour <- mydata$incsal_week / mydata$weekwhrs # from weeks to hours
mydata$ln_wage_hr <- log(mydata$incsal_hour) #create a new variable = ln hourly wage

#estimate model
model<-lm(ln_wage_hr~schooly, data=mydata)
summary(model)

#calculate 95% confidence intervals
confint(model)

#############
# question 4
#############

rm(list=ls())
database<-read.csv("money.csv")

### B ##
database$m_y<-database$m/database$y
database$log_m_y<-log(database$m_y)
database$log_r<-log(database$r)

### C ##
#Scatterplots with linear fit
plot(database$m_y ~ database$r)
abline(lm(m_y ~ r, data=database),col="blue")

plot(database$log_m_y ~ database$r)
abline(lm(log_m_y ~ r, data=database),col="blue")

plot(database$log_m_y ~ database$log_r)
abline(lm(log_m_y ~ log_r, data=database),col="blue")

###E-F##
model1<-lm(m_y~r, data=database)
summary(model1)
confint(model1)

model2<-lm(log_m_y~r, data=database)
summary(model2)
confint(model2)

model3<-lm(log_m_y~log_r, data=database)
summary(model3)
confint(model3)

###G##
#An example for a manual calculation of the confidence interval for the prediction
mean_r<-aggregate(r~1, data=database, mean)
mean_r
#mean r is 0.1583681

summary(model1)

#Root MSE is 0.15263

#Standard error is 0.09103
#Therefore, sum of squared deviations from mean r: is 0.15263^2/0.09103^2=2.81132

S_PE<-0.15263*sqrt((1+1/96+((0.1583681-0.1583681)^2)/2.81132))
S_PE

S_PE2<-0.15263*sqrt((1+1/96+((0.1583681*2-0.1583681)^2)/2.81132))
S_PE2

S_PE3<-0.15263*sqrt((1+1/96+((0.1583681*0.5-0.1583681)^2)/2.81132))
S_PE3
#standard error of prediction is 0.1534229

#predicted value for mean r is alpha+beta*mean_r = 0.37854+0.1583681*(-0.54896)=0.2916022

#Upper value of CI1:
0.2916022+S_PE*1.96
#Lower value of CI1
0.2916022-S_PE*1.96


#Upper value of CI2:
0.205+S_PE2*1.96
#Lower value of CI2
0.205-S_PE2*1.96

#Upper value of CI2:
0.335+S_PE3*1.96
#Lower value of CI2
0.335-S_PE3*1.96


Under-grad Econ TLV-U
# Purpose:  show examples of prediction confidence interval,
#           and multiple regression analysis
# Updated 16/11/2021

# setup -------------------------------------------------------------------

# clear enviorment
rm(list=ls())

# set working directory
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

#PUTTING THIS COMMAND ONCE AT THE BEGINNING,
#AND AFTERWARDS THE REGRESSION AUTOMATICALLY EXCLUDES NAs"
options(na.action = "na.exclude")
library(dplyr)
library(ggplot2)
# tell R not to use Scientific notation
options(scipen=999)

# Predicting Y with Confidence-Intervals (CI) ----------------------------------------------------------------------

# load house price dataframe, for PS5
df <- read.csv("hprice.csv")
head(df)

# fit a linear regression model
# where we predict prices using the houses area
mod <- lm(price ~ area, data = df)

# lets say we want to predict Y (prices) for a house with area (150)
# now, create a data with the X's you want to predict with
fake_df <- data.frame(area = 150)

# now predict using the fitted model on this fake data
prediction <- predict.lm(object = mod, newdata = fake_df,
                         interval = "prediction", level = 0.95)
prediction

# note the output -
## fit - the predicted value,
## lwr - the lower confidence interval (at 95% signf. lvl.)
## upr - the upper confidence interval (at 95% signf. lvl.)

# we can also visualize this using a graph
# where we plot the linear regression,
# and the confidence interval of the prediction
# Note, i use here the ggplot2 library
ggplot(data=df, aes(x=area, y=price)) +
  geom_point() + geom_smooth(method = "lm")

# mutliple regression -----------------------------------------------------

# above, predicted Y(price) only using one X(area)
# say we want to also add `bedrooms` as a predictor
# can use multiple X's, with the following syntax
multp_mod <- lm(price ~ area + bedrooms, data = df)

# check out the regressions summary
summary(multp_mod)

# the coefficient on `area` (2.128) is the average increase to Y(price)
# if the house sqr area goes up by 1, keeping the number of bedrooms constant.
# the coefficient on bedrooms (-20) is the average increase to Y(price)
# if the house has 1 more bedroom, keeping the area constant.
