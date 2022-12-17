library(tidyverse) 
library(modelsummary)

#noy swisa

#_________________question1________________
mydata = read.csv("income_survey_2011.csv")

#section ??
workstat_type =c('salaried worker','self-employed',
'self-employed and salaried worker') 
df = subset(mydata, weekwhrs > 0 & (2011 - birthy >= 25 | 2011 - birthy <= 54) & 
                           (workstat %in% workstat_type))

#section ??
df$schooly[df$schooly>50] = NA
df[is.na(df)] <- 0

#section ??
# from month to weeks
df$incsal_week = df$incsal / df$mwweeks
# from weeks to hours
df$incsal_hour = df$incsal_week / df$weekwhrs
#ln of hour sal
df$in_sal_hour = log(df$incsal_hour)

#section ??
# estimate model: hourly sal = b0 + b1 * schooly + u
#min b1-hat and b2-hat for sum(ui-hat)^2
#fisrt normal equation ----> sum(ui-hat) = 0
#second normal equation ----> sum [(ui-hat)*xi] = 0

#section ??
model = lm(in_sal_hour ~ schooly, data = df)
plot(in_sal_hour ~ schooly,data=df, ylab="hourly sal", xlab= "School Years")
abline(reg=model, col="pink")
summary(model)

#section ??
#normal equations 
#df$uhat <- residuals(model) - thats an option
uhat = model$residuals
sum(uhat) #first equation - (not exact zero - it supposed to be like that)
sum(uhat*df$schooly) #second equation - (not exact zero - it supposed to be like that)

#there are some missing items from the residuals

#section ??
xi = df$schooly
yi = df$in_sal_hour

beta_1 = cov(xi,yi) / var(xi)
beta_0 = mean(yi) - beta_1*mean(xi)

#beta1
beta1 = model$coefficients[2]
beta1

#beta0
beta0 = model$coefficients[1]
beta0



#section ??
#yhat
yhat = predict(model)


#section ??
mean(yhat)
mean(df$in_sal_hour)


#_________________question2________________

#section ??
df_len = nrow(df)
a1 = df_len / 3
a2 = a1*2

model1 <- lm(in_sal_hour[1:a1] ~ schooly[1:a1],data=df)
model2 <- lm(in_sal_hour[a1:a2] ~ schooly[a1:a2],data=df)
model3 <- lm(in_sal_hour[a2:df_len] ~ schooly[a2:df_len],data=df)

modelsummary::modelsummary(models = list(model1, model2, model3))

#section ??
plot(in_sal_hour ~ schooly,data=df, ylab="hourly sal", xlab= "School Years")
abline(reg=model1, col="blue")
abline(reg=model2, col="yellow")
abline(reg=model3, col="red")

#section ??
#we got estimation for b0-hat and b1-hat for each part of the DF 


#section ??
#the estimators are different beacuse we use 3 different parts of the
#data frame which means we use 3 different samples which will get us different results


#_________________question3________________

#word file