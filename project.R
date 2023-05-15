#Authors - Ido Candiotti &  Noy Segal Swisa 


# Settings ----------------------------------------------------------------

rm(list=ls()) # del all objects and functions
gc() #cleans memory
options(scipen=999) # tell R not to use Scientific notation
options(digits = 5) # controls how many digits are printed by default
options(na.action=na.exclude)


# Import ------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(car) 
library(sandwich)
library(lmtest) 
library(lfe) 
library(stargazer)
library(AER)
library(rstatix)
library(plotrix)
library(lmtest)
library(AER)
library(modelsummary)


#Data----------------------------------------------------------------------
data <- read.csv("~/Desktop/TAU/Economics/Econometrics/data/term_paper.csv")
#summary(data)

#Part B

#Summary and standard deviation -------------------------------------------------------------------------

summary_df = sapply(data, summary)
summary_df = sapply(summary_df,head)

min_col <- summary_df[1, ]
median_col <- summary_df[3, ]
mean_col <- summary_df[4, ]
max_col <- summary_df[6, ]
sd_col <- apply(data, 2, sd , na.rm = T)

#q1 <- summary_stats[2, ] #25%
#q3 <- summary_stats[5, ] #75%

summary_df = data.frame(mean_col,sd_col,median_col,min_col,max_col, row.names = names(data)) 
summary_df <- slice(summary_df, -1)
summary_df


#main_varibles = c('score','sc1','black','white','boy','cltype1',
                  #'schtype1','hdeg1','totexp1','ses1','cs')
#df_subset <- summary_df[row.names(summary_df) %in% main_varibles,]
#df_subset


#Question 2-------------------------------------------------------------------------

data$cs_sqr = data$cs * data$cs
data1=subset(data, data$cltype1=="2")

model1 = lm(score ~ boy + cs + cs_sqr + hdeg1 + totexp1 + ses1 , data1)
summary(model1)


#Question 5 -------------------------------------------------------------------------

model2 = lm(boy ~ sc1  , data)
model3 = felm(boy ~ sc1 | schid1n , data)
fixed_effects = getfe(model3)
summary(model2)
summary(model3)

model4 = lm(hdeg1 ~ sc1  , data)
model5 = felm(hdeg1 ~ sc1 | schid1n , data)
fixed_effects = getfe(model5)
summary(model4)
summary(model5)

model6 = lm(totexp1 ~ sc1  , data)
model7 = felm(totexp1 ~ sc1 | schid1n , data)
fixed_effects = getfe(model7)
summary(model6)
summary(model7)

model8 = lm(ses1 ~ sc1  , data)
model9 = felm(ses1 ~ sc1 | schid1n , data)
fixed_effects = getfe(model9)
summary(model8)
summary(model9)

modelsummary(models = list(model2, model4, model6, model8))
modelsummary(models = list(model3, model5, model7, model9)) #fe models


# Question 7 --------------------------------------------------------------

model10 = felm(sc1 ~ black + white + boy + sbirthy + ses1 | schid1n , data )
summary(model10)
#fixed_effects = getfe(model10)

#hypothesis test for Statistical significance 
linearHypothesis(model10,c('black = 0','white=0','boy=0','sbirthy=0','ses1=0'))

#Breusch-Pagan test
bptest(model10)

#White correction for heteroscedasticity
coeftest(model10,vcov = vcovHC(model10,type = "HC"))


#modelsummary(models = list(model10, coeftest(model10,vcov = vcovHC(model10,type = "HC"))))

# Question 9 --------------------------------------------------------------

model11 = felm(score ~ sc1 | schid1n, data)
summary(model11)
fixed_effects = getfe(model11)

# Questions 10 + 11 + 12 --------------------------------------------------------------

data$ses1_sc1 = data$ses1 * data$sc1
model12 = felm(score ~ sc1 + ses1 + ses1_sc1 + totexp1  |schid1n , data)
summary(model12)
#fixed_effects = getfe(model12)

#Hypothesis test for statistical significance 
linearHypothesis(model12,c('sc1 = 0','ses1=0','ses1_sc1=0','totexp1=0'))

#Comparison between models
modelsummary(models = list(model11, model12))

linearHypothesis(model12, c('ses1_sc1=0'))

#Part C


# Question 13 -------------------------------------------------------------
model13 = lm (cs ~ sc1, data )
summary(model13)
model14 = felm (cs ~ sc1 | schid1n, data )
summary(model14)
cov(data$cs,data$sc1)



# Question 14 -------------------------------------------------------------

#IV
model15 = felm (score ~ sc1 | schid1n, data )  #y~z
summary(model15)
model16 = felm (cs ~ sc1 | schid1n, data ) #x~z
summary(model16)

b1_yz = as.numeric(model15$coefficients[1])
b1_xz = as.numeric(model16$coefficients[1])

b1IV = b1_yz/b1_xz
paste("beta IV = ",b1IV)


# Question 15 -------------------------------------------------------------

model17 = felm(score ~ sc1 + black + white + ses1 + boy  | schid1n, data)
summary(model17)


# Question 16 -------------------------------------------------------------

#tsls

model18 = felm(score ~ cs + black + white+ ses1 + boy | schid1n, data) #base
summary(model18)

model19 = felm(cs ~ sc1 + black + white + ses1 + boy |schid1n, data) #first stage
summary(model19)

data$cs_hat = fitted(model19)#get the cs hat values

model20 = felm(score ~ cs_hat + black + white + ses1 + boy |schid1n , data) #second stage
summary(model20)






