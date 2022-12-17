library(tidyverse) 
library(modelsummary)

#noy swisa

#_________________question2________________
mydata = read.csv("SLEEP75.csv")

#section ??
model = lm(sleep ~ totwrk , data = mydata)
mod_sum = summary(model)
mod_sum
beta0 = model$coefficients[1]
beta1 = model$coefficients[2]
print(paste('Y = ',beta0,beta1,"X"))
rsqr = mod_sum$r.squared
n = mod_sum$df[2] + mod_sum$df[1]
#section ??
minus2 = (-120) * beta1 

\