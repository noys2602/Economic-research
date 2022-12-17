#Noy Swisa Segal

# import ------------------------------------------------------------------

library(tidyverse) 
library(modelsummary)


# settings ----------------------------------------------------------------



rm(list=ls()) # del all objects and functions
gc() #cleans memory
options(scipen=999) # tell R not to use Scientific notation
options(digits = 5) # controls how many digits are printed by default


# q 1 ---------------------------------------------------------------------

hprice <- read.csv("~/Desktop/TAU/Economics/Econometrics/hprice.csv")
model1 = lm(price ~ age + bedrooms + bathrooms + otherrooms, hprice)
model2 = lm(price ~ age + bedrooms + bathrooms + otherrooms + area , hprice)
modelsummary::modelsummary(models = list(model1, model2))

model3 = lm(price ~ age + area, hprice )

mod_sum2 =  summary(model2)
mod_sum3 =  summary(model3)

rsqr2 = mod_sum2$r.squared
rsqr3 = mod_sum3$r.squared


# q 2  --------------------------------------------------------------------

wage1 <- read.csv("~/Desktop/TAU/Economics/Econometrics/wage1.csv")
model4 = lm(lwage ~ educ + exper + tenure , wage1 )

shira <- data.frame(educ=12,exper=20,tenure=10)
ziv <- data.frame(educ=12,exper=20,tenure=8)
predict.lm(object = model4, newdata = shira, interval = "prediction", level = 0.95)
predict.lm(object = model4, newdata = ziv  , interval = "prediction", level = 0.95)

model5 = lm(lwage ~ educ + exper + exper**2 + tenure , wage1 )
mod_sum4 = summary(model4)
mod_sum5 = summary(model5)

lst1 = mod_sum4$coefficients[2]
lst_2 = c(mod_sum4$coefficients[2],mod_sum4$coefficients[3])


# q 3 ---------------------------------------------------------------------

model6 = lm(educ ~ exper + tenure, wage1)
mod_sum6 = summary(model6)
wage1$res6 = mod_sum6$residuals 

model7 = lm(lwage~res6 ,wage1)
mod_sum7 = summary(model7)
mod_sum7$coefficients[2]


# q 4 ---------------------------------------------------------------------

model8 = lm(lwage ~ educ + exper, wage1)
model9 = lm(lwage ~ educ, wage1)
model10 = lm(exper ~ educ, wage1)

modelsummary::modelsummary(models = list(model8, model9))












