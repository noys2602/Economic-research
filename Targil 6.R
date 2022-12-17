#Noy Swisa Segal

# import ------------------------------------------------------------------

library(tidyverse) 
library(modelsummary)


# settings ----------------------------------------------------------------


rm(list=ls()) # del all objects and functions
gc() #cleans memory
options(scipen=999) # tell R not to use Scientific notation
options(digits = 5) # controls how many digits are printed by default


# q 2 ---------------------------------------------------------------------

wage2 <- read.csv("~/Desktop/TAU/Economics/Econometrics/wage2.csv")
wage2$pexp = wage2$age -wage2$educ -6
wage2$pexp_sqr = (wage2$pexp)**2

model0 = lm(lwage ~ educ + pexp + pexp_sqr + age, wage2)
model0_sum = summary(model)

model1 = lm(lwage ~ educ + pexp + pexp_sqr , wage2)
model1_sum = summary(model1)

model2 = lm(lwage ~ educ + pexp + pexp_sqr + iq, wage2)
model2_sum = summary(model2)

model3 = lm(lwage ~ educ + pexp + pexp_sqr + iq + feduc, wage2)
model3_sum = summary(model3)

model4 = lm(lwage ~ educ + pexp + pexp_sqr + iq + feduc + meduc, wage2)
model4_sum = summary(model4) 


# q 4  --------------------------------------------------------------------

model5 = lm(lwage ~ pexp, wage2)
cor(wage2$pexp,wage2$pexp_sqr)

lm(pexp~pexp_sqr, wage2)










