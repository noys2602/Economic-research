library(dplyr)
library(tidyverse)


#____________________question 1____________________

mydata = read.csv("income_survey_2011.csv")

#subset#
mydata_section1 = subset(mydata, weekwhrs > 0 & 2011 - birthy >= 25 & 2011 - birthy <= 54 & 
                           (workstat=='self-employed' | workstat== 'salaried worker'))
 
print('section 2 - subset file ')
mydata_section1

#different averege monthly salary men and women#
df_men <- mydata %>%
  filter(sex == "male")

men_incsal = mean(df_men$incsal)

df_women <- mydata %>%
  filter(sex == "female")

women_incsal = mean(df_women$incsal)

monthly_gap_men_vs_women = men_incsal - women_incsal
print('section 3a - monthly gap men vs women')
monthly_gap_men_vs_women

#different averege hourly salary men and women#
men_avg_weeks = c(df_men$mwweeks)
men_avg_hours_per_week = c(df_men$weekwhrs)
men_avg_hour_of_work = mean(men_avg_hours_per_week*men_avg_weeks)
men_avg_per_hour = men_incsal / men_avg_hour_of_work

women_avg_weeks = c(df_women$mwweeks)
women_avg_hours_per_week = c(df_women$weekwhrs)
women_avg_hour_of_work = mean(women_avg_hours_per_week*women_avg_weeks)
women_avg_per_hour = women_incsal / women_avg_hour_of_work

print('section 3b - hourly pay diff men vs women')
men_avg_per_hour - women_avg_per_hour

#imigrants before 2001 
df_men_before_2001 = df_men %>%
  filter(immigry < 2001 | immigry == 'before 1947' )
men_before2001_avg_weeks = c(df_men_before_2001 $mwweeks)
men_before2001_avg_hours_per_week = c(df_men_before_2001 $weekwhrs)
men_before2001_avg_hour_of_work = 
  mean(men_before2001_avg_hours_per_week*men_before2001_avg_weeks)
men_before2001_incsal = mean(df_men_before_2001$incsal)
men_before2001_avg_per_hour = men_before2001_incsal / men_before2001_avg_hour_of_work
print('section 3c ')
print ('immigration before 2001 salary per hour')
men_before2001_avg_per_hour

#imigirants after 2001
df_men_after_2001 = df_men %>%
  filter(immigry != 'before 1947' & immigry > 2001)
men_after2001_avg_weeks = c(df_men_after_2001 $mwweeks)
men_after2001_avg_hours_per_week = c(df_men_after_2001 $weekwhrs)
men_after2001_avg_hour_of_work = 
  mean(men_after2001_avg_hours_per_week*men_after2001_avg_weeks)
men_after2001_incsal = mean(df_men_after_2001$incsal)
men_after2001_avg_per_hour = men_after2001_incsal / men_after2001_avg_hour_of_work
print ('immigration after 2001 salary per hour')
men_after2001_avg_per_hour

#made in israel
df_men_born_in_israel = df_men %>%
  filter(grepl('israel', origin))
men_born_in_israel_avg_weeks = c(df_men_born_in_israel$mwweeks)
men_born_in_israel_hours_per_week = c(df_men_born_in_israel$weekwhrs)
men_born_in_israel_incsal = mean(df_men_born_in_israel$incsal)
men_born_in_israel_avg_hour_of_work = mean(men_born_in_israel_avg_weeks * men_born_in_israel_hours_per_week)
men_born_in_israel_avg_per_hour = men_born_in_israel_incsal / men_born_in_israel_avg_hour_of_work
print ('born in israel salary per hour')
men_born_in_israel_avg_per_hour


print ("born in israel vs immigration after 2001")
men_born_in_israel_avg_per_hour - men_after2001_avg_per_hour

print ("immigration before 2001 vs born in israel")
men_before2001_avg_per_hour - men_born_in_israel_avg_per_hour 

print ("immigration before 2001 vs immigration after 2001")
men_before2001_avg_per_hour - men_after2001_avg_per_hour


#testing results
#unique(df_men_after_2001$immigry)
#unique(df_men_before_2001$immigry)
#unique(df_men_born_in_israel$origin)

print ("graphs")
print ('section 4a')
table(mydata$schooly)
hist(mydata$schooly,color = rainbow(1))

print('replacing above 50 with NA')
mydata$schooly[mydata$schooly>50] = NA
table(mydata$schooly)

print('section 4b')


df_sex = aggregate(mydata$schooly, list(mydata$sex), FUN=mean, na.rm = T)
barplot(df_sex$x,names.arg = df_sex$Group.1,main = 'avg school years 
        different sex',col = rainbow(10))

print('section 4c')


df_ages = aggregate(mydata$schooly, list(mydata$age), FUN=mean, na.rm = T)
barplot(df_ages$x,names.arg = df_ages$Group.1,main = 'avg school years 
        different ages',col = rainbow(10))

#____________________question 2____________________

df = read.csv("grades.csv")
summary(df$test) 
print ('Section 1 - mean = 66.8, Min = 25 ,Max = 100')
summary(df$targilim) 
print ('Section 2 - mean = 6.493, Min = 0 ,Max = 10')

df_max_exc <- df %>%
  filter(targilim == 10)
print('Section 3 - my estimation for the
      expected value will be the mean - 74.65217 ')
mean(df_max_exc$test,na.rm = T)

df_at_least_5_exc <- df %>%
  filter(targilim == 5)
print('Section 4 - my estimation for the
      expected value will be the mean - 52.8333 ')
mean(df_at_least_5_exc$test,na.rm = T)

print('section 5 -  graph')
plot(df$termpaper,df$test,main = 'test vs termpaper', col = rainbow(1))

print('section 6 - corraltion' )
x = df$targilim
y = df$test
cor(x,y,use = 'complete.obs')

print('section 7 - this is a weak cor ')








