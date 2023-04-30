# Empirical Assignment 4:
# Author: Cory Baird

library(dplyr)

# A. Import Data
## A.1 Slower Import
#df = read.csv('Input/220419COVID19MEXICO.csv')

## A.2 Faster Import
library(data.table)
df_covid = fread('Input/220419COVID19MEXICO.csv')

# 1. How many individuals have been tested for covid19 (use the variable RESULTADO_ANTIGENO)?
df_covid %>% 
  #select(RESULTADO_ANTIGENO) %>% 
  count(RESULTADO_ANTIGENO)

df_covid %>% 
  mutate(
    dummy_tested = # resultado_antigeno for tested positive or tested negative
  ) %>% 
  select(dummy_tested) %>% 
  summary()

# Positive tests
# 2.1 What proportion have tested positive?   
df_covid %>% 
  filter(RESULTADO_ANTIGENO!=97) %>% 
  mutate(
    dummy_positive = #CREATE DUMMY HERE
  ) %>% 
  summarise(
    pos_percent = mean(dummy_positive)
  )

## 2.2 By Gender

# USE SAME COE ABOVE AND ADD GENDER

# 3. Mortality
## 3.A Dummy
df_covid %>% 
  count(FECHA_DEF)

df_covid = df_covid %>% 
  mutate(
    dummy_death = #NOT EQUAL TO '9999-99-99'
  )

## 3.1 What proportion of individuals testing positive passed away? 
df_covid %>% 
  filter(RESULTADO_ANTIGENO==1 & RESULTADO_LAB==1) %>% 
  summarise(
    death_pct = mean(dummy_death) * 100
  )

## 3.2 By gender?  
df_covid %>% 
  group_by(#GENDER HERE
           ) %>% 
  filter(RESULTADO_ANTIGENO==1 | RESULTADO_LAB==1) %>% 
  summarise(
    death_pct = mean(dummy_death) * 100
  )

# 3.3 By indigenous status?
df_covid %>% 
  group_by(#INDIGENOUS HERE
           ) %>% 
  filter(RESULTADO_ANTIGENO==1 | RESULTADO_LAB==1) %>% 
  summarise(
    death_pct = mean(dummy_death) * 100
  )


# 4. Analyze the probability of passing away by pre existing conditions and age/gender/indigenous status using a regression analysis. 
df_covid = df_covid %>% 
  mutate(
    dummy_indig = as.numeric(INDIGENA==1),
    dummy_diab = as.numeric(DIABETES==1),
    dummy_asm = as.numeric(ASMA==1),
    dummy_imu = as.numeric(INMUSUPR==1),
    dummy_hyper = as.numeric(HIPERTENSION==1),
    #OTRAS_COM
    #CARDIOVASCULAR
    #OBESIDAD
    #RENAL_CRONICA
    dummy_smoke = as.numeric(TABAQUISMO==1)
  )

# Removes scientific method
options(scipen=999)

# Regression 1
reg = glm(dummy_death ~ EDAD + dummy_indig, data=df_covid, family = "binomial")
summary(reg)

# Regression 2
reg = glm(dummy_death ~ EDAD + dummy_indig + dummy_smoke + dummy_diab + dummy_asm + dummy_imu + dummy_hyper, 
          data=df_covid, family = "binomial")
summary(reg)

plot(df_covid$dummy_death, df_covid$EDAD)






## 4.2 Train test 
install.packages('caret')
library(caret)


x = df_covid %>% select(dummy_indig, dummy_smoke)
y = df_covid %>% select(dummy_death)
up_sample = upSample(x, y, list = FALSE, yname = "dummy_death")
up_sample$x
table(up_sample$dummy_death) 



