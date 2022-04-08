#Empirical Exercise 3: 
#Discrimination: studying labor market participation and earnings differentials
#The objective of this exercise is to study differences in wages between different groups of individuals, in particular the effects of indigenous status and of gender on labor market participation and earnings using the Mexican Family Life Survey. Using the 2002 data round

# Author: 
# Information on data set
# Data set: http://www.ennvih-mxfls.org/english/assets/hh02dta_b3a.zip

#Codebook link here:
# http://www.ennvih-mxfls.org/english/assets/hh02cb_b3a.pdf

## A.1 Download and load packages
#Step 1
install.packages('downloader')
install.packages('foreign')
install.packages('dplyr')
install.packages('srvyr')
install.packages('stringr')

#Step 2
library(foreign) #Imports dta files
library(dplyr) #Data manipulation
library(downloader) #Downloads files from the internet
library(srvyr) #Survey
library(stringr) #Manipulate

## B.Download files
## B.1 Download
url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_b3a.zip"
#File name
file_name = "mxflx_house_char.zip"

# Downloads file only if it does not exist already
if (file.exists(file_name)){
  print('File already downloaded')
}else{
  # "Downloader" library--function: download.file()
  download.file(url, file_name)
  # Unzip folder
  unzip(file_name)
}

# 1. Labor market participation rate
## 1.1 What is the labor market participation rate of men versus women aged 16 to 65?
## 1.1.A Import labor market participation DATA
df_lf = read.dta() # iiia_tb.dta

### 1.1.A.1 Create dummy
#Hint: (Use question TB02 to construct this variable.)

# First show unique values
df_lf %>% 
  select(tb02_1) %>% 
  unique()

# Create dummy for tb02
df_lf = df_lf %>% 
  mutate(dummy_lfp = #YOURCODE HERE
  )

df_lf %>% 
  select(tb02_1, dummy_lfp) %>% 
  head(5)

### 1.1.B Import age DATA
df_age = read.dta()  #Import iiia_portad.dta
df_gender = read.dta()   #Import iiia_hm.dta

### 1.1.B.1 Rename age column
df_age = df_age  %>% 
  rename('age'  = 'edad' )

### 1.1.B.2 rename gender column
df_gender = df_gender %>% 
  rename('gender' = 'hm16')

df_gender %>% 
  names()

### 1.1.C merge age and lf
df = merge() # merge df_age and df_lf by folio and ls
df = merge() # merge df and df_gender

### 1.1.D Create male dummy
# Either hm16 or if you renamed gender

df = df %>% 
  mutate(dummy_male = #YOUR CODE HERE 
         )
df %>% 
  names()

df %>% 
  select(dummy_male) %>% 
  head(5)

### 1.1.1 Calculate participation age 16 to 65
df %>% 
  filter(age>=20 & age<=30) %>% #Filter age
  filter(dummy_lfp != 'NA') %>% #Filter na related labor market dummy
  group_by(hm17) %>% 
  summarise(output =  mean(dummy_lfp)) # Use labor market dummy


### 1.2. What is the labor market participation of indigenous versus non indigenous individuals aged 16 to 65? 
# Hint: (Use the variable ED03 to define who is indigenous.)
### 1.2.A Import data
df_indig = read.dta() #iiia_ed.dta

### 1.2.B Create dummy
df_indig %>% 
  select(ed03) %>% 
  unique()

df_indig = df_indig %>% 
  mutate(dummy_indig = #your code here as.numeric()
         )

### 1.2.C Merge
df = merge(df, df_indig, by = c('folio','ls'))

### 1.2.1 Calculate
df %>% 
  filter() %>% #Filter age
  filter(dummy_lfp != 'NA') %>% #Filter na related labor market dummy
  group_by() %>% #use indig
  summarise( #copy code from from 1.1.1
  )

# 2. Earnings and Hours worked:  Main Job only.
## 2.A Use previously imported data
df_lf %>% 
  names()

#2.1 TOTAL EARNINGS
df_wage = df_lf %>% 
  select(folio, ls, tb35a_2)

# Rename
df_wage = df_wage %>% 
  rename('total_consum_final' = 'tb35a_2')

# 2.2 hours worked per week (TB27p)
df_lf %>% 
  select(tb27p) %>% 
  summary()

### 2.2.1 Select hours worked week data
df_hours = df_lf %>% 
  select(folio,ls, tb27p) 

df_hours = df_hours %>% 
  rename( 'hrs_week' = 'tb27p' ) 

df_hours = df_hours %>% 
  mutate(hrs_month = hrs_week * 4.3) 

### 2.2.2 Merge with wage data
df_wage = merge(df_wage, df_hours,  by = c('folio', 'ls'))


### 2.2.3 Merge df_wage with original df data set
df = merge() 


# 2.4 Construct levels of schooling for all in the sample using the question Ed06.
#Hint:  I suggest creating a set of dummy variables e.g. with Primary or less
# Secondary schooling and more than Secondary.  
#You may also use Ed07 if you would like to construct a measure of number of years of schooling.

### 2.4.1 Construct dummies
df = df %>% 
  mutate(edu_primary_less = as.numeric(ed06==2 | ed06==3),
         edu_secondary = as.numeric(ed06>3 & ed06<11)) 


# 3. Question 3
# 3.1 What are the average and median labor market earnings for women and men in the previous month from their main job?
### 3.3.1 Create hourly wage
df = df %>% 
  mutate(hourly_wage = total_consum_final / hrs_month)

df = df %>% 
  filter(is.finite(hourly_wage))

### 3.1.2 Calculate med and mean of earnings BY GENDER
# Monthly average
df %>% 
  filter(total_consum_final != 'NA' ) %>% # filter na
  group_by(gender) %>% # group by gender 
  summarise(monthly_wage_mean = mean(total_consum_final),
            monthly_wage_median = median(total_consum_final))

# Weekly wage
df %>% 
  filter(hourly_wage  != 'NA' ) %>% # filter na
  group_by(gender) %>% # group by gender 
  summarise(hourly_wage_mean = mean(hourly_wage ),
            hourly_wage_median = median(hourly_wage ))

# 3.2 What are the average and median labor market earnings for indigenous and non-indigenous individuals in the previous month from their main job?
df %>% 
  filter(total_consum_final !='NA') %>% # filter na from total wage data
  group_by() %>% # group by indig dummy
  summarise()


# 3.3 Regression
#Carry out a regression of earnings per hour as a function: of age, education, gender and indigenous status
### Create separate data frame
reg = df  %>% 
  select(hourly_wage, age, dummy_male, dummy_indig, edu_secondary) 

### 3.3.A Regression example from video
df %>% 
  filter(hourly_wage<200) %>% 
  select(hourly_wage, age) %>% 
  plot(ylim= c(0,80),)
abline(lm(hourly_wage ~ age, data=reg%>% filter(hourly_wage<200)), col = 'red')

lm(hourly_wage ~ age, data=reg%>% filter(hourly_wage<200))


### 3.3.1
output = lm(hourly_wage ~ age + edu_secondary +, data=reg)
summary(output)



