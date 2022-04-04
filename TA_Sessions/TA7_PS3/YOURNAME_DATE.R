#Empirical Exercise 3: 
#Discrimination: studying labor market participation and earnings differentials
#The objective of this exercise is to study differences in wages between different groups of individuals, in particular the effects of indigenous status and of gender on labor market participation and earnings using the Mexican Family Life Survey. Using the 2002 data round

# Author: 
# Information on data set
# Code book: http://www.ennvih-mxfls.org/english/assets/hh02cb_b3a.pdf
# Data set: http://www.ennvih-mxfls.org/english/assets/hh02dta_b3a.zip

#Codebook link here:
#http://www.ennvih-mxfls.org/english/assets/hh02cb_bc.pdf

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

## B.1.1 Explaining B.1. function
# If statement
x = 4
if (x>2){ # condition defined ()
  print('Number is bigger than 2')
}else{
  print('Number is smaller than 2')
}
# Check if file exists: Response is TRUE or FALSE
file.exists('mxflx_house_char.zip')

# 1. Labor market participation rate
## 1.1 What is the labor market participation rate of men versus women aged 16 to 65?
## 1.1.A Import labor market participation DATA
df_lf = read.dta() # read iia_tb.dta

### 1.1.A.1 Create dummy
#Hint: (Use question TB02 to construct this variable.)

# First show unique values
df_lf %>% 
  select(tb02_1) %>% 
  unique()

# Create dummy for tb02
df_lf = df_lf %>% 
  mutate(dummy_lfp = #CODE HERE
           )


### 1.1.B Import age DATA
df_age = read.dta() #Import iiia_portad.dta
df_gender = read.dta()  #Import iiia_hm.dta

### 1.1.B.1 Rename age column
df_age = df_age  %>% 
  rename('age'  = 'edad' )

### 1.1.C merge age and lf
df = merge(df_age, df_lf, by = c('folio','ls'))
df = merge(df, df_gender, by = c('folio','ls'))

### 1.1.1 Calculate participation age 16 to 65
df %>% 
  filter() %>% #Filter age
  filter() %>% #Filter nas
  group_by() %>% #use hm16 column
  summarise() # Use labor market dummy


### 1.2. What is the labor market participation of indigenous versus non indigenous individuals aged 16 to 65? 
# Hint: (Use the variable ED03 to define who is indigenous.)
### 1.2.A Import data
df_indig = read.dta() #Import iiia_ed.dta


### 1.2.B Merge
df = merge(df, df_indig, by = c('folio','ls'))

### 1.2.C Create dummy
df %>% 
  select(ed03) %>% 
  unique()

df = df %>% 
  mutate(dummy_indig = as.numeric(ed03==1))

df %>% 
  names()

### 1.2.1 Calculate
df %>% 
  filter(age>15 & age<66) %>% 
  filter(dummy_lfp !='NA') %>%
  group_by(dummy_indig) %>% 
  summarise(mean(dummy_lfp) * 100)

# 2. Earnings and Hours worked:  Main Job only.
## 2.A Use previously imported data
df_lf %>% 
  names()

#2.1 TOTAL EARNINGS

## Hint: total earnings in the previous month (set of questions composing TB35A)
#To construct the earnings variable, make sure to include both individuals who only report total earnings as well as 
#to sum together individuals who provide more detailed categories of earnings.

### 2.1.A Sum detailed categories
# Columns: tb35aa_2 to tb35ai_21
df_wage_detail =  df_lf %>% 
  select(contains('tb35a') & contains('2')) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_consum_detailed = rowSums(.)) %>% 
  cbind(df_lf %>% select(folio, ls)) %>% 
  select(folio, ls, total_consum_detailed)

## 2.1.B Add total earnings to df
df_wage = df_lf %>%
  select(folio,ls, tb35a_2) %>% 
  rename('total_consum' = 'tb35a_2' )

# 2.1.C Merge
df_wage = merge(df_wage_detail, df_wage, by = c('folio', 'ls'))

## Delete df_wage_detail
rm(df_wage_detail)

## 2.1.D Combine total_consum and total_consum_detailed columns
# Hint: use case_when function inside mutate function
df_wage = df_wage %>% 
  mutate(total_consum_final = 
           case_when(
             total_consum_detailed != 0 & is.na(total_consum) ~ total_consum_detailed, 
             total_consum_detailed != 0 & !is.na(total_consum) ~ total_consum_detailed,
             !is.na(total_consum) ~ 0,
           )) 

## Drop unecess consum cols
df_wage = df_wage %>% 
  select(-total_consum_detailed, -total_consum)


# 2.2 hours worked per week (TB27)
df_lf %>% 
  select(tb27s) %>% 
  summary()

### 2.2.1 Select hours worked week data


### 2.2.2 Merge with wage data

# Delete unecessary df_hours data
rm(df_hours)

# 2.3 weeks worked last year (TB29).  
df_lf %>% 
  select(tb29_p1) %>% 
  summary()

### 2.3.1 Select weeks worked


### 2.3.2 Merge with wage data

# Delete unecessary df_hours data
rm(df_weeks)


# 2.4 Construct levels of schooling for all in the sample using the question Ed06.
#Hint:  I suggest creating a set of dummy variables e.g. with Primary or less
# Secondary schooling and more than Secondary.  
#You may also use Ed07 if you would like to construct a measure of number of years of schooling.

### 2.4.1 Construct dummies
df_edu = df_indig %>% 
  mutate(edu_primary_less = as.numeric(ed06==2 & ed06==3),
         edu_secondary = as.numeric(ed06>3 & ed06<11))

### Remove na values and select only certain columns
df_edu = df_edu%>% 
  select(folio, ls, edu_primary_less, edu_secondary) %>% 
  na.omit()

### 2.4.2 Combine with wage data


# 3. Question 3
# 3.1 What are the average and median labor market earnings for women and men in the previous month from their main job?

### 3.1.1 Add gender to wage data

### Merge
df_wage = merge(df_wage, df_gender, by =c('folio','ls'))

### 3.1.2 Calculate med and mean of earnings BY GENDER
df_wage %>% 
  filter() %>% # filter na
  group_by() %>% # group by gender 
  summarise(wage_mean = mean(total_consum_final),
            wage_median = median(total_consum_final))

# 3.2 What are the average and median labor market earnings for indigenous and non-indigenous individuals in the previous month from their main job?
### 3.2.1 Add indig data

### Merge
df_wage = merge(df_wage, df_indig, by =c('folio','ls'))

### 3.2.2 Calculate med and mean of earnings BY INDIG
df_wage %>% 
  filter() %>% # filter na from total wage data
  group_by(dummy_indig) %>% # group by indig dummy
  summarise(wage_mean = mean(total_consum_final),
            wage_median = median(total_consum_final))

# 3.3 Regression
### 3.3.1 Create hourly wage
df_wage = df_wage %>% 
  mutate(hourly_wage = total_consum_final / hrs_week)

### 3.3.2 Add age
df_age = df_age %>% 
  select(folio, ls, age)
df_wage = merge(df_wage, df_age, by = c('folio','ls'))

### 3.3.3 regression
#Carry out a regression of earnings per hour as a function: of age, education, gender and indigenous status
### Create separate data frame
reg = df_wage %>% 
  select(hourly_wage, age, dummy_male, dummy_indig, edu_secondary) %>% 
  filter(is.finite(hourly_wage)) # Get rid of inifite values

### 3.3.1
lm(y~x, data=reg)



