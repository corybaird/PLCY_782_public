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

# B. Download Data
## B.1 Household Data
### Url name
url_house  = 'http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip'
#File name
file_name = "mxfls_house.zip"

# Downloads file only if it does not exist already
if (file.exists(file_name)){
  print('File already downloaded')
}else{
  # "Downloader" library--function: download.file()
  download.file(url_house, file_name)
  # Unzip folder
  unzip(file_name)
}

## B.2 Weights
url = 'http://www.ennvih-mxfls.org/english/assets/hh02w_bc.zip'
file_name = "mxfls_wts.zip"

# Downloads file only if it does not exist already
if (file.exists(file_name)){
  print('File already downloaded')
}else{
  # "Downloader" library--function: download.file()
  download.file(url, file_name)
  unzip("mxfls_wts.zip")
}

# C. Import data
## C.1 Household data: Dwelling
df_house_file = "hh02dta_bc/c_cv.dta"
df_house = read.dta(df_house_file) 

## C.2 Weights
df_wt = read.dta('hh02w_bc.dta')
df_wt %>% head(2)

### C.2.1 Renname weights
df_wt = df_wt %>% 
  rename('weights' = 'fac_libc')

# Show columns
df_wt %>% 
  names()

### C.2.2 Remove leading zeros from folio column
df_wt = df_wt %>% 
  mutate(folio = str_replace(folio, "^0+" ,"") # This is the line of code that was not working
         )

# C.3 Individ data
df_ind_file = "hh02dta_bc/c_ls.dta"
df_ind= read.dta(df_ind_file)


## D. Merge data
## D.1 Check for overlapping columns
## Wt columns
df_wt %>% 
  names()

# House columns
df_house %>% 
  names()

## D.2 Merge
### D.2.1 Merge weights and household
df = merge(df_wt, 
      df_house, 
      by =c('folio'))


# 1. Question 1: USE WEIGHTS
# Provide a table of means and standard deviations of the following variables 

## 1.1 Total household size
### 1.1.A Create new data frame which has household size
df_homesize = df_ind %>% 
  group_by(folio) %>% 
  count()

df_homesize %>% 
  head(2)

### 1.1.B Rename
df_homesize = df_homesize %>% 
  rename('family_members' = 'n' )

df_homesize %>% 
  head(2)

### 1.1.C Add weights
df_homesize = merge(df_homesize, df_wt, by ='folio')
df_homesize %>% head(3)

## 1.1.1 Un-weighted mean
ans = df_homesize %>% 
  summarize(ans1_mean = mean(family_members),
            ans1_sd = sd(family_members) ) 
ans # Save answer as ans 



## 1.1.2 Weight mean
ans1 = df_homesize %>% 
  as_survey(weights = weights) %>%
  summarize(ans1_meanWt = survey_mean(family_members),
            ans1_sdWt = survey_sd(family_members) ) 

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans

df_homesize %>% 
  summary(5)

## 1.2 Number of children under 18 in the household
### 1.2.A Create new dataframe for children under 18
df_under18 = df_ind %>% 
  filter(ls02_2 < 19) %>% #ls02_2= Age 
  group_by(folio) %>% 
  count()

## 1.2.B Add weights 
df_under18 = merge(df_under18, df_wt, by ='folio')
df_under18 %>% 
  head(3)

## 1.2.C rename
df_under18 = df_under18  %>% 
  rename('Number_under18' = 'n' )
df_under18 %>% 
  head(3)

### 1.2.1 Un-weighted mean
ans1 = df_under18 %>% 
  summarize(
    ans2_mean = mean(Number_under18),
    ans2_sd = sd(Number_under18)
  )

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans


### 1.2.1 Un-weighted mean
ans1 = df_under18 %>% 
  as_survey(weights = weights) %>% 
  summarize(
    ans2_meanWt = survey_mean(Number_under18),
    ans2_sdWt = survey_sd(Number_under18)
  )

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans


## 1.3 Proportion of households with a toilet
### 1.3.A Show unique values 
#cv16
df %>% 
  select(cv16) %>% 
  head(5)

df %>% 
  select(cv16) %>% 
  unique()

### 1.3.1 Create dummy
df %>% 
  mutate(dummy_toilet = as.numeric(cv16==1)) %>% 
  select(dummy_toilet, cv16) %>% 
  head(20)

#Method 1
df = df %>% 
  mutate(dummy_toilet = as.numeric(cv16==1)) 

#Method 2
df %>% 
  mutate(cv16 =  recode(cv16, '4'=0, '3'=0, '2'=0, '1' =1))
### 1.3.1.1 Show result
df %>% 
  select(dummy_toilet, cv16) %>% 
  tail(5)

### 1.3.2 Show mean
df %>% 
  select(dummy_toilet) %>% 
  summary()

df %>% 
  filter(dummy_toilet != 'NA') %>% 
  select(dummy_toilet) %>%
  summary()

### 1.3.3 Remove na
df %>% 
  filter(dummy_toilet != 'NA') %>% 
  select(dummy_toilet) %>% 
  summary()

### 1.3.3 UN-WEIGHTED mean
ans1 = df %>% 
  filter(dummy_toilet != 'NA') %>% 
  summarize(ans3_mean = mean(dummy_toilet),
            ans3_sd = sd(dummy_toilet)
            ) 

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans

### 1.3.4 WEIGHTED mean
# Corresponds to question 2
ans1 = df %>% 
  filter(dummy_toilet != 'NA') %>% 
  as_survey(weights = weights) %>% 
  summarize(ans3_meanWt = survey_mean(dummy_toilet),
            ans3_sdWt = survey_sd(dummy_toilet)
            ) 
# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans

## 1.4 Proportion of households with a separate room for cooking
#Hint: c_cv.dta dataset
# Hint: Either create a dummy or recode existing data from 3-0 1-1

### Manipulate code here ###

# Show how to create dummy with original column and new dummy colum
df_house %>% 
  mutate(
    dummy_kitchen = as.numeric(cv05 == 1)
  ) %>% 
  select(dummy_kitchen, cv05) %>% 
  head(10)

## Create dummy
df_house = df_house %>% 
  mutate(
    dummy_kitchen = as.numeric(cv05 == 1)
  ) 

## Merge
df_house = merge(df_wt, df_house, by='folio')


### 1.4.1 Unweighted mean
ans1 = df_house %>% 
  filter(dummy_kitchen != 'NA') %>% 
  summarize(ans4_mean = mean(dummy_kitchen),
            ans4_sd = sd(dummy_kitchen)
  ) 
# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans

### 1.4.2 Weighted mean
ans1 = df_house %>% 
  as_survey(weights = weights) %>% 
  filter(dummy_kitchen != 'NA') %>% 
  summarize(ans4_meanWt = survey_mean(dummy_kitchen),
            ans4_sdWt = survey_sd(dummy_kitchen)
  ) 

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans

## 1.5 Proportion of households with a telephone
#Hint: c_cv.dta dataset
# Hint: Either create a dummy or recode existing data from 3-0 1-1
df_house %>% 
  mutate(
    dummy_telephone = as.numeric(cv01_1 == 1)
  ) %>% 
  select(dummy_telephone, cv01_1) %>% 
  head(10)

df_house = df_house %>% 
  mutate(
    dummy_telephone = as.numeric(cv01_1 == 1)
  )

### Manipulate code here ###

### 1.5.1 Unweighted mean
ans1= df_house %>% 
  filter(dummy_telephone != 'NA') %>% 
  summarize(ans5_mean = mean(dummy_telephone),
            ans5_sd = sd(dummy_telephone)
  ) 

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans

### 1.5.2 Weighted mean
ans1 = df_house %>% 
  as_survey(weights = weights) %>% 
  filter(dummy_telephone != 'NA') %>% 
  summarize(ans5_meanWt = survey_mean(dummy_telephone),
            ans5_sdWt = survey_sd(dummy_telephone)
  ) 

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans

## 1.6 Proportion with a dirt (soil) floor. 
# Hint: c_cvo.dta dataset 
### 1.6.A Merge in c_cvo.dta dataset
#### 1.6.A.1 Import
df_house_ob = read.dta('hh02dta_bc/c_cvo.dta')

#### 1.6.A.2 Merge
df_house_ob %>% 
  head(4)

df_house_ob = merge(df_house_ob, df_wt, by ='folio')

df_house_ob %>% 
  names()

df_house_ob = df_house_ob %>% 
  mutate(
    dummy_floor_dirt = as.numeric(cvo05_1 == 3)
  )

### 1.6.1 Unweighted mean
ans1 = df_house_ob %>% 
  filter(dummy_floor_dirt  != 'NA') %>% 
  summarize(ans6_mean = mean(dummy_floor_dirt),
            ans6_sd = sd(dummy_floor_dirt)
  ) 

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans

### 1.6.2 Weighted mean
ans1 = df_house_ob %>% 
  filter(dummy_floor_dirt  != 'NA') %>% 
  as_survey(weights = weights) %>%
  summarize(ans6_meanWt = survey_mean(dummy_floor_dirt),
            ans6_sdWt = survey_sd(dummy_floor_dirt)
  ) 

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans


# 2.characteristics of the (declared) head of the household.
# Corresponds to question 3

## 2.1 Age
### 2.1.1 Filter only the head of household
ans1 = df_ind %>% 
  filter(ls05_1 == 1 & ls02_2!='NA') %>% 
  summarise(
    ans7_mean = mean(ls02_2),
    ans7_sd = sd(ls02_2)
  )

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans

## 2.2 proportion who have completed primary school (six grades)

# Method 1: By hand
df_ind %>% 
  filter(ls05_1 == 1) %>% 
  group_by(ls14) %>% 
  count() 

df_ind %>% 
  filter(ls05_1 == 1) %>% 
  count()

# Method 2
df_ind_hh = df_ind %>% 
  filter(ls05_1 == 1)

df_ind_hh = df_ind_hh %>% 
  mutate(dummy_primary = as.numeric(ls14 == 3))

ans1 = df_ind_hh %>% 
  filter(dummy_primary!='NA') %>% 
  summarize(
    ans8_mean = mean(dummy_primary),
    ans8_sd = sd(dummy_primary)
  )
ans1

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans


# 2.3 proportion who are working or contributing to HH expenditure.
df_ind_hh = df_ind_hh %>% 
  mutate(
    dummy_working = as.numeric(ls12==1)
  )

ans1 = df_ind_hh %>% 
  filter(dummy_working!='NA') %>% 
  summarize(
    ans9_mean = mean(dummy_working),
    ans9_sd = sd(dummy_working)
  )

# Save answer to dataframe: Save as ans1 then overwrite ans (which contains other answers)
ans = cbind(ans, ans1)
ans


df_ind_hh %>% 
  mutate(dummy_primary_mult = as.numeric(ls14 == 3 | ls14==4)) %>% 
  select(ls14, dummy_primary_mult) %>% 
  head(10)


### 2.E Export answers
# Package used to transpose df
install.packages('sjmisc')
library(sjmisc)
ans_final = ans %>% 
  as_tibble() %>% 
  select(-contains('se')) %>% 
  rotate_df()

# Resets row names as cols
setDT(ans_final, keep.rownames = TRUE)
ans_final = ans_final %>% rename('answer'='rn')

# Pivot table
install.packages('tidyr')
library(tidyr)
ans_final = ans_final %>% 
  separate(answer, c('ans', 'stat')) %>% 
  pivot_wider(names_from = stat, values_from = V1)

ans_final = ans_final  %>% mutate_if(is.numeric, round, digits=3)

ans_final %>% 
  write.csv('Solutions.csv')

# 3. characteristics of children in the household between the ages of 6 and 18
# Corresponds to question 4
## 3.1 Filter by age
### You must find the age column

# Filter
df_school = df_ind %>% 
  filter(ls02_2>5 & ls02_2<19)

df_school = df_school %>% 
  mutate(attend_dummy = as.numeric(ls16 == 1))


df_attendance = df_school %>% 
  filter(attend_dummy!='NA') %>% 
  group_by(ls02_2) %>% 
  summarize(mean_attendance = mean(attend_dummy))

df_attendance = df_attendance %>% 
  rename('Age'= 'ls02_2')

## 3.2 Use ggplot 
# This is shown in this note: https://github.com/corybaird/PLCY_782_public/blob/main/TA_Sessions/TA3_PS1/TA3_PS1_Notes.ipynb

df_attendance %>% 
  ggplot(aes(x = Age, y =mean_attendance)) + geom_bar(stat="identity")

ggsave('q4_plot.png')


