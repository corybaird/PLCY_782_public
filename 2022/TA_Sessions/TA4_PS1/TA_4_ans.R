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
# "Downloader" library--function: download.file()
download.file(url_house, file_name)
# Unzip folder
unzip(file_name)

## B.2 Weights
url = 'http://www.ennvih-mxfls.org/english/assets/hh02w_bc.zip'
file_name = "mxfls_wts.zip"
download.file(url, file_name)
unzip("mxfls_wts.zip")


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
df_wt %>% 
  names()

### C.2.2 Remove leading zeros from folio column
df_wt = df_wt %>% 
  mutate(folio = str_replace(folio, "^0+" ,"")) 

df_wt %>% 
  head(5)

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
# Method 1-- Recommended method
df_homesize %>% 
  summarize(family_mean_unweighted = mean(family_members),
            family_sd_unweighted = sd(family_members) ) 

# Method 2
df_homesize %>% 
  select(family_members) %>% 
  summary()

# Method 3
df_homesize %>% 
  pull(family_members) %>% 
  mean()
  
df_homesize %>% 
  pull(family_members) %>% 
  sd()


## 1.1.2 Weight mean
# Corresponds to question 2
### Method 1
df_homesize %>% 
  as_survey(weights = weights) %>% 
  summarize(
    MEAN = survey_mean(family_members),
    SD = survey_sd(family_members)
  )
  

# Method 2
df_homesize %>% 
  summarize(
    family_mean_WEIGHTED = weighted.mean(family_members, weights),
  )


## 1.2 Number of children under 18 in the household
### 1.2.A Create new dataframe for children under 18
df_under18 = df_ind %>% 
  filter(ls02_2 < 19) %>% 
  group_by(folio) %>% 
  count()

df_under18 %>% 
  head(4)
## 1.2.B Add weights 
df_under18 = merge(df_under18, df_wt)
df_under18 %>% 
  head(3)

## 1.2.C rename
df_under18 = df_under18  %>% 
  rename('Number_under18' = 'n' )
df_under18 %>% 
  head(3)

### 1.2.1 Un-weighted mean
df_under18 %>% 
  summarize(
    under18_weighted_mean = mean(Number_under18),
    under18_weighted_sd = sd(Number_under18)
  )

### 1.2.1 Un-weighted mean
df_under18 %>% 
  as_survey(weights = weights) %>% 
  summarize(
    under18_weighted_mean = survey_mean(Number_under18),
    under18_weighted_sd = survey_sd(Number_under18)
  )


## 1.3 Proportion of households with a toilet
### 1.3.A Show unique values 
#cv16
df %>% 
  select(cv16) %>% 
  unique()

### 1.3.1 Create dummy
df = df %>% 
  mutate(dummy_toilet = as.numeric(cv16==1)) 

### 1.3.1.1 Show result
df %>% 
  select(dummy_toilet, cv16) %>% 
  tail(5)

### 1.3.2 Show mean
df %>% 
  select(dummy_toilet) %>% 
  summary()

### 1.3.3 Remove na
df %>% 
  filter(dummy_toilet != 'NA') %>% 
  select(dummy_toilet) %>% 
  summary()

### 1.3.3 UN-WEIGHTED mean
df %>% 
  filter(dummy_toilet != 'NA') %>% 
  summarize(mean = mean(dummy_toilet),
            #add sd) 

### 1.3.4 WEIGHTED mean
# Corresponds to question 2
df %>% 
  filter(dummy_toilet != 'NA') %>% 
  as_survey(weights = weights) %>% 
  summarize(toilet_weighted_MEAN = survey_mean(dummy_toilet),
            toilet_weight_SD = survey_sd(dummy_toilet)
            ) 

## 1.4 Proportion of households with a separate room for cooking
#Hint: c_cv.dta dataset
# Hint: Either create a dummy or recode existing data from 3-0 1-1

### Manipulate code here ###

### 1.4.1 Unweighted mean


### 1.4.2 Weighted mean


## 1.5 Proportion of households with a telephone
#Hint: c_cv.dta dataset
# Hint: Either create a dummy or recode existing data from 3-0 1-1

### Manipulate code here ###

### 1.5.1 Unweighted mean


### 1.5.2 Weighted mean


## 1.6 Proportion with a dirt (soil) floor. 
# Hint: c_cvo.dta dataset 
### 1.6.A Merge in c_cvo.dta dataset
#### 1.6.A.1 Import
df_house_ob = read.dta('hh09dta_bc/c_cvo.dta')

#### 1.6.A.2 Merge



### 1.6.1 Unweighted mean



### 1.6.2 Weighted mean




# 2.characteristics of the (declared) head of the household.
# Corresponds to question 3

## 2.1 Age
### 2.1.1 Filter only the head of household
df_ind %>% 
  filter(ls05_1 == 1) 
  ## Add summarise function to get answer


## 2.2 proportion who have completed primary school (six grades)
df_ind %>% 
  filter(ls05_1 == 1) %>% 
  group_by(ls14) %>% 
  count() %>% 
  mutate(pct = n/length(df))
  

# 2.3 proportion who are working or contributing to HH expenditure.



# 3. characteristics of children in the household between the ages of 6 and 18
# Corresponds to question 4
## 3.1 Filter by age
### You must find the age column
df_ind %>% 
  filter(AGECOLUMN>8 & AGECOLUMN)# AGECOLUMN is not correct find the age from the codebook


## 3.2 Use ggplot 
# This is shown in this note: https://github.com/corybaird/PLCY_782_public/blob/main/TA_Sessions/TA3_PS1/TA3_PS1_Notes.ipynb






