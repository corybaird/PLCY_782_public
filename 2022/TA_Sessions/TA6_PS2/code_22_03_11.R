# Author: Cory Baird

# A.1 Install packages
#Step 1
install.packages('downloader')
install.packages('dplyr')
install.packages('ggplot2')

#Step 2
library(foreign) #Imports dta files
library(dplyr) #Data manipulation
library(ggplot2)


## A.2 Download data 
# Information on data set
# Code book: http://www.ennvih-mxfls.org/english/assets/hh02cb_b1.pdf
# Data set: http://www.ennvih-mxfls.org/english/ennvih-1.html

### A.2.1 Download
#URL 
url_cons = "http://www.ennvih-mxfls.org/english/assets/hh02dta_b1.zip"
#File name
file_name = "mxfls_cons.zip"
# "Downloader" library--function: download.file()
download.file(url_cons, file_name)
#Unzip file
unzip("mxfls_cons.zip")

### A.2.2 Download data:
url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip"
#File name
file_name = "mxfls.zip"
# "Downloader" library--function: download.file()
download.file(url, file_name)
unzip("mxfls.zip")

## A.2.1 Import file into R
df_cons = read.dta("hh02dta_b1/i_cs.dta")
df_cons_2 = read.dta("hh02dta_b1/i_cs1.dta")


# 1. Calculate a measure of total consumption and per capita consumption for each household in the 2002 round

## 1.1 Grab consumption data for month, week, 3 month
## 1.1.1 Monthly data
df_month = df_cons %>% 
  select(contains('cs16') & ends_with('2') | 'folio')

df_month %>% 
  head(4)


## 1.1.2 Weekly data
df_week = df_cons %>% 
  select(contains('cs02') & ends_with('2') | 'folio') 


# 1.1.2.1 Convert data to monthly
df_week %>% 
  names()

df_week = df_week %>% 
  mutate_at(vars(cs02a_12:cs02e_32), ~ . *4.3)

df_week %>% 
  head(2)

## 1.1.3 Three month data
# Hint use df_cons_2 data
df_three_mon = df_cons_2 %>% 
  select(contains('cs22') & ends_with('2') | 'folio') 

df_three_mon %>% 
  head(3)

# 1.1.3.1 Convert data to monthly
df_three_mon  %>% 
  names()

###############
#YOUR CODE HERE  same as  1.1.2.1
################

df_three_mon  %>% 
  head(2)

### 1.1.4 Merge all data

###############
#YOUR CODE HERE
################
df_cons_all = merge(data1, data2, by = '')
df_cons_all = merge(df_cons_all, data3, by ='')



### 1.1.5 Sum row data
### 1.1.5.1 Replace na with zero
df_cons_all = df_cons_all %>% 
  replace(is.na(.), 0)

df_cons_all %>% 
  summary()

### 1.1.5.2 Sum rows

df_cons_all = df_cons_all %>% 
  select(-folio)%>% 
  mutate(
    total_cons = rowSums(.)
  ) %>% 
  cbind(df_cons_all %>% select(folio))

df_cons_all %>% 
  head(2)
## Note: total_cons gives us the answer to question "Calculate a measure of total consumption"


### 1.2 Count family members
#### 1.2.1 Download data
url_house  = 'http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip'
#File name
file_name = "mxfls_house.zip"
# "Downloader" library--function: download.file()
download.file(url_house, file_name)
# Unzip folder
unzip(file_name)

#### 1.2.2 Import data
df_ind_file = "hh02dta_bc/c_ls.dta"
df_ind = read.dta(df_ind_file)

### 1.2.3 Create home size count

### Note: Code can be found in last assignment
###############
#YOUR CODE HERE
################

## Delete df_ind data
rm(df_ind)

#### 1.2.4 Calculate per capita consumption
#### 1.2.4.1 Merge homesize with total cons

df_cons_all = df_cons_all %>% 
  select(folio, total_cons, family_members)

df_cons_all %>% 
  names()

#### 1.2.4.2 Merge homesize with total cons

###############
#YOUR CODE HERE
################

#### 1.2.4.3 Summarize
## Note: total_cons gives us the answer to question "Calculateper capita consumption"

###############
#YOUR CODE HERE  same as  1.1.2.1
################

df_cons_all %>% 
  select(percap_cons) %>% 
  summary()


# 2. Calculate the set of poverty rates nationwide for the head count and average poverty gap, average poverty gap squared. Assume the poverty line=500 pesos per person.  
# Provide poverty rates based on household consumption per capita.

## 2.1 Set poverty line and create dummy
povertyline = 500 #saved object
df_cons_all = df_cons_all %>% 
  mutate(pov_dummy = as.numeric(percap_cons < povertyline)) 


# 2.2 Calculate prevalence of households living in poverty 
# Note this is not the head count
df_cons_all %>% 
  summarize(pov_dummy = mean(pov_dummy) * 100)

## 2.3 Calculate headcount
# Note: Filter pov_dummy and count homesize

### 2.3.1 Count total number of people in poverty
# Method 1
df_cons_all %>% 
  group_by(pov_dummy) %>% 
  summarize(
    people = sum(family_members)
  ) 
print(17097 / (17097+16953))

# method 2
total_people = sum(df_cons_all$family_members)
total_people == 17097+16953

### 2.3.2 Calculate pct of people living in poverty
df_cons_all %>% 
  filter(pov_dummy==1) %>% 
  summarize(number_people_poverty = sum(family_members),
            pct_people_poverty = number_people_poverty / total_people)


# 3. Repeat 2) by area of residence. 
#How does the poverty rate change by rural/urban residence?   
#What can you say about the severity of poverty in rural versus urban areas?

## 3.1 Import estrato data
df_region = read.dta("hh02dta_bc/c_portad.dta")
df_region = df_region %>% select(folio, estrato)

## 3.2 Merge with cons data
df_cons_all = merge(df_region, df_cons_all, by ='folio')
df_cons_all %>% 
  head(3)

## 3.3 Add groupby estrato function to 2.3.2 code
###############
#YOUR CODE HERE  same as  1.1.2.1
################


