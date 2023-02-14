# Author: Cory Baird
# Information on data set
# Code book: http://www.ennvih-mxfls.org/english/assets/hh02cb_b1.pdf
# Data set: http://www.ennvih-mxfls.org/english/ennvih-1.html

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
### A.2.1 Download
#URL 
url_cons = "http://www.ennvih-mxfls.org/english/assets/hh02dta_b1.zip"
#File name
file_name = "mxfls_cons.zip"
# "Downloader" library--function: download.file()
if (file.exists(file_name)){
  print('File already downloaded')
}else{
  download.file(url_cons, file_name)
  #Unzip file
  unzip("mxfls_cons.zip")}


### A.2.2 Download data:
url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip"
#File name
file_name = "mxfls.zip"
# "Downloader" library--function: download.file()
if (file.exists(file_name)){
  print('File already downloaded')
}else{
  download.file(url, file_name)
  unzip("mxfls.zip")
}


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

df_three_mon = df_three_mon %>% 
  mutate_at(vars(cs22a_2:cs22h_2), ~ . /3)

df_three_mon  %>% 
  head(2)

#### OPTIONAL yearly data
#df_year = df_cons_2 %>% 
#  select(contains('cs27') & ends_with('2') | 'folio') 
#df_year = df_year %>% 
#  mutate_at(vars(cs27a_2:cs27f_2), ~ . /12)


### 1.1.4 Merge all data
df_cons_all = merge(df_month, df_week, by = 'folio')
df_cons_all = merge(df_cons_all, df_three_mon, by = 'folio')

#### OPTIONAL yearly data
#df_cons_all = merge(df_cons_all, df_year, by = 'folio')

### 1.1.5 Sum row data
### 1.1.5.1 Replace na with zero
df_cons_all = df_cons_all %>% 
  replace(is.na(.), 0)

### 1.1.5.2 Sum rows
df_cons_all = df_cons_all %>% 
  select(-folio) %>% 
  mutate(
    total_cons = rowSums(.)
  ) %>% 
  cbind(df_cons_all %>% select(folio))

df_cons_all %>% 
  head(2)


### 1.2 Count family members
#### 1.2.1 Download data
url_house  = 'http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip'
#File name
file_name = "mxfls_house.zip"
# "Downloader" library--function: download.file()
if (file.exists(file_name)){
  print('File already downloaded')
}else{
  download.file(url_house, file_name)
  # Unzip folder
  unzip(file_name)
}

#### 1.2.2 Import data
df_ind_file = "hh02dta_bc/c_ls.dta"
df_ind = read.dta(df_ind_file)

### 1.2.3 Create home size count
df_homesize = df_ind %>% 
  group_by(folio) %>% 
  count()

df_homesize = df_homesize %>% 
  rename('family_members' = 'n' )

#### 1.2.4 Calculate per capita consumption
#### 1.2.4.1 Merge homesize with total cons
df_cons_all = merge(df_cons_all, df_homesize, by ='folio')

df_cons_all = df_cons_all %>% 
  select(folio, total_cons,family_members)

df_cons_all %>% 
  names()

#### 1.2.4.2 Merge homesize with total cons
df_cons_all = df_cons_all %>% 
  mutate(percap_cons = total_cons/family_members)


#### 1.2.4.3 Summarize
## Note: total_cons gives us the answer to question "Calculateper capita consumption"
df_cons_all %>% 
  summarize(percap_cons_mean = mean(percap_cons))

ans = df_cons_all %>% 
  summarise(ans1_percapcons = mean(percap_cons))



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

### 2.3.1 Count total number of people
total_people = sum(df_cons_all$family_members)

### 2.3.2 Calculate pct of people living in poverty
pov = df_cons_all %>% 
  filter(pov_dummy==1) %>% 
  summarize(ans2_1_povtotal = sum(family_members),
            ans2_1_povpct = ans2_1_povtotal / total_people)

ans = cbind(ans, pov)

### 2.3.3 Pov gap
df_cons_all %>% 
  filter(pov_dummy==1) %>% 
  head(5)

# Poverty gap
### 2.3.3.A Visualize: Poverty gap calculation for each household
df_cons_all %>%
  filter(pov_dummy==1) %>% 
  mutate(
    ans_2_1_povgap = ((povertyline - percap_cons) )
  ) %>% 
  head(5)

### 2.3.3.1 Poverty gap calculation for each household: RATIO
# Method 1
pov_gap_ratio = df_cons_all %>%
  filter(pov_dummy==1) %>% 
  summarize(
    ans_2_2_povgap = mean((povertyline - percap_cons) / povertyline)
                       )
pov_gap_ratio

ans = cbind(ans, pov_gap_ratio)

# Method 2: tells us the poverty: RAW GAP
pov_gap_raw = df_cons_all %>%
  filter(pov_dummy==1) %>% 
  summarize(
    pov_gap = mean((povertyline - percap_cons)) # Method 2 doesn't divide by pov line
  )
pov_gap_raw

## Method 2 can be convert to method 1 by simply dividing by poverty line
(pov_gap_raw/povertyline) == pov_gap_ratio


# 2.3.4 Poverty gap squared
pov_gap_sq = df_cons_all %>%
  filter(pov_dummy==1) %>% 
  summarize(
    ans_2_3_povgapSQ = mean(((povertyline - percap_cons) / povertyline)**2))

ans = cbind(ans, pov_gap_sq)

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

## 3.3 Calculate poverty rates for regions
### 3.3.A Create urban dummy
df_cons_all = df_cons_all %>% 
  mutate(urban_dummy = as.numeric(estrato ==1 |estrato==2))

## 3.3.1 Headcount
### People total
pop_total = df_cons_all %>% 
  group_by(urban_dummy) %>% 
  summarize( ans3_1_povpct = sum(family_members))

# People in pov
pop_pov = df_cons_all %>% 
  filter(pov_dummy == 1) %>% 
  group_by(urban_dummy) %>% 
  summarize( ans3_1_povpct = sum(family_members))

# People in pov / people total
headcount = pop_pov / pop_total
headcount = headcount %>% replace(is.na(.), 0)

## 3.3.2 Poverty gap
pov_gap_ratio = df_cons_all %>%
  filter(pov_dummy==1) %>% 
  group_by(urban_dummy) %>% 
  summarize(
    ans3_2_povgap = mean((povertyline - percap_cons) / povertyline)
  )
ans_3 = merge(headcount, pov_gap_ratio, by = 'urban_dummy')
ans_3
### 3.3.3 Poverty gap squared
pov_gap_sq = df_cons_all %>%
  filter(pov_dummy==1) %>% 
  group_by(urban_dummy) %>% 
  summarize(
    ans3_3_povgapSQ = mean(((povertyline - percap_cons) / povertyline)**2))

ans_3 = merge(ans_3, pov_gap_sq, by = 'urban_dummy')

## 4. Export data to excel
## 4.A create folder
dir.create('Output')

# 4.B Package to rotate df
install.packages('sjmisc')
library(sjmisc)

## 4.1 Output answer 1 and 2
ans %>% 
  select(-contains('total')) %>% # Gets rid of total column
  rotate_df() %>% # Transpose DF
  round(3)  %>% 
  write.csv('Output/ans_1_2.csv')

## 4.2 Output answer 3
ans_3 %>% 
  select(-contains('total')) %>% # Gets rid of total column
  rotate_df() %>% # Transpose DF
  round(3)  %>% 
  write.csv('Output/ans_3.csv')


ans %>% 
  select(-contains('total')) %>% # Gets rid of total column
  rotate_df() %>% 
  round(3)
