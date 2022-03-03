# TA_6_PS2

# A.1 Install packages
#Step 1
#install.packages('downloader')
#install.packages('dplyr')
#install.packages('ggplot2')

#Step 2
library(foreign) #Imports dta files
library(dplyr) #Data manipulation
library(ggplot2)


## A.2 Download data 
### A.2.1 Data set: http://www.ennvih-mxfls.org/english/ennvih-1.html
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


# 1. Select columns

## 1.A Show columns
df_cons %>% 
  names()


## 1.1 Select column containing specific string (text)
df_cons  %>% 
  select(contains('cs02')) %>% 
  names()


## 1.2 Select column ending with specific string (text)
df_cons  %>% 
  select(ends_with('32')) %>% 
  names()


## 1.3 Combine ends_with and contains functions
df_cons  %>% 
  select(ends_with('32') & contains('cs04')) %>% 
  names()


# 2. Multiply multiple columns by scalar
df_cons %>% 
  names()
  

## 2.1 Select multiple columns
df_cons %>% 
  select(cs16h_2:cs18_2) %>% 
  head(4)


## 2.2 Select multiple columns and mutate them
df_cons %>% 
  mutate_at(vars(cs16h_2:cs18_2), ~ . *4.3) %>% # multiply columns selected by 4.3
  select(cs16h_2:cs18_2)%>% 
  head(4)


# 3. Row sums
## 3.A create example dataframe
df_cons_example = df_cons %>% 
  select(cs16h_2,cs16i_2,cs16f_2) 

df_cons_example %>% 
  head(5)


## 3.1 Row sum
df_cons_example %>% 
  mutate(
    total_cons = rowSums(.)
           ) %>% 
  head(5)


## 3.2 Row sum with NA filter
df_cons_example %>% 
  replace(is.na(.), 0) %>% # replaces all NA with zero
  mutate(
    total_cons = rowSums(.)
  ) %>% 
  head(5)


## 4. Histogram

## 4.a Save data
df_cons_example = df_cons_example %>% 
  replace(is.na(.), 0) %>% # replaces all NA with zero
  mutate(
    total_cons = rowSums(.)
  )  

## 4.1 Histogram
histogram_df = df_cons_example %>% 
  filter(total_cons<10000) #Remove outlier for example

histogram_df %>% 
  ggplot(aes(x=total_cons))+ geom_histogram(color="darkblue", fill="lightblue")


# 5. Poverty line--Using saved objects

## 5.1 Filter by saved object

povertyline = 300 #saved object

df_cons_example = df_cons_example %>% 
  mutate(pov_dummy = as.numeric(total_cons<povertyline)) 

df_cons_example %>% 
  head(5)


### 5.1.1 Calculate poverty rate with dummy

df_cons_example %>% 
  summarise(
    pov_rate = mean(pov_dummy)
  )

### 5.2 Pov gap
### 5.2.1 Create poverty gap for every observation
df_cons_example %>% 
  filter(pov_dummy==1) %>% 
  mutate(pov_gap = povertyline-total_cons) 


### 5.2.2 Using mean function with NAs
df_cons_example %>% 
  filter(pov_dummy==1) %>% 
  mutate(pov_gap = povertyline-total_cons) %>% 
  summarise(
    pov_gap_calculation = mean(pov_gap, rm.na=TRUE) 
  )


### 5.2.3 Write answer as excel
df_cons_example %>% 
  filter(pov_dummy==1) %>% 
  mutate(pov_gap = povertyline-total_cons) %>% 
  summarise(
    pov_gap_calculation = mean(pov_gap, rm.na=TRUE) ,
    pov_gap_obs = n(),
    pov_rate =  mean(pov_dummy),
  ) %>% write.csv('Answer_cons.csv')








