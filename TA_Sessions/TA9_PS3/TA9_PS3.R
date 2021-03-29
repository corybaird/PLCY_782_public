#############################

##  PLCY782, Empirical exercise 3   ##

#############################

##Name: 


## A.1 Import libraries
#Step 1
install.packages('downloader')
install.packages('dplyr')
install.packages('ggplot2')


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


### A.2.3 Download data:
url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_b3a.zip"
#File name
file_name = "mxfls_adults.zip"
# "Downloader" library--function: download.file()
download.file(url, file_name)
unzip("mxfls_adults.zip")


# B.1 Import data
df_labor =  read.dta("hh02dta_bc/c_ls.dta")


# B.2 Import data
df_income = read.dta("hh02dta_b3a/iiia_tb.dta")


# C.1 Rename data

df_labor = df_labor %>% 
  rename('worked_last_12months'='ls12',
         'gender'= 'ls04',
         'age' = 'ls02_2',
  )


df_income = df_income %>% 
  rename('hrs_worked' ='tb44p_2',
         'monthly_income' = 'tb35a_2',
         'activty_lastweek' ='tb02_1')


# C.2 Recode

df_labor = df_labor %>% 
  mutate(
    gender = recode(gender, '3'=0, '1'=1)
  )

# D. Merge data

df_merge = merge(df_income, df_labor, by=c("ls","folio"), all=FALSE)


# 1.1 What is the labor market participation rate of men and women aged 16 to 65?
## 1.1.1 Filter by age
df_16_65 = df_merge  %>% filter(age>15 & age<65)

## 1.1.2 Labor market participation dummy
df_16_65 = df_16_65 %>% 
  mutate(participation_dummy = as.numeric(activty_lastweek==1 | activty_lastweek==2))


## 1.1.2 Calculate
# hint: groupby, summarise

#### CODE BLOCK ####



# 1.2 What are the average and median labor market earnings for women and men 
# -- in the previous month from their main job?

# Method 1
df_16_65 %>% 
  filter(monthly_income!='NA') %>% 
  group_by(gender) %>% 
  summarise(
    income_mean = mean(monthly_income), #'monthly_income' = 'tb35a_2'
    income_median = median(monthly_income)
  )


# Method 2
df_16_65 %>% 
  group_by(gender) %>% 
  replace(is.na(.),0) %>% 
  summarise(
    income_mean = mean(monthly_income), #'monthly_income' = 'tb35a_2'
    income_median = median(monthly_income)
  )



# 1.3 What are average hours worked in the last week for men and women?
### 1.3.1 Inspect data



# 1.4 What is the average income per hour worked for men and women?

### 1.4.1 Calculate hourly wage
df_16_65 = df_16_65 %>% 
  mutate(Monthly_hours = hrs_worked*4.3, #tb44p_2 = hrs_worked
         Hourly_wage  = monthly_income/Monthly_hours # tb35a_2 = monthly_income
  ) 


### 1.4.2 Show answer





# 2.1 Construct levels of schooling for all in the sample using questions Ed06, Ed07 and Ed08.
df_school = read.dta('hh02dta_b3a/iiia_ed.dta')

## 2.1.1 Construct table
school_table = df_school %>% 
  select(ed07_1) %>% 
  table()

school_table

### 2.1.2 Construct table by percentage
school_table_pct = school_table %>% 
  prop.table() %>% 
  round(3)*100

### 2.1.3 Make prettier table
cat_names = c('Did not complete 1st', '1st', '2nd','3rd','4th','5th','6th','7th or more','other','dont know')
school_table_pct %>% 
  as_tibble() %>% #convert to data frame
  mutate(category = cat_names) %>% 
  select(-'.') %>% 
  rename('percent of those answered'='n')


# 2.2 Carry out a regression of earnings per hour as a function of age, education levels and gender. 

### 2.2.1 Construct simple linear regression
regression = lm(Hourly_wage ~ gender, data= df_16_65 ) #y= hourly wage, x=gender
regression

### 2.2.2 Regression table
summary(regression)



### 2.2.2 Construct multiple linear regression
df_16_65 = merge(df_16_65, df_school, by=c('folio','ls'))

regression = lm(Hourly_wage ~ gender+age+ed06, data=df_16_65 ) #y= hourly wage, x=gender
regression

### 2.2.2 Regression table
summary(regression)


df_16_65 %>% select(ed06)
