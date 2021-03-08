#############################

##  PLCY782, Empirical exercise 2   ##

#############################

#You should use this template to complete the problem set

##Name: 


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
# Show columns
df_cons %>% names()
# Rename
df_cons = df_cons %>% rename('Household_ID'='folio')

#df_cons is used to create cons1 and cons 2 dataframes


# B.1. Select weekly variables

## B.1.1 Find the data
# Create new cons1 dataframe

cons1 = df_cons %>% 
  select(Household_ID, contains('cs1') & ends_with('_2')
         )


## B.1.2 Multiply the data by 4.3
cons1 =  cons1 %>% 
  mutate_at(vars(cs16a_2:cs18_2),  ~ . *4.3) %>% 
  head(5



# B.2. Select monthly consumption data

cons2 = df_cons %>% 
  select(
    #HOUSE_ID, SELECT MONTHLY COLUMNS
  )


# B.3. Select 3 month data

df_cons_3month = read.dta('hh02dta_b1/i_cs1.dta')
df_cons_3month = df_cons_3month%>% rename('Household_ID'= 'folio')


## B.3.1 Select consumption data

cons3 = df_cons_3month %>% 
  select(
    #HOUSE_ID, SELECT MONTHLY COLUMNS
  )


## B.3.2 Divide data by 3
cons3 %>% 
  mutate_at(vars(cs16a_2:cs18_2),  ~ . /3) 


# B.4 Merge consumption variables into one dataframe
## B.4.1 Merge consum by house id

df_cons_merge = merge(cons1, cons2, by='Household_ID')

## B.4.2 Merge last conusmption to cons1 and cons2 merged data

df_cons_merge = merge(df_cons_merge, cons3, by='Household_ID')


# 1. Calculate a measure of total consumption and per capita consumption for each household in the 2002 round. 
## Note:To calculate per capita note you will have to calculate the number of individuals in each household.

## 1.1 Total consumption
# Hint: This provides you the answer if you did the previous steps correctly

df_cons_merge = df_cons_merge %>% 
  mutate(
    total_cons =  select(df_cons_merge,-Household_ID) %>% # selects all rows other than househould ID
      replace(is.na(.), 0) %>% # replaces na with 0
      rowSums(.) # sums the rows
  )


# 1.2 Total consumption per capita
### 1.2.1 Access family member data
# We must import data from assignment 1

df = read.dta("hh02dta_bc/c_ls.dta")
df = df %>%  
  rename("Age"= "ls02_2",
         "Attendance" = "ls16",
         "Gender" = "ls04",
         "Household_ID" = 'folio',
         "Individual_ID"= 'ls')


### 1.2.2 Create new data frame with counts of family members
## Hint use groupby and count as we did for the 

df_familymembers = DFNAME %>% 
  group_by(GROUPBYCOLUMN) %>% 
  count()


## 1.3 Merge family member data with consumption data

df_cons_merge = merge(df_cons_merge, df_familymembers, by='Household_ID')


### 1.3.1 Create per capita column
# Hint: use mutate to divide total consumption by family members

df_cons_merge %>% 
  mutate(
    percapita = CONSUM/FAMILYMEMBERS
  )


# 2. Calculate the set of poverty rates nationwide using the FGT indicators of poverty
#Assume the poverty line=600 pesos per person. 
# Hint: see section 5.1 of the following note https://github.com/corybaird/PLCY_782_public/blob/main/TA_Sessions/TA6_PS2/TA_6_PS2_Notes.ipynb

# Set poverty line
povertyline = 600

## 2.1 Head count

df_cons_merge %>% 
  mutate(pov_dummy = as.numeric(CONSUM<povertyline)) %>% 
  summarise(mean(pov_dummy))


## 2.2 Poverty gap

df_cons_merge %>% 
  filter(pov_dummy==1) %>% 
  mutate(poverty_gap = povertyline-CONSUM) %>% 
  summarise(mean(poverty_gap))


## 2.3 Poverty gap squared

df_cons_merge %>% 
  filter(pov_dummy==1) %>% 
  mutate(poverty_gap = (povertyline-CONSUM)^2) %>% 
  summarise(mean(poverty_gap))



# 3.1 Report poverty by area of residence. 
#3.1.1 Import residence data from "c_portad"¶

residence_df = read.dta('hh02dta_bc/c_portad.dta')
residence_df = residence_df%>% rename('Household_id' = 'folio')


## 3.1.2 Merge with consumption data

df_cons_merge = merge(df_cons_merge,residence_df)


### 3.1.3 How does the poverty rate change by rural/urban residence?
# Hint: Groupby estrato and calculate poverty rate

df_cons_merge %>% 
  group_by(estrato) %>% 
  filter() %>% 
  mutate() %>% 
  summarise() 



###   What can you say about the severity of poverty in rural versus urban areas?




