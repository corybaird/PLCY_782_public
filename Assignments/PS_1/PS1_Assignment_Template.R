# A.1 Install packages

#Step 1
#install.packages('downloader')
#install.packages('foreign')
#install.packages('dplyr')
#install.packages('ggplot2')

#Step 2
library(foreign) #Imports dta files
library(dplyr) #Data manipulation
library(downloader) #Downloads files from the internet
library(ggplot2)

## A.2 Data set 
#URL 
url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip"
#File name
file_name = "mxfls.zip"
# "Downloader" library--function: download.file()
download.file(url, file_name)
unzip("mxfls.zip")
df = read.dta("hh02dta_bc/c_ls.dta")

## B.1 Exploratory data analysis

#Show first rows
df  %>% head(4)

#Show columns
df %>% names()

#Summary statistics
df  %>% summary()

#Data types
df  %>% str()

## B.2 Rename

df_renamed = df %>%  
  rename("Age"= "ls02_2",
         "Attendance" = "ls16",
         "Gender" = "ls04",
         "Household_ID" = 'folio',
         "Individual_ID"= 'ls')

## B.3 Recode


df_renamed = df_renamed  %>% 
  mutate(
    Attendance = recode(Attendance, "3"=0, "1"=1),
    Gender = recode(Gender, "3"=0, "1"=1)
  )


# 1.Provide a table of means and standard deviations of the following variables  

## 1.1 total household size:
# Hint: Groupby and count functions



## 1.2 number of children under 18 in the household: 
# Hint: Filter, groupby and count, summary functions




## 1.3 proportion of households with a toilet ()
# Hint: Groupby and summarise
# Hint: import new data set

# Note: 1 = house has toilet

### 1.3.1 Explore data type



### 1.3.2 Solution: Method 1
# Hint: mutate, filter, summarise



## 1.4 proportion with a separate room for sleeping
# Hint: mutate, filter, summarise

### 1.4.1 Explore data




## 1.5 and proportion using firewood for cooking
# Hint: Mutate and summarise

### 1.5.1 Explore data



### 1.5.2 Method 1: replace function
df_dwelling %>% 
  mutate(
    firewood_dummy = replace(cv20_1a, is.na(cv20_1a), 0)
  ) %>% 
  summarise(
    mean(firewood_dummy),
    sd(firewood_dummy)
  )


### 1.5.3 Method 2: case_when


df_dwelling %>% 
  mutate(firewood_dummy = case_when(cv20_1a==1~1, TRUE~0)) %>% 
  summarise(
    mean(firewood_dummy),
    sd(firewood_dummy)
  )



# 2.2. Now we will study the characteristics of the (declared) head of the household.  
# Provide a table of means and standard deviations of the following variables for the household heads:

## 2.1 average age

# Filter out non head of house


# Hint: filter out na summarise



## 2.2 proportion male
# Hint: filter out na summarise



## 2.3 average education level
# Hint: filter out na summarise


## 2.4 proportion who are working and average earnings in the past year.
# Hint: filter out na summarise



# 3. Now we will study the characteristics of children in the household between the ages of 6 and 18. 
## 3.A Filter ages



## 3.1 Make a graph of the proportion of children who attend school by age and by gender. 
# Hint: groupby, summarise ggplot, geom_bar

### 3.1.1 Attendance by Age


### 3.1.2 Attendance by Gender
# Hint: groupby, summarise ggplot, geom_bar


## 3.2 Now make a graph of the proportion of children who report working by age and by gender.

### 3.2.1 Age
# Hint: groupby, summarise ggplot, geom_bar



### 3.2.2 Gender
# Hint: groupby, summarise ggplot, geom_bar



# 4. In Mexico, many live in extended families for instance where children, parents and grandparents may live within the same household.  
# Using the variable LS05, carry out a short analysis of the different types of households that exist in Mexico. 


# 4.1 What proportion are households are composed only of household head, spouse and children? 
# Hint: groupby, count, mutate



## 4.2 Do most children live in these type of households or in extended families?  Explain your findings. 



