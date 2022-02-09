# A.1 Install packages

#Step 1
#install.packages('downloader')
#install.packages('foreign')
#install.packages('dplyr')
install.packages('ggplot2')
install.packages('italy')

#Step 2
library(foreign) #Imports dta files
library(dplyr) #Data manipulation
library(downloader) #Downloads files from the internet
library(italy)
library(ggplot2)

## A.2 Data set 
#Step 1
install.packages('italy')
#Step 2
library('italy')

italy_df = italy10


## B.1 Exploratory data analysis

#Show first rows
italy_df  %>% head(4)

#Show columns
italy_df %>% names()

#Summary statistics
italy_df  %>% summary()

#Data types
italy_df  %>% str()

### B.1.1 Change column names to lowercase
names(italy_df) = italy_df %>% names() %>% tolower()

### B.1.2 Recode

italy_df = italy_df %>% 
  rename(
    "birth_year" = "anasc",
    "citizen_status" = 'qual',
    "town_size" = 'studio',
    "quality_life" = 'q',
    'working_status' ='nascreg',
    'education_level' = 'sett'
    )



# 1.Provide a table of means and standard deviations of the following variables  

# 1.1 total household size:
italy_df %>% 
  select(parent) %>% 
  unique()

italy_df %>% 
  select(parent) %>% 
  summary()

### 1.1.1 Groupby count

#For the sake of this example lets assume the following:
#Parent= household, member of house= id
# Note: example is extreme because 7864 people are counted as a child of parent 1
italy_df %>% 
  group_by(parent) %>% 
  count()


### 1.1.2 Groupby count summary gives us summary stats

italy_df %>% 
  group_by(parent) %>% 
  count() %>% 
  summary()


### 1.1.3 Alternative method

italy_df %>% 
  group_by(parent) %>% 
  summarise(members = n()) %>% 
  pull(members) %>% 
  mean()


# 1.2 number of children under 18 in the household: 
# Hint: Filter, Groupby and count functions

# Note: birth_year is year of birth

italy_df %>% 
  filter(birth_year>1990) %>% 
  group_by(parent) %>% 
  summarise(members = n())


# 1.3 proportion of households with a toilet 
# Hint: Use c_cv.dta
# Hint:  Mutate summarise

italy_df %>%
  select(citizen_status) %>% 
  unique()

# Let's say that citizen_status==6 is equal to italian citizen
italy_df %>% 
  mutate(citizen_dummy = as.numeric(citizen_status==6)) %>% 
  summarise(
    citizen_pct = mean(citizen_dummy),
    citizen_sd = mean(citizen_dummy)
  )
  


# 1.4 proportion with a separate room for sleeping
# Hint: Mutate summarise

italy_df %>% 
  mutate(city_dummy = as.numeric(town_size==1)) %>% 
  summarise(
    city_pct = mean(city_dummy),
    city_sd = mean(city_dummy)
  )


# 1.5 and proportion using firewood for cooking
# Hint: ANSWER

# Find cv20_1a in c_cv.dta
df_dwelling %>% 
  mutate(
    firewood_dummy = replace(cv20_1a, is.na(cv20_1a), 0)
  ) %>% 
  summarise(
    #Add summary stats here
  )


# 2.2. Now we will study the characteristics of the (declared) head of the household.  
# Provide a table of means and standard deviations of the following variables for the household heads:

# Find head of household rows (observations) with filter function

## 2.1 average age
# Filter out na then summarise

italy_df %>% 
  filter(citizen_status!='NA') %>% 
  summarise(mean(citizen_status))


## 2.2 proportion male
# Filter out na then summarise

italy_df %>% 
  mutate(male_dummy = recode(sex, "2"=1, "1"=0) )%>% 
  filter(male_dummy!='NA') %>% 
  summarise(mean(male_dummy),
            sd(male_dummy))


## 2.3 average education level
# Filter out na then summarise

italy_df %>% 
  filter(education_level!='NA') %>% 
  summarise(mean(education_level),
            sd(education_level))


## 2.4 proportion who are working and average earnings in the past year.
# Filter out na then summarise

italy_df %>% 
  filter(working_status!='NA' & quality_life!='NA') %>% 
  summarise(mean(working_status),
            mean(quality_life))


# 3. Now we will study the characteristics of children in the household between the ages of 6 and 18. 
## 3.A Filter ages

italy_df %>% 
  filter(birth_year>1990 & birth_year<2010) %>% 
  summarise(
    mean(cit)
  )


## 3.1 Make a graph of the proportion of children who attend school by age and by gender. 

italy_df %>% 
  filter(birth_year>1990 & birth_year<2010) %>% 
  mutate(city_dummy = as.numeric(town_size==1)) %>%
  group_by(city_dummy) %>% 
  summarise(
    city_living = mean(cit)
  ) %>% ggplot(aes(x=city_dummy, y=city_living))+ geom_bar(stat="identity") + 
  xlab('Attendance (%)') + 
  ylab('Age')


## 3.2 Now make a graph of the proportion of children who report working by age and by gender.

### 3.2.1 Age


### 3.2.2 Gender


# 4. In Mexico, many live in extended families for instance where children, parents and grandparents may live within the same household.  
# Using the variable LS05, carry out a short analysis of the different types of households that exist in Mexico. 


# 4.1 What proportion are households are composed only of household head, spouse and children? 


## 4.2 Do most children live in these type of households or in extended families?  Explain your findings. 



