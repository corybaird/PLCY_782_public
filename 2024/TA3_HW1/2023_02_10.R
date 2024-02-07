# Author: 
# Date: 2023-Feb
# Title: Assignment 1

# A.1 Import Library 
# Step 1
install.packages('dplyr')
# Step 2
library(dplyr)

# A.2 Import Data
df = read.csv('ipumsi_00003.csv')

#Don't run this code! This is simply to make the data smaller 
#df = df %>% filter(AGE>50)

## A.3 Inspect data
# Names without DPLYR
names(df)

# NAMES with dplyr
df %>% 
  names()

# R. Review
## R.1 Select
df_age_school = df %>% 
  select(AGE, SCHOOL) %>% 
  head(5)

df %>% 
  select(SCHOOL) %>% 
  head(2)

## R.2 Filter 
df_gender = df %>% 
  filter(SEX==2) %>% 
  select(SEX, AGE, SCHOOL) %>% 
  head(10)

df %>% 
  filter(AGE>50) %>% 
  select(AGE) %>% 
  head(2)

## R.3 Summarise
df %>% 
  summarise(Avg_SEX = mean(SEX),
            Avg_age = mean(AGE))

## R.4 Groupby
df %>% 
  group_by(SEX) %>% 
  summarise(Avg_Age = mean(AGE))

## R.5 Mutate
df %>% 
  mutate(CORY = AGE + 10) %>% 
  select(AGE, CORY) %>% 
  head(3)

# 1. What proportion of children between the ages of 12 and 18 are enrolled in school? 
## 1.A To Answer we must create a dummy column
### 1.A.1: Step 1 look at unique values and cross check with the codbook
df %>% 
  select(SCHOOL) %>% 
  unique()

### 1.A.2. Step 2: See if the dummy works
df %>% 
  mutate(SCHOOL_DUMMY = as.numeric(SCHOOL==2)) %>% 
  select(SCHOOL_DUMMY, SCHOOL) %>% 
  filter(SCHOOL_DUMMY==1) %>% 
  head(5)

### 1.A.3 Step 3: Save to the dummy to dataframe
df = df %>%  #NOTICE WHEN WE SAVE THE DUMMY WE OVERWRITE THE FILE df = df %>% OPERATION
  mutate(SCHOOL_DUMMY = as.numeric(SCHOOL==1))

# 1.A.4 Example: Calculate mean from dummy
df %>% 
  summarise(
    attend_num = sum(SCHOOL_DUMMY),
    attend_avg = mean(SCHOOL_DUMMY)*100
  )

# 1.1 ANSWER What proportion of children between the ages of 12 and 18 are enrolled in school? 
df %>% 
  filter(
    # TYPE YOUR CODE HERE! You must filter by two conditions
    # You can find the answer to this note: https://github.com/corybaird/PLCY_782_public/blob/main/TA_Sessions/TA2_DPLYR/TA2_DPLYR_Notes.ipynb
  ) %>% 
  group_by(AGE) %>% 
  summarise(
    # TYPE YOUR CODE HERE 
  )


# 1.2 How does the proportion enrolled in school differ by gender?
## 1.2.A Create gender dummy
# Step 1
df %>% 
  select(SEX) %>% 
  unique()

# Step 2 TEST
df %>% 
  mutate(Male_dummy = as.numeric(SEX==1)) %>% 
  select(Male_dummy, SEX) %>% 
  head(5)

# Step 3
df = df %>% 
  mutate(Male_dummy = as.numeric(SEX==1)) #CHECK IN THE CODEBOOK IF 1=MALE

### 1.2.1 ANSWER using groupby and summarise


# 1.3 Proportion enrolled in school differ by urban/rural areas
### 1.3.A Create urban dummy

#YOUR CODE HERE. COPY AND PASTE FROM 1.2 or type your own code


# 2. Repeat 1 without using the weights. 
# Cory will cover this next week 
# 2.1 Proportion of children between the ages of 12 and 18 are enrolled in school? 


# 2.2 Proportion enrolled in school differ by gender


# 2.3 Proportion enrolled in school differ by urban/rural areas




