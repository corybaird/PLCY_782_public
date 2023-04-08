# Empirical Exercise 3 
# Due date: Wed. April 21st, 2023 Midnight
install.packages('dplyr')
library(dplyr)

# A. Import data
file_name = "ipumsi_00006.csv.gz"
df = read.csv(file_name)

# B. 
df %>% 
  str()

# 1. What is the labor market participation of men and women aged 16 to 65? 
# Provide labor market participation rates by 5 year age ranges for men and women.
## 1.A Inspect age
df %>% 
  select(AGE) %>% 
  summary()

# 1.1 Subset data
df = df %>% 
  filter(AGE > 15 & AGE<66)


# 1.2 Create cutoffs
age_cutoffs= c(seq(0, 65, by = 5))

df = df  %>%  
  mutate(age_groups = cut(AGE, age_cutoffs)) %>% 
  select(AGE, age_groups) %>%
  filter(AGE<25 & AGE>20) %>% 
  head(20) 

df = df  %>%  
  mutate(age_groups = cut(AGE, age_cutoffs)) 

## 1.2 Find average employment for age groups
df %>% 
  groupby(age_groups) %>% 
  summarise(
    mean_emp = mean()
  )



# 2. Construct at least two different definitions of the informal sector to answer the following questions.
## 2.A Examine informal sector column 
df %>% 
  select()

# 2.A. In the absence of an informal sector column -> Find Occupation column and then choose the occupations that you think are informal
df %>% 
  select(OCCUPATION COLUMN) %>% 
  unique()

# 2.B Create dummy: Employment (this is only an example so change out age)
## Method 1
df %>% 
  mutate(
    informal_dummy = as.numeric(OCCUPATION_COLUMN %in% c(200,301,305))
  ) 

## Method 2
df %>% 
  mutate(
    informal_dummy = as.numeric(OCCUPATION_COLUMN==200 | OCCUPATION_COLUMN==301 | OCCUPATION_COLUMN==305),
    informal_dummy_2 = as.numeric(OCCUPATION_COLUMN==200 | OCCUPATION_COLUMN==301)
  ) 


# Example of dummy on Age
df %>% 
  mutate(
    age_dummies = as.numeric(AGE==20 | AGE==21 | )
  ) 


df %>% 
  mutate(
   age_20_dummy = as.numeric(AGE==20)
  ) %>% 
  select(AGE, age_20_dummy) %>% 
  filter(AGE ==20) %>% 
  head(10)

# 2.C Create dummy: Men versus women
# May require you to filter out nas or non-responses then create dummy

#
# 2.1 What proportion of men and women who work are in the informal sector?
### 2.1.1 USE informal dummy number 1
df %>% 
  group_by(
    # MEN OR WOMEN DUMMY
  ) %>% 
  summarise(
    mean_informal = mean(informal_dummy)
  )


### 2.1.2 USE informal dummy number 2


# 2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?
### 2.2.1 USE informal dummy number 1
df %>% 
  groupby(
    informal_dummy
  ) %>% 
  summarise(
    mean_hours = 
  )

### 2.2.2 USE informal dummy number 2



# 2.3 Compare total earnings, average earnings per hour and median earnings per hour in the 
#formal and informal sector for men and women.
### 2.3.1 USE informal dummy number 1

df %>% 
  groupby(
    informal_dummy, male_dummy, 
  ) %>% 
  summarise(
    mean_earnings = mean(EARNINGS),
    median_earnings = median(EARNINGS),
    sum_earnings = sum(EARNINGS)
  )

### 2.3.2 USE informal dummy number 2


# Example of 
df %>% 
  group_by(SEX, MARST) %>% 
  summarise(mean_age = mean(AGE))

df %>% 
  select(MARST) %>% 
  unique()

# 2.4 Are there occupations where earnings in the informal sector are close to or higher than the formal sector?
# THis question is answered above. Look at summarise function which shows breakdown of earnings




