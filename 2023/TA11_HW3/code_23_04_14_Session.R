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

df %>% 
  select(AGE) %>% 
  summary()

# 1.2 Create cutoffs
seq(0, 65, by = 15)

age_cutoffs= c(seq(0, 65, by = 5))

df %>%  
  mutate(age_groups = cut(AGE, age_cutoffs))%>% 
  select(AGE, age_groups) %>% 
  head(20) 

df = df  %>%  
  mutate(age_groups = cut(AGE, age_cutoffs)) 

## 1.2 Find average employment for age groups
### 1.2.1 Create dummy USE EMP STAT OR OTHER LABOR MARKET COLUMNd
df = df %>% 
  mutate(dummy_employed = as.numeric(EMPSTAT==1))


### 1.2.2 Solution
df %>% 
  group_by(age_groups) %>% 
  summarise(
    mean_emp = mean( #YOUR CODE HERE
                    )
  )


# 2. Construct at least two different definitions of the informal sector to answer the following questions.
## 2.A Examine informal sector column 
# 2.A. In the absence of an informal sector column -> Find Occupation column and then choose the occupations that you think are informal
df %>% 
  count(CLASSWKD) 
  

# 2.B Create dummy: Employment (this is only an example so change out age)
## Method 1
df %>% 
  mutate(
    informal_dummy_1 = as.numeric(CLASSWKD %in% c(120)),
    informal_dummy_2 = as.numeric(CLASSWKD %in% c(120,0)),
  ) %>% 
  filter(CLASSWKD==120) %>% 
  select(CLASSWK, informal_dummy_1, informal_dummy_2) %>% 
  head(5)


## Method 2
df = df %>% 
  mutate(
    informal_dummy_1 = as.numeric(CLASSWKD==120),
    informal_dummy_2 = as.numeric(CLASSWKD==120|CLASSWKD==0)
    )


# 2.C Create dummy: Men versus women
# May require you to filter out nas or non-responses then create dummy
df = df %>% 
  mutate(dummy_male = as.numeric(SEX==1)) 


# 2.1 What proportion of men and women who work are in the informal sector?
### 2.1.1 USE informal dummy number 1
df %>% 
  group_by(
   # YOUR CODE HERE
  ) %>% 
  summarise(
    mean_informal = mean( # YOUR CODE HERE
                          )
  )


### 2.1.2 USE informal dummy number 2
df %>% 
  group_by(
    # YOUR CODE HERE
  ) %>% 
  summarise(
    mean_informal = mean( # YOUR CODE HERE
    )
  )


# 2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?
### 2.2.1 USE informal dummy number 1
df %>% 
  group_by(
    informal_dummy_1,dummy_male
  ) %>% 
  summarise(
    mean_hours = mean(HRSWORK1)
  ) %>% 
  mutate(informal_dummy_1 = recode(informal_dummy_1,'1'='Informal','0'='Formal'),
         dummy_male = recode(dummy_male,'1'='Male','0'='Female'),
         )

### 2.2.2 USE informal dummy number 2




# 2.3 Compare total earnings, average earnings per hour and median earnings per hour in the 
#formal and informal sector for men and women.
### 2.3.1 USE informal dummy number 1
df %>% names()

# REMOVE unwanted data
df = df %>% 
  filter(HRSWORK1!=999)

# 2.3.2 Create hourly wage

df %>% 
  mutate(
    weekly_income = INCWAGE/4.3,
    #avg_wage = weekly/hours_worked_week,
    avg_wage = INCWAGE/HRSWORK1
         ) %>% 
  select(INCWAGE, HRSWORK1, weekly_income, avg_wage) %>% 
  head(5)



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









