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

df %>% 
  select(AGE) %>% 
  unique() %>% 
  arrange(AGE)

# How do we filter out unwanted obs?

# 1.1 Create cutoffs
age_cutoffs= c(seq(0, 100, by = 5))

df  %>%  
  mutate(age_groups = cut(AGE, age_cutoffs, include.lowest = TRUE, include.highest=FALSE)) %>% 
  select(AGE, age_groups) %>%
  filter(AGE==5) %>% 
  head(10)

## 1.2 Find average employment for age groups


# 2. Construct at least two different definitions of the informal sector to answer the following questions.
## 2.A Examine informal sector 
df %>% 
  select()

# 2.B Create dummy
df %>% 
  mutate(
    # CREATE DUMMY HERE
  )

#
# 2.1 What proportion of men and women who work are in the informal sector?



# 2.2 What are the average hours worked of men and women in the informal sector versus the formal sector?



# 2.3 Compare total earnings, average earnings per hour and median earnings per hour in the formal and informal sector for men and women.



# 2.4 Are there occupations where earnings in the informal sector are close to or higher than the formal sector?


