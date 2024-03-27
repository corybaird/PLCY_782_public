#install.packages('haven')
install.packages('dplyr')
install.packages('ggplot2')
#library(haven)
library(ggplot2)
library(dplyr)

# Author: YOURNAME
# Date 03-13-24

# A. Import data
df = read.dta("22626-0002-Data.dta")

# B. Create poor dummy
# B.1 Remove NA
df = df %>% 
  filter(POOR != "Invalid skip" & POOR!= "Out of range")

# B.2 Create dummy
df = df %>% 
  mutate(dummy_poor = as.numeric(POOR=="Yes")) 

# 1. Using the POOR variable, provide the headcount ratio on poverty in India by groups for the following: 
# 1.1 Caste and religion
df %>% 
  group_by(GROUPS8) %>% 
  summarise(
    mean_poor = mean(dummy_poor) * 100
  )

# How to present the data in your assignment
# Option 1: CSV TO EXCEL
df %>% 
  group_by(GROUPS8) %>% 
  summarise(
    mean_poor = mean(dummy_poor) * 100
  ) %>% 
  write.csv('FILE.csv')

# Option 2: ggplot2
df %>% 
  group_by(GROUPS8) %>% 
  summarise(
    mean_poor = mean(dummy_poor) * 100
  ) %>% ggplot(aes(x=GROUPS8, y = mean_poor)) + 
  geom_bar(stat = 'identity') 


# 1.2 Number of children (make groups of 0, 1 to 2, 3 to 4 and 5 or more children) 
df %>% 
  select(NCHILD) %>% 
  unique()

df = df %>%
  mutate(NCHILD = ifelse(NCHILD <= 4, NCHILD, 5)
  ) 

df %>% 
  select(NCHILD) %>% 
  unique()

df %>% 
  group_by(NCHILD) %>% 
  summarise()


# 1.3 Urban/rural residence.
# REFER TO ASSIGNMENT 1

#2. Now, calculate the level of poverty using the average poverty gap indicator. 
#To do this, you will have to compare COPC and PCPL to calculate how far each household is from the poverty line for those households below the poverty line.
df %>% mutate(pov_ratio = COPC / PCPL) %>% 
  filter(pov_ratio<1) %>% 
  select(pov_ratio, COPC, PCPL) %>% 
  head(10)

df_poor = df %>% mutate(pov_line_ratio = COPC / PCPL) %>% 
  filter(pov_line_ratio <1)


df_poor %>% 
  select(pov_line_ratio) %>% 
  summary()

# 3. Now provide the average poverty gap measure of poverty by groups for the following: 
# 3.1 Caste and religion
df_poor %>% 
  group_by() %>% 
  summarise(pov_line_ratio)


# 3.2 Number of children (make groups of 0, 1 to 2, 3 to 4 and 5 or more children.) 


# 3.3 Urban/rural residence.

