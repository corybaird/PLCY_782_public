#install.packages('haven')
install.packages('dplyr')
#library(haven)
library(dplyr)

# Author: Cory Baird
# Date 03-13-24

# A. Import data
df = read.dta("22626-0002-Data.dta")

# A.1 Columns
df %>% 
  names()

# Using the POOR variable, provide the headcount ratio on poverty in India by groups for the following: 

# Inspect poor
df %>% 
  select(POOR) %>% 
  head(2)

# Remove poor observatsion
df %>% 
  filter(POOR != "Invalid skip" & POOR!= "Out of range") %>% 
  select(POOR) %>% 
  unique()

# Remove non poor
df = df %>% 
  filter(POOR != "Invalid skip" & POOR!= "Out of range")

# Create dummy: Explore
df %>% 
  mutate(dummy_poor = as.numeric(POOR=="Yes")) %>% 
  select(POOR, dummy_poor) %>% 
  head(5)

# Create dummy: Implement
df = df %>% 
  mutate(dummy_poor = as.numeric(POOR=="Yes")) 


# 1. Caste and religion

df %>% 
  select(GROUPS8) %>% 
  unique()

df %>% 
  group_by(GROUPS8) %>% 
  summarise(
    mean_poor = mean(dummy_poor)
  )

1) Caste and religion, 2) Number of children (make groups of 0, 1 to 2, 3 to 4 and 5 or more children) and 3) Urban/rural residence.




