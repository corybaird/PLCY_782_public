# poor is a dichotomous (0/1) variable indicating whether the household is below this poverty line or not
# Calculate poverty line ratio by dividing the monthly consumption per capita (copc) by the official poverty line (pcpl).
# The poverty line (pcpl) varies by state and urban/rural residence.

# A. Install Packages
install.packages('haven')
install.packages('dplyr')
library(haven)
library(dplyr)

# I. Read Data
df = read_dta('22626-0002-Data.dta')

# 1. Using the POOR variable, provide the headcount ratio on poverty in India by groups for the following: 
## 1.A.1 Subset data to exclude
df = df %>% 
  filter(POOR!=-4 & POOR!=-6)

### 1.A.1 Total headcount
# Method 1
df %>% 
  summarise(headcount = mean(POOR) * 100)


# 1.1) Caste and religion
# Step 1: Search for column name
df %>% 
  select(GROUPS8) %>% 
  unique()

df %>% 
  group_by(GROUPS8) %>% 
  summarise(
    mean_pov = mean(POOR) * 100
  ) 

### 1.1.1 IF you want to export the data to csv
df %>% 
  group_by(GROUPS8) %>% 
  summarise(
    mean_pov = mean(POOR) * 100
  ) %>% 
  write.csv('answer_1.csv')


# 1.2) Number of children (make groups of 0, 1 to 2, 3 to 4 and 5 or more children)
### 1.2.1 Unique values
df %>% 
  select(NCHILD) %>% 
  unique() 

### 1.2.2 Create cut offs
cutoffs = c(0,2,4,17)
df = df  %>%  
  mutate(nchild_group = cut(NCHILD, cutoffs, include.lowest = TRUE)) 

##### 1.2.2.1 Check whether cutoff worked
df %>% 
  #filter(NCHILD==4) %>% 
  select(nchild_group, NCHILD) %>% 
  head(2)  


### 1.2.3 Answer
df %>% 
  group_by(
    # YOUR CODE HERE
  ) %>% 
  summarise(
    mean_pov = # YOUR CODE HERE
  )



# 1.3) Urban/rural residence.
### 1.3.1 Unique
df %>% 
  select(URBAN)%>% 
  unique() 

### 1.3.2 
df %>% 
  group_by(
    # YOUR CODE HERE
  ) %>% 
  summarise(
    # YOUR CODE HERE
  )


#2. Calculate pov level using the average poverty gap indicator. 
# 2.A How? Compare COPC and PCPL to calculate how far each household is from the poverty line for those households below the poverty line.
## 2.A.1 Create new data set with renamed data
df_gap = df %>% 
  rename('consumption'='COPC', 'pov_line'='PCPL') 

# 2.A.2 Subset columns and poor rows
df_gap = df_gap %>% 
  select(POOR, pov_line, consumption, GROUPS8, NCHILD, nchild_group, URBAN) %>% 
  filter(POOR==1) 

# 2.A.3 Create pov_gap_ratio
df_gap = df_gap %>% 
  mutate(
    pov_gap = # Subtract consumption from pov_gap
    )


### 2.A.4 Answer: Pov Gap Ratio
df_gap %>% 
  summarise(pov_gap = mean(pov_gap))

# 2.1) Caste and religion
# Hint: groupby caste and summarisemean pov_gap_ratio
df_gap %>% 
  group_by(GROUPS8) %>% 
  summarise(
    mean_pov_gap = mean(pov_gap)
  )
  

# 2.2) Number of children (make groups of 0, 1 to 2, 3 to 4 and 5 or more children.) 
df_gap %>% 
  group_by(
    # YOUR CODE HERE
  ) %>% 
  summarise(
    # YOUR CODE HERE
  )

# 2.3) Urban/rural residence.



