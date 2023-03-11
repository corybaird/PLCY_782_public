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
## 1.A.1 Show unique values
df %>% 
  select(POOR) %>% 
  unique()

## 1.A.2 Subset data to exclude
df = df %>% 
  filter(POOR!=-4 & POOR!=-6)

### 1.A.3 Total headcount
# Method 1
df %>% 
  summarise(headcount = mean(POOR))

# Method 2
mean(df$POOR)

# Method 3
df %>% 
  pull(POOR) %>% 
  mean()


# 1.1) Caste and religion
# Step 1: Search for column name
df %>% 
  select(contains('r',ignore.case=TRUE)) %>% 
  names()

# 1.2) Number of children (make groups of 0, 1 to 2, 3 to 4 and 5 or more children)
# Step 1: Search for column name
df %>% 
  select(contains('child',ignore.case=TRUE)) %>% 
  names()
# Step 2: Unique values
df %>% 
  select(NCHILD) %>% 
  unique() 
# Step 3 Create new column with cutoffs
cutoffs = c(0,2,4,17)
cutoffs

# Show new column
df  %>%  
  mutate(nchild_group = cut(NCHILD, cutoffs)) %>% 
  select(nchild_group) %>% 
  unique()
  #filter(NCHILD==1) %>% 
  #head(4)

# 1.2.1 # Answer hint: Use the new nchild_group and poor dummy in a summarise
# Compare to Dummy
df  %>%  
  mutate(nchild_group = cut(NCHILD, cutoffs),
         nchild_overtwo = as.numeric(NCHILD>=2)
         ) %>% 
  #group_by(NCHILD) %>% 
  group_by(nchild_overtwo) %>% 
  summarise(
    mean(SWEIGHT)
  )
  

# 1.3) Urban/rural residence.
df %>% 
  select(URBAN)%>% 
  unique() 


#2. Calculate pov level using the average poverty gap indicator. 
# 2.A How? Compare COPC and PCPL to calculate how far each household is from the poverty line for those households below the poverty line.
## 2.A.1 Create new data set with renamed data
df_gap = df %>% 
  rename('consumption'='COPC', 'pov_line'='PCPL') 

# 2.A.2 Subset columns and poor rows
df_gap = df_gap %>% 
  select(POOR, pov_line, consumption) %>% 
  filter(POOR==1) 

# 2.A.3 Create pov_gap_ratio
df_gap = df_gap %>% 
  mutate(
    pov_gap = pov_line - consumption,
    pov_gap_ratio = (pov_line - consumption) / pov_line,
  )

### 2.A.4 Answer: Pov Gap Ratio
df_gap %>% 
  summarise(pov_gap = mean(pov_gap),
            pov_gap_ratio = mean(pov_gap_ratio))

# 2.1) Caste and religion

# Hint: groupby caste and summarisemean pov_gap_ratio


# 2.2) Number of children (make groups of 0, 1 to 2, 3 to 4 and 5 or more children.) 


# 2.3) Urban/rural residence.



