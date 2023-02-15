# Author: 
# Date: 2023-Feb
# Title: Assignment 1

# A.1 Import Library 
# Step 1
install.packages('dplyr')
install.packages('srvyr')
install.packages('ggplot2')
# Step 2
library(dplyr) # Manipulation data
library(srvyr) # Weighted averages
library(ggplot2)
# A.2 Import Data
df = read.csv('ipumsi_00003.csv')

################IMPORTANT CODE##########################
# The assignment focuses on ages of 12 and 18 
# As a result we need to filter out all ages other than 12~18
# You can find the answer to this note: https://github.com/corybaird/PLCY_782_public/blob/main/TA_Sessions/TA2_DPLYR/TA2_DPLYR_Notes.ipynb
df =  df %>% 
  filter(
    AGE>20 & AGE<25 #YOU MUST CHANGE THE AGE INEQUALITIES TO filter for 12-18 year old
  ) 

# Note that if you code works you should see the number of obs. drop
# The number of obs. (which are simply rows) can be seen in the top right quadrant of r-studio 

## A.3 Inspect data
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
df_ans_11 = df %>% 
  group_by(AGE) %>% 
  summarise(
    School_mean = mean(SCHOOL_DUMMY) 
  )

## 1.G Graph
df_ans_11 %>% 
  ggplot(aes(x=AGE, y=School_mean)) + 
  geom_bar(stat = 'identity') + 
  xlab('Age') + 
  ylab('Attendance\n(%)') +
  ggtitle('Look mom I can scatter plot')+ 
  theme(plot.title = element_text(hjust = 0.5)) #title

# 1.2 How does the proportion enrolled in school differ by gender?
## 1.2.A Dummy
df = df %>% 
  mutate(Male_dummy = as.numeric(SEX==1)) #CHECK IN THE CODEBOOK IF 1=MALE

### 1.2.1 ANSWER using groupby and summarise
df_ans_12 = df %>% 
  group_by(Male_dummy) %>% 
  summarise(
    School_mean = mean(SCHOOL_DUMMY) 
  )

## 1.2.G Graph
df_ans_12 %>% 
  ggplot(aes(x=Male_dummy, y=School_mean)) + 
  geom_bar(stat = 'identity') + 
  xlab('YOURCODEHERE') + 
  ylab('Attendance\n(%)') + 
  coord_cartesian(ylim = c(0.76, 0.78))


# 1.3 Proportion enrolled in school differ by urban/rural areas
### 1.3.A Create urban dummy
df = df %>%  #NOTICE WHEN WE SAVE THE DUMMY WE OVERWRITE THE FILE df = df %>% OPERATION
  mutate(Urban_dummy = as.numeric(URBAN==1))

### 1.3.1 Answer
df_ans_13 = df %>% 
  group_by(Urban_dummy) %>% 
  summarise(
    School_mean = mean(SCHOOL_DUMMY) 
  )
df_ans_13
# 1.3.G Graph
df_ans_13 %>% 
  ggplot(aes(x=Urban_dummy, y=School_mean)) + 
  geom_bar(stat = 'identity') + 
  xlab('YOURCODEHERE') + 
  ylab('Attendance\n(%)') 


# 2. Repeat 1 without using the weights. 
# Cory will cover this next week 
# 2.1 Proportion of children between the ages of 12 and 18 are enrolled in school? 
df %>% 
  as_survey(weights = HHWT) %>%
  group_by(AGE) %>% 
  summarise(
    School_mean = mean(SCHOOL_DUMMY) 
  )%>% head(2)

# 2.2 Proportion enrolled in school differ by gender


### YOUR CODE HERE ###

# 2.3 Proportion enrolled in school differ by urban/rural areas

### YOUR CODE HERE ###

# 3. What proportion of children are working outside the home? 
df %>% 
  names()

# 3.1 Provide evidence on whether children who work outside the home less likely to attend school.   
### 3.1.A Create WORK DUMMY

### YOUR CODE HERE######

### 3.1.1 Answer
df_ans_31 = df %>% 
  group_by(emp_dummy) %>% 
  summarise(
    School_mean = mean(SCHOOL_DUMMY) 
  )

df_ans_31 %>% 
  ggplot(aes(x=emp_dummy, y=School_mean)) + 
  geom_bar(stat = 'identity') + 
  xlab('YOURCODEHERE') + 
  ylab('Attendance\n(%)') 
  

# 3.2 Compare this evidence for boys and girls and by urban/rural areas.
## 3.2.1 Boys and girls
df_ans_32 = df %>% 
  group_by(Male_dummy, emp_dummy) %>% 
  summarise(
    School_mean = mean(SCHOOL_DUMMY) 
  )

### 3.2.2
df_ans_32 %>% 
  ggplot(aes(x=Male_dummy, y=School_mean))+
  geom_bar(stat="identity") + 
  facet_grid(. ~emp_dummy) 

## 3.2.2 Urban vs. Rural
df %>% 
  group_by(Male_dummy, emp_dummy) %>% 
  summarise(
    School_mean = mean(SCHOOL_DUMMY) 
  )






