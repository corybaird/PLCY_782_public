# Review session 1

# Save comments
print('Run code')

save_dataname = "this is a string"



# R.1.1 Read csv file from github
url = 'https://raw.githubusercontent.com/corybaird/PLCY_610_public/master/Discussion_sections/Disc1_Intro/vote.csv'

df = read.csv(url)


## R.1.2
head(df, 5)


## R.1.3
names(df)


## R.1.4
summary(df)


## R.1.5
mean(df$age)


# DPLYR

#Step 1
#install.packages('dplyr')

#Step 2
library(dplyr)


## 1.B
head(df, 2)

df %>% 
  head(2)


# 1. Select: Column select
## 1.1 
df %>% 
  select(vote, income) 

## 1.2 Select
### 1.2.1
df %>% 
  select(starts_with('v')) %>% 
  head(5)


### 1.2.2
df %>% 
  select(ends_with('ion'))%>% 
  head(2) #Shows only first 2 obs


### 1.2.3 
df %>% 
  select(contains('c'))%>% 
  head(2) 


# 2. Filter
## 2.1 
df %>% 
  filter(education>3)

## 2.2
df %>% 
  filter(income>6 & income<9) %>% 
  head(5)


## 2.3
df %>% 
  filter(vote!=1) %>% 
  head(3)


#3. Arrange
## 3.1 Ascending

df %>% 
  arrange(vote)

## 3.2 Descending

df_sorted = df %>% 
  arrange(desc(age))


# 4. Mutate

## 4.1 Create column

df = df  %>% 
  mutate(
    Income_log = log(income)
    )

## 4.2

df  %>% 
  mutate(income_log = log(income),
         income_thousands = income*1000) %>% 
  head(3)


#5. Summarise
df %>% 
  summarise(
    mean_educ = mean(education),
    mean_age = mean(age),
    median_edu = median(education)
    )


#6. Groupby

df %>% 
  group_by(education) %>% 
  summarise(
    MEAN_INCOME = mean(income)
  )








