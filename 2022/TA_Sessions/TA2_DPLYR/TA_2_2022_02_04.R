

url = 'https://raw.githubusercontent.com/corybaird/PLCY_610_public/master/Discussion_sections/Disc1_Intro/vote.csv'
df = read.csv(url)

# Step 1
#install.packages('dplyr')
# Step 2
library(dplyr)


# DPLYR versus non-dplyr
#df$state

df %>% 
  pull(state)

df %>% 
  select(state)

head(df, 2)

# 1.1 Select multiple columns
df %>% names()
  
df %>% 
  select(income, education) %>% 
  head(10)

# 1.2.1 Select by column name that STARTS with letter or string

df %>% 
  select(starts_with('v')) %>% 
  head(5)

df %>% #Dataframe
  select(ends_with('ion')) %>% 
  head(5)

# 1.3 Using and:& and or:|
### 1.3.1 and: &
df %>% 
  select(starts_with('v') & ends_with('ion')) %>% 
  head(4)

### 1.3.2 or: |
df %>% 
  select(starts_with('v') | ends_with('ion')) %>% 
  head(4)


# 2.1 Filter by 1 condition
df %>% 
  select(income) %>% 
  unique()

### 2.1.1. equal to
df %>% 
  filter(income==6) %>% 
  head(5)
# Greater than
df %>% 
  filter(income>6)  %>% 
  head(5)
# Less than
df %>% 
  filter(income<6)  %>% 
  head(5)

# Greater or equal
df %>% 
  filter(income<=6) 

# Not equal
df_nonvoters = df %>% 
  filter(vote!=1) 

# 3. Arrange
df = df %>% 
  arrange(desc(age))

# 4.1 Create one column
df %>% 
  mutate(income_plusone = income + 1) %>% 
  select(income, income_plusone) %>% 
  head(10)

df_log = df  %>% 
  mutate(income_log = log(income)) %>% 
  select(income, income_log) %>% head(5)

#4.2 Create multiple columns
df = df  %>% 
  mutate(income_log = log(income),
         income_thousands = income*1000,
         income_plusone = income + 1)


df %>% names()

# Summary stats

df %>% 
  summarise(
    mean_edu = mean(education),
    mean_age = mean(age),
    median_age = median(age),
    sd_age = sd(age)
  )


df %>% 
  group_by(education)  %>% 
  summarise(
    mean_age = mean(age),
    mean_vote = mean(vote)
  )



df %>% 
  group_by(vote)  %>% 
  summarise(
    mean_education = mean(education),
  )

# Mean of education
df %>% 
  summarise(
    mean_edu = mean(education),
  )

df %>% 
  group_by(income) %>% 
  summarise(
    mean_edu = mean(education),
  )






# Export csv
df_vote %>% 
  write.csv('vote.csv')

df_nonvoters %>% 
  write.csv('non_vote.csv')



