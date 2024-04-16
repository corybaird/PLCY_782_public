#Author: YOURNAME
# Date 03-13-24

# A. Import data
df = read.csv('ipumsi_00008 (1).csv')

# A.1 Truncate/subset data
df = df %>% filter(
  # YOUR CODE HERE
)

# B. Create dummies
## B.1 Labor
# Explore
df %>% 
  select(LABFORCE) %>% 
  unique()

# Create
df = df %>% 
  mutate(dummy_work = as.numeric(LABFORCE==2)
  )

## B.2 Age buckets
cutoffs = c(seq(15, 70, by = 5))
df %>% 
  mutate(
    age_bucket = cut(AGE, cutoffs, include.lowest=TRUE, include.highest=FALSE)
  ) %>% 
  select(age_bucket, AGE) %>% 
  head(5)


# 1.1 What is the labor market participation of men and women aged 16 to 65? 
df %>% 
  group_by(
    # YOUR CODE HERE
           ) %>% 
  summarise(
    mean(
      # YOUR CODE HERE
         )
  )

# 1.2 Provide labor market participation rates by 5 year age ranges for men and women.
df %>% 
  group_by(SEX, age_bucket) %>% 
  summarise(
    mean(dummy_work)
  )

# 2. Construct at least two different definitions of the informal sector to answer the following questions.
## 2.A # Create informal dummy
df %>% 
  select(EMPSECT) %>% 
  unique()
  
df %>% mutate(
    dummy_inform = as.numeric(EMPSECT == 20 | EMPSECT == 0)
  ) %>% 
  select(
    dummy_inform, EMPSECT
  ) %>% 
  head(10)

emp_list = c(20, 0)
df %>% mutate(
  dummy_inform = as.numeric(EMPSECT %in% emp_list)
) %>% 
  select(
    dummy_inform, EMPSECT
  ) %>% 
  head(10)



# 2.1 What proportion of men and women who work are in the informal sector?



# 2.2 Compare total earnings, average earnings per hour and median earnings per hour in the formal and informal sector for men and women.



# 2.3 Compare total earnings, average earnings per hour and median earnings per hour in the formal and informal sector for men and women.









