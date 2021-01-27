# TA1_intro_1 

## You should use this tutorial while referencing this notebook which shows both the code and output

## R.2 Running code

print('ALWAYS WRITE YOUR CODE HERE')
print('RUN THE CODE BY HIGHLIGHT THIS LINE OF CODE AND CLICKING THE RUN BUTTON TO THE UPPER RIGHT')
print('THE SHORTCUT ON MAC IS COMMAND,SHIFT')


## R.3 Writing comments

#You can write comments by using the hashtag
#Commented code will not be written
print('The commented code will display in the console but will not be read as code')



# 1. How to save data
## 1.1 Saving text (a.k.a. string objects)

new_string = "This is a string"


### 1.1.1 Now print our object "new_string"

#Use either print or simply type in the name of the object

print(new_string)


new_string


# 2. Import (save) excel data
## 2.1 Read csv file: On your computer

df = read.csv('vote.csv')


### 2.1.1 Common mistake

#Notice there is no = sign as we have in 2.1
read.csv('vote.csv')


## 2.2 Read csv file in sub-folder

df_arrests = read.csv("Sub_folder/arrests.csv")


## 2.3 Read csv file: From github

url = 'https://raw.githubusercontent.com/corybaird/PLCY_610_public/master/Discussion_sections/Disc1_Intro/vote.csv'
df = read.csv(url)


## 3.1 Display: first lines of the dataframe

# Shows the first 3 lines
head(df, 3)


# 3. Basics of the dataframe

## 3.2 Display: column names

names(df)


## 3.3 Display: summary stats

summary(df)


## 3.4 Selecting a column

#Shows mean of age column
mean(df$age)

#Shows standard deviation of age column
sd(df$age)


# 4. DPLYR

## 4.A Install library
# Common mistake to avoid!--Confirm you have installed libraries


#Step 1
#install.packages('dplyr')

#Step 2
library(dplyr)


## 4.B Pipe operator: %>%

df %>% head(2)


## 4.1 Select: Column select
### 4.1.1 Select multiple columns

df  %>% 
  select(vote, income) %>% 
  head(3)


### 4.1.2 Select: advanced conditions
### 4.1.2.1 Select by column name that STARTS with letter or string

df %>% #Dataframe
select(starts_with('v'))%>% #Select column named X
head(2) #Shows only first 2 obs


### 4.1.2.2 Select by column name that ENDS with letter or string

df %>% #Dataframe
select(ends_with('ion'))%>% #Select column named X
head(2) #Shows only first 2 obs


### 4.1.2.3 Select by column name that CONTAINS with letter or string

df %>% #Dataframe
select(contains('c'))%>% #Select column named X
head(2) #Shows only first 2 obs

## 4.2 Filter: Row select
### 4.2.1 Filter by 1 condition

df %>% 
filter(income>6) %>% 
head(3)


### 4.2.2 Filter by 2 conditions

df %>% 
filter(income>6 & income<9) %>% 
head(3)


### 4.2.3 Filter by not equal to condition

df %>% 
filter(vote!=1) %>% 
head(3)


## 4.3 Arrange: Rearrange rows by condition

df %>% 
arrange(age)%>% #Add desc(age) to show descending order
head(5)


## 4.4 Mutate: Create new column
### 4.4.1 Create one column

df  %>% 
mutate(income_log = log(income)) %>% 
head(3)


### 4.4.2 Create multiple columns

df  %>% 
mutate(income_log = log(income),
      income_thousands = income*1000) %>% 
head(3)


## 4.5 Summarise: Summary stats

df %>% 
summarise(
mean_educ = mean(education),
mean_age = mean(age),
median_edu = median(education))


## 4.6 Groupby

df %>% 
group_by(vote) %>% 
summarise(mean_income = mean(income))
