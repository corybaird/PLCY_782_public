# A.1 Install packages

#Step 1
#install.packages('downloader')
#install.packages('foreign')
#install.packages('dplyr')
#install.packages('ggplot2')

#Step 2
library(foreign) #Imports dta files
library(dplyr) #Data manipulation
library(downloader) #Downloads files from the internet
library(ggplot2)

## A.2 Data set 
#URL 
url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip"
#File name
file_name = "mxfls.zip"
# "Downloader" library--function: download.file()
download.file(url, file_name)
unzip("mxfls.zip")
df = read.dta("hh02dta_bc/c_ls.dta")


## B.1 Problem: "Could not find function “%>%”"

df_renamed = df %>%  
  rename("Age"= "ls02_2",
         "Attendance" = "ls16",
         "Gender" = "ls04",
         "Household_ID" = 'folio',
         "Individual_ID"= 'ls')

## B.2 Recode

df_renamed = df_renamed  %>% 
  mutate(
    Attendance = recode(Attendance, "3"=0, "1"=1),
    Gender = recode(Gender, "3"=0, "1"=1)
  )


# 1. Troubleshooting in R

## 1.1 PROBLEM: Column 'NAME' is not found
# PS1: Answer

#Method 1
df_renamed %>% 
  group_by(Household_ID) %>% #Groupby house
  count() %>% #Counts each member in each house
  summary() #Shows summary stats

#Method 2
df_renamed %>% 
  group_by(Household_ID) %>% #Groupby house
  summarise(members = n()) %>% 
  mutate(members_mean = mean(members),
         members_sd = sd(members)) 


#Method 3
df_renamed %>% 
  group_by(Household_ID) %>% #Groupby house
  count() %>% #counts
  pull(n) %>% #pull is like select. selects n column
  sd() #or use mean() instead of sd


## 1.2 PROBLEM: "Problem with function()"
# Method 1 
df_renamed %>% 
  filter(Age<18) %>% 
  group_by(Household_ID) %>% 
  count() %>% 
  pull(n) %>% 
  mean()


## 1.3 PROBLEM: object 'NAME OF DATA' not found
df_dwelling = read.dta('hh02dta_bc/c_cv.dta')

### 1.3.2 Solution: Method 1

df_dwelling  %>% 
  mutate(toliet_dummy = as.numeric(cv16==1)) %>% 
  filter(toliet_dummy!='NA') %>% 
  summarise(
    mean_toliet = mean(toliet_dummy),
    sd_toliet = sd(toliet_dummy)
  )


## 1.4 Problem: incorrect spacing, pipes, etc

### 1.4.1 Explore data

df_dwelling %>% 
  select(cv07) %>% 
  unique() 
  
### 1.4.2 Answer
  
df_dwelling %>% 
  select(cv07) %>% 
  mutate(no_sleeping_dummy = as.numeric(cv07==0)
         ) %>% 
  summarise(
    mean = 1- mean(no_sleeping_dummy),
    sd(no_sleeping_dummy)*100
  )


## 1.5 Problem: break down function into pieces

### 1.5.1 Explore data

df_dwelling %>% 
  select(cv20_1a) %>% 
  table %>% 
  sort(decreasing = TRUE)


### 1.5.2 Method 1: replace function
df_dwelling %>% 
  mutate(
    firewood_dummy = replace(cv20_1a, is.na(cv20_1a), 0)
  ) %>% 
  summarise(
    mean(firewood_dummy),
    sd(firewood_dummy)
  )


### 1.5.3 Method 2: case_when

df_dwelling %>% 
  mutate(firewood_dummy = case_when(cv20_1a==1~1, TRUE~0)) %>% 
  summarise(
    mean(firewood_dummy),
    sd(firewood_dummy)
  )



# 2.2.  Problem: does not filter NA
## 2.1 average age

# Filter out non head of house
df_head = df_renamed %>% filter(ls05_1==1)

# Hint: filter out na summarise
df_head %>% 
  filter(Age!= "NA") %>% 
  summarise(
    mean(Age),
    sd(Age)
  )


## 2.2 Problem: Missing pipe operator
df_head %>% names()

df_head %>% 
  filter(Gender!= "NA") %>% 
  summarise(
    mean(Gender),
    sd(Gender)
  )


# Omit 2.3. & 2.4


# 3. Now we will study the characteristics of children in the household between the ages of 6 and 18. 

df_ages = df_renamed%>% 
  filter(Age>5 & Age<19)


## 3.1 Break down by each line
### 3.1.1 
df_ages %>% 
  filter(Attendance!="NA") %>%
  group_by(Age) %>%
  summarise(
    Avg_attend = mean(Attendance)) %>% 
  ggplot(aes(x=Age, y=Avg_attend))+geom_bar(stat="identity")
  

### 3.1.2 Omitted


## 3.2 Problem: Improper recode
### 3.2.1 

df_ages %>% 
  rename("work_dummy"= "ls12") %>% 
  filter(work_dummy!= "NA") %>% 
  mutate(work_dummy = recode(work_dummy, "3"=2, "1"=1)) %>% 
  group_by(Age) %>% 
  summarise(
    Avg_work = mean(work_dummy)
  ) %>% ggplot(aes(x=Age, y= Avg_work)) + 
  geom_bar(stat="identity") + 
  xlab('Attendance (%)') + 
  ylab('Age') 


### 3.2.2 Omitted



# 4. Problem: code is too complicated
# 4.1 What proportion are households are composed only of household head, spouse and children? 
### 4.1.1  Answer
df_renamed%>% 
  rename('relatheadhh'='ls05_1') %>% 
  filter(relatheadhh!="NA")%>%
  mutate(relatheadhh2=as.numeric(relatheadhh>4))%>%
  group_by(Household_ID)%>%
  summarise(extfamilies = max(relatheadhh2)) %>% 
  filter(extfamilies==1) %>% 
  summarise(pct = 1-(n()/total_houses))


# 4.1.2 Answer
df_renamed%>% 
  rename('relatheadhh'='ls05_1') %>% 
  filter(relatheadhh!="NA")%>%
  mutate(relatheadhh2=as.numeric(relatheadhh>4))%>%
  group_by(Household_ID)%>%
  summarise(extfamilies = max(relatheadhh2)) %>% 
  filter(extfamilies==1) %>% 
  summarise(pct = n()/total_houses)



