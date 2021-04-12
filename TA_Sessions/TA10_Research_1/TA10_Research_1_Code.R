#TA10_Research_1


# This notebook covers the basics of data analysis for research

# A.1 Import libraries
# Step 1
install.packages('dplyr')
install.packages('ggplot2')

#Step 2
library(ggplot2)
library(dplyr)

# B.1 Import data
## B.1.1. Show folders in directory
list.dirs('Data')

## B.1.2 Show files in directory
list.files('Data/Enero')

list.files('Data/Enero',  full.names = TRUE)

## B.2 Import data set

file_name = "Data/Enero/\xb5rea - Desocupados.csv"

df = read.csv(file_name, 
              sep=';' #This is used when the file is seperated by ";" rather than ","
              )

## B.3 Data set basics
### B.3.1 Data types
df %>% 
  str()

### B.3.2 Show columns
df %>% 
  names()


# 1. Summary statistics!

df %>% 
  select(P7250, P7260, P7420S7, P7420S7A1,  P7420S8) %>% 
  summary()


# 2. Analysis: Dummies (or variables with few unique obs) 
# USE GROUPBY & SUMMARISE
df_groupby = df %>% 
  group_by(P7420S7) %>% 
  summarise(column_mean = mean(P7260))

df_groupby

# Replace NA
df_groupby = df_groupby %>% replace(is.na(.), 0)
df_groupby


## 2.1 Graph
df_groupby %>% 
  ggplot(aes(y="column_mean", x= "P7420S7")) + geom_bar(stat="identity")



## 2.2 T-tests

df %>% 
  select(P7310) %>%  
  unique()


group1 = df %>% 
  filter(P7310==1) %>% #Filter by group
  pull(P7260) #Variable of interest

group2 = df %>% 
  filter(P7310==2) %>% # Filter by group
  pull(P7260) #Variable of interest


t.test(group1, group2)


# 3. Regression

## 3.1. Pre-processing
## 3.1.1 Graph variables before you regress
df %>% 
  ggplot(aes(x=P7250, y=P7260))+geom_point()

## 3.1.2 Summary stats
df %>% 
  summary()


### 3.2 Regression
### 3.2.1 Simple linear regression

## Regression
linear_reg = lm(P7250 ~ P7260, data=df)
linear_reg

## Summary
summary(linear_reg)


## 3.2.1.1 Print latex
# You can create a latex doc in the could @: https://overleaf.com/

# Install xtable package
install.packages('xtable')
library(xtable)

# print table to console
print(xtable(linear_reg, type = "latex", #file= filename2.tex
             ))


### 3.2.2 Graph residuals
df %>% 
  ggplot(aes(x=linear_reg$residuals, y=P7260))+geom_point()


## 3.2.3 Robust standard errors
install.packages('MASS')
library(MASS)

summary(rlm(P7250 ~ P7260, data=df))


## 3.3 Fixed effects
install.packages('plm')
library(plm)

### 3.3.A Install data
data("Grunfeld", package="plm")
Grunfeld  %>% head(3)


### 3.3.1 Fixed effects: Individual fixed effects
grun.fe <- plm(inv~value+capital, index=c('firm','year'), data = Grunfeld, model = "within")
summary(grun.fe)$coefficients


### 3.3.2 Fixed effects: Individual AND time fixed effects
#Method 1: factor(year)
grun.fe <- plm(inv~value+capital+factor(year), index=c('firm','year'), data = Grunfeld, model = "within")

#Method 2: effect='twoways'
grun.fe <- plm(inv~value+capital, index=c('firm','year'), data = Grunfeld, model = "within", effect='twoways')
summary(grun.fe)$coefficients


### 3.3.3 Standard errors
#### 3.3.3.1 Baseline standard errors
summary(grun.fe)$coefficients


#### 3.3.3.2
coeftest(grun.fe, vcovHC(grun.fe, type = 'HC0', cluster = c('group','time')))


## 3.5 Logistic regression
# 3.5.A data set
mtcars %>% head(2)


## 3.5.1 Logistic regression
summary(glm(am~ cyl+disp+drat+mpg, family='binomial', mtcars))


# 4. Advanced R
## 4.1 Download: For loop

# folder names
folders = list.dirs('Data')
folders = folders[c(2:length(folders))] #remove data name


merge_df = data.frame()
for (folder in folders){
  print(folder)
  
  data_1 = read.csv("Data/Febrero/\xb5rea - Caracter\xa1sticas generales (Personas).csv", sep=';')
  merge_df = rbind(merge_df, data_1)
}


## 4.2 T-tests: For loop

df %>%summary()

for (column_of_interest in c('P7260','P7250')){
  print(column_of_interest)
  
  group1 = df %>% 
    filter(P7310==1) %>% #Filter by group
    pull(column_of_interest) #Variable of interest
  
  group2 = df %>% 
    filter(P7310==2) %>% # Filter by group
    pull(column_of_interest) #Variable of interest
  
  ttest = t.test(group1, group2)
  print(ttest)
}





