#TA11_Research_1


# This notebook covers the basics of data analysis for research

# A.1 Import libraries
# Step 1
install.packages('dplyr')
install.packages('ggplot2')

#Step 2
library(ggplot2)
library(dplyr)


# 1. Regression

# 1. Advanced R
## 1.1 Download: For loop

# folder names
folders = list.dirs('Data')
folders = folders[c(2:length(folders))] #remove data name


merge_df = data.frame()
for (folder in folders){
  print(folder)
  
  data_1 = read.csv("Data/Febrero/\xb5rea - Caracter\xa1sticas generales (Personas).csv", sep=';')
  merge_df = rbind(merge_df, data_1)
}


## 1.2 T-tests: For loop

### 1.2.1 Import data
file_name = "Data/Enero/\xb5rea - Desocupados.csv"
df = read.csv(file_name, 
              sep=';' #This is used when the file is seperated by ";" rather than ","
)


### For loop t-test
df %>% select(P7260,P7250) %>%summary()


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


# 2. Interpreting regressions

## 2.1 Regression: Level - Level
mtcars %>% head(3)

model = lm(mpg~wt+qsec+am, data=mtcars)
model

coefs = model$coefficients
coefs = coefs[(c(2:length(coefs)))]

coefs_num = coefs
coefs_names = names(coefs)

for (i in 1:length(coefs)){
  num = round(coefs_num[i],3)
  name = coefs_names[i]
  output = paste('For a increase of 1 in', name, '---->MPG changes by', num, sep=' ')
  print(output)
}


## 2.2 Regression: Log-level

model = lm(log(mpg)~wt+qsec+am, data=mtcars)
model

coefs = model$coefficients
coefs = coefs[(c(2:length(coefs)))]

coefs_num = coefs
coefs_names = names(coefs)

for (i in 1:length(coefs)){
  num = round(coefs_num[i],3)*100
  name = coefs_names[i]
  output = paste('For a increase of 1 in', name, '---->MPG changes by', num, 'PERCENT', sep=' ')
  print(output)
}




