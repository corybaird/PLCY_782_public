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

DATA = read.dta("hh02dta_bc/c_ls.dta")


## B.1 Problem: "Could not find function “%>%”"

df_renamed = df %>%  
  rename("Age"= "ls02_2",
         "Attendance" = "ls16",
         "Gender" = "ls04",
         "Household_ID" = 'folio',
         "Individual_ID"= 'ls')

# 1. Troubleshooting in R

## 1.1 PROBLEM: Column 'NAME' is not found

df_renamed %>% 
  names()

df %>% 
  names()

df %>% 
  select(ls)

## 1.2 PROBLEM: "error in filter()
df_age6 = df_renamed %>% 
  filter(Age6) 

## 1.3 PROBLEM: object 'No such file or directory'
df_dwelling = read.dta('hh02dta_bc/c_cv.dta')


## 1.4 Problem: Problem: Nas
df_renamed %>% 
  select(Age) %>% 
  filter(Age!='NA') %>%  
  summarise(Age = mean(Age))


## 1.5 Incorrect )) () 
df_renamed %>% 
  select(Age) %>% 
  filter(Age!='NA') %>% 
  summarise(Age_mean = mean(Age)
  )


            








