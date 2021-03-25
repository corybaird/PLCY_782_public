#############################

##  PLCY782, Empirical exercise 3   ##

#############################

##Name: 


## A.1 Import libraries
#Step 1
install.packages('downloader')
install.packages('dplyr')
install.packages('ggplot2')


#Step 2
library(foreign) #Imports dta files
library(dplyr) #Data manipulation
library(ggplot2)


## A.2 Download data 

### A.2.1 Data set: http://www.ennvih-mxfls.org/english/ennvih-1.html
#URL 
url_cons = "http://www.ennvih-mxfls.org/english/assets/hh02dta_b1.zip"
#File name
file_name = "mxfls_cons.zip"
# "Downloader" library--function: download.file()

download.file(url_cons, file_name)
#Unzip file
unzip("mxfls_cons.zip")


### A.2.2 Download data:
url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip"
#File name
file_name = "mxfls.zip"
# "Downloader" library--function: download.file()
download.file(url, file_name)
unzip("mxfls.zip")


### A.2.3 Download data:
url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_b3a.zip"
#File name
file_name = "mxfls_adults.zip"
# "Downloader" library--function: download.file()
download.file(url, file_name)
unzip("mxfls_adults.zip")


# B.1 Import data
df_labor =  read.dta("hh02dta_bc/c_ls.dta")


# B.2 Import data
df_income = read.dta("hh02dta_b3a/iiia_tb.dta")



# 1.1 What is the labor market participation rate of men and women aged 16 to 65?



# 1.2 What are the average and median labor market earnings for women and men in the previous month from their main job?
  


# 1.3 What are average hours worked in the last week for men and women?



# 1.4 What is the average income per hour worked for men and women?



# 2.1 Construct levels of schooling for all in the sample using questions Ed06, Ed07 and Ed08.



# 2.2 Carry out a regression of earnings per hour as a function of age, education levels and gender. 



# 2.2.1 What do your results reveal about differences in earnings between men and women?
  
  
  
  
  
  
  