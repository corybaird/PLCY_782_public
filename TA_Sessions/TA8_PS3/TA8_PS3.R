# TA8_PS3

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


# 1. Recode variables
## 1.1 Recode to integers
df_labor %>% 
  mutate(ls12 = recode(ls12, "3"=0, "1"=1)
  ) 


## 1.2 Recode to string
df_labor %>% 
  mutate(ls12 = recode(ls04, "1"="Male", "3"='Female')
  ) %>% 
  select(ls12) %>% 
  head(5)


# 2. Filter variables
df_labor %>% 
  filter(
    & #use and symbol to filter by two conditions
  )


# 3. Create cutoffs

## 3.1 seq(start_number, end_number, step)
seq(15, 30, by = 5)

## 3.2 List of cutoff numbers
age_cutoffs= c(seq(15, 30, by = 5), 65)
age_cutoffs

## 3.3 Create new variable
df_labor = df_labor %>%
  mutate(
    age_groups = cut(ls02_2, age_cutoffs, include.lowest = TRUE)
  ) 

### 3.3.1 Check newly created var
df_labor %>% 
  select(ls02_2, age_groups)


# 4. Create dummy using or conditon

df_income %>% 
  select(tb24_26p_cmo) %>% 
  unique()

df_income %>% 
  select(tb24_26p_cmo) %>% 
  mutate(
    profession_dummy = as.numeric(tb24_26p_cmo==83 | tb24_26p_cmo==161)
  ) %>% 
  filter(tb24_26p_cmo==83 )



