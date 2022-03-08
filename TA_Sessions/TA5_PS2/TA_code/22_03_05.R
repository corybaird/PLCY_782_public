
# Method 1: Only necessary 1 time
install.packages('downloader')
install.packages('dplyr')
install.packages('ggplot2')

# Method 2
#Step 2
library(foreign) #Imports dta files
library(dplyr) #Data manipulation
library(ggplot2)

## A.2 
#URL 
url_cons = "http://www.ennvih-mxfls.org/english/assets/hh02dta_b1.zip"
#File name
file_name = "mxfls_cons.zip"
# "Downloader" library--function: download.file()
download.file(url_cons, file_name)
#Unzip file
unzip("mxfls_cons.zip")


df_cons = read.dta("hh02dta_b1/i_cs.dta")

## 1.A 
df_cons %>% 
  names()


df_cons  %>% 
  select(contains('cs15')) %>% 
  names()

df_cons  %>% 
  select(ends_with('32')) %>% 
  names()

df_cons  %>% 
  select(starts_with('cs14')) %>% 
  names()

df_cons  %>% 
  select(ends_with('32') & contains('cs04')) %>% 
  names()

# 2. Multiply
df_cons %>% 
  select(cs16h_2:cs18_2) %>% 
  head(5)

df_cons %>% 
  names()

# 2.2 Select multiple columns and mutate them

### Select: select columns
### Mutate: create a new column/ overwrite a column
df_cons %>% 
  mutate(cs16h_2 = cs16h_2*100,
         csnew = cs10a*10) %>% 
  select(cs16h_2, csnew) %>% 
  head(6)


df_monthly = df_cons %>% 
  mutate_at(vars(cs16h_2:cs18_2), ~ . * 4.3) %>% 
  select(cs16h_2:cs18_2)


df_cons_example = df_cons %>% 
  select(cs16h_2,cs16i_2,cs16f_2) 

df_cons_example %>% 
  head(5)

df_cons_example = df_cons_example %>% 
  mutate(
    total_cons = rowSums(.)
  ) 

df_cons_example = df_cons_example %>% 
  replace(is.na(.), 0)

df_cons_example = df_cons_example %>% 
  mutate(
    total_cons = rowSums(.)
  ) 



histogram_df = df_cons_example %>% 
  filter(total_cons<10000)

histogram_df %>% 
  ggplot(aes(x=total_cons)) + geom_histogram(color="darkblue", bins=30, fill="lightblue")

# Poverty line

povertyline = 300 #saved object
povertyline

df_cons_example = df_cons_example %>% 
  mutate(pov_dummy = as.numeric(total_cons<=povertyline)) 

df_cons_example %>% 
  group_by(pov_dummy) %>% 
  summarise(mean_consum = mean(total_cons))

## Poverty rate
df_cons_example %>% 
  summarise(pov_rate = mean(pov_dummy)*100)

## Poverty gap
df_cons_example %>% 
  filter(pov_dummy==1) %>% 
  mutate(pov_gap = povertyline - total_cons) %>% 
  select(total_cons, pov_gap) %>% 
  head(10)


df_cons_example %>% 
  filter(pov_dummy==1) %>% 
  mutate(pov_gap = povertyline-total_cons) %>% 
  summarise(
    pov_gap_calculation = mean(pov_gap, rm.na=TRUE) 
  )

df_cons_example %>% 
  filter(pov_dummy==1) %>% 
  mutate(pov_gap = povertyline-total_cons) %>% 
  summarise(
    pov_gap_calculation = mean(pov_gap, rm.na=TRUE) ,
    pov_gap_obs = n(),
    pov_rate =  mean(pov_dummy),
  ) %>% 
  write.csv('my_data.csv')

df_cons_example %>% 
  write.csv('consumption.csv')



