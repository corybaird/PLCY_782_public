# # TA3_PS1_Code

# A.1 Download and import libraries

#Step 1
#install.packages('downloader')
#install.packages('foreign')
#install.packages('dplyr')


#Step 2
library(foreign) #Imports dta files
library(dplyr) #Data manipulation
library(downloader) #Downloads files from the internet



# B.1.MXFLS data

## B.1.1 Download data from the following website 
# Website url: http://www.ennvih-mxfls.org/english/ennvih-1.html
### B.1.1.1 Save strings of website and the file_name which you choose

#URL 
url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip"
#File name
file_name = "mxfls.zip"


### B.1.1.2 Download data
# "Downloader" library--function: download.file()
download.file(url, file_name)



### B.1.1.3 Show files in zip folder

unzip("mxfls.zip", list = TRUE)


### B.1.1.4 Unzip folder
unzip("mxfls.zip")


# B.2 Import data
#read.dta: foreign library
df = read.dta("hh02dta_bc/c_ls.dta")


df %>% 
head(5)


# 1. Functions necessary to complete PS1

## 1.1 Rename

### 1.1.1 Rename: without saving

# This function will not save changes

df %>%  
  rename("Age"= "ls02_2",
         "Attendance" = "ls16",
         "Gender" = "ls04")  %>%  head(3)


### 1.1.2 Rename: save

df_renamed = df %>%  
  rename("Age"= "ls02_2",
         "Attendance" = "ls16",
         "Gender" = "ls04",
        "Household_ID" = 'folio',
        "Individual_ID"= 'ls') 

#Check
df_renamed %>%  
select(Age, Attendance, Gender, Household_ID,Individual_ID)%>%  
head(5)



## 1.2 Filter: Drop rows based on condition
# - Remember filter is used in TA session 2 [here](https://nbviewer.jupyter.org/github/corybaird/PLCY_782_public/blob/main/TA_Sessions/TA2_DPLYR/TA2_DPLYR_Notes.ipynb)

#df %>%  
#filter(Age & Age) #FILL in yourself


# 2.1.Inspect data

## 2.1.1 Summary stats

df_renamed %>% 
  select(Attendance, Gender, Age) %>% 
  summary()


#### 2.1.1.1 Unique values
#

df_renamed %>% 
  select(Attendance) %>% 
  unique()


## 2.2 Drop na
## 2.2.1 Dropna by column

df_renamed %>%
  filter(Attendance!= "NA" ) %>%  count()


## 2.2.1 Dropna na by multiple cols

df_renamed %>%
  filter(Attendance!= "NA" & Age!="NA") %>% count() 


df_renamed_dropna = df_renamed %>%
  filter(Attendance!= "NA" & Age!="NA")


### 2.2.1.1 Unique values--NA should now be removed

df_renamed_dropna%>% 
  select(Attendance) %>% 
  unique()


# 3. Recode

df_renamed_dropna%>% 
  select(Attendance, Gender, Age) %>% 
  summary()


# 3.1 Recode

df_renamed_dropna = df_renamed_dropna %>% 
  mutate(
    Attendance = recode(Attendance, "3"=0, "1"=1),
    Gender = recode(Gender, "3"=0, "1"=1)
         )


# 3.2 Check recode

df_renamed_dropna%>% 
  select(Gender) %>% 
  unique()



## 3.3 Groupby and summarise (YOUR ANSWER)
### 3.3.1 Groupby and summarise to find answer

df_renamed_dropna %>%
  filter(Age<6)%>% 
  group_by(Age) %>% 
  summarise(
    Attend_pct = mean(Attendance)
  )




df_renamed_dropna %>% 
filter(Age<6)%>% 
group_by(Age)%>% summarise(
    Total_students = n(),
    
    Attend = sum(Attendance),
    
    Attend_pct = (Attend/Total_students)*100,
    
    Non_attend = Total_students-Attend,
)



### 3.3.2 Table function

df_renamed_dropna%>% 
filter(Age<6)%>% 
  select(Age, Attendance) %>% 
  table() 



# 4. Merge data
## 4.1 Add new data set

url = 'https://github.com/corybaird/PLCY_782_public/blob/main/TA_Sessions/TA3_PS1/hh02dta_b1/i_cs.dta?raw=true'

df_consum = read.dta(url)

df_consum %>% head(3)

df_consum %>% names()

df_renamed_dropna %>% names()

## 4.2 Rename column
##### Data SETS MUST HAVE OVERLAPPING COLUMN WITH THE SAME DATA AND SAME NAME

df_consum %>% 
  rename("Household_ID" = 'folio') %>%head(3)


df_consum = df_consum %>% 
  rename("Household_ID" = 'folio') 


## 4.3 Merge data

df_merge = merge(df_consum, df_renamed_dropna, by='Household_ID')

df_merge%>%head(3)

df_merge %>% names()


# 5. Graphing
## 5.1 Data

df_renamed_dropna %>% 
  filter(Age>3 & Age<8)%>% 
  group_by(Age)%>% 
  summarise(
    Attend = mean(Attendance)*100
    )

#Save data
barplot_data = df_renamed_dropna %>% 
  filter(Age>3 & Age<8)%>% 
  group_by(Age)%>% summarise(
    Attend = mean(Attendance)*100)


## 5.2 ggplot2
#install.packages('ggplot2')

library(ggplot2)


barplot_data %>% 
  ggplot(
    aes(x= Age, y=Attend)
         ) + geom_bar(stat="identity") 


## 5.3 Add colors, titles, etc

barplot_data%>% 
  ggplot(
    aes(x= Age, y=Attend)
  ) + geom_bar(stat="identity", width=.5, fill='red',color='blue') +
  xlab('') + #xlabel
  ylab('Attendance (%)') + #ylabel
  ggtitle('Look mom I can scatter plot') + #Title
  theme(plot.title = element_text(hjust = 0.5)) + #Centers the title
  theme(axis.text.x=element_text(angle=45, hjust=1)) # Tilts x-axis











