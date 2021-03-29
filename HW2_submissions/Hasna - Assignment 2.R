install.packages('downloader') #Imports dta files
install.packages('dplyr') #Data manipulation
install.packages('ggplot2') #Plotting 

library(foreign) 
library(dplyr) 
library(ggplot2) 


url = "http://www.ennvih-mxfls.org/english/assets/hh02dta_bc.zip"
file_name = "mxfls.zip"
download.file(url, file_name)
unzip("mxfls.zip")

url_cons = "http://www.ennvih-mxfls.org/english/assets/hh02dta_b1.zip"
file_name = "mxfls_cons.zip"
download.file(url_cons, file_name)
unzip("mxfls_cons.zip")


#Import file into R
df_cons = read.dta("hh02dta_b1/i_cs.dta")

# Show columns
df_cons %>% names()


#Select weekly consumption data
cons1 = df_cons %>% 
  select(folio, contains('cs0') & ends_with('2'))


##Multiply the data by 4.3 to turn into monthly from weekly 
cons1 =  cons1 %>% 
  mutate_at(vars(cs02a_12:cs06_2),  ~ . *4.3)

       
#Select monthly consumption data
       
cons2 = df_cons %>% 
  select(folio, contains('cs1') & ends_with('2'))
       
       
#Select 3 month consumption dataset from the i_cs1 file 
       
df_cons_3m = read.dta('hh02dta_b1/i_cs1.dta')
       

#Select 3 consumption data columns 
cons3 = df_cons_3m %>% 
  select(folio, contains('cs22') & ends_with('2'), contains('cs23') & ends_with('2'), 
         contains('cs24') & ends_with('2'), contains('cs25') & ends_with('2'),
         contains('cs26') & ends_with('2'))

#Divide data by 3 to turn monthly 
cons3 = cons3 %>% 
  mutate_at(vars(cs22a_2:cs26_2),  ~ . /3)

#Select yearly consumption data
cons4 = df_cons_3m  %>%
  select(folio, contains('cs27') & ends_with('2'), contains('cs28') & ends_with('2'),
         contains('cs29') & ends_with('2'), contains('cs30') & ends_with('2'),
         contains('cs31') & ends_with('2'), contains('cs32') & ends_with('2'),
         contains('cs33') & ends_with('2'), contains('cs34') & ends_with('2'),
         contains('cs35') & ends_with('2'))

##Divide the data by 12 to turn into monthly from yearly 
cons4 =  cons4 %>% 
  mutate_at(vars(cs27a_2:cs35a_32),  ~ . /12)

#Select monthly transportation consumption
cons5 = df_cons_3m  %>%
  select(folio, contains('cs36') & ends_with('2'))

       
#Merge consumption variables into one dataframe
#Merge  by folio 
       
df_cons_merge = merge(cons1, cons2, by='folio')
df_cons_merge = merge(df_cons_merge, cons3, by='folio')
df_cons_merge = merge(df_cons_merge, cons4, by='folio')
df_cons_merge = merge(df_cons_merge, cons5, by='folio')


#1.1 Total consumption

df_cons_merge = df_cons_merge %>% 
  mutate(total_cons =  select(df_cons_merge,-folio) %>% #- means do not select 
      replace(is.na(.), 0) %>% # replaces na with 0
      rowSums(.))  # sums the rows


#1.2 Total consumption per capita/ look at previous data to find # of hh members 
df = read.dta("hh02dta_bc/c_ls.dta")

df_familymembers = df  %>% 
  group_by(folio) %>% 
  count()

#Merge family member data with consumption data
df_cons_merge = merge(df_cons_merge, df_familymembers, by='folio')

df_cons_merge = df_cons_merge %>% 
  mutate(percapita_cons = total_cons/n)

#2 
povertyline = 600 

#2.1 head count 
df_cons_merge %>% 
  mutate(pov_dummy = as.numeric(percapita_cons < povertyline)) %>% 
  summarise(mean(pov_dummy))

#2.2 average poverty gap
df_cons_merge %>% 
  mutate(pov_dummy = as.numeric(percapita_cons < povertyline)) %>%
  filter(pov_dummy==1) %>% 
  mutate(poverty_gap = povertyline - percapita_cons) %>% 
  summarise(mean(poverty_gap))

#2.3 average poverty gap squared
            
df_cons_merge %>% 
  mutate(pov_dummy = as.numeric(percapita_cons < povertyline)) %>%
  filter(pov_dummy==1) %>% 
  mutate(poverty_gap = (povertyline-percapita_cons)^2) %>% 
  summarise(mean(poverty_gap))

#3 poverty by residence

residence_df = read.dta('hh02dta_bc/c_portad.dta')
df_cons_merge = merge(df_cons_merge, residence_df, by='folio')


#head count 
df_cons_merge %>%
  group_by(estrato) %>% 
  mutate(pov_rate = as.numeric(percapita_cons < povertyline))%>% 
  summarise(mean(pov_rate))

#average poverty gap
df_cons_merge %>% 
  group_by(estrato) %>% 
  mutate(poverty_gap = povertyline-percapita_cons) %>% 
  summarise(mean(poverty_gap))

#average poverty gap squared
df_cons_merge %>% 
  group_by(estrato) %>% 
  mutate(poverty_gap=(povertyline-percapita_cons)^2) %>% 
  summarise(mean(poverty_gap))