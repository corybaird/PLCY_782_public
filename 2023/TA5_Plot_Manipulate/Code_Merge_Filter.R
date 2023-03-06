# Link for original code:
#https://github.com/corybaird/SPP_Data_Seminar/blob/main/2022_Spring/R/Session_2/Module_6_ManipulateModel.ipynb

# Install packages
# Step 1
install.packages('WDI')
install.packages('dplyr')

# Step 2
library(WDI)
library(dplyr)

# Import Data
df_wdi = WDI(
  country = "all",
  indicator = c("NY.GDP.PCAP.KD", #GDP
                "SP.POP.DPND" # Age dependency
  ),
  start = 1980,
  end = 2020,
)

# Renaming
df_wdi = df_wdi %>% 
  rename('GDP' = 'NY.GDP.PCAP.KD',
         'Age_dep_ratio' = 'SP.POP.DPND')

# Select Function
df_wdi = df_wdi %>% 
  select(country, year, GDP, Age_dep_ratio)

# Data types
df_wdi %>% 
  str()

# Check for nas
df_wdi %>% 
  is.na() %>% 
  any()

#  Filter
df_wdi_2020 = df_wdi %>% filter(year== 2020)
df_wdi_2020 %>% tail(2)

# Create dummy
df_wdi_2020 = df_wdi_2020 %>% 
  mutate(high_inc = as.numeric(GDP>15000)) 

# 1.2.2 Omit na
df_wdi_2020 = df_wdi_2020 %>% 
  filter(!is.na(GDP))

# 1.3  Recode
df_wdi_2020 = df_wdi_2020  %>% 
  mutate(highinc_dummy_factor = recode(high_inc, '0'='Low', '1'='High'))

# 1.4 Cut-off dummies
cutoffs = c(seq(0, 60000, by = 10000))
cutoffs

df_wdi_2020 = df_wdi_2020  %>%  
  mutate(cut_variable = cut(df_wdi_2020$GDP, cutoffs, include.lowest=TRUE))

# Cut off dummy summary stats
df_wdi_2020 %>% 
  group_by(cut_variable) %>% 
  summarise(mean_gdp = mean(GDP))

# Filter by multiple inputs
country_list = c('Albania', 'Italy', 'France', 'Belgium')
df_wdi_2020  %>% filter(country %in% country_list)


# 2. Merging

## 2.1 Example 1
install.packages('gapminder')
library(gapminder)
gapminder %>% head(2)

gapminder_2007 = gapminder %>% filter(year==2007)

df_1 = gapminder_2007[1:3, c('year','lifeExp')]
df_2 = gapminder_2007[5:7, c('continent','country')]
cbind(df_1, df_2)

## 2.2 Example 1
# Import case data
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
nyt_cases_df = read.csv(url)
nyt_cases_df  %>% head(2)

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv'
nyt_mask_df = read.csv(url)
nyt_mask_df %>% head(2)

# Rename 
nyt_mask_df = nyt_mask_df %>% rename('fips' = 'COUNTYFP' )
nyt_mask_df %>% names()

df_merge = merge(nyt_cases_df, nyt_mask_df, by='fips')



