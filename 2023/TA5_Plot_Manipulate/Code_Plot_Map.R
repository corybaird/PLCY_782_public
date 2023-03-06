# Link for original code:
#https://github.com/corybaird/SPP_Data_Seminar/blob/main/2022_Spring/R/Session_2/Module_5_GraphMap.ipynb

install.packages('dplyr')
install.packages('ggplot2')
library(dplyr)
library(ggplot2)

url = 'https://raw.githubusercontent.com/corybaird/PLCY_610_public/master/Reference_materials/Tutorials_R_Stata_Python/R/W3_ggplot/global_covid.csv'
df = read.csv(url)

# Example 1
country_list = c('Nepal', 'Nicaragua', 'Mongolia')
df %>% 
  filter(name %in% country_list)%>% 
  ggplot(aes(x=name, y=confirm)) + 
  geom_bar(stat="identity", width=.5, fill='red',color='blue')+  
  xlab('Country') + 
  ylab('Confirmed Covid Cases') +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Example 2
df%>%
  arrange(desc(confirm))%>%
  filter(confirm>10000) %>% 
  ggplot(aes(x=name, y=confirm)) + 
  geom_bar(stat="identity", width=.5, fill='red',color='blue')+  
  xlab('Country') + 
  ylab('Confirmed Covid Cases') +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Example 3
df %>% 
  filter(confirm>5000) %>% 
  ggplot(aes(x=name, y=confirm)) + 
  geom_bar(stat="identity", width=.5, fill='red',color='blue')+  
  xlab('Country') + 
  ylab('Confirmed Covid Cases') +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



# Time Series
url = 'https://raw.githubusercontent.com/corybaird/PLCY_610_public/master/Reference_materials/Tutorials_R_Stata_Python/R/W3_ggplot/Covid_TS_global.csv'
df = read.csv(url)
df %>% head(2)

df %>% str()

as.Date()

# Convert time series to time object
df %>% 
  mutate(date_datetime = as.Date(date)) %>%
  select(date, date_datetime) %>% 
  head(5) 

df = df %>% 
  mutate(date_datetime = as.Date(date)) 

# Plot
japan = df %>% filter(country=='Japan')
japan %>% 
  ggplot(aes(x=date_datetime, 
             y=confirmed)) + 
  geom_line(alpha=0.5)



# Mapping
install.packages('leaflet')
library(leaflet)

#Enter longitude and latitude
long = -76.948270
lat = 38.983640
# Creat map
leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addMarkers(lng = long, lat = lat, popup = 'SPP')

# Import data
url = "https://assets.datacamp.com/production/repositories/1942/datasets/18a000cf70d2fe999c6a6f2b28a7dc9813730e74/ipeds.csv"
ipeds = read.csv(url)
ipeds %>% head(3)

# Add circle markers that color colleges using pal() and the values of sector_label
pal <- colorFactor(palette = c("red", "blue", "#9b4a11"), 
                   levels = c("Public", "Private", "For-Profit"))
map2 <- 
  map %>% 
  addCircleMarkers(data = ipeds, radius = 2, 
                   color = ~pal(sector_label), 
                   label = ~paste0(name, " (", sector_label, ")"))%>% 
  addLegend(pal = pal, 
            values = c("Public", "Private", "For-Profit"))

# Print map2
map2



# Pie Chart
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Basic piechart
ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


# Follow up with resource on adding values directly to bar chart etc


