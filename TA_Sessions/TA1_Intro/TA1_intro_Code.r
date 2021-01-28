#You can write comments by using the hashtag
#Commented code will not be written

print('The commented code will display in the console but will not be read as code')

new_string = "This is a string"

#Use either print or simply type in the name of the object
print(new_string)

new_string

df = read.csv('vote.csv')

#Notice there is no = sign as we have in 2.1
read.csv('vote.csv')

df_arrests = read.csv("Sub_folder/arrests.csv")

url = 'https://raw.githubusercontent.com/corybaird/PLCY_610_public/master/Discussion_sections/Disc1_Intro/vote.csv'
df = read.csv(url)

# Shows the first 3 lines
head(df, 3)

names(df)

summary(df)

#Shows mean of age column
mean(df$age)

#Shows standard deviation of age column
sd(df$age)
