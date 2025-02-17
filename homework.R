#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10 (10/10)

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
install.packages("tidyverse")

library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")
  
### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER

View(ds)
glimpse(ds$Year)

ds$Year <- as.numeric(ds$Year)  #this gave me the output: "NAs introduced by coercion," but there were no NAs when I viewed the data. I'm not sure why I got the warning message if it didn't actually happen.
View(ds)
str(ds)

typeof(ds$Year)

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER

ds <- ds %>% rename_with(tolower)
View(ds)

### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER

ds <- ds %>% mutate(decade = floor(year / 10) * 10)
View(ds)

### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER

ds <- ds %>% arrange(rank)
View(ds)

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER

top10 <- ds %>% filter(rank < 11) %>% select(song, artist)
View(top10)

### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER

ds_sum <- ds %>% summarize(earliest = min(year, na.rm = T), most_recent = max(year, na.rm = T), mean = mean(year, na.rm = T))

print(ds_sum)
View(ds_sum)

### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

#ANSWER
ds_sum_artist_song <- ds %>% filter(year == 1879 | year == 2020 | year == 1980) %>% arrange(year)
View(ds_sum_artist_song)
  
#Mcomment: Looks good! You can also reference the variable (rather than hard code the year)

ds %>% filter(year == round(ds_sum$min_yr) | 
                year == round(ds_sum$mean_yr) | 
                year == round(ds_sum$max_yr) ) %>% arrange(year)

### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER

#fixing the error to 1979
ds_sum_artist_song <- ds %>% mutate(year = ifelse(year == 1879, 1979, year))
View(ds_sum_artist_song)

#Mcomment: Referencing year is risky (what if there was more than 1 1879), you can also reference something else (like song name)
ds  <- ds %>% mutate(year = ifelse(song == "Brass in Pocket", 1979, year)

#fixing decade
ds_sum_artist_song <- ds_sum_artist_song %>% mutate(decade = floor(year / 10) * 10)

#recalculating responses from question 6
ds_sum <- ds_sum_artist_song %>% summarize(earliest = min(year, na.rm = T), most_recent = max(year, na.rm = T), mean = mean(year, na.rm = T))
View(ds_sum)

#recalculating responses from question 7
ds_sum_artist_song_corrected <- ds_sum_artist_song %>% filter(year == 1937 | year == 2020 | year == 1980) %>% arrange(year)
View(ds_sum_artist_song_corrected)


### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER
ds %>% group_by(decade, na.rm = T) %>% summarize(avg_rank = mean(rank), num_songs = n())

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

#ANSWER
ds %>% count(decade) %>% slice_max(order_by = n, n = 1)
  
