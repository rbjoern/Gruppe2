# ASSIGNMENT 1, GROUp 2:
library("readr")
library("lubridate")
library("stringr")
library("ggplot2")
library("dplyr")
library("maps")

df <- read_csv("https://raw.githubusercontent.com/MuseumofModernArt/collection/master/Artworks.csv")

df.paintings <- subset(df, Classification == "Painting")


#QUESTION 7
# Extract nationality from ArtistBio
df.paintings$Demonym.1 <- str_extract(df.paintings$ArtistBio, "[A-z]*, ")
df.paintings$Demonym.1 <- gsub(", ", "", df.paintings$Demonym.1)

#Data: Count paintings for each nationality
df.country <- tally(group_by(df.paintings, Demonym.1))

# We seek aid from a list of countries based on nationalities 
countrylist <- read.csv("http://t2a.co/blog/wp-content/uploads/2014/03/Countries-List.csv")

# ... and merge this data with our own to gain country names 
df.countrycoded <- merge(df.country, countrylist, by = "Demonym.1" )
df.countrycoded <- subset(df.countrycoded, df.countrycoded$Demonym.1 != "") # We ignore the less welldefined ones
df.countrycoded <- subset(df.countrycoded, df.countrycoded$Name != "Congo (Republic of)") # We do not wish to count the Congolese gentlemen for both Congos

#We lose twenty paintings without welldefined countries, which is deemed acceptable 
sum(df.country$n) # 2229
sum(df.countrycoded$n) # 229

#We now begin construction of the map!
map <- map_data("world") 

#First, we find out how well the country names in our data fit the maps package
map.1 <- left_join(df.countrycoded, map, by = c("Name" = "region")) 

subset(map.1, is.na(map.1$long)) # Oh no! We cant find three of our countries

#Manual correction (sorry)
df.countrycoded$Name <- gsub("United States of America", "USA", df.countrycoded$Name)
df.countrycoded$Name <- gsub("United Kingdom", "UK", df.countrycoded$Name)
df.countrycoded$Name <- gsub("Russian Federation", "Russia", df.countrycoded$Name)

#Recheck
map.1 <- left_join(df.countrycoded, map, by = c("Name" = "region")) 
subset(map.1, is.na(map.1$long)) # Found them!

#We use a right join instead, so as to make countries without paintings grey rather than removing them. 
#Now, they merely have NA in the n variable
map.2 <- right_join(df.countrycoded, map, by = c("Name" = "region")) 

#We then construct the world map
p = ggplot(map.2, aes(x = long, y = lat, group = group, fill = n))
p + geom_polygon() +
  expand_limits(x = map.2$long, y = map.2$lat)