library("readr")
library ("dplyr")
library("stringr")
library("ggplot2")
library("lubridate")
library("maps")

df = read_csv("https://raw.githubusercontent.com/MuseumofModernArt/collection/master/Artworks.csv")

# ____ Question 1 ____
# Create a new dataframe of the stock of paintings at
# MOMA for each month in the year.
df$month = months(df$DateAcquired)
Paintings <- data.frame(subset(df, Classification == "Painting"))
Paintings_month <- data.frame(table (Paintings$Classification, Paintings$month))
Paintings_month$Var1 <- NULL

#____ Question 2 ____
# Use ggplot2 and your new data frame
# to plot the the stock of paintings on the y-axis and the date on the x-axis.
#     what kind of geom do you think is appropriate? why?
#     color the geom you have chosen red
#     add a title and custom axis labels

Paintings_month <- transform(Paintings_month, Var2 = reorder(Var2, Freq))
p = ggplot(data = Paintings_month, aes(x = Var2 , y = Freq))
p + geom_point(fill="red", colour="red") + ggtitle("Stock of paintings at MOMA") + labs(x="Month", y="Stock of pantings")

#____ Question 3 ____
# Create the same plot but this time the color should reflect the stock of paintings for
# curator approved and non-curator approved paintings, respectively
DataQ3 <- data.frame(table (Paintings$Classification, Paintings$month, Paintings$CuratorApproved))
DataQ3$Var1 <- NULL
DataQ3 <- transform(DataQ3, Var2 = reorder(Var2, Freq))
colnames(DataQ3)[colnames(DataQ3)=="Var3"] <- "CuratorApproved"
p = ggplot(DataQ3, aes(x=Var2, y=Freq, colour = CuratorApproved))
p + geom_point(stat = "identity") + coord_flip()
p + geom_point(fill="blue") + ggtitle("Stock of paintings at MOMA") + labs(x="Month", y="Stock of pantings")


#____ Question 4 ____
# Create a new data frame of the stock of paintings grouped by what department the painting belongs to.
Department_sum<- data.frame(count(Paintings, Department, wt=NULL, sort=FALSE))


#____ Question 5 ____
#Plot this data frame using ggplot2. Which department has had
#the highest increase in their stock of paintings?

# Plotting & flipping
p = ggplot(Department_sum, aes(x = Department, y = n))
p + geom_bar(stat = "identity") + coord_flip()

# Solution: As seen on Barplot the department with the highest increase in their stock of paintings is 
# Paintings And sculptures. 


# ____ Question 6 ____
# Write a piece of code that counts the number of paintings by each artist in the data set. 
# List the 10 painters with the highest number of paintings in MOMAs collection.

Artists<- data.frame(count(Paintings, Artist, wt=NULL, sort=T))

# Changing Missing name to Unknown Artist"
Artists[Artists == ""] <- "Unknown Artist"

# Listing the 10 artists with the highest number of paintings. 
head(Artists,n=10) 

# Solution: As seen in the console the artist with the highest number of paintings is Picasso, followed by Henri Matisse.

# ____ Question 7 ____
# The variable ArtistBio lists the birth place of each painter. 
# Use this information to create a world map where each country is colored according
# to the stock of paintings in MOMAs collection.'

df.paintings <- subset(df, Classification == "Painting")
# Extract nationality from ArtistBio
df.paintings$Demonym.1 <- str_extract(df.paintings$ArtistBio, "[A-z]*, ")
df.paintings$Demonym.1 <- gsub(", ", "", df.paintings$Demonym.1)

# Data: Count paintings for each nationality
df.country <- tally(group_by(df.paintings, Demonym.1))

# We seek aid from a list of countries based on nationalities 
countrylist <- read.csv("http://t2a.co/blog/wp-content/uploads/2014/03/Countries-List.csv")

# ... and merge this data with our own to gain country names 
df.countrycoded <- merge(df.country, countrylist, by = "Demonym.1" )
df.countrycoded <- subset(df.countrycoded, df.countrycoded$Demonym.1 != "") # We ignore the less welldefined ones
df.countrycoded <- subset(df.countrycoded, df.countrycoded$Name != "Congo (Republic of)") # We do not wish to count the Congolese gentlemen for both Congos

# We lose twenty paintings without welldefined countries, which is deemed acceptable 
sum(df.country$n) # 2229
sum(df.countrycoded$n) # 229

# We now begin construction of the map!
map <- map_data("world") 

# First, we find out how well the country names in our data fit the maps package
map.1 <- left_join(df.countrycoded, map, by = c("Name" = "region")) 

subset(map.1, is.na(map.1$long)) # Oh no! We cant find three of our countries

# Manual correction (sorry)
df.countrycoded$Name <- gsub("United States of America", "USA", df.countrycoded$Name)
df.countrycoded$Name <- gsub("United Kingdom", "UK", df.countrycoded$Name)
df.countrycoded$Name <- gsub("Russian Federation", "Russia", df.countrycoded$Name)

# Recheck
map.1 <- left_join(df.countrycoded, map, by = c("Name" = "region")) 
subset(map.1, is.na(map.1$long)) # Found them!

# We use a right join instead, so as to make countries without paintings grey rather than removing them. 
# Now, they merely have NA in the n variable
map.2 <- right_join(df.countrycoded, map, by = c("Name" = "region")) 

# We then construct the world map
p = ggplot(map.2, aes(x = long, y = lat, group = group, fill = n))
p + geom_polygon() +
  expand_limits(x = map.2$long, y = map.2$lat)

# ____ Question 8 ____
# The Dimensions variable lists the dimensions of each painting.
# Use your data manipulation skills to calculate the area of each painting (in cm).
# Create a data frame of the five largest and five smallest paintings in MOMAs collection.

# Creating new variabel which include the text in parenthesis
Paintings$cm = gsub(".*\\((.*)\\).*", "\\1", Paintings$Dimensions)

# Taking the height fromt the cm variable
Paintings$height_cm = gsub( " .*$", "", Paintings$cm)

# Finding the lenght from the cm variable
Paintings$length_cm = str_extract(Paintings$cm, "x [0-9]*.[0-9] cm")
Paintings$length_cm = gsub("x ", "", Paintings$length_cm)
Paintings$length_cm = gsub(" cm", "", Paintings$length_cm)

# Creating variable for size
Paintings$size = (as.numeric(Paintings$height_cm))*(as.numeric(Paintings$length_cm))

# Sort desc
Paintings = Paintings[order(-Paintings$size),]

# Creating new dataset with highest 5 sizes
top = head(Paintings,5)

# Sort asc
Paintings = Paintings[order(Paintings$size),]

# Creating new dataset with smallest 5 sizes
bottom = head(Paintings,5)

# Merge two new dataframes
Together = merge(top, bottom, all=TRUE)
# Solution: The dataset Togehter include the five smallest and five largest paintings.


