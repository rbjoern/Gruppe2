########################################################
####### APPENDIX 3: MERGING AND CLEANING DATA ##########
########################################################

library("readr")
library("stringr")
library("dplyr")
library("gdata")
library("stargazer")

# Cleaning dataset "Transfers"
# Transfermarket.com, load data
transfers <- read_csv("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/transfers.csv")

#Clean transfer value
# Numerical value is found.
# Free transfer is set to having the value 0. 
transfers$transferfee   <- gsub("Free transfer", "0.0k", transfers$transferfee)
# The number is extracted with the values [k,m] in the end.
transfers$fee1          <- str_extract(transfers$transferfee, "[0-9]*[:punct:]*[0-9]+[k,m]")
# The variables are split into two variables, both made numeric and then mulitplied.
transfers$fee2          <- str_extract(transfers$fee1, "[k,m]+")
transfers$fee2          <- gsub("k", "1", transfers$fee2)
transfers$fee2          <- gsub("m", "1000", transfers$fee2)
transfers$fee1          <- gsub("[k,m]", "", transfers$fee1)
transfers$fee1          <- as.numeric (transfers$fee1)
transfers$fee2          <- as.numeric (transfers$fee2)
transfers$fee           <- transfers$fee1*transfers$fee2
# Transferfee is now measuring the value in thousands pounds

#We now clean the data so that one can see whether a club buys or sells a player, rather than whether a sale is arrival/departure
# Creating two seperate subsets, Arrivals and Departures;
Arrival     <- subset(transfers, transfers$transfertype == "Arrivals")
Departure   <- subset(transfers, transfers$transfertype == "Departures")
vars        <- c("name", "age", "position", "marketvalue", "otherclub", "season", "club", "fee")

# Dropping variables of no interest.
Arrival     <- Arrival[vars]
Departure   <- Departure[vars]

# Removes objects which are no longer used
rm(vars)
rm(transfers)

# Renaming variables. In this way we can seperate "Selling" and "Buying" club depending on the type of transfer. 
names(Arrival)      <- c("Name", "Age", "Position", "Marketvalue", "Selling", "Season", "Buying", "Transferfee")
names (Departure)   <- c("Name", "Age", "Position", "Marketvalue", "Buying", "Season", "Selling", "Transferfee")

vars4       <- c("Name", "Age", "Season", "Position","Transferfee", "Marketvalue", "Selling", "Buying")
Arrival     <- Arrival[vars4]
Departure   <- Departure[vars4]

# Removes objects which are no longer used
rm(vars4)

# Merge transfers and delete observations with no information on transferfee. 
merge.transfer <- rbind(Arrival, Departure)
merge.transfer <- unique(merge.transfer[, 1:8])
clean.transfer <- subset(merge.transfer, !is.na(Transferfee))

#As some have the an age value equal to birth year (both negative and positive) these are used to calculate the actual age
#Using season value to calculate the age in the year of the season
#Deleting observations with the age=0
clean.transfer$Season <- as.numeric(clean.transfer$Season)
clean.transfer$Age <- ifelse(clean.transfer$Age<0,(clean.transfer$Season + clean.transfer$Age), clean.transfer$Age)
clean.transfer$Age <- ifelse(clean.transfer$Age>200,(clean.transfer$Season - clean.transfer$Age), clean.transfer$Age)
clean.transfer$Age <- ifelse(clean.transfer$Age == 0,NA, clean.transfer$Age)

# Removes merge.transfer, Arrival and Depature dataframes which are no longer used
rm(merge.transfer)
rm(Arrival)
rm(Departure)

#Load player data from premierleague.com
# Without col_types, read_csv mistakenly believes season to be a date, and fails to load the column
players   <- read_csv("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/players.csv",
                       col_types = list(season = col_character()))
vars2     <- c("fullName", "season", "APPEARANCES", "AVERAGE_GOALS_PER_MATCH", "AVERAGE_POINTS_PER_MATCH", 
           "ASSISTS", "CLEAN_SHEETS", "FOULED", "FOULS", "RED_CARDS", "YELLOW_CARDS", "SUBSTITUTIONS_OFF", "SUBSTITUTIONS_ON",
           "TOP_SCORERS", "WEIGHT","SHORTEST", "DRAW_RATIO", "WIN_RATIO", "LOSS_RATIO")
working   <- players[vars2]

# Removes vars2 and players which are no longer used
rm(vars2)
rm(players)

# A variable in clean form might be affected by a player working in another soccer league
# Therefore are all variables are calculated as the score per appearence
working$Assists           <- working$ASSISTS/working$APPEARANCES
working$CleanSheets       <- working$CLEAN_SHEETS/working$APPEARANCES
working$Fouled            <- working$FOULED/working$APPEARANCES
working$Fouls             <- working$FOULS/working$APPEARANCES
working$RedCards          <- working$RED_CARDS/working$APPEARANCES
working$YellowCards       <- working$YELLOW_CARDS/working$APPEARANCES
working$SubstitutionsOff  <- working$SUBSTITUTIONS_OFF/working$APPEARANCES
working$SubstitutionsOn   <- working$SUBSTITUTIONS_ON/working$APPEARANCES
working$TopScores         <- working$TOP_SCORERS/working$APPEARANCES

#Choosing variables to keep
vars3     <- c("fullName", "season", "APPEARANCES", "AVERAGE_GOALS_PER_MATCH", "AVERAGE_POINTS_PER_MATCH", "Assists", "CleanSheets", 
           "Fouled", "Fouls", "RedCards", "YellowCards", "SubstitutionsOff", "SubstitutionsOn", "TopScores", 
           "WEIGHT", "SHORTEST", "DRAW_RATIO", "WIN_RATIO", "LOSS_RATIO")
working   <- working[vars3]

# Removes vars3
rm(vars3)

#Renaming variables
names (working)   <- c("Name", "Season", "Appearances", "AvgGoals", "AvgPoints", "Assists", "CleanSheets", "Fouled", "Fouls", "RedCards", 
                     "YellowCards", "SubstitutionsOff", "SubstitutionsOn", "TopScores", "Weight", "Height", "DrawRatio", "WinRatio", "LossRatio")

# Keeping season as last year of the season
working$Season    <- working$Season %>% substr(6,9) %>% as.integer()

# Merging statistics of transfers and statistics on players
# Left join, since we only analyze transfers, and hence are uninterested in other player statistics
merge             <- left_join(clean.transfer, working, by.x = "Name", by.y = "Season")

# Removes clean.transfer and working which are no longer used
rm(clean.transfer)
rm(working)

# Removing observations not available
merge             <- subset(merge, !is.na(Appearances))


#We now compute real prices via a CpI from the World Bank
#We load the data, and merge it into the existing data set
index             <- read.table("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/CP%20Index.txt", 
                      header = TRUE)
names (index)     <- c("Season", "CPI_Index_2010", "CPI_Index_2014")

Clean.data        <- left_join(merge, index, by.x = "Season")

#We forgot to clean the market value variable! 
#We do this swiftly, and then resume real value computation
# Cleans market value
Clean.data$MV = ifelse(Clean.data$Marketvalue=="-", NA, as.character(Clean.data$Marketvalue))

# Isolates multiplier for market value
Clean.data$multiplierMV = substr(as.character(Clean.data$MV), nchar(as.character(Clean.data$MV))-1+1, nchar(as.character(Clean.data$MV)))

# Cleans multiplier
Clean.data$factorMV = ifelse(Clean.data$multiplierMV=="m",
                            as.numeric(1000000),
                            ifelse(Clean.data$multiplierMV=="k",
                                   as.numeric(1000),
                                   as.numeric(0)
                            ))

# Extracts only digits from transfer pricing
Clean.data$MV = as.numeric(str_extract(Clean.data$MV, "\\d+\\.*\\d*"))

# Creates price variable as product of transferpricing and the factor
Clean.data$Marketvalue = Clean.data$MV*Clean.data$factorMV

# Divides Marketvalue by 1000
Clean.data$Marketvalue = Clean.data$Marketvalue/1000

# Removes variables from marketvalue calculation
Clean.data$MV           = NULL
Clean.data$multiplierMV = NULL
Clean.data$factorMV     = NULL

# Removes merge and index
rm(merge)
rm(index)

#We calculate fixed prices by dividing with the index 
Clean.data$Transferfee_real = Clean.data$Transferfee/Clean.data$CPI_Index_2014
Clean.data$Marketvalue_real = Clean.data$Marketvalue/Clean.data$CPI_Index_2014

# Removes index since it is no longer used. We also remove nominal variables
Clean.data$CPI_Index_2014 = NULL
Clean.data$CPI_Index_2010 = NULL
Clean.data$Transferfee    = NULL
Clean.data$Marketvalue    = NULL

#We save the data (on github)
write.csv(Clean.data, file="merged.csv", row.names = FALSE)

Clean <- as.data.frame (Clean.data)
stargazer(Clean, type="html", title="Descriptive statistics", digits=2,  out="Summary Statistics.html", summary.stat = c("n", "min", "p25", "median", "p75", "max", "mean"))
