library("readr")
library("stringr")
library("dplyr")
library("gdata")

# Cleaning dataset "Transfers"
# Transfermarket.com
transfers <- read_csv("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/transfers.csv")

# Nummerical value is found.
# Free transfer is set to having the value 0. 
# The number is subtracted with the values [k,m] in the end.
# The variables are split into two variables, both made numeric and then mulitplied.
# Transferfee is now measuring the value in thousands pounds
transfers$transferfee   <- gsub("Free transfer", "0.0k", transfers$transferfee)
transfers$fee1          <- str_extract(transfers$transferfee, "[0-9]*[:punct:]*[0-9]+[k,m]")
transfers$fee2          <- str_extract(transfers$fee1, "[k,m]+")
transfers$fee2          <- gsub("k", "1", transfers$fee2)
transfers$fee2          <- gsub("m", "1000", transfers$fee2)
transfers$fee1          <- gsub("[k,m]", "", transfers$fee1)
transfers$fee1          <- as.numeric (transfers$fee1)
transfers$fee2          <- as.numeric (transfers$fee2)
transfers$fee           <- transfers$fee1*transfers$fee2

# Creating two seperate subsets, Arrivals and Departures;
Arrival     <- subset(transfers, transfers$transfertype == "Arrivals")
Departure   <- subset(transfers, transfers$transfertype == "Departures")
vars        <- c("name", "age", "position", "marketvalue", "otherclub", "season", "club", "fee")

# Dropping variables of no interest.

Arrival     <- Arrival[vars]
Departure   <- Departure[vars]

# Renaming variables. In this way we can seperate "Selling" and "Buying" club depending on the type of transfer. 
names(Arrival)      <- c("Name", "Age", "Position", "Marketvalue", "Selling", "Season", "Buying", "Transferfee")
names (Departure)   <- c("Name", "Age", "Position", "Marketvalue", "Buying", "Season", "Selling", "Transferfee")

vars4       <- c("Name", "Age", "Season", "Position","Transferfee", "Marketvalue", "Selling", "Buying")
Arrival     <- Arrival[vars4]
Departure   <- Departure[vars4]

# Merge transfers and delete observations with no information on transferfee. 
merge.transfer <- rbind(Arrival, Departure)
merge.transfer <- unique(merge.transfer[, 1:8])
clean.transfer <- subset(merge.transfer, !is.na(Transferfee))

# Without col_types, read_csv mistakenly believes season to be a date, and fails to load the column
players   <- read_csv("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/players.csv",
                       col_types = list(season = col_character()))
vars2     <- c("fullName", "season", "APPEARANCES", "AVERAGE_GOALS_PER_MATCH", "AVERAGE_POINTS_PER_MATCH", 
           "ASSISTS", "CLEAN_SHEETS", "FOULED", "FOULS", "RED_CARDS", "YELLOW_CARDS", "SUBSTITUTIONS_OFF", "SUBSTITUTIONS_ON",
           "TOP_SCORERS", "WEIGHT","SHORTEST", "DRAW_RATIO", "WIN_RATIO", "LOSS_RATIO")
working   <- players[vars2]

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

#Renaming variables
names (working)   <- c("Name", "Season", "Appearances", "AvgGoals", "AvgPoints", "Assists", "CleanSheets", "Fouled", "Fouls", "RedCards", 
                     "YellowCards", "SubstitutionsOff", "SubstitutionsOn", "TopScores", "Weight", "Height", "DrawRatio", "WinRatio", "LossRatio")

# Keeping season as last year of the season
working$Season    <- working$Season %>% substr(6,9) %>% as.integer()

# Merging statistics of transfers and statistics on players
merge             <- left_join(clean.transfer, working, by.x = "Name", by.y = "Season")

# Removing observations not available
merge             <- subset(merge, !is.na(Appearances))

index             <- read.table("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/CP%20Index.txt", 
                      header = TRUE)
names (index)     <- c("Season", "CPI Index")

Clean.data        <- left_join(merge, index, by.x = "Season")
