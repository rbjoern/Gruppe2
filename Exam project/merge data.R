library("readr")
library("stringr")
library("dplyr")

#load data
#Without col_types, read_csv mistakenly believes season to be a date, and fails to load the column
df.players <- read_csv("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/players.csv",
                 col_types = list(season = col_character()))
df.players$season <- df.players$season %>% substr(1,4) %>% as.integer()
colnames(df.players)[2] <- "name"

df.transfers <- read_csv("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/transfers.csv")

df.merge <- left_join(df.transfers, df.players, by.x = "name", by.y = "season")