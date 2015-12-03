library("readr")

#Without col_types, read_csv mistakenly believes season to be a date, and fails to load the column
tjek <- read_csv("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/players.csv",
                 col_types = list(season = col_character()))
