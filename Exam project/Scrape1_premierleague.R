library("jsonlite")
library("reshape")
library("plyr")
library("dplyr")
library("tidyr")

#Premierleague.com keeps player statistics stored in JSON-files. 
#The following code extracts all statistics from 1992-2015. 
#Runtime is approximately 20 minutes.


  #We run a nested loop (via lapply), changing the URL of the JSON file to reflect the indidual statistic and season
#The site contains information about the following scores in individual JSON files. 
scores <- c("TOP_SCORERS", "APPEARANCES", "ASSISTS", "YELLOW_CARDS", "RED_CARDS", "FOULS", "FOULED", "TALLEST", "SHORTEST", "WIN_RATIO", "LOSS_RATIO", "DRAW_RATIO", "AVERAGE_GOALS_PER_MATCH", "AVERAGE_POINTS_PER_MATCH", "CLEAN_SHEETS", "SUBSTITUTIONS_ON", "SUBSTITUTIONS_OFF", "OLDEST", "YOUNGEST", "WEIGHT")   
#Data is also seperated by season, from 1992-1993 to 2015-2016, a total of 24 seasons. 
seasons = 0:23

#We apply a procedure to each season, and return a list of dataframes (one for each season) which are then merged below
df.allseasons <- lapply(seasons, function(i){

  #We apply a function to all the scores to extract the data, returning a list of dataframes, one for each score
  df.allscores <- lapply(scores, function(score) {
    #The function creates the url which leads to the JSOn files, which are dependant on current loop of score and season
    url <- paste("http://www.premierleague.com/ajax/player/index/BY_STAT/", score, "/null/null/null/null/null/", 1992 + i, "-", 1993 + i, "/null/null/1000/4/2/2/1/null.json", sep = "")
  
    # We then parse this into a list via the jsonlite package in R
    list <- fromJSON(txt=url)
    
    #The list contains a number of elements. 
      #The one of primary interest is a dataframe containing player information 
    df.score <- list$playerIndexSection$index$resultsList
    
      # A few other elements are of interest, and also saved: Season, and the score currently processed
      df.score$score <- list$playersIndexViewContext$playerInformationContext
      df.score$season <- list$playersIndexViewContext$season
    
      #The data.frame contains a smaller dataframe with various club names. 
        #This make data management harder, since one has to refer to a df inside a df.
        #We therefore extract the relevant information - club name - and then drop irrelevant variables
      df.score$club <- df.score$club$name
      df.score <- df.score[c("id", "fullName", "club", "value", "score", "season")]
      #dropped variables "position", "cmsAlias", "lastName",
      
      #We'd like a small status update, to make sure the code is running. 
      print(paste("... Scraped: Season", 1992 + i, "-", 1993 + i, ",", score))
      Sys.sleep(1) #We let the system sleep momentarily, to avoid badgering their servers
    #The functuon returns the dataframe
    return(df.score)
  })

  #We then merge the list of dataframes into a long dataframe by appending them to each other, and order it appropriately
  season.long <- ldply(df.allscores, rbind)
  season.long <- season.long[order(season.long$id),]
  
  #We are interested in individual players, so they form observational unit. 
  #Hence, we transform the data to a wide format with one row per player/season combo
  season.wide <- season.long %>% spread(key = "score", value = "value")
  
  
  print(paste("... Finished scraping season!"))
  Sys.sleep(1)
    return(season.wide)
})

print(paste("Finished scraping!"))

#One again, we append the list of dataframes on top of each other
df.players <- ldply (df.allseasons, rbind)

#Finally, we save our work to avoid having to repeat ourselves 
write.csv(df.players, file="players.csv", row.names = FALSE)