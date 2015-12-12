########################################################
####### APPENDIX 2: TRANSFERMARKT.CO.UK SCRAPER ########
########################################################

library("rvest")
library("dplyr")
library("plyr")

#First, we write a function which can scrape a single page
#Then we run it for all seasons 
#Finally we clean the data a little bit by assigning values from header rows, and then deleting the header rows

#The following function extracts a dataframe with all players displayed at one time on a page 
scrape_season <- function(link) {
  css.selectors <- c(".hide-for-small .spielprofil_tooltip , .box+ .box .table-header , .show-for-small+ .box .table-header , .spieler-transfer-cell",
                    ".box+ .box .table-header , .show-for-small+ .box .table-header , .alter-transfer-cell",
                    ".box+ .box .table-header , .show-for-small+ .box .table-header , .pos-transfer-cell",
                    ".show-for-small+ .box .table-header , .box+ .box .table-header , .mw-transfer-cell",
                    ".box+ .box .table-header , .show-for-small+ .box .table-header , .verein-transfer-cell , .verein-flagge-transfer-cell a",
                    ".box+ .box .table-header , .show-for-small+ .box .table-header , .abloese-transfer-cell , .rechts a")
    
    name        <-  read_html(link)  %>%
                    html_nodes(css=css.selectors[[1]]) %>%
                    html_text()
    age         <-  read_html(link)  %>%
                    html_nodes(css=css.selectors[[2]]) %>%
                    html_text()
    position    <-  read_html(link)  %>%
                    html_nodes(css=css.selectors[[3]]) %>%
                    html_text()
    marketvalue <-  read_html(link)  %>%
                    html_nodes(css=css.selectors[[4]]) %>%
                    html_text()
    otherclub   <-  read_html(link)  %>%
                    html_nodes(css=css.selectors[[5]]) %>%
                    html_text()
    transferfee  <- read_html(link)  %>%
                    html_nodes(css=css.selectors[[6]]) %>%
                    html_text()
    
  return(data.frame(cbind(name, age, position, marketvalue, otherclub, transferfee), stringsAsFactors = FALSE))
}

#We now run the function for all 24 seasons 
#Runtime approx 10 minutes

seasons <- 0:23

list.dfs <- lapply(seasons, function(j) {

url <- paste("http://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id=", 1992 + j, sep="")

print(paste("... scraped season", 1992+j, sep=" "))
df <- scrape_season(url)

df <- cbind(df, season = 1992 + j)

Sys.sleep(1)

return(df)

})

print("Scraping done!")

df.unclean <- ldply(list.dfs, rbind)

#The following procedure cleans the data. 
  #In order to obtain information on the premier league club involved in the transaction, we needed two
  # "header" rows for each club. On the site, clubs are divided 1) headers dividing each club (e.g. 
  # Chelsea's transactions), and then subheaders dividing those transactions into arrivals/departures
  # Below we copy this information for each block of data (i.e clubs, and departures/arrivals)

df <- df.unclean

#Classifies by club by identifying the lines with only club names, and changing the variable
for (i in 1:nrow(df)) {
  if (df$name[i] == df$age[i]) {
    df$club[i] <- df$name[i]
  } else {
    df$club[i] <- df$club[i -1]
  }
}

print("Clubs assigned")

#After classifying, we don't need the rows only consisting of club names
df <- df %>% filter(df$name != df$age)

print("Club headers removed")

#Next we need to move the repeated variable name lines, identifying arrivals versus departures
for (i in 1:nrow(df)) {
  if (df$name[i] == "Arrivals") {
    df$transfertype[i] <- "Arrivals"
  } else { if (df$name[i] == "Departures") {
    df$transfertype[i] <- "Departures"
  } else {
    df$transfertype[i] <- df$transfertype[i - 1]
  }}
}

print("Transfer types assigned")

#And now we no longer need the variable header rows. 
df <- df %>% filter(df$name != "Arrivals", df$name != "Departures")

print("Variable headers removed")

#Finally, we save our work to avoid having to repeat ourselves 
write.csv(df, file="transfers.csv", row.names = FALSE)