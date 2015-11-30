library("rvest")
library("dplyr")
library("plyr")


url <- "http://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id=2015&s_w&leihe=0&intern=0"

#Returned list of dataframes/vectors, but merge did not work
#list.transfers <- lapply(css.selectors, function(selector){
#  df <-  read_html(url)  %>%
#             html_nodes(css=selector) %>%
#              html_text() %>% data.frame()
#})
#df.transfers <- ldply(list.transfers, rbind)

#needs to be cleaned!
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
    
  return(cbind(name, age, position, marketvalue, otherclub, transferfee))
}

df <-   data.frame(scrape_season(url), stringsAsFactors = FALSE)

df <- df %>% mutate(club = ifelse(name==age, name, ""))

#Classifies by club by identifying the lines with only club names, and changing the variable
#TODO: Needs to count rows automatically
for (i in 1:431) {
  if (df$name[i] == df$age[i]) {
    df$club[i] <- df$name[i]
  } else {
    df$club[i] <- df$club[i -1]
  }
}

#After classifying, we don't need the rows only consisting of club names
df <- df %>% filter(df$name != df$age)

?ifelse
