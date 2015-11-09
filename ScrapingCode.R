# Packages
library("rvest")
library("dplyr")
library("plyr")

#DATA GATHERING
#The scraping works in the following way. 
#1) We create an empty dataframe with the desired variables
#2) We run a loop which targets each page of Ipaidabribe.com separately. For each page, we create a temporary
#   data set with the same variables, and fill it out with scraped data. 
#3) At the end of each loop stage, we append the temporary data set to the final one. 


# Creating of relevant CSS
css.selector.title=".heading-3 a"
css.selector.amount=".paid-amount span"
css.selector.namedep=".name a"
css.selector.detail=".transaction a"
css.selector.views=".overview .views"
css.selector.city=".location"
css.selector.date=".date"

# We create an empty dataframe, where data can be stored (and reset the dataframe, if the code was run earlier)
df <- data.frame(title=character(), amount=character(),department=character(), detail=character(), views=character(), city=character(), date=character())

#The loop takes each page with 10 bribe reports, and scrapes the indicated number of posts
number_of_posts <- 1000
for (i in seq(0,number_of_posts-10,10)){ #number_of_posts minus 10, since 990 is the start of the last page
  link = paste("http://www.ipaidabribe.com/reports/paid?page=", i, sep="")

#For each page, we make a temporary dataset with the different parts we are interested in
temp.titles <- read_html(link)  %>%
  html_nodes(css=css.selector.title) %>%
  html_text() %>% as.list() %>% ldply()

temp.amount <- read_html(link)  %>%
  html_nodes(css=css.selector.amount) %>%
  html_text() %>% as.list() %>% ldply()

temp.department <- read_html(link)  %>%
  html_nodes(css=css.selector.namedep) %>%
  html_text() %>% as.list() %>% ldply()

temp.detail <- read_html(link)  %>%
  html_nodes(css=css.selector.detail) %>%
  html_text() %>% as.list() %>% ldply()

temp.views <- read_html(link)  %>%
  html_nodes(css=css.selector.views) %>%
  html_text() %>% as.list() %>% ldply()

temp.city <- read_html(link)  %>%
  html_nodes(css=css.selector.city) %>%
  html_text() %>% as.list() %>% ldply()

temp.date <- read_html(link)  %>%
  html_nodes(css=css.selector.date) %>%
  html_text() %>% as.list() %>% ldply()

#Combines all temporary datasets to one data set, which resembles the final one. 
temp <- bind_cols(temp.titles, temp.amount, temp.department, temp.detail, temp.views, temp.city, temp.date)
colnames(temp) <- c("title", "amount", "department", "detail", "views", "city", "date")

#We append the data scraped in the current loop to the general data set
df <- bind_rows(df, temp)

#We'd like to be able to follow the scraping..
print(paste("... scraped", i+10, "of", number_of_posts, sep = " "))

#We do not want to bombard the server, so we let the system sleep a bit before we start the next page. 
#This is partly because we are nice, and partly because the server seems to shut us down otherwise. 
Sys.sleep(1)


}

print(paste("Scraping is done!"))

# Danner dato variabel
bribe$date = as.Date(bribe$date, "%B %d, %Y")

# Laver numerisk værdi af views
bribe$views = as.integer(str_extract(bribe$views, "[0-9]+"))

# Laver numerisk værdi af amount
bribe$amount = str_extract(gsub(",", "", bribe$amount), "[0-9]+")

write.csv(df, file="Bribes.csv", row.names = FALSE)




#We clean up the workspace a bit
#rm(temp.titles, temp.amount, temp.department, temp.detail, temp.views, temp.city, temp.date)
