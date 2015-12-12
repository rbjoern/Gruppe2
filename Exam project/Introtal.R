########################################################
####### APPENDIX 5: FIGURE FOR THE INTRODUCTION ########
########################################################

library("readr")
library("stringr")
library("dplyr")
library("gdata")
library("ggplot2")

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

index             <- read.table("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/CP%20Index.txt", 
                                header = TRUE)
names (index)     <- c("season", "CPI_Index_2010", "CPI_Index_2014")

transfers        <- left_join(transfers, index, by.x = "season")

transfers$fee.real = transfers$fee/transfers$CPI_Index_2014

transfervalue <- transfers %>% 
                  filter(!is.na(fee.real)) %>% 
                  group_by(season, transfertype) %>%
                  summarise(
                  value = sum(fee.real)
                  )

q <- ggplot(transfervalue, aes(x=season, y=value/1000, colour=transfertype)) 
q + geom_line(size = 1) + 
  theme_light()   +
    scale_color_manual(values=c("#000000", "#999999")) + 
  labs(x = "Season", y="Total value in 2014  prices in mio. pounds ") + 
  theme(axis.title = element_text(family = "Times", color="#000000"))

# Creating two seperate subsets, Arrivals and Departures;
Arrival     <- subset(transfervalue, transfervalue$transfertype == "Arrivals")
mean(Arrival$value)
Departure   <- subset(transfervalue, transfervalue$transfertype == "Departures")
mean(Departure$value)