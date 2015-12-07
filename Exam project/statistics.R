library("readr")
library("stringr")
library("dplyr")
library("stargazer")


Clean <- read_csv("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/merged.csv")

New <- as.data.frame (Clean)

stargazer(New, type="html", title="Descriptive statistics", out="table1.html")




library("ggplot2")
p = ggplot(data = Bribes.department, aes(x = reorder(department, totalbribes), y = totalbribes)) 
p = p + geom_bar(stat = "identity")  + coord_flip()
p = p + labs(title = "Figure 1: Total amount", x = "Department", y = "Total amount paid") + theme_minimal()
p