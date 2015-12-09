library("tree")
library("readr")
library("dplyr")
library("randomForest")
library("gbm")

#Load data
df <- read_csv("https://raw.githubusercontent.com/rbjoern/Gruppe2/master/Exam%20project/Data/merged.csv",
            col_types = list(Marketvalue_real = col_numeric(), Fouled = col_numeric(), 
            Fouls = col_numeric(), Assists=col_numeric())) %>%
            as.data.frame()
df$Position <- as.factor(df$Position)

summary(df$Transferfee_real)

#Drops variables which are not fitting in a tree (mainly strings, weight has nas, Topscores duplicates avg goals)
drops <- c("Selling", "Buying", "Marketvalue_real", "Weight", "TopScores")
df.tree <- df[,!names(df) %in% drops] 
rm(drops)

df.tree <- df.tree %>% filter(Transferfee_real < 25000) %>% filter (Transferfee_real != 0)


#Check check for NA's 
#sapply(df.tree, function(x) sum(is.na(x)))

#Simple tree
set.seed(25)
train <- sample(1:nrow(df.tree), nrow(df.tree)*(4/5))
tree.1 <- tree(Transferfee_real~. -Name - Season, df.tree, subset=train)
plot(tree.1)
text(tree.1, pretty=0)

?tree

#Pruning: Visual
cv.tree.1 <- cv.tree(tree.1)
plot(cv.tree.1)
summary(cv.tree.1)

#Pruning: Actual
prune.1 <- prune.tree(tree.1, best=8)
plot(prune.1)
text(prune.1)

#Test set
yhat.tree1 <- predict(tree.1, newdata = df.tree[-train,])
df.test <- df.tree[-train, "Transferfee_real"]
#plot(yhat.tree1, df.test)
#abline(0,1)
sqrt(mean((yhat.tree1-df.test)^2))


sqrt(mean((mean(df.tree$Transferfee_real)-df.test)^2))

#Random forest
rf.1 <- randomForest(Transferfee_real ~ . -Name - Season, data = df.tree, 
                          subset=train, mtry = 17/3, importance=TRUE, ntree=500)
yhat.rf <- predict(rf.1, newdata=df.tree[-train,])
#plot(yhat.rf, df.test)
#abline(0,1)
importance(rf.1)
sqrt(mean((yhat.rf-df.test)^2))
varImpPlot(rf.1)


#boosting
#boost.1 <- gbm(Transferfee_real ~ . - Name - Season, data = df.tree[train,], 
#             distribution = gaussian, n.trees = 5000, interaction.depth=4)
#ummary(boost.1)