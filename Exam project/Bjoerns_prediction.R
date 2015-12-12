########################################################
####### APPENDIX 4: STATISTICAL LEARNING ###############
########################################################

#Required packages
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

#Drops variables which are not fitting in a tree (mainly strings, weight has nas, Topscores duplicates avg goals)
drops <- c("Selling", "Buying", "Marketvalue_real", "Weight", "TopScores")
df.tree <- df[,!names(df) %in% drops] 
rm(drops)

# Removes outliers (High & low)
#High outliers are removed in order to increase validity of the prediction, since the prediction 
#clearly cannot catch why they are so far above the others
# Zeros are removed since they are free transfers, which is not to say they're priced at zero in a transaction. 
df.tree <- df.tree %>% filter(Transferfee_real < 25000) %>% filter (Transferfee_real != 0)


#We use a randomly sampled training set of eighty percent of the total 
set.seed(25)
train <- sample(1:nrow(df.tree), nrow(df.tree)*(4/5))

#We then construct the best simple tree
tree.1 <- tree(Transferfee_real~. -Name - Season, df.tree, subset=train)
plot(tree.1)
text(tree.1, pretty=0)

#How well does the model do on the evaluation set
yhat.tree1 <- predict(tree.1, newdata = df.tree[-train,])
df.test <- df.tree[-train, "Transferfee_real"]
#plot(yhat.tree1, df.test)
#abline(0,1)
mean((yhat.tree1-df.test)^2)
sqrt(mean((yhat.tree1-df.test)^2))

# ~ Monkey (compare to a monkey which simply guesses at the average)
sqrt(mean((mean(df.tree$Transferfee_real)-df.test)^2))


#We will now prune the tree. First we compute a visual of how much the model error decreases as the tree grows
cv.tree.1 <- cv.tree(tree.1)
plot(cv.tree.1$size, cv.tree.1$dev, type="s")

#We then prune the tree until the optimal point, which here is no pruning at all.
#The next code chunk is therefore legacy code from earlier trees, such that it is. 
prune.1 <- prune.tree(tree.1, best=19)
plot(prune.1)
text(prune.1)

#Test set
yhat.tree1 <- predict(prune.1, newdata = df.tree[-train,])
df.test <- df.tree[-train, "Transferfee_real"]
#plot(yhat.tree1, df.test)
#abline(0,1)
sqrt(mean((yhat.tree1-df.test)^2))

# ~ Monkey
sqrt(mean((mean(df.tree$Transferfee_real)-df.test)^2))

# Bagging: Random forest with all 17 parameters at all times
rf.1 <- randomForest(Transferfee_real ~ . -Name - Season, data = df.tree, 
                     subset=train, mtry = 17, importance=TRUE, ntree=1000)
yhat.rf <- predict(rf.1, newdata=df.tree[-train,])
plot(yhat.rf, df.test)
abline(0,1)
importance(rf.1)
sqrt(mean((yhat.rf-df.test)^2))
varImpPlot(rf.1)


#Random forest: Fewer parameters at each split
rf.1 <- randomForest(Transferfee_real ~ . -Name - Season, data = df.tree, 
                   subset=train, mtry = 17/3, importance=TRUE, ntree=1000)
yhat.rf <- predict(rf.1, newdata=df.tree[-train,])
plot(yhat.rf, df.test)
abline(0,1)
importance(rf.1)
sqrt(mean((yhat.rf-df.test)^2))
varImpPlot(rf.1)

print(rf.1)
importance(rf.1)
plot(rf.1)
plot( importance(rf.1), lty=2, pch=16)
lines(importance(rf.1))
imp = importance(rf.1)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3)) 
for (i in seq_along(impvar)) {
  partialPlot(rf.1, raw, impvar[i], xlab=impvar[i],
              main=paste('Partial Dependence on', impvar[i]),
              ylim=c(0, 1))
}


# Boosting
boost.1 <- gbm(Transferfee_real~. -Name -Season, data = df.tree[train,], distribution = "gaussian", n.trees = 10000, interaction.depth=4,
               shrinkage = )
#summary(boost.1)
yhat.boost <- predict(boost.1, newdata=df.tree[-train,], n.trees = 10000)
sqrt(mean((yhat.boost-df.test)^2))



