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


# Removes outliers (High & low)
df.tree <- df.tree %>% filter(Transferfee_real < 25000) %>% filter (Transferfee_real != 0)


#Simple tree
set.seed(25)
train <- sample(1:nrow(df.tree), nrow(df.tree)*(4/5))
tree.1 <- tree(Transferfee_real~. -Name - Season, df.tree, subset=train)
plot(tree.1)
text(tree.1, pretty=0)

# ~ Monkey
sqrt(mean((mean(df.tree$Transferfee_real)-df.test)^2))

#Test set
yhat.tree1 <- predict(tree.1, newdata = df.tree[-train,])
df.test <- df.tree[-train, "Transferfee_real"]
#plot(yhat.tree1, df.test)
#abline(0,1)
sqrt(mean((yhat.tree1-df.test)^2))

#Pruning: Visual
cv.tree.1 <- cv.tree(tree.1)
plot(cv.tree.1)
summary(cv.tree.1)

#Pruning: Actual
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

# Bagging
rf.1 <- randomForest(Transferfee_real ~ . -Name - Season, data = df.tree, 
                     subset=train, mtry = 17, importance=TRUE, ntree=1000)
yhat.rf <- predict(rf.1, newdata=df.tree[-train,])
plot(yhat.rf, df.test)
abline(0,1)
importance(rf.1)
sqrt(mean((yhat.rf-df.test)^2))
varImpPlot(rf.1)


#Random forest
rf.1 <- randomForest(Transferfee_real ~ . -Name - Season, data = df.tree, 
                     subset=train, mtry = 17/3, importance=TRUE, ntree=300)
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
boost.1 <- gbm(Transferfee_real~. -Name -Season, data = df.tree[train,], distribution = "gaussian", n.trees = 5000, interaction.depth=4,
               shrinkage = )
#summary(boost.1)
yhat.boost <- predict(boost.1, newdata=df.tree[-train,], n.trees = 5000)
sqrt(mean((yhat.boost-df.test)^2))



