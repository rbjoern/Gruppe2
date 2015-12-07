library("tree")
library("rpart")
library("MASS")
library("randomForest")


                                  ################################ Prediction ################################ 


# Creating training dataset
set.seed(25)
train = sample(1:nrow(Clean.data), nrow(Clean.data)/2)

Clean.data$Position = as.factor(Clean.data$Position)

# Growing tree on training data
tree_1=tree(Transferfee_real ~ . -Marketvalue_real -Season - AvgGoals -Buying -Name -Selling, Clean.data, subset = train)
summary(tree_1)

# Plot of first decision tree
plot(tree_1)
text(tree_1 ,pretty=0)

cv.tree_1=cv.tree(tree_1)
plot(cv.tree_1$size ,cv.tree_1$dev ,type='b')








#################################################################

prune.tree_1=prune.tree(tree_1 ,best=5)
plot(prune.tree_1)
text(prune.tree_1 ,pretty=0)

yhat=predict(tree_1 ,newdata=Clean.data[-train ,])
Clean.test=Clean.data[-train ,"Transferfee_real"]
plot(yhat,Clean.test, pretty=0)
abline (0 ,1)
sqrt(mean((yhat-Clean.test)^2))

Clean.data = as.data.frame(Clean.data)

# Random forest
set.seed (25)
bag.price=randomForest(Transferfee_real ~ . +as.factor(Position) -Marketvalue_real -Season - AvgGoals -Name -Position -Selling -Buying, data=Clean.data,
                          mtry=15, importance =TRUE, subset=train)
bag.price










