library("tree")


                                  ################################ Prediction ################################ 

names(final)

first = tree(price~ . -marketvalue , final)