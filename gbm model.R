setwd("C:/Data mining project/")
library(caret)
library(gbm)
library(caTools)

##### reading data 

data<- read.csv("train.csv")


##### spliting data into train and test 


set.seed(123)
sample<- sample.int(n= nrow(data), size = floor(.75*nrow(data)), replace= F)
train<- data[sample, ]
test<- data[-sample, ]

##### fiting the gbm model 

gbmfit<- gbm(SalePrice~. , data=train, distribution = "gaussian", interaction.depth = 3, shrinkage= 0.001, 
             bag.fraction = 0.5,train.fraction= 0.5, n.trees=4102, n.minobsinnode= 10, keep.data= TRUE,
             verbose= FALSE, n.cores= 1, cv.folds= 3)



### check performance using an out-of-bag estimator

?gbm.perf()
best.iter<- gbm.perf(gbmfit, plot.it= TRUE, oobag.curve= FALSE, overlay= FALSE, method= "OOB")
print(best.iter)

### check performance using an 50% heldout test set

best.iter1<- gbm.perf(gbmfit, plot.it= TRUE, oobag.curve= FALSE, overlay= FALSE, method= "test")
print(best.iter1)

##plot the variable influence ### comparison between 1 tree and optimum number of trees
summary(gbmfit, n.trees= 1)
#3 variables with some level of relative influence

summary(gbmfit, n.tress= best.iter)
#65 variables with some level of relative influece


# printing the first and last trees
print(pretty.gbm.tree(gbmfit,1))
print(pretty.gbm.tree(gbmfit,gbmfit$n.trees))


###prediction on test data
f.predict <- predict(gbmfit,train,best.iter)
f.predict1 <- predict(gbmfit,test,best.iter)


##mape of train and test to check the difference in errors

mean(abs(f.predict- train$SalePrice)/ train$SalePrice)
mean(abs(f.predict1-test$SalePrice)/test$SalePrice)

# least squares error
train_lse<- print(sum(train$SalePrice - f.predict)^2)
test_lse<- print(sum((test$SalePrice-f.predict1)^2))




