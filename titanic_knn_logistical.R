titanic <- read.csv("train.csv", stringsAsFactors = TRUE)
##logistical regression using all of the variables within the dataset this time.
summary(titanic)
titanic_sub <- titanic[-which(is.na(titanic$Age)),]
titanic_sub <- titanic_sub[, -4]
titanic_fit1 <- glm(Survived ~ Age + SibSp + Pclass + Sex + Embarked + Cabin, data = titanic_sub, family = binomial)
summary(titanic_fit1)
titanic_probs <- predict(titanic_fit1, type = "response")
titanic_pred <- rep("Died", nrow(titanic_sub))
titanic_pred[titanic_probs > 0.5] <- "Survived"
table(titanic_pred, titanic_sub[,"Survived"])
(382 + 231)/ nrow(titanic_sub)#No way 85.9 percent accuracy. Very good!!! The embarked and cabin variables did matter!! But still this is only a training set accuracy rate. Will need to see what the testing set says.

##knn 
#from my other experiments I found out the k nearest neighbors doesn't work with factors within the variable partition of the algorithm.
#this time I will attempt to normalize the data.

titanic_sub_norm <- as.data.frame(lapply(titanic_sub[c("Pclass","Sex","Ticket","Cabin", "Embarked")], FUN = as.numeric))
titanic_sub[,c("Pclass","Sex","Ticket","Cabin", "Embarked")] <- titanic_sub_norm

normalize <- function(x){
	return((x-min(x))/(max(x) - min(x)))
}

titanic_sub_norm <- as.data.frame(lapply(titanic_sub[c("Age","Fare","Sex","Pclass","Cabin","Ticket","Embarked")], normalize))
summary(titanic_sub_norm)
titanic_sub[,c("Age","Fare","Sex","Pclass","Cabin","Ticket","Embarked")] <- titanic_sub_norm
titanic_sub$Survived <- as.factor(titanic_sub$Survived)
titanic_sub <- titanic_sub[,-1]
train <- sample(1:nrow(titanic_sub), 500)
titanic_train <- titanic_sub[train,]
titanic_test <- titanic_sub[-train,]
titanic_train_labels <- titanic_train$Survived
titanic_test_labels <- titanic_test$Survived
library(class)
library(gmodels)
titanic_test_knn <- knn(train = titanic_train, test = titanic_test, cl = titanic_train_labels, k = 10)
CrossTable(x = titanic_test_labels, y = titanic_test_knn, prop.chisq = FALSE)

#Validation set accuracy rate:
(132 + 80)/nrow(titanic_test)# Sweet, the validation set error rate is 0.972. This is really good.

