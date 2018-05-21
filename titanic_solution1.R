##titanic submission KNN:
##Training:
titanic <- read.csv("train.csv", stringsAsFactors = TRUE)
titanic_sub <- titanic[-which(is.na(titanic$Age)),]
titanic_sub_norm <- as.data.frame(lapply(titanic_sub[c("Pclass","Sex","Ticket","Cabin", "Embarked")], FUN = as.numeric))
titanic_sub[,c("Pclass","Sex","Ticket","Cabin", "Embarked")] <- titanic_sub_norm

normalize <- function(x){
	return((x-min(x))/(max(x) - min(x)))
}

titanic_sub_norm <- as.data.frame(lapply(titanic_sub[c("Age","Fare","Sex","Pclass","Cabin","Ticket","Embarked")], normalize))
summary(titanic_sub_norm)
titanic_sub[,c("Age","Fare","Sex","Pclass","Cabin","Ticket","Embarked")] <- titanic_sub_norm
titanic_sub$Survived <- as.factor(titanic_sub$Survived)
colnames(titanic_sub)
titanic_sub <- titanic_sub[,c(-4, -6, -9)]
train <- sample(1:nrow(titanic_sub), 500)
titanic_train <- titanic_sub[train,]
titanic_test <- titanic_sub[-train,]
titanic_train_labels <- titanic_train$Survived
titanic_test_labels <- titanic_test$Survived
library(class)
library(gmodels)
titanic_test_knn <- knn(train = titanic_train[,-1], test = titanic_test[,-1], cl = titanic_train_labels, k = 10)
CrossTable(x = titanic_test_labels, y = titanic_test_knn, prop.chisq = FALSE)

##Imputation Experiment: We need to impute 177 na values within the age variable. I believe that Pclass is the best means to create accurate age approximations.
titanic_imput <- titanic[,c(-4, -9)]
titanic_imput[,3] <- as.factor(titanic_imput[,3])
summary(titanic_imput)
aggregate(data = titanic_imput, Age ~ Pclass, mean, na.rm = TRUE)
ave_age <- ave(titanic_imput$Age, titanic_imput$Pclass, FUN = function(x) mean(x, na.rm = TRUE))
titanic_imput$Age <- ifelse(is.na(titanic_imput$Age), ave_age, titanic_imput$Age)#Sweet this imputation technique actually worked.

##KNN algorithm Redo with imputation dataset:
#First need to change all variables to numeric values:
titanic_imput_numer <-as.data.frame(lapply(titanic_imput[c(3,4,9,10)], FUN = as.numeric))
titanic_imput[,c(3,4,9,10)] <- titanic_imput_numer

#Next we need to normalize the values:
titanic_imput_norm <- as.data.frame(lapply(titanic_imput[c(3,4,5,6,8,9,10)], normalize))
summary(titanic_imput_norm)
titanic_imput[,c(3,4,5,6,8,9,10)] <- titanic_imput_norm
titanic_imput$Survived <- as.factor(titanic_imput$Survived)

#Training the knn() function
train <- sample(1:nrow(titanic_imput), 600)
titanic_train <- titanic_imput[train,]
titanic_test <- titanic_imput[-train,]
titanic_train_labels <- titanic_train$Survived
titanic_test_labels <- titanic_test$Survived
library(class)
library(gmodels)
titanic_test_knn <- knn(train = titanic_train[,-1], test = titanic_test[,-1], cl = titanic_train_labels, k = 10)
CrossTable(x = titanic_test_labels, y = titanic_test_knn, prop.chisq = FALSE)

#model accuracy check:
data.frame(titanic_test[,c(1,2)], titanic_test_knn)
titanic_test$PassengerId[which(titanic_test[,2] != titanic_test_knn)]
#Using these two commands, I can say that my model is very accurate!!!

##Testing Set:
titanic_solution <- read.csv("test.csv", stringsAsFactors = TRUE)
dim(titanic_solution)
colnames(titanic_solution)
summary(titanic_solution)#Fare has one na value and age has 86. We will need to impute new values into these rows for the knn algorithm to work.
titanic_solution <- titanic_solution[,c(-3, -8)]

#impute step:
#The age variable:
aggregate(data = titanic_solution, Age ~ Pclass, mean, na.rm = TRUE)
ave_age <- ave(titanic_solution$Age, titanic_solution$Pclass, FUN = function(x) mean(x, na.rm = TRUE))
titanic_solution$Age <- ifelse(is.na(titanic_solution$Age), ave_age, titanic_solution$Age)
#the fare variable:
aggregate(data = titanic_solution, Fare ~ Pclass, mean, na.rm = TRUE)
ave_fare <- ave(titanic_solution$Fare, titanic_solution$Pclass, FUN = function(x) mean(x, na.rm = TRUE))
titanic_solution$Fare <- ifelse(is.na(titanic_solution$Fare), ave_fare, titanic_solution$Fare)

#Transforming all factor strings into numeric values:
titanic_solution_numer <-as.data.frame(lapply(titanic_solution[c(3,8,9)], FUN = as.numeric))
titanic_solution[,c(3,8,9)] <- titanic_solution_numer

#Normalize the values for the knn() function:
titanic_solution_norm <- as.data.frame(lapply(titanic_solution[c(2:9)], normalize))
summary(titanic_solution_norm)
titanic_solution[,c(2:9)] <- titanic_solution_norm

##simulation knn() nonparametric machine learning technique:
titanic_solution_pred <- knn(train = titanic_train[,c(-1,-2)], test = titanic_solution[,-1], cl = titanic_train_labels, k = 10)

titanic_solution_df <- data.frame(PassengerId = titanic_solution[,1], Survived = titanic_solution_pred)

write.table(x = titanic_solution_df, file = "/Users/masonkarsevar/Desktop/rworks/titanic_solution.csv", sep = ",", na = "!", row.names = F)

