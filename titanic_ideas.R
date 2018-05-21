##Titanic problem:
titanic <- read.csv("train.csv", stringsAsFactors = FALSE)
dim(titanic)#this is rather a small dataset. It only has 891 rows and 12 columns. 

#Let's look at the trends between each column (or rather variable).
pairs(titanic)#Hard to discern any trends within the dataset at this early of a stage. 
colnames(titanic)#We can most likely take out name (since that isn't really relevent for the binary classifier that we will be creating) and passengerID (since it is partially irrelevent for the problem at hand). 
#The variables parch and sibsp can be taken out of the dataset as well during the modeling stages but I will leave them in the analysis for now.

pairs(titanic[,c("Survived","Pclass","Sex","Age","SibSp","Ticket","Fare","Cabin","Embarked")])
boxplot(titanic[,"Survived"], titanic[,"Fare"])
library(tidyverse)

titanic %>% 
	group_by(Pclass) %>%
	count(Survived)#Just as I suspected class 3 did have more deaths than all the other classes (incidentally), but from a population standpoint they did make up most of the ship's passengers. 
	
titanic %>% 
	group_by(Pclass) %>%
	count() -> titanic_count 
	
titanic_count[,2]/nrow(titanic)#Class 1 only makes up 0.242 percent, 2nd 0.20, and 3rd 0.551 

titanic %>% 
	group_by(Survived, Pclass) %>%
	count(Sex)#Wow most of the people who died in the wreck were males and the 3rd class made up most of the deaths with regards to gender lines. Hence gender can be regarded as a large determinant with Pclass. 
##glm() function first:
titanic_fit <- glm(Survived ~ Pclass + Age + SibSp + Fare + Cabin + Sex + Embarked, data = titanic, family = binomial)
summary(titanic_fit)#According to the model Cabin location doesn't really have much statistical significance while the variables Pclass, Age, SibSp, and Sex all have p-value well under 0.05. 
#And so I will create a new model with only these variables. Even with that said though, I need to keep an eye on CabinC22 and c26 as this is the only level in the Cabin variable that goes below 0.05. 

titanic_fit2 <- glm(Survived ~ Pclass + Age + SibSp + Sex, data = titanic, family = binomial)
summary(titanic_fit2)
titanic.probs <- predict(titanic_fit2, type = "response")
titanic_pred = rep("Died", nrow(titanic))
titanic_pred[titanic.probs > 0.5] <- "Survived"
table(titanic_pred, titanic[,"Survived"])
(342 + 128) / nrow(titanic)#Wow that's really bad, the logistical model only achieved a success rate of 0.527. This is really just as bad as guessing. And this isn't even the testing prediction rate. 

##QDA:
library(MASS)
titanic_qda <- qda(Survived ~ Pclass + Age + SibSp + Sex, data = titanic)
titanic_qda
qda.class <- predict(titanic_qda, titanic[,-2])$class
table(qda.class, titanic[,2])
(360 + 213) / nrow(titanic)# A success rate of 0.643 percent. this is a little better but not that great. One thing that this illustrates about the dataset is that it's quadratic.

#Since the QDA method fixed the success rate by almost 10 percent, I will need to look into quadratic classifier models. the main question is which variable has a none linear relationship with the response varible?

##Support Vector Machine experiment with a polynomial transformation:
library(e1071)
set.seed(123)
titanic.svm <- svm(Survived ~ ., data = titanic.train, kernel = "polynomial", degree = 2)
titanic.svm$index 
titanic.pred <- predict(titanic.svm, titanic.train)
table(predict = titanic.pred, truth = titanic.train[,2])

##K-nearest neighbors:
summary(titanic)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- factor(titanic$Survived, levels = c(0,1), labels = c("D","S"))
#Changing the following variables to factor so that the knn algorithm can create an appropriate decision boundary.
titanic_sub <- titanic[,c("Survived","Pclass","Sex","Age","Fare")]# I have a feeling that subsetting the data to these four predictor veriables is the best move
summary(titanic.train)#Before using the knn algorithm the Age, ticket, and Fare variables will have to be normalized.
normalize <- function(x){
	return((x-min(x))/(max(x) - min(x)))
}#this function normalizes the data to a -1 to 1 scale. Another alternative is the scale() function which can convert data into z-scores. 
which(is.na(titanic_sub[,c("Age","Fare")]))#will use this command to remove the na values from the age variable column. It seems that "Age is the only variable in the dataset with na values. 
na_remove <- which(is.na(titanic_sub[,c("Age","Fare")]))
titanic_sub <- titanic_sub[-na_remove,]
titanic_sub_new <- titanic_sub
#now let's normalize both the Age and the Fare variables.
titanic_sub_norm <- as.data.frame(lapply(titanic_sub[c("Age","Fare")], normalize))
titanic_sub[,c("Age","Fare")] <- titanic_sub_norm[,c("Age", "Fare")]
#Now everything is ready for the knn() algorithm 
#the age and fare variables are now normalized. I really hope this works.

#Data paritions between a validation set and a training set:
train <- sample(1:nrow(titanic_sub), 500)
titanic_sub_train <- titanic_sub[train,]
titanic_sub_test <- titanic_sub[-train,]
titanic_train_labels <- titanic_sub_train$Survived
titanic_test_labels <- titanic_sub_test$Survived

library(class) #Will use the knn() algorithm within the class package.
titanic.knn1 <- knn(train = titanic_sub_train, test = titanic_sub_test, cl = titanic_train_labels, k = 2)
