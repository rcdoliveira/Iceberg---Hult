###Load required packages###
#AUC
if(!require("AUC")){
  install.packages('AUC',
                   repos="https://cran.rstudio.com",
                   quiet = TRUE)
}

#RF
if(!require("randomForest")){
  install.packages('randomForest',
                   repos="https://cran.rstudio.com",
                   quiet = TRUE)
}
require(ggplot2)

#Load data
train <- read.csv(file.choose(),sep=";",stringsAsFactors = F)
head(train)
str(train)

#Clean data in train dataset
train$PassengerId <- NULL
train$Cabin <- NULL
train$Ticket <-NULL

#Clean Age and insert random numbers to missing Ages
mean <- round(mean(train$Age, na.rm= TRUE))
sd <- round(sd(train$Age, na.rm= TRUE))
train$Age[is.na(train$Age)]<- round(rnorm(1,mean,sd))

#Adjusting title and cleaning name
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
train$Name <-NULL

#Assigning numbers to title
table(train$Title,train$Survived)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

#  reassign mlle, ms, and mme accordingly
train$Title[train$Title == 'Mlle']        <- 'Miss' 
train$Title[train$Title == 'Ms']          <- 'Miss'
train$Title[train$Title == 'Mme']         <- 'Mrs' 
train$Title[train$Title %in% rare_title]  <- 'Rare Title'

table(train$Title,train$Survived)
train$Title <- ifelse(train$Title == "Mr", 0, ifelse(train$Title== "Rare Title", 1, ifelse(train$Title== "Master",2,3)))

#Creating the Family Size variable
train$Fsize <- train$SibSp + train$Parch + 1
train$Fsize <- train$Fsize/max(train$Fsize)

#Adding missing values to Embarked
train$Embarked[train$Embarked == ""]<- "S"

#Normalizing categories Sex and Embarked
train$Sex<- ifelse(train$Sex=="female",1,0)
train$Embarked <- ifelse(train$Embarked == "C", 1, ifelse(train$Embarked== "S", 2,3))

#Creating variable for training model and cleaning data of Survived
ytrain <- train$Survived
train$Survived <- NULL

str(train)
train$Fare <- NULL

#train$Fare <- train$Fare/max(train$Fare)
train$Age <- train$Age/max(train$Age)
train$Pclass <- train$Pclass/max(train$Pclass)


# CLEANING TEST DATA
test <- read.csv(file.choose(),sep=";",stringsAsFactors = F)
head(test)
str(test)


#Clean data
test$Cabin <- NULL
test$Fare <- NULL

#Clean Age
mean <- round(mean(test$Age, na.rm= TRUE))
sd <- round(sd(test$Age, na.rm= TRUE))
test$Age[is.na(test$Age)]<- round(rnorm(1,mean,sd))

#Creating Family Size for test
test$Fsize <- test$SibSp + test$Parch + 1
test$Fsize <- test$Fsize/max(test$Fsize)


test$Embarked[test$Embarked == ""]<- "S"


test$Sex<- ifelse(test$Sex=="female",1,0)
test$Ticket <-NULL
test$Embarked <- ifelse(test$Embarked == "C", 1, ifelse(test$Embarked== "S", 2,3))

#test$Fare <- train$Fare/max(train$Fare)
test$Age <- test$Age/max(test$Age)
test$Pclass <- test$Pclass/max(test$Pclass)

#Adjusting title and cleaning name
test$Title <- gsub('(.*, )|(\\..*)', '', test$Name)
test$Name <-NULL

#Assigning numbers to title
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

#  reassign mlle, ms, and mme accordingly
test$Title[test$Title == 'Mlle']        <- 'Miss' 
test$Title[test$Title == 'Ms']          <- 'Miss'
test$Title[test$Title == 'Mme']         <- 'Mrs' 
test$Title[test$Title %in% rare_title]  <- 'Rare Title'

test$Title <- ifelse(test$Title == "Mr", 0, ifelse(test$Title== "Rare Title", 1, ifelse(test$Title== "Master",2,3)))


#Creating random forest model
rFmodel <- randomForest(factor(ytrain) ~ Pclass + Sex + Age + Parch + SibSp + Embarked + Title + Fsize, data = train, importance=TRUE)

#Predicting
result <- predict(rFmodel,test)
  

imp <- importance(rFmodel, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#1D188C") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

#Exporting Data
solution <- data.frame(PassengerID = test$PassengerId, Survived = result)
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
