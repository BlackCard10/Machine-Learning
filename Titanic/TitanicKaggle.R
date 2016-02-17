
# Load the necessary packages
library(dplyr)
library(randomForest)
library(rpart)
library(ggplot2)


# Set the seed
set.seed(111)


# Import the testing data and the training data
test <- read.csv("~/Desktop/R Project/Machine-Learning/Titanic/Test-Titanic.csv")
train <- read.csv("~/Desktop/R Project/Machine-Learning/Titanic/Train-Titanic.csv")
test$Survived <- 0 


# Check for missing data
check.missing <- function(x) return(length(which(is.na(x))))
data.frame(sapply(train,check.missing))
data.frame(sapply(test,check.missing))


# Combine the test and train
combined <- rbind(train, test)

# Set the values of blanks in embarked category.  We'll use Southhampton because most people
# Embarked from there
table(combined$Embarked)
combined$Embarked[which(combined$Embarked == "")] <- "S"


# Passenger on row 1044 has NA for Fare.  We'll set this to mean Fare paid.  
combined$Fare[which(is.na(combined$Fare))] <- mean(combined$Fare, na.rm = TRUE)



# Convert names column to characters
combined$Name <- as.character(combined$Name)


# Take the titles out from the names
combined$Title<-sapply(combined$Name,function(x) strsplit(x,'[.,]')[[1]][2])
combined$Title<-gsub(' ','',combined$Title)
combined$Title[combined$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined$Title[combined$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combined$Name <- as.factor(combined$Name)
combined$Title <- as.factor(combined$Title)



# Find all the factor elements in train
factor_index <- sapply(train, function(x) is.factor(x))


# Subset train by not factors
cor_sub <- train[,!factor_index]


# Return only the complete cases of cor_sub
cor_sub2 <- cor_sub[complete.cases(cor_sub),]


# Change Age column to a numeric in cor_sub2
cor_sub2$Age <- as.numeric(cor_sub2$Age)


# Find a correlation matrix of cor_sub2
cor(cor_sub2)


# Calculate the mean age by gender to predict missing ages.  
# Replaced: See decision tree method below.  
#train %>% select(Age, Sex) %>% group_by(Sex) %>% summarise(mean(Age, na.rm = TRUE))


# maleAge <- combined$Sex == "male" & is.na(combined$Age) == TRUE
#		combined[maleAge, "Age"] <- 31.00
		

# femaleAge <- combined$Sex == "female" & is.na(combined$Age) == TRUE 
#		combined[femaleAge, "Age"] <- 28.00


# Use a decision tree in order to compute the missing values in Age
ageTree <- rpart(Age ~ PassengerId + Survived + Pclass + Sex + SibSp + Parch + Fare + Embarked, 
			data = combined[!is.na(combined$Age),], method = "anova") 
agePrediction <- predict(ageTree, combined[is.na(combined$Age),])
combined$Age[which(is.na(combined$Age))] <- agePrediction
		 
		 
# Family size column
combined$familySize <- combined$Sibsp + combined$Parch


# Mother Column
combined$Mother <- 0 # Initialize an empty new column
motherTest <- combined$Sex == "female" & combined$SibSp != 0 & combined$Age >= 18
	combined[motherTest, "Mother"] <- 1	
  

# Split the combined into test_new and train new
train_new <- combined[1:891,]
test_new <- combined[892:1309,]
test_new$Survived <- NULL
 

# Create a decision tree model using imputed Age and WC column
# myTree <- rpart(Survived ~ PassengerId + Pclass + Sex + Age + SibSp + Parch + 
#                           Fare + Embarked + Mother, data = train_new, method = "class") 

# Create a random forest model using imputed Age, Mother and title column
myForest <- randomForest(as.factor(Survived) ~ PassengerId + Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Mother + Title, data = train_new, importance = TRUE, ntree=500) 

# Create a plot of the importance of each element to the random forest
imp <- importance(myForest, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

# View the Decision Tree
#fancyRpartPlot(myTree)


# Predict the outcome
my_prediction <- predict(myForest, test_new)


# Create a dataframe with my solution and passengerid
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)


# Write a csv with the solution 
write.csv(my_solution, file = "2RandomForest.csv", row.names = FALSE)





