
# Open the packages necessary
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


# Import and store the data
train <- read.csv("~/Desktop/R Project/Kaggle/train.csv", stringsAsFactors = TRUE)
test <- read.csv("~/Desktop/R Project/Kaggle/test.csv", stringsAsFactors = TRUE)

# Create a decision tree using all variables
set.seed(10)
my_tree <- rpart(QuoteConversion_Flag ~.,train, method = "class")


# Use a fancy plot
fancyRpartPlot(my_tree)


# Use the printcp function to print out the error under varrying levels of the complexity parameter 
cp <- printcp(my_tree)

# Predict the outcome of the model using the prediction matrix
pred <- predict(my_tree, train, type = "class")

# Create a confusion matrix 
conf <- table(train$QuoteConversion_Flag, pred)

# Test the accuracy
acc <- sum(diag(conf))/sum(conf)

# Clone the train data set to use for feature induction
train.FI <- train


# Count the number of 'na' values per column
na_count <- sapply(train, function(y) sum(length(which(is.na(y)))))


# Create a data frame of the count of NA's per column
na_count <- data.frame(na_count)


# Return the names of the variables where na_count is greater than 0
names_na <- names(train)[which(na_count >0)]
number_na <- na_count[na_count > 0,]
blanks_matrix <- rbind(names_na, number_na)
blanks_matrix


# Transform NAs in train.FI PropertyField29 to 0's.  O's are choose as this 
# is the mode of the feature column
na.indexPF29 <- which(is.na(train.FI$PropertyField29))
train.FI$PropertyField29[na.indexPF29] <- -1

# Transform NAs in train.FI in PersonalField84 to 0's. 0's are chosen as this
# is the mode of the feature column
na.indexPF84 <- which(is.na(train.FI$PersonalField84))
train.FI$PersonalField84[na.indexPF84] <- -1

# Use train.FI in a new decision tree model
my.treeFI <- rpart(QuoteConversion_Flag ~ ., train.FI, method = "class")

# Predict the outcome of the model using the prediction 
predFI <- predict(my.treeFI, train.FI, type = "class")

# Create a confusion matrix 
confFI <- table(train.FI$QuoteConversion_Flag, pred)

# Test the accuracy
accFI <- sum(diag(confFI))/sum(confFI)

# Look at the cp matrix to determine the 10 parts cross validation 
cpFI <- printcp(my.treeFI)








 
                           





