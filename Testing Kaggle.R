
# Open the packages necessary
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


# Import and store the data
train <- read.csv("~/Desktop/Kaggle /Homesight Data/train.csv")
test <- read.csv("~/Desktop/Kaggle /Homesight Data/test.csv")

# Create a decision tree using all variables
set.seed(10)
my_tree <- rpart(QuoteConversion_Flag ~.,train, method = "class")

# Use a fancy plot
fancyRpartPlot(my_tree)

# Count the number of 'na' values per column
na_count <- sapply(train, function(y) sum(length(which(is.na(y)))))

# Create a data frame of the count of NA's per column
na_count <- data.frame(na_count)

# Return the names of the variables where na_count is greater than 0
names_na <- names(train)[which(na_count >0)]
number_na <- na_count[na_count > 0,]
blanks_matrix <- rbind(names_na, number_na)
blanks_matrix

# Create a confusion matrix on the data and pull out the accuracy
cp <- printcp(my_tree)

# Predict the outcome of the model using the prediction 
pred <- predict(my_tree, train, type = "class")

# Create a confusion matrix 
conf <- table(train$QuoteConversion_Flag, pred)

# Test the accuracy
acc <- sum(diag(conf))/sum(conf)

# Clone the train data set to use for feature engineering
train.FI <- train

# Fit a linear model to predict the columns containing significant # of NAs
lm.fitPF84 <- lm(PersonalField84 ~ ., data = train.FI)
lm.fitPF29 <- lm(PropertyField29 ~. , data = train.FI)
summary(lm.fitPF84)
summary(lm.fitPF29)





 
                           





