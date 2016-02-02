
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


# Clone the train data set to use for feature testing
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


# Transform NAs in train.FI PropertyField29 to 0's.  O's are choosen as this 
# is the mode of the feature column
na.indexPF29 <- which(is.na(train.FI$PropertyField29))
train.FI$PropertyField29[na.indexPF29] <- 0

# Transform NAs in train.FI in PersonalField84 to 0's. 0's are chosen as this
# is the mode of the feature column
na.indexPF84 <- which(is.na(train.FI$PersonalField84))
train.FI$PersonalField84[na.indexPF84] <- 0

# Use train.FI in a new decision tree model
my.treeFI <- rpart(QuoteConversion_Flag ~ ., train.FI, method = "class")


# Look at the cp matrix to determine the 10 parts cross validation 
cpFI <- printcp(my.treeFI)



# Find out which factor levels are unequal.  

factorCheck <- function(x,y) {

	for(attr in colnames(x)) {

    	if (is.factor(x[[attr]])) {
    
       	 new.levels <- setdiff(levels(x[[attr]]), levels(y[[attr]]))
        
        	if ( length(new.levels) == 0 ){ 
        		print(paste(attr, '- no new levels')) 
        
         	} else {
         
            	print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
        
        	}
    	}
	}
}

# Replace unequal value types between test and train.   

test$PersonalField16[which(test$PersonalField16 == "XG")] <- NA 
test$PersonalField16[which(test$PersonalField16 == "YG")] <- NA 
test$PersonalField16[which(test$PersonalField16 == "ZM")] <- NA 
                           
test$PersonalField17[which(test$PersonalField17 == "XF")] <- NA
test$PersonalField17[which(test$PersonalField17 == "XZ")] <- NA
test$PersonalField17[which(test$PersonalField17 == "YO")] <- NA
test$PersonalField17[which(test$PersonalField17 == "ZJ")] <- NA

test$PersonalField18[which(test$PersonalField18 == "XB")] <- NA

test$PersonalField19[which(test$PersonalField19 == "ZS")] <- NA


test$PropertyField5[which(test$PropertyField5 == "")] <- "Y"  # This is the mode of PF5


test$PropertyField7[which(test$PropertyField7 == "T")] <- NA


test$PropertyField30[which(test$PropertyField30 == "")] <- "N" # This is the mode of PF30


test$PropertyField37[which(test$PropertyField37 == " ")] <- "N" # This is the mode of PF37



# Find the optimal 'cp' from print.cp using the best version of tree.  
Optimal_CP <- my_tree$cptable[which.min(my_tree$cptable[,"xerror"])]

# View the CP with the lowest relative error 
plotcp(my_tree)


# Prune the tree using the optimal CP
my_treePruned <- prune(my_tree, cp = my_tree$cptable[which.min(my_tree$cptable[,"xerror"])])

# Plot pruned tree. Notice that this is the tree given initially before pruning.  
fancyRpartPlot(my_treePruned)

# Use the model to Predict the test data
Prediction <-predict(my_tree, test, type = "class")

# Form a submission dataframe for Kaggle
Submission <- data.frame(QuoteNumber = test$QuoteNumber, QuoteConversion_Flag = Prediction)s

# Write a csv that contains all submission
write.csv(Submission, file = "firstSubmission.csv", colnames = TRUE) 


 



 
                           





