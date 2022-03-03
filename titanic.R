#============================ Load raw data=====================================
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# adding survived column to the test
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# combining the data sets
data.combined <- rbind(train, test.survived)

# structure of the combined data sets
str(data.combined)

# converting char to factor because ml algorithms doesn't like char
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Sex <- as.factor(data.combined$Sex)
data.combined$Embarked <- as.factor(data.combined$Embarked)
data.combined$Cabin <- as.factor(data.combined$Cabin)
data.combined$Name <- as.factor(data.combined$Name)
data.combined$Ticket <- as.factor(data.combined$Ticket)
train$Pclass <- as.integer(train$Pclass)
# take a look at gross survival rate
table(data.combined$Survived)

# distribution across classes
table(data.combined$Pclass)




# load ggplot2 to see the visual distribution
library(ggplot2)

# Hypothesis - Rich folks had better survival rate
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar() +
  xlab("Population Class") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Number of Unique names
length(unique(data.combined$Name))

# Two duplicate names, take a closer look
#  Get and Store the duplicate names as vector
dup.names <- as.character(
  data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Look at variables of the duplicates and the original to verify its a duplicate
data.combined[which(data.combined$Name %in% dup.names),]
                                          
# What's up with the Mr and Mrs thing 
library(stringr)

# Any co-relation with other variables (e.g, sibsp)?
misses <- data.combined[which(str_detect(as.character(
  data.combined$Name),"Miss.")),]
misses[1:5,]

# Hypothesis - Title name co-relates with the age
mrses <- data.combined[which(str_detect(as.character(
  data.combined$Name),"Mrs.")),]
mrses[1:5,]

# Check out males to see if the pattern continues 
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]



# Expanding on the relationship between the Survived and the Pclass by adding
# Title variable to the
# data set and then explore the potential 3-dimensional relationship

# Create an utility function to extract Title
extractTitles <- function(Name) {
  Name <- as.character(Name)

  if(length(grep("Miss.", Name) > 0)) {
    return("Miss.")
  } else if(length(grep("Mrs.", Name) > 0)) {
    return("Mrs.")
  } else if(length(grep("Master.", Name) > 0)) {
    return("Master.")
  } else if(length(grep("Mr.", Name) > 0)) {
    return("Mr.")
  } else {
    return("Other")
  }
}

Title <- NULL

for (i in 1:nrow(data.combined)) {
  Title <- c(Title, extractTitles(data.combined[i, "Name"]))
}

data.combined$Title <- as.factor(Title)






# Since we only have survived label for the train data set
# use the first 891
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Population Class")

# Whats the distribution of females to the male in train & test
table(data.combined$Sex)

# Visualize the 3-way relationship of sex, Pclass and survival and analysis

ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Population Class")

# Its seems that age and sex is important for the survival from the analysis
# Lets look at the distribution of the sex in the data.combined
summary(data.combined$Age)
summary(data.combined[1:891, "Age"])


# Just to be through, take a look at the survival rates in relation with 
# Pclass, sex and age 
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_bar(width = 5)

# Validate the master Title as a good proxy for the boys
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

# Lets look at the "miss" we know its complicated, examine further
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_bar(width = 5) +
  ggtitle("Misses by Class")

# Misses have different survival rates, could be a candidate for feature engg
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
length(misses.alone[which(misses.alone$Age <= 14.5),])

# Summary of the Siblings&Spouses
summary(data.combined$SibSp)
# Can we treat SibSp as factor
length(unique(data.combined$SibSp))
data.combined$SibSp <- as.factor(data.combined$SibSp)

# Visualize the survival rate of SibSp with Pclass and Title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Class, Tittle")
  
# Visualize the survival rate of Parch with Pclass and Title
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Class, Tittle")

# Lets try some feature engineering, what about creating family size feature
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$Familysize <- as.factor(temp.parch + temp.sibsp + 1)

# Visual it and see if the family size is predictive for Pclass and Title
ggplot(data.combined[1:891,], aes(x = Familysize, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Class, Tittle")


# Take a look at the Ticket variable, see if its predictive
str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

# Lets take the first character of the ticket and see if there is a pattern
ticket.firstch <- ifelse(data.combined$Ticket == "", " ",
                         substring(data.combined$Ticket, 1, 1))
unique(ticket.firstch)

# Convert as factor back to visualize in the plot
data.combined$ticket.firstch <- as.factor(ticket.firstch)
# Lets visualize the ticket.firstch for survival rate
ggplot(data.combined[1:891,], aes(x = ticket.firstch, fill = Survived)) +
  geom_bar(width = 0.5) 

# Ticket seems like it might be predictive
# Lets visualize it with Pclass and Title if it has a pattern
ggplot(data.combined[1:891,], aes(x = ticket.firstch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Class, Tittle")

# Next up fare, lets see if fare is predictive
str(data.combined$Fare)
summary(data.combined$Fare)
length(unique(data.combined$Fare))

# Visualize the fare across people
ggplot(data.combined, aes(x = Fare)) +
  geom_bar(width = 5) +
  ggtitle("Fare Distribution")
# Visualize fare with Pclass
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Class, Tittle")


# Lets take a look at cabin variable
str(data.combined$Cabin)
# Convert it as Character because too many factors and its useless
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]
# Looks like the first letter means something and there are also multiple cabin
# No cabin label may refer 3rd class and multiple refer A class
# Replace empty string with u for unknown
data.combined$Cabin[which(data.combined$Cabin == "")] <- "U"
data.combined$Cabin[1:100]
# Introduce new variable of cabin with its first character
data.combined$cabin.firstch <- as.factor(substr(data.combined$Cabin, 1, 1))
 
# Plot the variable cabin.firstch and see if there is pattern in survival rate
ggplot(data.combined[1:891,], aes(x = cabin.firstch, fill = Survived)) +
  geom_bar(width = .5) +
  ggtitle("Survival by Cabin.firstch")
# Plot for Survival rate for cabin.firstch with Pclass
ggplot(data.combined[1:891,], aes(x = cabin.firstch, fill = Survived)) +
  geom_bar(width = .5) +
  facet_wrap(~Pclass) +
  ggtitle("Survival by Cabin.firstch with Class")
# Plot for Survival rate for cabin.firstch with Pclass and Title
ggplot(data.combined[1:891,], aes(x = cabin.firstch, fill = Survived)) +
  geom_bar(width = .5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Survival by Cabin.firstch with Class and Title")

# What about the folks with multiple cabins
data.combined$cabin.multiple <- as.factor(
  ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))
# Plot the variable to see the survival rate
ggplot(data.combined[1:891,], aes(
  x = cabin.multiple, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title)


# Lets see if where you boarded on titanic has to do with the survival rate
str(data.combined$Embarked)
# Plot them against Pclass
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass)

#
#
#============================Exploratory Modelling==============================
#
#
#

library(randomForest)

# Train a random forest with default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.lable <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.lable,
                     importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Train a random forest model using pclass, title & sibsp
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.lable, 
                     importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Train a random forest model using pclass, title & parch
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.lable, 
                     importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# Train a random forest model using pclass, title, parch & sibsp
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "Parch", "SibSp")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.lable, 
                     importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

# Train a random forest model using pclass, title & familysize
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "Familysize")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.lable, 
                     importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

# Train a random forest model using pclass, title, sibsp & familysize
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Familysize")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.lable, 
                     importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)
# Familysize has info of sibsp init, so sibsp isn't inclusively doing anything

# Train a random forest model using pclass, title, sibsp & familysize
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "Familysize")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.lable, 
                     importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)
# Familysize has info of parch init, so parch isn't inclusively doing anything


#
#
#=============================Cross Validation==================================
#
#

# before increasing the accuracy of the model, we need to see if we aren't 
# over-fitting the train data, for that estimate model error rate on test 
# data-set(i.e., unknown data). For this lets submit the best model rf.5 to
# kaggle to see if the OOB error is accurate.

# Subset our test data and features
test.submit.df <- data.combined[892:1309, c("Title", "Pclass", "Familysize")]

# Predict the test data using model rf.5 and test dataframe
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write out a csv file for kaggle submission
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20211125_01.csv", row.names = FALSE)

# Our Submission score was 0.77990 and the our accuracy was 0.8182

# Lets look into cross-validation using the caret package to see if we 
# get more accurate estimates.


library(caret)
library(doSNOW)

# Research has shown that 10-fold CV repeated 10 times is the best place to
# start, however there are no hard and fast rules - this is where the experience
# of the Data Scientist comes into play. We'll start with 10-fold CV, repeated 
# 10 times and see how it goes.

# Leverage caret to create 100 total folds, but ensure that the ratio of these 
# that survived and perished in each fold matches the overall training set. This
# is known as stratified validation and generally provides better results.

set.seed(2372)
cv.10.folds <- createMultiFolds(rf.lable, k = 10, times = 10)

# Check Stratification
table(rf.lable)
342/549
# grab a single point in the 100
table(rf.lable[cv.10.folds[[72]]])
307/494
table(rf.lable[cv.10.folds[[27]]])
308/494
table(rf.lable[cv.10.folds[[19]]])
308/494

# set up caret's trainControl object per above
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)

# set up doSNOW package for multi-core training. This is helpful as we're 
# going to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# set seed and train
set.seed(32234)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.lable, method  = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

# Shutdown Cluster
stopCluster(cl)

# Check out results
rf.5.cv.1

# The above is only slightly more pessimistic than than rf.5 OOB prediction,
# not pessimistic enough, because in competition it scored only 0.77
set.seed(3378237)
cv.5.folds <- createMultiFolds(rf.lable, k = 5, times = 10)

# Train control for the above 50fold index
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)
# doSNOW
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# now seed and train 
set.seed(378322)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.lable, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)
# Stop the cluster
stopCluster(cl)

rf.5.cv.2

# looks like 5 fold is not pessimistic as well
# and from kaggle the knowledge is the cross-validation train and test 
# proportion needs to mimic the data-set train and test proportion
set.seed(3378237)
cv.3.folds <- createMultiFolds(rf.lable, k = 3, times = 10)

# Train control for the above 30fold index
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)
# doSNOW
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# now seed and train 
set.seed(378322)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.lable, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.3)
# Stop the cluster
stopCluster(cl)

rf.5.cv.3


#
#
#
#==========================Exploratory Modeling 2==============================
#
#
#
#

# installed.packages(rpart)
library(rpart)
library(rpart.plot)
# lets use 10fold CV because its better than the 3 fold we expect it to
# create a utility function
rpart.cv <- function(seed, training, labels, ctrl){
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  # Leverage formula interface for training
  set.seed(seed)
  rpart.cv <- train(x = training, y = labels, method = "rpart",
                    tuneLength = 100, trControl = ctrl)
  # shutdown cluster
  stopCluster(cl)
  
  return(rpart.cv)
}

# Grab features, use c() to combine values to create a list or a vector
features <- c("Pclass", "Title", "Familysize")
rpart.train.1 <- data.combined[1:891, features]

# run the CV and check the results
rpart.cv.1 <- rpart.cv(230239, rpart.train.1, rf.lable, ctrl.3)

# to get final model
rpart.cv.1$finalModel
# plot 
prp(rpart.cv.1$finalModel, under = TRUE, extra = 1)

# The plot bring out some interesting lines of investigation, Namely:
#       1. Titles of "Mr." and "Other" are predicted to perish with 
#          83.2 % accuracy.
#       2. Titles of "Master.", "Miss." & "Mrs." in 1st & 2nd class are 
#           predicted to survive with overall accuracy of 94.9%
#       3. Titles of "Master.", "Miss." & "Mrs." in 3rd class with family size
#          of 5, 6, 7, 8, 11 are predicted to perish with 100% accuracy
#       4. Titles of "Master.", "Miss." & "Mrs." in 3rd class with family
#          size of anything other than 5,6,8,11 are predicted to survive
#          with accuracy of 60%

# Both rpart and rf cofirms that title is important, lets investigate 
table(data.combined$Title)
# there is something in the title variable "Other"
# parse out the last name and title
name.splits <- str_split(data.combined$Name, ",")
name.splits[7]
lastnames <- sapply(name.splits, "[", 1)
lastnames[1:8]

# add lastnames to data just incase if we needed it 
data.combined$lastnames <- lastnames
# we need to extract the thing in between last name and first name
# now for title

name.splitss <- str_split(sapply(name.splits, "[", 2), " ")
data.combined$titles <- sapply(name.splitss, "[", 2)
unique(titles)

# whats the "the" title
data.combined[which(titles == "the"),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Don.", "Jonkheer.")] <- "Sir."
titles[titles %in% c("Capt.", "Col.", "Major.")] <- "Officer"
table(titles)

data.combined$titles <- as.factor(titles)

# visualize the new titles 
ggplot(data.combined[1:891,], aes(x = titles, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survived rate of titles by Pclass")

# now just make those special titles regular titles according to the correct sex
data.combined$titles[titles %in% c("Dr.", "Officer", "Rev.", "Sir.")]  <- "Mr."
data.combined$titles[titles == "Lady."] <- "Mrs."

# Now to fix if there were any "female" Dr. or Officer
indexof <- which(data.combined$Sex == "female" & data.combined$titles == "Mr.")
data.combined$titles[indexof,] <- "Mrs."
data.combined$newtitles <- as.character(titles)


# visualize the new titles like before
ggplot(data.combined[1:891,], aes(x = titles, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survived rate of titles by Pclass")

# Grab the features
featuresnew <- c("Pclass", "titles", "Familysize")
rpart.train.2 <- data.combined[1:891, featuresnew]
# run cv and check out the results
rpart2.cv1 <- rpart.cv(29823, rpart.train.2, rf.lable, ctrl.3)
rpart2.cv1

# plot
prp(rpart2.cv1$finalModel, type = 0, extra = 1, under = TRUE)



# Lets take a look at the 1st class "Mr."
indexc1mr <- which(data.combined$Pclass == "1" & data.combined$titles == "Mr.")
c1mr <- data.combined[indexc1mr,]
summary(c1mr)

# lets look at the survived ones
summary(c1mr[c1mr$Survived == "1",])
View(c1mr[c1mr$Survived == "1",])

# llets look at the same of the high fares
index <- which(data.combined$Ticket == "PC 17755"
               | data.combined$Ticket == "PC 17611"
               | data.combined$Ticket == "113760")
View(data.combined[index,])

# lets see the survival rates for the 1st class Mr. by fare
ggplot(c1mr, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class Mr. Survival rates by Fare")

# Engineer feature with same fare size
ticketsize <- rep(0, nrow(data.combined))
avg_fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  currentT <- tickets[i]
  partyindex <- which(data.combined$Ticket==currentT)
  curravgfare <- data.combined[partyindex[i], "Fare"]/ length(partyindex)

for (j in 1:length(partyindex)){
  ticketsize[partyindex[j]] <- length(partyindex)
  avg_fare[partyindex[j]] <- curravgfare
  }
}

data.combined$avg_fare <- avg_fare
data.combined$ticketsize <- ticketsize
