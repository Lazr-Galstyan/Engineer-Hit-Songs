# Get the data
data <- read.csv("data.csv")
source("DataAnalyticsFunctions.R")

# Download the packages
install.packages("CART")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tree")
install.packages("partykit")
install.packages("randomForest")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("class")
install.packages("pROC")
install.packages("mlbench")
install.packages("ggplot2")
install.packages("GGally")
install.packages("plyr")
install.packages("car")
install.packages("MASS")
install.packages("clusterGeneration")
install.packages("glmnet")
install.packages("cv.glmnet")
install.packages("corrplot")
library(corrplot)
library(MASS)
library(clusterGeneration)
library(car)
library(plyr)
library(GGally)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(CART)
library(tree)
library(partykit)
library(randomForest)
library(rpart.plot)
library(rpart)
library(caret)
library(class)
library(pROC)
library(mlbench)
library(glmnet)
library(cv.glmnet)

# Turning off scientific numbers
options(scipen = 999) 

##### 1. DATA CLEANING #####
# Characteristics of the data
head(data)
dim(data)
tail(data)

# Remove the X column
data <- data %>%
  dplyr::select(-X)
head(data) 
dim(data) # 1963 rows 19 columns

# Join the song_title and artist coulmns
data$song_artist <- paste(data$artist,"-",data$song_title)
head(data)

# Select the distinct rows
data <- distinct(data, song_artist, .keep_all=TRUE)

# Remove the last column
data <- data %>%
  dplyr::select(-play.count)

# Remove the columnd called "target"
data <- data %>%
  dplyr::select(-target)
head(data)

# We will change artist.rank as a factor
data$artist.rank<-as.factor(data$artist.rank)

# We will use data_rm for linear regression later
data_rm <- data %>% 
  dplyr::select(-song_title, -artist, -song_artist)
data_rm$key <- as.factor(data_rm$key)
summary(data_rm)
str(data_rm)
head(data)

##### 2. DATA VISUALIZATION #####
# correlogram
# Creating a dataset that excludes factor variables -- We will only use this for the plots
data1 <- data %>% 
  dplyr::select(-song_title, -artist, -artist.rank, -key, -song_artist,-target)
head(data1)
corrplot(cor(data1), type = "lower", tl.srt=20)




# Explore the relationship between two attributes after correlogram
# scatterplot: energy vs acousticness
P0 <- ggplot(data,aes(x =energy,y = acousticness)) +
  geom_point()+ggtitle('Energy vs Acousticness') + xlab("Energy") + ylab("Acousticness") + geom_smooth()
P0

# scatterplot: loudness vs acousticness
P1 <- ggplot(data,aes(x =acousticness,y = loudness)) +
  geom_point()+ggtitle('Acousticness vs Loudness') + xlab("Acousticness")+ ylab("Loudness") +geom_smooth()
P1

# scatterplot: loudness vs energy
P2 <- ggplot(data,aes(x =energy,y = loudness)) +
  geom_point()+ggtitle('Loudness vs Energy') + xlab("Energy")+ ylab("Loudness") +geom_smooth()
P2

# scatterplot: valence vs danceability
P3 <- ggplot(data,aes(x =valence,y = danceability)) +
  geom_point()+ggtitle('Valence vs Dancibility') + xlab("Valence")+ ylab("Danceability") +geom_smooth()
P3

# Browsing the spread of danceability,valence and loudness

ggplot(data, aes(danceability)) + xlab("Danceability") + ylab("Density") + ggtitle("Density of Danceability")+ geom_density()
ggplot(data, aes(energy)) + xlab("Energy") + ylab("Density") + ggtitle("Density of Energy")+ geom_density()
ggplot(data, aes(acousticness)) + xlab("Acousticness") + ylab("Density") + ggtitle("Density of Acousticness")+ geom_density()
ggplot(data, aes(valence)) + xlab("Valence") + ylab("Density") + ggtitle("Density of Valence") +  geom_density()
ggplot(data, aes(loudness)) + xlab("Loudness") + ylab("Density") + ggtitle("Density of Loudness")+ geom_density()
ggplot(data, aes(play.count.th)) + xlab("Play Count (Thousands)") + ylab("Density") + ggtitle("Density of Total Play Count")+  geom_density() 

# Top 10 songs with artist name

top10 <-top_n(data,10,play.count.th)
p <-ggplot(top10,aes(x=song_artist,y=play.count.th,fill=artist,angle=150)) + 
  xlab("Song - Artist") + ylab("Play Count (Thousands)") + geom_col()
p + theme(axis.text.x= element_text(face="bold",size=6,angle=13))      


# Boxplot:acousticness range in different artist.rank
names(data)
data$artist.rank <- as.factor(data$artist.rank)
ggplot(data,aes(x=artist.rank,y=c(acousticness),fill=artist.rank))+
  geom_boxplot()

# Boxplot:Danceability range in different artist.rank
ggplot(data,aes(x=artist.rank,y=c(danceability),fill=artist.rank))+
  geom_boxplot()

# Boxplot:loudness range in different artist.rank
ggplot(data,aes(x=artist.rank,y=c(loudness),fill=artist.rank))+
  geom_boxplot()

#Boxplot:engergy range in different artist.rank
ggplot(data,aes(x=artist.rank,y=c(energy),fill=artist.rank))+
  geom_boxplot()

# scatterplot: playcount vs acousticness
P4 <- ggplot(data,aes(x =loudness,y = play.count.th)) +
  geom_point()+ggtitle('Loudness vs Play Count') + xlab("Loudness") + ylab("Play Count (Thousands)")
P4

P5 <- ggplot(data,aes(x =energy,y = play.count.th)) +
  geom_point()+ggtitle('Energy vs Play Count') + xlab("Instrumentalness") + ylab("Play Count (Thousands)")
P5

# We will make a pairplot to see the realtionship between two attributes
datarelation <- data %>% 
  dplyr::select(acousticness,danceability,energy,instrumentalness,key,liveness,loudness,mode,speechiness,tempo,valence)
ggpairs(datarelation)

# Browse our data

summary(data$play.count)
data.high <- ddply(data,c("artist"), subset, rank(play.count.th) <= 10)

# subsetting data with only top artists
data.drake <- subset(data, subset = artist == "Drake")
data.rick <- subset(data, subset = artist == "Rick Ross")
data.disclosure <- subset(data, subset = artist == "Disclosure")
data.walk <- subset(data, subset = artist == "WALK THE MOON")
data.backstreet <- subset(data, subset = artist == "Backstreet Boys")
data.fidlar <- subset(data, subset = artist == "FIDLAR")



##### 3. MODELING #####
# We will make linear regression, linear regression with interractions, regression tree, random forest, and K-nn regression
# We will do 10-fold cross validation to see which of the models performs better
# We will not do 10-fold cross validation for random forest, since it randomizes the variable selection during each tree split.
# => Random Forest is not prone to overfit unlike other models


# make a regression model
ols <- lm(play.count.th ~ ., data=data_rm)

coef(ols)


summary(ols) 

# Checking four Assumptions
# Check multicollinearity 
vif(ols) 
# ----- Since all the values are below 5,
#   --- we can say we don't have any mulicollinearity

# Evaluate homoscedasticity
# Non-constant error variance test, H0=Non-Constant Var,Ha=Constant Var
ncvTest(ols) 
# ----- We reject the H0 
#   --- Constant Variance/Homoscedasticity is kept

# Durbin-Watson test for Autocorrelated Error
durbinWatsonTest(ols) 
# ----- Since p-value is lower than 0.05,
#   --- We don't have Autocorrelation

# Data Partition
set.seed(123)
ind <- sample(2,nrow(data_rm), replace = TRUE, prob = c(0.7,0.3))
train_data1 <- data_rm[ind == 1,]
test_data1 <- data_rm[ind == 2,]

# Regularization via Lasso
# Function of support
support<- function(x, tr = 10e-6) {
  m<- rep(0, length(x))
  for (i in 1:length(x)) if( abs(x[i])> tr ) m[i]<- i
  m <- m[m>0]
  m
}

# Function of R-squared
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}

# Model Building - glm
glm <- glm(play.count.th ~. , data=data_rm)

Mx <- model.matrix(glm)[,-1]
My <- data_rm$play.count.th
lasso <- glmnet(Mx,My)
length(support(lasso$beta)) # 1053
lassoCV <- cv.glmnet(Mx,My)
summary(lasso)
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))


# plotting for CV Lasso
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))

features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min) # 19
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) # 1
features.1se
data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)

### implementing k-fold cross validation
n <- nrow(train_data1)
nfold <- 5
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### creating OOS - empty dataset
OOS <- data.frame(regression = rep(NA,nfold), PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), L.min=rep(NA,nfold), L.1se=rep(NA,nfold)) 
OOS <- as.data.frame(t(OOS))
### Use a for loop to run through the nfold trails


for(k in 1:nfold){ 
  train <- which(foldid!=k)
  rmin <- glm(My~., data=data.min, subset=train)
  if (length(features.1se) == 0){  r1se <- glm(play.count.th~1, data=train_data1, subset=train) 
  } else {r1se <- glm(My~., data=data.1se, subset=train)
  }
  regression <- glm(play.count.th ~., data=train_data1, subset=train)
  
  
  ## fit the models
  pred.reg <- predict(regression, newdata = train_data1[-train,], type = "response")
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se <- predict(r1se, newdata=data.1se[-train,], type="response")
  
  ## calculate and log R2
  OOS$regression[k] <- R2(y=train_data1$play.count.th[-train], pred = pred.reg)
  OOS$regression[k]
  
  # Post lasso
  OOS$PL.min[k] <- R2(y=My[-train], pred=predmin)
  OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se)
  
  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx[train,],My[train],lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train],lambda = lassoCV$lambda.1se)
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se <- predict(lasso1se, newx=Mx[-train,], type="response")
  OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin)
  OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se)
  
  
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}


# the mean of the results stored in the dataframe OOS

colMeans(OOS)

m.OOS <- as.matrix(OOS)
m.OOS <- m.OOS[,c(6:10)]
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=0, yjust=0),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:6))
if (nfold >= 5){
  # Boxplot for all model's R^2
  boxplot(m.OOS, ylab=expression(paste("OOS ",R^2)), xlab="", main="5-fold Cross Validation")
}


colMeans(m.OOS) # OOS R^2 Value

###
# REGRESSION TREE
head(data_rm)
# Data Partition
set.seed(1)
ind <- sample(2,nrow(data_rm), replace = TRUE, prob = c(0.7,0.3))
train_tree <- data_rm[ind == 1,]
test_tree <- data_rm[ind == 2,]
# Making regression Tree
head(data_rm)
tree <- tree(play.count.th~.,data = train_tree)
summary(tree)
# Plot the tree
plot(tree)
text(tree,pretty = 0)
# Make and plot regression tree with rpart package (Optional)
tree1 <- rpart(play.count.th~.,data = data_rm, cp = .005)
rpart.plot(tree1,type=1)
# We now use K-fold cross validation ( using cv.tree() function ) in order to determine the optimal level of
# tree complexity. This will help us decide whether pruning the tree will improve performance.
cv_tree <- cv.tree(tree)
cv_tree
# Plot the tree to see the optimal number of the leaves
plot(cv_tree$size,cv_tree$dev/1000000, type = 'b', xlim = c(1,10), xlab = "Tree Size", ylab = "Deviance", main = "Tree size and Deviance (In millions)")
# After looking at the plot, we can see how the deviance decreases because of the numbers of the independet variables used
# Prune the tree to make a new tree with 7 variables
prune_tree <- prune.tree(tree, best = 7)
plot(prune_tree)
text(prune_tree, pretty = 0)
# Predict with the cv tree
tree_pr <- predict(prune_tree, newdata = test_tree)
# R-squared of test data
R2_tree <- R2(test_tree$play.count.th, tree_pr, family = "gaussian")
R2_tree # 37.6%



###
# RANDOM FOREST
# Data Partition
set.seed(123)
ind <- sample(2,nrow(data_rm), replace = TRUE, prob = c(0.7,0.3))
train_data <- data_rm[ind == 1,]
test_data <- data_rm[ind == 2,]
# By default random forest takes 500 trees and p/3 variables, where p is equal to independet variables
# Random forest uses average to predict new data with for regression
# Random Forest
set.seed(1234)
rf <- randomForest(play.count.th ~ .,data = train_data)
print(rf)
attributes(rf)
# Predicting with train dataset
p1 <- predict(rf, data = train_data)
head(p1)
head(train_data$play.count.th)
# Predict with test dataset
p2 <- predict(rf, newdata = test_data)
head(p2)
plot(p2, test_data$play.count.th)
abline(0,1)
# Calculate MSE
mean((p2 - test_data$play.count.th)^2)
# Calculate R-squared for test dataset
R.2 <- R2(test_data$play.count.th,p2,family = "gaussian")
R.2  # 38.6
# Error Rate of Random Forest
plot(rf$mse, main = "Number of Trees and Error Rate", xlab = "Number of Trees", ylab = "Error Rate")
plot(rf$mse, xlim = c(350,450), main = "Number of Trees and Error Rate Range(350:450)",
     xlab = "Number of Trees", ylab = "Error Rate")
# We see that after around 76 trees the error rate does not decrease
# Tune mtry
t <- tuneRF(train_data[,-15], train_data[,15],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 60,
            trace = TRUE,
            improve = 0.05)
print(t)
# New random forest model with 81 trees and mtry 4
rf1 <- randomForest(play.count.th~.,data = train_data, ntree = 400, mtry = 6)
# Predict for the test dataset
p3 <- predict(rf1, newdata = test_data)
# R-squared for the test dataset
R.2.1 <- R2(test_data$play.count.th,p3,family = "gaussian")
R.2.1 # R-squared for the test dataset is equal to 37.2% -- Random Forest


###
# KNN cross validation
# Make train/test datasets
set.seed(1234)
ind <- sample(2,nrow(data_rm), replace = TRUE, prob = c(0.7,0.3))
train_k <- data_rm[ind == 1,]
test_k <- data_rm[ind == 2,]
# Make KNN model
# Do 10 fold cross-validation 3 times
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 3)
# Make K-nn model
k1 <- train(play.count.th~., data = train_k, 
            tuneGrid = expand.grid(k=1:70), 
            method = 'knn', # Use knn method
            metric = 'Rsquared', # To us R-squared as the matric of evaluation
            trControl = trControl, # The algorithm to be used to get the optimal transference plan
            preProc = c('center', 'scale')) # To standardize the data 
# Model Performance
k1 # We see that the final value is when k = 12 
   # 10-fold cross-validation repeated 3 times
# Plot k1
plot(k1)
plot(k1, xlim = c(10,20)) 
# ----- We see from the plot that the R-squared maximizes 
#   --- when we have k equal to 12

# Get the important variables
varImp(k1)
# ----- We see that the artist.rank is the most important variable,
#   --- and key is the least important one

# Do prediction with our k-nn model
k1_predict <- predict(k1, newdata = test_k)
# Plot the predictions
plot(k1_predict~test_k$play.count.th)
# R squared of the test dataset
R.2.K <- R2(test_k$play.count.th,k1_predict,family = "gaussian")
R.2.K # We get test R squared equal to 24.8%

### Comparing all the OOS R-squared values, our final model is:
###  --- Regression Tree
prune_tree <- prune.tree(tree, best = 7)
