# clear Workspace
rm(list = ls())
library(dplyr)
library(data.table)
library(plyr)
library(knitr)
library(ggplot2)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(randomForest)
library(Metrics)

## Part I: Preprocessing and EDA
#1.1 Load the datasets
Train <- read.table("./train.csv",head = T, sep = ',')
Test <- read.table("./test.csv",head = T, sep=',')

#Getting rid of the IDs but keeping the test IDs in a vector. These are needed to compose the submission file
test_labels <- Test$Id
Test$SalePrice <- NA
Train$Id <- NULL
Test$Id <- NULL

all <- rbind(Train, Test)
dim(all)
#Col number which has missing values
NAcol <- which(colSums(is.na(all)) > 0)
#Number of missing rows in columns above
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
#Class of missing col
sapply(all[NAcol], class)
cat('There are', length(NAcol), 'columns with missing values')


Train$MasVnrType[is.na(Train$MasVnrArea)]
Test$MasVnrType[is.na(Test$MasVnrArea)]


#Character Imputation ;Replace NA with Unknown

#add "None" as a level for group1
addLevel <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "None")))
  return(x)
}

#For the rest group2 columns, qualitative assign the most frequest, quantitative assign 0
missFun1 <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- 0
    x
  } else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}

#Summarized function (missFun): fill missing and remove some columns
missFun <- function(t) {
  #missing GarageYrBlt should equal to YearBuilt(if year <1980/75 then how to derive?)
  t$GarageYrBlt[is.na(t$GarageYrBlt)] = t$YearBuilt[is.na(t$GarageYrBlt)]
  # if the houses with veneer area NA are also NA in the veneer type,
  # find the one that should have a MasVnrType,
  # assign the veneer as the most popular (that is not none)
  if(length(which(is.na(t$MasVnrType) & !is.na(t$MasVnrArea)))>0){
    t[is.na(t$MasVnrType) & !is.na(t$MasVnrArea),]$MasVnrType <- names(sort(-table(t$MasVnrType)))[2]
  }
  #For training set in several columns, missing value should assign "None" value as a category, using myFun1
  group1 <-t[,c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature")]
  group2 <-t[,!(colnames(t) %in% c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature"))]
  group1 <- as.data.frame(lapply(group1, addLevel))
  group1[is.na(group1)] <- "None"
  group2=data.table(group2)
  group2<-group2[, lapply(.SD, missFun1)]
  #get the new training group without missing value
  newt=cbind(group1,group2)
  return(newt)
}  

#Fill in missing data 
Train$Id <- NULL
Test$SalePrice <- NULL
nTrain=missFun(Train)
nTest=missFun(Test)

# COnvert into Factor or numeric
numericVars <- which(sapply(nTrain, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector numericVarNames for use later on
cat('There are', length(numericVars), 'numeric variables')

factorVars <- which(sapply(nTrain, is.factor)) #index vector factor variables
factorVarNames <- names(numericVars) #saving names vector factorVarNames for use later on
cat(' and there are', length(factorVars), 'factor variables in Train dataset')

# Perform transformati0on/ Labeling of vars

transFun1 <- function(x) {
  if (is.integer(x)) {
    x<-as.integer(x)
    x
  } 
  else if (is.factor(x)) {
    x<-as.factor(x)
    x
  } 
  x
}

# Defining levels (from data description)
Qual_lev <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
# for ExterQual, ExterCond, BsmtQual, BsmtCond, HeatingQC, KitchenQual,FireplaceQu, GarageQual, GarageCond, PoolQC
BsmtFin_lev <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
# for BsmtFinType1, BsmtFinType2
BsmtExp_lev <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
# for BsmtExposure
Func_lev <- c('Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4, 'Min2' = 5, 'Min1' = 6, 'Typ' = 7)
# for Functional
transFun <- function(t) {
  
  col1 <- c('ExterQual','ExterCond','BsmtQual','BsmtCond','HeatingQC','KitchenQual','FireplaceQu','GarageQual','GarageCond', 'PoolQC')
  col2 <- c('BsmtFinType1','BsmtFinType2','BsmtExposure','MiscFeature','Alley','Fence', 'GarageType','GarageFinish', 'Functional')
  col3 <- c('MasVnrType','MSSubClass','YrSold','MoSold','YearBuilt','YearRemodAdd')
  colT <- append(col1, col2)
  colT <- append(colT, col3)
  
  group1 <- subset(t, select=col1) #train
  group2 <- subset(t, select=col2) #train
  group3 <- subset(t, select=col3) #train
  group4 <- t[,!(colnames(t) %in% colT)] #train
  group1 <- as.data.frame(
    lapply(group1, function(x) {
      revalue(x,Qual_lev)
    })) #train
  
  group1<-lapply(group1, as.integer)
  
  group2$BsmtFinType1 <- as.integer(revalue(group2$BsmtFinType1, BsmtFin_lev)) #train
  group2$BsmtFinType2 <- as.integer(revalue(group2$BsmtFinType2, BsmtFin_lev)) #train
  group2$BsmtExposure <- as.integer(revalue(group2$BsmtExposure, BsmtExp_lev)) #train
  group2$MiscFeature <- as.factor(group2$MiscFeature) #train
  group2$Alley <- as.factor(group2$Alley) #train
  group2$Fence <- as.factor(group2$Fence) #train
  group2$GarageType <- as.factor(group2$GarageType) #train
  group2$GarageFinish <- as.factor(group2$GarageFinish) #train
  group2$Functional <- as.integer(revalue(group2$Functional, Func_lev)) #train
  
  group3<-lapply(group3, as.factor)
  
  group4<-lapply(group4, transFun1)
  
  newt <- cbind(group1,group2,group3, group4) #train
  
  
  return(newt)
}
#Transform training set
nTrain=transFun(nTrain)
nTest=transFun(nTest)

# Feature Engineering
featureFun <- function(t) {
  
  #calculate total bathrooms
  t <- t %>% mutate(
    TotalBath = FullBath + HalfBath*0.5 + BsmtFullBath + BsmtHalfBath*0.5)
  
  #whether house is new or not
  t <- t %>% mutate(
    IsNew = ifelse(as.numeric(t$YrSold)==as.numeric(t$YearBuilt), 1, 0))
  t$IsNew <- as.factor(t$IsNew)
  #0=No Remodeling, 1=Remodeling
  t <- t %>% mutate(
    Remodel = ifelse(as.numeric(t$YearBuilt)==as.numeric(t$YearRemodAdd), 0, 1))
  t$Remodel <- as.factor(t$Remodel)
  
  #total age of the house after built/remodeled
  t <- t %>% mutate(
    Age = as.numeric(t$YrSold)-as.numeric(t$YearRemodAdd))
  #total square feet
  t <- t %>% mutate(
    TotalSqFeet = GrLivArea + TotalBsmtSF)
  #Consolidating Porch variables
  t <- t %>% mutate(
    TotalPorchSF = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch)
  #did not check neighbourhood
  
}
nTrain<-featureFun(nTrain)
nTest<-featureFun(nTest)

#check different typies of variables for training
numericVars <- which(sapply(nTrain, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector numericVarNames for use later on
cat('There are', length(numericVars), 'numeric variables')
sapply(nTrain[numericVars], class)

factorVars <- which(sapply(nTrain, is.factor)) #index vector factor variables
factorVarNames <- names(numericVars) #saving names vector factorVarNames for use later on
cat(' and there are', length(factorVars), 'factor variables in Train dataset')
sapply(nTrain[factorVars], class)

## Visualization

#Divide into char and numeric
all_numVar <- nTrain[numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations >0.5
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)


##1.6 PreProcessing predictor variables
#Combine Test and Train and split numeric and factor; one hot encoding for factor variable
nTrain1 = nTrain
nTest1 = nTest
nTest1$SalePrice <- NA
all = rbind(nTrain, nTest1)
DFnumeric <- all[, names(all) %in% numericVarNames]
DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']
cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

###Skewness and normalizing of the numeric predictors
#Transform variable into log value if it is skewed

for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

#Normalizing the data

PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

##One hot encoding the categorical variables
#Using the model.matrix() function to change factor variables into dummy variable do this one-hot encoding.

DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

#combining all (now numeric) predictors into one dataframe 
combined <- cbind(DFnorm, DFdummies) 

##Dealing with skewness of response variable
skew(all$SalePrice)
qqnorm(all$SalePrice)
qqline(all$SalePrice)

#The skew of 1.87 indicates a right skew that is too high, and the Q-Q plot shows that sale prices are also not normally distributed. To fix this I am taking the log of SalePrice.
all$SalePrice <- log(all$SalePrice) #there is no 0 so just take log
skew(all$SalePrice)

#the Q-Q plot is looks much better now.
qqnorm(all$SalePrice)
qqline(all$SalePrice)

## 2 Modeling

train1 <- combined[!is.na(all$SalePrice),]
train2 <- combined[!is.na(all$SalePrice),]
test1 <- combined[is.na(all$SalePrice),]
train1$SalePrice=NULL

#2.1 Multi-linear model

library(olsrr)
model <- lm(SalePrice ~ OverallQual+GrLivArea+GarageCars+TotalBath+GarageArea+TotalBsmtSF+FullBath+TotRmsAbvGrd+GarageYrBlt+Age+KitchenQual+BsmtQual+ExterQual, data = train2)
proc.time()
k <- ols_step_best_subset(model)
proc.time()
plot(k)
k
k1 <- ols_step_forward_p(model)

##2.2 ridge regression model
set.seed(2018)
my_control <-trainControl(method="cv", number=5)
ridgeGrid <- expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0005))
#check time
ptm <- proc.time()
ridge_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=ridgeGrid) 
proc.time() - ptm
ridge_mod$bestTune
min(ridge_mod$results$RMSE) #RMSE is 0.1382241

RidgePred <- predict(ridge_mod, test1)
pred_ridge <- exp(RidgePred) 

##2.3 lasso regression model

set.seed(2018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
#check time
ptm <- proc.time()
lasso_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
proc.time() - ptm
lasso_mod$bestTune
min(lasso_mod$results$RMSE) #RMSE is 0.1290906

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance
varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))
cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')

colnames(train1)[which(lassoImportance$Overall>0.05)]
lasso_numVar <- (train1)[which(lassoImportance$Overall>0.05)]
lasso_numVar$SalePrice=train2$SalePrice
lasso_numVar <- lasso_numVar %>% select(SalePrice, everything())
cor_lasso <- cor(lasso_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with SalePrice
cor_lasso_sorted <- as.matrix(sort(cor_lasso[,'SalePrice'], decreasing = TRUE))
#select only high corelations
# CorHigh <- names(which(apply(cor_lasso_sorted, 1, function(x) abs(x)>0.5)))
# cor_numVar <- cor_numVar[CorHigh, CorHigh]
a<- corrplot.mixed(cor_lasso, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

LassoPred <- predict(lasso_mod, test1)
pred_lasso <- exp(LassoPred) 
head(pred_lasso) 

##2.4 ElasticNet regression model
#Elastic net is a regularized regression method that linearly combines the L1 and L2 penalties of the lasso and ridge methods.
set.seed(2018)
elnetGrid <- expand.grid(alpha = 0.5, lambda = seq(0.001,0.1,by = 0.0005))
#check time
ptm <- proc.time()
elnet_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=elnetGrid) 
proc.time() - ptm
elnet_mod$bestTune
min(elnet_mod$results$RMSE)

elnetPred <- predict(elnet_mod, test1)
pred_elnet <- exp(elnetPred) 
head(pred_lasso) 

##2.5 Random Forest
#separate training and testing dataset from the available data

library(caret)
x_train=train1
y_train=all$SalePrice[!is.na(all$SalePrice)]
train=cbind(x_train,y_train)
names(train) <- make.names(names(train))
set.seed(123)
partition <- createDataPartition(y=y_train,
                                 p=.5,
                                 list=F)
training <- train[partition,]
testing <- train[-partition,]
#check time
ptm <- proc.time()
model_1 <- randomForest(y_train~ ., data=training)
proc.time() - ptm
# Predict using the test set
prediction <- predict(model_1, testing)
model_output <- cbind(testing, prediction)
#Test with RMSE
rmse(model_output$y_train,model_output$prediction)

## Using caret
control <- trainControl(method="repeatedcv", number=10, repeats=3)
mtry <- sqrt(ncol(train1))
tunegrid <- expand.grid(.mtry=mtry)
metric <- "RMSE"
rf_default <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)],  method='rf', metric = metric, tuneGrid=tunegrid,trControl=control)
print(rf_default)


## 2.6 XGBoost model

xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

#Use 5 fold cross validation to find the best hyperparameter values

xgb_caret <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='xgbTree',
                   trControl= my_control, tuneGrid=xgb_grid) 
xgb_caret$bestTune #take 1.20+ minutes

#The best result is using 
#Max_depth=3  4
#eta=0.05 0.01
#gamme = 0
#colsample_bytree = 1
#Min_child_weight=4 3
#subsample = 1

label_train <- all$SalePrice[!is.na(all$SalePrice)]
# separate training and test into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train1), label= label_train)
test1$SalePrice<-NULL
dtest <- xgb.DMatrix(data = as.matrix(test1))

#apply the setting found earlier

default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.01,
  gamma=0,
  max_depth=4, 
  min_child_weight=2, 
  subsample=1,
  colsample_bytree=1
)

#cross validation
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 1001, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F) 

#The best RMSE is 0.128208 at 961 round
#train the model using the best iteration found by cross validation
#check time
ptm <- proc.time()
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 961)
proc.time() - ptm

XGBpred <- predict(xgb_mod, dtest)
pred_XGB <- exp(XGBpred) 
head(pred_XGB)

#check out important variables
library(Ckmeans.1d.dp) 
mat <- xgb.importance (feature_names = colnames(train1),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)

# 4 Stacking

sub_data <- data.frame(Id = test_labels, SalePrice = (pred_XGB+pred_lasso+pred_elnet)/3)
sub_data_XGB <- data.frame(Id = test_labels, SalePrice = pred_XGB)
sub_data_lasso <- data.frame(Id = test_labels, SalePrice = pred_lasso)
sub_data_ridge <- data.frame(Id = test_labels, SalePrice = pred_ridge)
sub_data_elnet <- data.frame(Id = test_labels, SalePrice = pred_elnet)
head(sub_data)
write.csv(sub_data, file = 'sub_data_2.csv', row.names = F)
write.csv(sub_data_lasso, file = 'sub_data_lasso.csv', row.names = F)
write.csv(sub_data_XGB, file = 'sub_data_xbg.csv', row.names = F)
write.csv(sub_data_ridge, file = 'sub_data_ridge.csv', row.names = F)
write.csv(sub_data_elnet, file = 'sub_data_elnet.csv', row.names = F)

