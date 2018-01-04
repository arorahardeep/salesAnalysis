###
# This is the model for predicting the SaleFlag and SaleCount for https://www.kaggle.com/flenderson/sales-analysis dataset
# Author  : Hardeep Arora
# Date    : 09-Dec-2017
###

# Libraries
library(data.table)
library(xgboost)
library(caret)
library(DMwR)
library(pROC)
library(ggplot2)

# Read the data
df <- fread("../data/SalesKaggle3.csv")

# Separate it into Sales and Inventory data
historicSales <- df[df$File_Type=="Historical",]
activeInv <- df[df$File_Type=="Active",]

target.sales <- as.data.table(historicSales$SoldFlag)
target.sales.count <- as.data.table(historicSales$SoldCount)

# Columns to Exclude
cols = c("File_Type","Order","SKU_number")
historicSales <- historicSales[,!cols,with=FALSE]

historicSales[, `:=`(mic=mean(ItemCount)), by=ReleaseYear]

# Convert MarketingType to numeric
historicSales$MarketingType <- ifelse(historicSales$MarketingType == "D",1,2)

# Standardize the data
#historicSales <- as.data.table(scale(historicSales))

# Balance the classes using oversampling
historicSales$SoldFlag <- as.factor(historicSales$SoldFlag)
historicSales1 <- SMOTE(SoldFlag ~ . , 
                       data= historicSales, perc.over = 100, perc.under = 200)

#historicSales1 <- historicSales1[ItemCount < 150, ]

target.sales <- as.data.table(as.numeric(as.character(historicSales1$SoldFlag)))
target.sales.count <- as.data.table(historicSales1$SoldCount)

historicSales <- historicSales1[,!c("SoldFlag","SoldCount"),with=FALSE]

# Divide the data into train and test set (0.8,0.2)
testSize <- 0.2
indexes = sample(1:nrow(historicSales), size=testSize*nrow(historicSales))

# Split data
test <- historicSales[indexes,]
train <- historicSales[-indexes,]

lbl_train.sales <- target.sales[-indexes,]
lbl_test.sales <- target.sales[indexes,]

lbl_train.sales.count <- target.sales.count[-indexes,]
lbl_test.sales.count <- target.sales.count[indexes,]


# Train the model for SaleFlag Prediction
new_tr <- model.matrix(~.,data = train) 
new_ts <- model.matrix(~.,data = test)

lbl_train <- lbl_train.sales$V1
lbl_test <- lbl_test.sales$V1

dtrain <- xgb.DMatrix(data = new_tr,label = lbl_train) 
dtest <- xgb.DMatrix(data = new_ts,label=lbl_test)

params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.01, 
               gamma=0, 
               max_depth=7, 
               min_child_weight=1, 
               subsample=1, 
               colsample_bytree=1)

set.seed(101)
xgb.sales <- xgb.train (params = params, 
                   data = dtrain, 
                   nrounds = 510, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 10, 
                   early_stopping_rounds = 10, 
                   maximize = T , 
                   eval_metric = "auc")

mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb.sales)
xgb.plot.importance (importance_matrix = mat) 

xgbpred <- predict (xgb.sales,dtest)
plot(pROC::roc(response = lbl_test.sales$V1,
               predictor = xgbpred,
               levels=c(0, 1)),lwd=1.5)

test$pred <- xgbpred

ggplot(test, aes(x=pred, colour =factor(lbl_test.sales$V1), 
                 fill = factor(lbl_test.sales$V1), alpha = 0.3)) + 
  geom_density()

xgbpred <- ifelse (xgbpred > 0.5,1,0)
confusionMatrix (xgbpred, lbl_test)

# Train the model for SaleCount Prediction
new_tr <- model.matrix(~.,data = train) 
new_ts <- model.matrix(~.,data = test)

lbl_train <- lbl_train.sales.count$V1
lbl_test <- lbl_test.sales.count$V1

dtrain <- xgb.DMatrix(data = new_tr,label = lbl_train) 
dtest <- xgb.DMatrix(data = new_ts,label=lbl_test)

params <- list(booster = "gbtree", 
               objective = "reg:linear", 
               eta=0.1, 
               gamma=0, 
               max_depth=7, 
               min_child_weight=1, 
               subsample=1, 
               colsample_bytree=1)

set.seed(101)
xgb.sales.count <- xgb.train (params = params, 
                        data = dtrain, 
                        nrounds = 1510, 
                        watchlist = list(val=dtest,train=dtrain), 
                        print_every_n = 10, 
                        #early_stopping_rounds = 10, 
                        maximize = T , 
                        eval_metric = "rmse")

mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb.sales.count)
xgb.plot.importance (importance_matrix = mat) 

xgbpred <- predict (xgb.sales.count,dtest)

test1 <- test
test1$SoldCount <- lbl_test
test1$PredSC <- ifelse(xgbpred < 0.1, 0, xgbpred)

### Predict Sale on the Inventory
cols = c("File_Type","Order","SKU_number","SoldCount","SoldFlag")
activeInv <- activeInv[,!cols,with=FALSE]

# Convert MarketingType to numeric
activeInv$MarketingType <- ifelse(activeInv$MarketingType == "D",1,2)

new_inv <- model.matrix(~.,data = activeInv)
dinv <- xgb.DMatrix(data = new_inv)
xgbpredInv <- predict (xgb.sales,dinv)
xgbpredInv <- ifelse (xgbpredInv > 0.5,1,0)
activeInv$SoldFlag <- xgbpredInv
salesCount <- predict (xgb.sales.count,dinv)
activeInv$SoldCount <- ifelse(salesCount < 0.1, 0, salesCount)



