## Loading Required Packages
library(dplyr)
library(ggplot2)
library(car)
library(caTools)
library(caret)
library(Matrix)
library(xgboost)
library(randomForest)

## Create Dummies Function
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

##### Importing Data
## Setting Working Directory and Importing Files 
setwd("E:\\KEDB\\R_ML\\Project_4")
## Training Data
Main_training_Data  = data.frame(read.csv('hr_train.csv'), stringsAsFactors = F)
## Testing Data
Main_testing_Data = data.frame(read.csv('hr_test.csv'), stringsAsFactors = F)

## Identifying Train and Test Columns
Main_training_Data$DataType = 'TRAIN'
Main_testing_Data$DataType = 'TEST'
Main_testing_Data$left = ''

## Removing NA values for response variable in Training Set
Main_training_Data$left = ifelse(is.na(Main_training_Data$left), na.omit(Main_training_Data$left) , Main_training_Data$left)

## DummyScope salary , sales , number_projects , time_spend_company
## Conversions Time_spend_company
Complete_DataSet = rbind(x = Main_training_Data , y = Main_testing_Data)

Complete_DataSet = Complete_DataSet %>% 
mutate(
  left = unlist(left),
  number_project = unlist(as.factor(number_project)),
  average_montly_hours = unlist(as.factor(average_montly_hours)),
  time_spend_company = unlist(as.factor(time_spend_company)),
  Work_accident = unlist(as.factor(Work_accident)),
  promotion_last_5years = unlist(as.factor(promotion_last_5years))
  )
Complete_DataSet = CreateDummies(Complete_DataSet,'number_project',100)
Complete_DataSet = CreateDummies(Complete_DataSet,'time_spend_company',60)
Complete_DataSet = CreateDummies(Complete_DataSet,'sales',100)
Complete_DataSet = CreateDummies(Complete_DataSet,'salary',100)
Complete_DataSet = CreateDummies(Complete_DataSet,'average_montly_hours',10)

## Seperating the Data
set.seed(123)
Final_train1=Complete_DataSet %>% filter(DataType=='TRAIN') %>% select(-DataType)
Final_test1=Complete_DataSet %>% filter(DataType=='TEST') %>% select(-DataType,-left)

### Model Building
### Checking VIF to remove the variables with multi-colinearity
RM_VIF1=lm(left ~ . -time_spend_company_3 -number_project_4 -time_spend_company_2  ,Final_train1)
sort(vif(RM_VIF1),decreasing = T)[1:3]
summary(RM_VIF1)

# Making Store as a factor
Final_train1$left = as.factor(Final_train1$left)
levels(Final_train1$left)
Final_test1$left = ''


## GLM Model
model1 = glm(left ~ . -time_spend_company_3 -number_project_4 -time_spend_company_2  , Final_train1 , family = 'binomial')
val.score = predict(model1,newdata = Final_train1 , type = 'response')
auc_score=auc(roc(Final_train1$left , val.score))
auc_score
fitted.results12 <- ifelse(val.score > 0.409,1,0)
table_mat = table(Final_train1$left , fitted.results12)
### Saving the obtained predicted column in a new training data set
Loggy_Train = data.frame(Final_train1)
Loggy_Train$left = Final_train1$left
Loggy_Train$GLM_left = fitted.results12
## Predicting the data in Testing set and saving them in testing dataframe
val_score_test = predict(model1,newdata = Final_test1 , type = 'response')
fitted_test_results <- ifelse(val_score_test > 0.409,1,0)
TestGLM_left = fitted_test_results


### Model 2
library("randomForest")
set.seed(889)
rf_model_train1 = randomForest(left ~ . -time_spend_company_3 -number_project_4 -time_spend_company_2  , Final_train1 , stepFactor=0.5, improve=1e-5, ntree=500, mtry=10 )
## Predicting
pred1_train = predict(rf_model_train1,Final_train1)
confusionMatrix(pred1_train, Final_train1$left)
plot(rf_model_train1)
### Saving the obtained predicted column in a new training data set
RF_MOD1 = Loggy_Train
RF_MOD1$RF1_left = pred1_train
## Predicting the data in Testing set and saving them in testing dataframe
pred1_test = predict(rf_model_train1,Final_test1)
TestRF1_left = pred1_test


### MOdel 3
### Training through caret Model = XGBoosting
### To check what models are available to names(getModelInfo())
## Converting the Data into Matrix for eXtreem-Boosting_model

### Variables Conversion
Final_train_lable = as.factor(Final_train1$left)
Final_trainMat = sparse.model.matrix(left ~ . -left, data = Final_train1 )
Final_StoreMatrix_Train = xgb.DMatrix(data = as.matrix(Final_trainMat) , label = Final_train_lable)

Final_test_lable = as.factor(Final_test1$left)
Final_testMat = sparse.model.matrix(left ~ . -left, data = Final_test1 )
Final_StoreMatrix_Test = xgb.DMatrix(data = as.matrix(Final_testMat) , label = Final_test_lable)


ControlParameters_XG = trainControl(method = 'cv' ,
                                    number = 8,
                                    allowParallel = TRUE,
                                    verboseIter = FALSE,
                                    returnData = FALSE
)


parameterGrid_XG <- expand.grid(nrounds = c(100,200,300),  # this is n_estimators in the python code above
                                max_depth = c(10, 15, 20, 25,30),
                                colsample_bytree = seq(0.5, 0.9, length.out = 5),
                                ## The values below are default values in the sklearn-api. 
                                eta = 0.05,
                                gamma=0,
                                min_child_weight = 1,
                                subsample = 1
)


### XGB Model
XG_Model = train(Final_StoreMatrix_Train , Final_train_lable ,
                     method = 'xgbTree' ,
                     trControl = ControlParameters_XG,
                     tuneGrid = parameterGrid_XG
) 


predTrain_final_XG = predict(XG_Model,Final_StoreMatrix_Train)
confusionMatrix(predTrain_final_XG, Final_train1$left)


predTest_final_XG = predict(XG_Model,Final_StoreMatrix_Test)
TestXGB_left = predTest_final_XG

Stack_training_Set = RF_MOD1
Stack_training_Set$XGB_left = predTrain_final_XG

#### Adding rows to testing stack
Stack_testME = Final_test1
Stack_testME$GLM_left = TestGLM_left
Stack_testME$RF1_left = TestRF1_left
Stack_testME$XGB_left = TestXGB_left

## Converting the new stacked dataframe into matrix
Stack_train_lable = as.factor(Stack_training_Set$left)
Stack_trainMat = sparse.model.matrix(left ~ . -left, data = Stack_training_Set )
Stack_StoreMatrix_Train = xgb.DMatrix(data = as.matrix(Stack_trainMat) , label = Stack_train_lable)

Stack_test_lable = as.factor(Stack_testME$left)
Stack_testMat = sparse.model.matrix(left ~ . -left, data = Stack_testME )
Stack_StoreMatrix_Test = xgb.DMatrix(data = as.matrix(Stack_testMat) , label = Stack_test_lable)

### The last fucking stackking model
### Final Model
## Using the earlier trControl and tuneGrid
Stack_XG_Model = train(Stack_StoreMatrix_Train , Stack_train_lable ,
                       method = 'xgbTree' ,
                       trControl = ControlParameters_XG,
                       tuneGrid = parameterGrid_XG
) 

## Building a Confusion Matrix
predTrain_StackTrain = predict(Stack_XG_Model,Stack_StoreMatrix_Train)
confusionMatrix(predTrain_StackTrain, Stack_training_Set$left)

## Trying to do a test Predicts
predTest_Stack = predict(Stack_XG_Model,Stack_StoreMatrix_Test)

# Writing this shit to a damn file
write.csv(predTest_Stack, 'SaiKarthik_StackingModel.csv', row.names = F)
