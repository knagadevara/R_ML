## Loading Required Packages
library(dplyr)
library(ggplot2)
library(car)
library(caTools)
library(caret)
library(randomForest)
library(Matrix)
library(xgboost)
library(magrittr)


#Create Dummies Function
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

## Setting Working Directory and Importing Files 
setwd("D:\\PersonalFiles\\MyOldFiles\\R_ML\\Project_2")

## Training Data
Store_df_train  = data.frame(read.csv('store_train.csv'), stringsAsFactors = F)
Main_training_Data = data.frame(subset(Store_df_train , select = -c(Id) , stringsAsFactors = F))
rm(Store_df_train)

## Testing Data
Store_df_test = data.frame(read.csv('store_test.csv'), stringsAsFactors = F)
Main_testing_Data = data.frame(subset(Store_df_test , select = -c(Id) , stringsAsFactors = F)) 
rm(Store_df_test)

## Identifying Train and Test Columns
Main_training_Data$DataType = 'TRAIN'
Main_testing_Data$DataType = 'TEST'
Main_testing_Data$store = ''

## Removing NA values for response variable in Training Set
Main_training_Data$store = ifelse(is.na(Main_training_Data$store), na.omit(Main_training_Data$store) , Main_training_Data$store)

Main_training_Data = Main_training_Data %>% 
 mutate(
   ToTalSales = as.numeric(sales0+sales1+sales2+sales3+sales4)) %>% 
  select(-c(Areaname,CouSub,State,storecode,sales0,sales1,sales2,sales3,sales4,countyname))

## To remove Outliers on Training Data
emore = boxplot(Main_training_Data$ToTalSales,plot = FALSE)$out
Outliers_Rows = Main_training_Data[which(Main_training_Data$ToTalSales %in% emore),]
Main_training_Data = Main_training_Data[-which(Main_training_Data$ToTalSales %in% emore),]
boxplot(Main_training_Data$ToTalSales)


Main_testing_Data = Main_testing_Data %>% 
  mutate(
    ToTalSales = as.numeric(sales0+sales1+sales2+sales3+sales4)) %>% 
  select(-c(Areaname,CouSub,State,storecode,sales0,sales1,sales2,sales3,sales4,countyname))

## Merging the Data
Complete_DataSet = rbind(x = Main_training_Data , y = Main_testing_Data)

for(col in names(Complete_DataSet)){
  if(sum(is.na(Complete_DataSet[,col]))>0 & !(col %in% c("DataType","store"))){
    Complete_DataSet[is.na(Complete_DataSet[,col]),col]=mean(Complete_DataSet[Complete_DataSet$DataType=='TRAIN',col],na.rm=T)
  }
}


## Mutating and removing unwanted variables
Complete_DataSet = Complete_DataSet %>% 
  mutate(
   ToTalSales = scale(ToTalSales),
   ToTalSales = as.numeric(ToTalSales),
   country = as.factor(country),
   store_Type = as.factor(store_Type),
   state_alpha = as.factor(state_alpha)
#    Areaname = as.factor(Areaname)
   )

## Merging the Data
Complete_DataSet = CreateDummies(Complete_DataSet,'store_Type',30)
Complete_DataSet = CreateDummies(Complete_DataSet,'state_alpha', 30)
Complete_DataSet = CreateDummies(Complete_DataSet,'country', 30)
Complete_DataSet = CreateDummies(Complete_DataSet,'countytownname', 30)

## Seperating the Data
set.seed(123)
Final_train1=Complete_DataSet %>% filter(DataType=='TRAIN') %>% select(-DataType)
Final_test1=Complete_DataSet %>% filter(DataType=='TEST') %>% select(-DataType,-store)

### Imputing the NA values in the predictor variable 'store'
Final_train1 = rfImpute(store ~ . , data = Final_train1 , iter = 20)

# Checking VIF to remove the variables with multi-colinearity
RM_VIF1=lm(store~. ,Final_train1)
sort(vif(RM_VIF1),decreasing = T)[1:3]

# Making Store as a factor
#Final_train1$store = as.factor(Final_train1$store)
#evels(Final_train1$store)

##Second Split 2
set.seed(456)
TT_split = sample.split(Final_train1 , SplitRatio = 0.75)
train_train1 = subset(Final_train1,TT_split==TRUE)
test_train1  = subset(Final_train1,TT_split==FALSE)

## Tuning to get the best mTry values
tuner_train = tuneRF(train_train1[,-c(train_train1$store)],train_train1[,train_train1$store], 
                     stepFactor = 0.25,
                     plot = TRUE,
                     ntreeTry = 500,
                     trace = TRUE,
                     improve = 0.05)

### Training through caret Model = RandomForest
### To check what models are available to names(getModelInfo())
### proximity=TRUE to check put the Proximity_Matrix
### for Classification mtry: No of variables to be consider at weak learning at each split is square-root of number of variables . can be changed later.
### for Regression mtry: No of variable to be consider at weak learning at each split is default setting at number fo variables devided by 3. can be changed later.
### tuning mtry will give a lower 'out of bag' error rate

ControlParameters = trainControl(method = 'cv' ,
                                 number = 5,
                                 savePredictions = TRUE
                                )
parameterGrid = expand.grid(mtry=c(10))

RF_Car_Model = train(store ~ . ,
                     data = train_train1 ,
                     method = 'rf' ,
                     trControl = ControlParameters,
                     TuneLength = 10 ,
                     tuneGrid = parameterGrid
                     ) 

predTest_final = predict(RF_Car_Model,train_train1)
confusionMatrix(predTest_final, train_train1$store)

test_train1$store = as.factor(test_train1$store)

predTest1_final = predict(RF_Car_Model,test_train1)
confusionMatrix(predTest1_final, test_train1$store)


## Converting the Data into Matrix for eXtreem-Boosting_model
trainMat = sparse.model.matrix(store ~ . -store, data = train_train1 )
#head(trainMat)
train_lable = train_train1[,'store']
StoreMatrix_Train = xgb.DMatrix(data = as.matrix(trainMat) , label = train_lable)


testMat = sparse.model.matrix(store ~ . -store, data = test_train1 )
test_lable = test_train1[,'store']
StoreMatrix_Test = xgb.DMatrix(data = as.matrix(testMat) , label = test_lable)

##### Parameters
ncls = length(unique(train_lable))

xgb_params = list('objective' = 'multi:softprob',
                  'eval_metric' = 'mlogloss' ,
                  'num_class' = ncls)

watchlist = list(train = StoreMatrix_Train , test = StoreMatrix_Test)


xB_model1 =  xgb.train(params = xgb_params , 
                       data = StoreMatrix_Train ,
                       nrounds = 75 ,
                       watchlist = watchlist ,
                       eta = 0.075)

error_xg = data.frame(xB_model1$evaluation_log)
plot(error_xg$iter , error_xg$train_mlogloss , col = 'blue')
lines(error_xg$iter , error_xg$test_mlogloss , col = 'red')
min(error_xg$test_mlogloss)
error_xg[error_xg$test_mlogloss == 0.529722 ,]
pxg_tr = predict(xB_model1 , newdata = StoreMatrix_Train)
pxg = predict(xB_model1 , newdata = StoreMatrix_Test)

pred_mat_tr = matrix(pxg , nrow = ncls , ncol = length(pxg_tr)/ncls) %>% 
  t() %>%  data.frame() %>%  mutate(lable = train_lable , max_prob = max.col(.,"last")-1)

pred_mat = matrix(pxg , nrow = ncls , ncol = length(pxg)/ncls) %>% 
  t() %>%  data.frame() %>%  mutate(lable = test_lable , max_prob = max.col(.,"last")-1)


table(pred_mat$max_prob, test_train1$store)

### Training through caret Model = XGBoosting
### To check what models are available to names(getModelInfo())

ControlParameters_XG = trainControl(method = 'cv' ,
                                 number = 5,
                                 savePredictions = TRUE
)
parameterGrid_XG = expand.grid(mtry=c(9,10,11,19))

XG_Model = train(store ~ . ,
                     data = train_train1 ,
                     method = 'rf' ,
                     trControl = ControlParameters_XG,
                     TuneLength = 10 ,
                     tuneGrid = parameterGrid_XG
) 

predTrain_final_XG = predict(XG_Model,train_train1)
confusionMatrix(predTrain_final, train_train1$store)

predTest_final_XG = predict(RF_Car_Model,test_train1)
confusionMatrix(predTest_final, test_train1$store)

## Writing the best model to file
write.csv(pred1_train_test_Final,'SaiKarthik_Nagadevara_P2_part2.csv',row.names = F ,col.names = T)
