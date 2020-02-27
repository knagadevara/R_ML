## Loading Required Packages
library(dplyr)
#library(ggplot2)
library(car)
library(caTools)
library(caret)
library(randomForest)
library(utils)

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

# precision <- function(matrix) {
#   # True positive
#   tp <- matrix[2, 2]
#   # false positive
#   fp <- matrix[1, 2]
#   return (tp / (tp + fp))
# }
# 
# recall <- function(matrix) {
#   # true positive
#   tp <- matrix[2, 2]# false positive
#   fn <- matrix[2, 1]
#   return (tp / (tp + fn))
# }


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
#   ToTalSales = scale(ToTalSales),
    country = as.factor(country),
    store_Type = as.factor(store_Type),
    state_alpha = as.factor(state_alpha),
    Areaname = as.factor(Areaname))

## Merging the Data
Complete_DataSet = CreateDummies(Complete_DataSet,'store_Type',30)
Complete_DataSet = CreateDummies(Complete_DataSet,'state_alpha', 30)
Complete_DataSet = CreateDummies(Complete_DataSet,'country', 30)
#Complete_DataSet = CreateDummies(Complete_DataSet,'Areaname', 30)
Complete_DataSet = CreateDummies(Complete_DataSet,'countytownname', 30)

## Seperating the Data
set.seed(123)
Final_train1=Complete_DataSet %>% filter(DataType=='TRAIN') %>% select(-DataType)
Final_test1=Complete_DataSet %>% filter(DataType=='TEST') %>% select(-DataType,-store)

# Checking VIF to remove the variables with multi-colinearity
# RM_VIF1=lm(store~. ,Final_train1)
# sort(vif(RM_VIF1),decreasing = T)[1:3]


# Making Store as a factor
Final_train1$store = as.factor(Final_train1$store)
levels(Final_train1$store)

## Split2
set.seed(456)
TT_split = sample.split(Final_train1 , SplitRatio = 0.75)
train_train1 = subset(Final_train1,TT_split==TRUE)
test_train1  = subset(Final_train1,TT_split==FALSE)

#### Model 1
set.seed(789)
rf_model_train1 = randomForest(formula = store ~ . , data = train_train1 ,
                               ntree = 400,
                               mtry = 10 , 
                               importance = TRUE,
                               proximity = TRUE
#                               classwt = 0,
#                               samplesize = c(500,600,700)
)

#attributes(rf_model_train1)#, Final_train1$store)
pred1_train = predict(rf_model_train1,train_train1)
confusionMatrix(pred1_train, train_train1$store)
plot(rf_model_train1)

## Testing the model
pred1_train_test = predict(rf_model_train1,test_train1)
confusionMatrix(pred1_train_test, test_train1$store)


#### Model 2
set.seed(788)
rf_model_train2 = randomForest(formula = store ~ . , data = train_train1 ,
                               ntree = 400,
                               mtry = 50 , 
                               importance = TRUE,
                               proximity = TRUE)


#attributes(rf_model_train1)#, Final_train1$store)
pred1_train2 = predict(rf_model_train2,train_train1)
confusionMatrix(pred1_train2, train_train1$store)
plot(rf_model_train1)


## Testing the model
pred1_train_test2 = predict(rf_model_train2,test_train1)
confusionMatrix(pred1_train_test2, test_train1$store)

## Model 3
set.seed(953)
rf_notune = randomForest(store ~ . , data = train_train1)
pred1_notune = predict(rf_notune,train_train1)
confusionMatrix(pred1_notune, train_train1$store)

## Tuning mTry
tuner_train = tuneRF(Final_train1[,-c(Final_train1$store)],Final_train1[,Final_train1$store], 
       stepFactor = 0.25,
       plot = TRUE,
       ntreeTry = 500,
       trace = TRUE,
       improve = 0.05)



## Applying Model 1 on Actual
## Submitted with mtry 19 on 400 ntree score is 77
## Tried with mtry = 10
set.seed(789)
rf_model_train1_final = randomForest(formula = store ~ . , data = Final_train1 ,
                               ntree = 400,
                               mtry = 10 , 
                               importance = TRUE,
                               proximity = TRUE)

#attributes(rf_model_train1)#, Final_train1$store)
pred1_train_final = predict(rf_model_train1_final,Final_train1)
confusionMatrix(pred1_train_final, Final_train1$store)
plot(rf_model_train1_final)

## Testing on Actual
pred1_train_test_Final = predict(rf_model_train1_final,Final_test1)

write.csv(pred1_train_test_Final,'SaiKarthik_Nagadevara_P2_part2.csv',row.names = F)

## Applying Model 2 on Actual
set.seed(789)
rf_model_train2_final = randomForest(formula = store ~ . , data = Final_train1 ,
                                     ntree = 1000,
                                     mtry = 75 , 
                                     importance = TRUE,
                                     proximity = TRUE)

#attributes(rf_model_train1)#, Final_train1$store)
pred1_train2_final = predict(rf_model_train2_final,Final_train1)
confusionMatrix(pred1_train2_final, Final_train1$store)
plot(rf_model_train2_final)
# 
# ## Testing the model on Train
# pred1_train_test = predict(rf_model_train1,Final_train1)
# confusionMatrix(pred1_train_test, Final_train1$store)

## Testing on Actual
pred1_train2_test_Final = predict(rf_model_train2_final,Final_test1)

write.csv(pred1_train_test_Final,'SaiKarthik_Nagadevara_P2_part2_mtry75.csv',row.names = F ,col.names = T)

### Training through caret
RF_Car_Model = train(store~. ,
                     train_train1 ,
                     method = 'rf' , 
                     TuneLength = 10 ,
                     trControl = trainControl(
                     method = 'cv' ,
                     number = 2)) 
  #                     classProbs = TRUE))
?make.names

predTest_final = predict(RF_Car_Model,train_train1)
confusionMatrix(predTest_final, train_train1$store)

predTest1_final = predict(RF_Car_Model,test_train1)
confusionMatrix(predTest1_final, test_train1$store)


# Fit a GBM
set.seed(102)  # for reproducibility
gbm1 <- gbm(store ~ ., data = train_train1, var.monotone = c(0, 0, 0, 0, 0, 0),
            distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)  
