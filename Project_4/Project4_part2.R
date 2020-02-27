## Loading Required Packages
library(dplyr)
library(ggplot2)
library(car)
library(caTools)
library(caret)
library(randomForest)
library(utils)

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

##### Importing Data
## Setting Working Directory and Importing Files 
setwd("D:\\PersonalFiles\\MyOldFiles\\R_ML\\Project_4")
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

str(Main_training_Data)
View(Main_training_Data)

## To remove Outliers on Training Data
emore = boxplot(Main_training_Data$average_montly_hours,plot = FALSE)$out
Outliers_Rows = Main_training_Data[which(Main_training_Data$average_montly_hours %in% emore),]
Main_training_Data = Main_training_Data[-which(Main_training_Data$average_montly_hours %in% emore),]
boxplot(Main_training_Data$average_montly_hours)



## DummyScope salary , sales , number_projects , time_spend_company
## Conversions Time_spend_company

Complete_DataSet = rbind(x = Main_training_Data , y = Main_testing_Data)
sort(table(Main_testing_Data$average_montly_hours))

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

# for(col in names(Complete_DataSet)){
#   if(sum(is.na(Complete_DataSet[,col]))>0 & !(col %in% c("DataType","left"))){
#     Complete_DataSet[is.na(Complete_DataSet[,col]),col]=mean(Complete_DataSet[Complete_DataSet$DataType=='TRAIN',col],na.rm=T)
#   }
# }

table(Complete_DataSet$left)

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

## Split2
set.seed(456)
TT_split = sample.split(Final_train1 , SplitRatio = 0.75)
train_train1 = subset(Final_train1,TT_split==TRUE)
test_train1  = subset(Final_train1,TT_split==FALSE)


model1 = glm(left ~ . -time_spend_company_3 -number_project_4 -time_spend_company_2  ,train_train1 , family = 'binomial')
library(e1071)
library(pROC)
summary(model1)
val.score = predict(model1,newdata = train_train1 , type = 'response')
auc_score=auc(roc(train_train1$left , val.score))
auc_score
fitted.results12 <- ifelse(val.score > 0.409,1,0)
table_mat = table(train_train1$left , fitted.results12)
table_mat_abs = table(train_train1$left , val.score > 0.415)


install.packages("ROCR")
library(ROCR)
ROCR_pred = prediction(val.score , train_train1$left)
ROCR_pref = performance(ROCR_pred , 'tpr' , 'fpr')
plot(ROCR_pref, colorize = TRUE, text.adj = c(-0.2, 1.7))
perf1 = performance(ROCR_pred, "sens", "spec")
plot(perf1)
plot(ROCR_pref, colorize = TRUE)
plot(ROCR_pref, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
plot(ROCR_pref, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),main = "ROC CURVE")
abline(a=0, b=1)
auc_score21 <- performance(ROCR_pred, measure = "auc")
auc_score21 <- auc_score21@y.values[[1]]
auc_score21
auc_score21 <- round(auc_score21, 4)
legend (.5,.4,auc_score21, title = "AUC", cex =1)




#### Model 1
set.seed(889)
rf_model_train1 = randomForest(left ~ . -time_spend_company_3 -number_project_4 -time_spend_company_2  ,Final_train1 )

#attributes(rf_model_train1)#, Final_train1$store)
pred1_train = predict(rf_model_train1,Final_train1)
confusionMatrix(pred1_train, Final_train1$left)
plot(rf_model_train1)

## Testing on Actual
pred1_train_test_Final = predict(rf_model_train1,Final_test1)

write.csv(pred1_train_test_Final,'SaiKarthik_Nagadevara_P4_part2.csv',row.names = F)



##
tuner_train = tuneRF(train_train1[,-c(train_train1$left)],train_train1[,train_train1$left], 
                     stepFactor = 0.25,
                     plot = TRUE,
                     trace = TRUE,
                     mtryStart = 10,
                     ntreeTry = 20,
                     improve = 0.05)
##
tuner_train = tuneRF(train_train1[,-c(train_train1$left)],train_train1[,train_train1$left], 
                     stepFactor = 0.25,
                     plot = TRUE,
                     trace = TRUE,
                     improve = 0.05)

table(is.na(train_train1$left))

class(train_train1$left)
