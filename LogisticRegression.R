#sensitivity vs 1 - specificity
### Precision = Correct Positives / Total Predicted Positives
### Recall = Correct Positives / Actual Positives
library(dplyr)
library(caTools)
library(ggplot2)
install.packages('ROCR')
library(ROCR)
setwd("D:\\PersonalFiles\\Data")
titanic = data.frame(read.csv('titanic.csv'), stringsAsFactors = F)
#View(titanic)

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


## Objective is to Evaluate what is the worth of a customer to put them in a Bucket.

## Data Cleaning Step
AgeMean=mean(titanic$Age,na.rm = T)

titanic = titanic %>% 
  mutate(Sex = as.numeric(titanic$Sex=='male'),
         Cabin = ifelse(Cabin=='','UNKNOWN',substr(Cabin,1,1)),
         Age = ifelse(is.na(Age), AgeMean , Age)) %>% 
  select(-Sex)

###titanic$Cabin = ifelse(titanic$Cabin=='','Unknown',substr(titanic$Cabin,1,1))
###titanic$Age   = ifelse(is.na(titanic$Age), AgeMean ,titanic$Age)

titanic = CreateDummies(titanic,'Embarked',0)
titanic = CreateDummies(titanic,'Pclass',0)
titanic = CreateDummies(titanic,'SibSp',20)
titanic = CreateDummies(titanic,'Parch',50)
titanic = CreateDummies(titanic,'Cabin',20)

View(titanic)
set.seed(123)
TitnicSample = sample.split(titanic,SplitRatio=0.75)
TrainingData_75 = subset(titanic,TitnicSample==T)
TrainingData_25 = subset(titanic,TitnicSample==F)

dim(TrainingData_25)

fit = glm(Survived~., data = titanic , family = binomial())
#ggplot(fit,aes()) +  geom_density(alpha = .2, fill = "#FF6666")

summary(fit)

predict_me = predict(fit,TrainingData_25,type = 'response')

table_mat = table(TrainingData_25$Survived , predict_me > 0.5)
table_mat

accuracy_Test = sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

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

prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec

f1 <- 2 * ((prec * rec) / (prec + rec))
f1


ROCR_pred = prediction(predict_me , TrainingData_25$Survived)
ROCR_pref = performance(ROCR_pred , 'tpr' , 'fpr')
plot(ROCR_pref, colorize = TRUE, text.adj = c(-0.2, 1.7))


# Do we have to remove 'Y' from thr test set?

p1 = (y=1/x)
p0 = (y=0/x) = 1 - p1
## Study more about likely hood.
