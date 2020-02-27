library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
# Use ameliea package to check the NA values in dataset
setwd("D:\\PersonalFiles\\MyOldFiles\\R_ML\\Project_1")
housing_df_train = data.frame(read.csv('housing_train.csv'), stringsAsFactors = F)

## removinf the dependant variable from training set
# housing_df_train1 = subset(housing_df_train , select = -c(Price) )
housing_df_train1 = subset(housing_df_train ,  stringsAsFactors = F)

## Combining two datasets with 
#Total_Data = merge(x=housing_df_train1,y=housing_df_test,all=T)



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

str(housing_df_train)

### Removing NA values from Data
housing_df_train = separate(housing_df_train , Address , sep=' ' , into = c('code' , 'SubArea' , 'Street'))
Total_Data_1 = housing_df_train %>%
  mutate (
         Price = ifelse(is.na(Price), na.omit(Price) , Price),
         Bedroom2 = ifelse(is.na(Bedroom2), na.omit(Bedroom2) , Bedroom2),
         Bathroom = ifelse(is.na(Bathroom), na.omit(Bathroom) , Bathroom),
         Car = ifelse(is.na(Car), na.omit(Car) , Car),
         Landsize = ifelse(is.na(Landsize), 'UNKNOWN' , Landsize),
         BuildingArea = ifelse(is.na(BuildingArea), 'UNKNOWN' , BuildingArea),
         YearBuilt = ifelse(is.na(YearBuilt), 'UNKNOWN' , YearBuilt) )%>% 
#         CouncilArea = ifelse(CouncilArea=='', 0 , CouncilArea)) %>% 
  select(-c(BuildingArea , code , Street , YearBuilt , SubArea))


Total_Data_1 = CreateDummies(Total_Data_1, 'Rooms' , 100)
Total_Data_1 = CreateDummies(Total_Data_1, 'Type' , 1000)
Total_Data_1 = CreateDummies(Total_Data_1, 'Method' , 100)
Total_Data_1 = CreateDummies(Total_Data_1, 'Bedroom2' , 25)
Total_Data_1 = CreateDummies(Total_Data_1, 'Bathroom' , 50)
Total_Data_1 = CreateDummies(Total_Data_1, 'Car' , 20)
#Total_Data_1 = CreateDummies(Total_Data_1, 'Suburb' , 20)

#table(Total_Data_1$SubArea)
View(Total_Data_1)

set.seed(456)
Training_Set = sample.split(Y = Total_Data_1$Price , SplitRatio = .75) 
Training_Set_75 = subset(Total_Data_1 , Training_Set==T)
Testing_Set_25 = subset(Total_Data_1 , Training_Set==F)


View(Training_Set_75)
fit = lm(Price~. , Training_Set_75)
whtAlias = alias(fit)
View(whtAlias)
Fit_Data = summary(fit)




## What is the variance in Price?
Price = Total_Data_1$Price
var(Price)
PriceVariance = mean(Price,na.rm = T) / length(Price) - 1
PriceVariance


## What is the count of NA values in YearBuilt?
CountYear = Training_Set %>%
  group_by(YearBuilt=='UNKNOWN') %>%
  summarise(n())
CountYear
str(Total_Data_1)
## What is the difference in average price between house type h and t?
Values = tapply(Total_Data_1$Price, Total_Data_1$Type, mean)
class(Values)
Dimension = Values['h'] - Values['t']
Dimension

## How many unique values variable postcode takes?
PostCode = Training_Set$Postcode
as.factor(PostCode)
length(unique(PostCode))

## How should you treat post code . As a categorical 
## variable or numeric variable ( write "categorical" 
## or "numeric" as your answer) Note: Answers are not case sensitive.
#-> Categorical

## Does distance follow a normal distribution?
## Note: Just write 'Yes' or 'No' . If you write sentences, 
## automated grading will consider it incorrect .
## Answers are not case sensitive . 
plot(density(ieu))
ieu = sort(Training_Set$Distance , decreasing = F)
ieu
MyPlot = ggplot(data = Training_Set , aes(x = Distance )) + geom_histogram()
MyPlot

#MyPlot = ggplot(data = Training_Set$Distance , aes()) + geom_point()

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#normalize(Training_Set$Distance)#, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")



## Which seller has maximum value transactions? [ sum of price ]
## Note: you need to write name of just that seller . Any further 
## details will result in your answer being marked as wrong. Answers 
## are not case sensitive.
# Method 1
Sell_Price = tapply(Training_Set$Price, Training_Set$SellerG ,  sum)
max(Sell_Price)

# Method 2
SellerWho = Training_Set %>% 
  group_by(SellerG) %>% 
  summarise(tot = sum(Price)) %>% 
  filter(tot == max(tot))
SellerWho


## Which CouncilArea has maximum average price?
## Note: you need to write name of just that CouncilArea.
## Any further details will result in your answer being marked as wrong.
## Answers are not case sensitive.
CounWho = Total_Data_1 %>% 
  group_by(CouncilArea) %>% 
  summarise(mean_Coun = mean(Price)) %>% 
  filter(mean_Coun == max(mean_Coun))
View(CounWho)
sort(CounWho['mean_Coun'])

Values = tapply(Total_Data_1$Price, Total_Data_1$CouncilArea, mean)
max(Values)

## which CouncilArea has maximum variance in the price?
## Note: you need to write name of just that CouncilArea.
## Any further details will result in your answer being marked as wrong.
## Answers are not case sensitive . 

WhCon = Total_Data_1 %>% 
  group_by(CouncilArea) %>% 
  summarise(CouncilAreaVariance = var(Price)) %>% 
  filter(CouncilAreaVariance == max(CouncilAreaVariance))
View(WhCon)



## Should we use Address as is in the modeling process?
## Note : Just write 'Yes' or 'No' . If you write sentences,
## automated grading will consider it incorrect .Answers are not case sensitive.
Total_Data = separate(Training_Set , Address , sep=' ' , into = c('code' , 'SubArea' , 'Street'))
View(Total_Data)
table(Total_Data$Suburb)
str(Total_Data$Suburb)

