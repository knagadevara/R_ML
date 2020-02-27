## Loading Required Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(caTools)

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
Main_training_Data = data.frame(subset(Store_df_train , select = -c(Id , storecode) , stringsAsFactors = F))

## Testing Data
Store_df_test = data.frame(read.csv('store_test.csv'), stringsAsFactors = F)
Main_testing_Data = data.frame(subset(Store_df_test , select = -c(Id , storecode) , stringsAsFactors = F)) 

## Identifying Train and Test Columns
Main_training_Data$DataType = 'TRAIN'
Main_testing_Data$DataType = 'TEST'
Main_testing_Data$store = ''
#ncol(Main_training_Data)
#ncol(Main_testing_Data)

## Removing NA values for response variable in Training Set
Main_training_Data$store = ifelse(is.na(Main_training_Data$store), na.omit(Main_training_Data$store) , Main_training_Data$store)


## what is the total sales (sum of all sales) of Supermarket Type1 
## in area Kennebec County, ME?
ToTSales_KennbecCounty = Main_training_Data %>%
filter(Areaname == 'Kennebec County, ME',store_Type == 'Supermarket Type1') %>%
  summarise(sum(sales0 + sales1 + sales2 + sales3 + sales4))

##  should country be treated as numeric type or character? Note: Answers are not case sensitive . 
whatCount = levels(Main_training_Data$country)
table(is.na(Main_training_Data$country))
View(sort(prop.table(table(Main_training_Data$country))))

## Find out number of unique categories of variable Areaname.
as.factor(Main_training_Data$Areaname)
levels(Main_training_Data$Areaname)

## For store type grocery store what is the response rate ? 
## [ what % of obs have response value as 1 ]  Round off to two decimal digits. 
## Note : Answer needs to be in hundreds . Ex : 12.34 ( NOT 0.1234 )
WhatResGrocery = Main_training_Data %>% 
  group_by(store_Type , store) %>% 
  summarise(n())


prop = (182 / 3338) * 100
WhatResGrocery2 = tapply(Main_training_Data$store, Main_training_Data$store_Type, sum)
what_gross = prop.table(table(Main_training_Data$store_Type,exclude = Main_training_Data$store == 1))
View(Main_training_Data$store_Type)
str(Main_training_Data)

## Do all the sales variable follow normal distribution?
plotsales0 = plot(density(sort(Main_training_Data$sales0)))
plotsales1 = plot(density(Main_training_Data$sales1))
plotsales2 = plot(density(Main_training_Data$sales2))
plotsales3 = plot(density(Main_training_Data$sales3))
plotsales4 = plot(density(Main_training_Data$sales4))


### Number of outliers for total sales 
### based on following limits (q1-1.5*IQR, q3+1.5*IQR)?
#Solution
Total_Sales = Main_training_Data$sales0 + Main_training_Data$sales1 + Main_training_Data$sales2 + Main_training_Data$sales3 + Main_training_Data$sales4
View(Total_Sales)
boxplot(Total_Sales)
summary(Total_Sales)
TsalesOutMin = (3422 - 1.5*IQR(Total_Sales))
TsalesOutMax = (4969 + 1.5*IQR(Total_Sales))
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 2173    3422    3888    4307    4969   11140
WhatMinTot = Total_Sales[Total_Sales < TsalesOutMin]
WhatMaxTot = Total_Sales[Total_Sales > TsalesOutMax]
ToTNum = length(WhatMinTot) + length(WhatMaxTot)
mtot = rbind(WhatMinTot , WhatMaxTot)


## which store type has maximum variance in total sales?
MxStVar = tapply(Total_Sales, Main_training_Data$store_Type , var)
sort(MxStVar)[4]

## How many dummies will you create for variable state_alpha?
levels(Main_training_Data$state_alpha) - 1
