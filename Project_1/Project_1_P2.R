## Loading Required Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(caTools)
# 
# ## Removing Outliers Function
# remove_outliers = function(x , na.rm = TRUE , ... ){
#   
#   ## Finding the position of first and third quertile
#   qnt = quantile(x , probs = c(.25,.75), na.rm = na.rm , ...)
#   H = 1.5 * IQR(x , na.rm = na.rm)
#   y = x
#   y[x < (qnt[1] - H)] <-- NA
#   y[x > (qnt[2] + H)] <-- NA
#   
#   #get rid of NA
#   x[!is.na(x)]
# }
# 

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
setwd("D:\\PersonalFiles\\MyOldFiles\\R_ML\\Project_1")

## Training Data
housing_df_train  = data.frame(read.csv('housing_train.csv'), stringsAsFactors = F)
Main_training_Data = data.frame(subset(housing_df_train , select = -c(Address) , stringsAsFactors = F))

## Testing Data
housing_df_test = data.frame(read.csv('housing_test.csv'), stringsAsFactors = F)
Main_testing_Data = data.frame(subset(housing_df_test , select = -c(Address) , stringsAsFactors = F)) 

## Identifying Train and Test Columns
Main_training_Data$DataType = 'TRAIN'
Main_testing_Data$DataType = 'TEST'
Main_testing_Data$Price = ''
#ncol(Main_training_Data)
#ncol(Main_testing_Data)

## Removing NA values for response variable in Training Set
Main_training_Data$Price = ifelse(is.na(Main_training_Data$Price), na.omit(Main_training_Data$Price) , Main_training_Data$Price)

nrow(Main_training_Data)

boxplot(Main_training_Data$BuildingArea)
summary(Main_training_Data$BuildingArea)

table(is.na(Merged_DataSet$BuildingArea))
table(is.na(Merged_DataSet$YearBuilt))
table(is.na(Merged_DataSet$Postcode))
table(is.na(Merged_DataSet$Landsize))
View(sort(prop.table(table(Merged_DataSet$Postcode))))





## Merging the Data
Merged_DataSet = rbind(x = Main_training_Data , y = Main_testing_Data)

str(Merged_DataSet)

# Complete_DataSet = Merged_DataSet %>%
#   mutate (
#     Bedroom2 = ifelse(is.na(Bedroom2), na.omit(Bedroom2) , Bedroom2),
#     Bathroom = ifelse(is.na(Bathroom), na.omit(Bathroom) , Bathroom),
#     Car = ifelse(is.na(Car), na.omit(Car) , Car),
#     Landsize = as.numeric(ifelse(is.na(Landsize), 'UNK' , Landsize)),

# #    CouncilArea = ifelse(CouncilArea=='', 'UNKNOWN' , CouncilArea),
#     Price = as.numeric(Price))
# #    Suburb = as.character(Suburb),
# #    SellerG = as.character(SellerG))

## Removing NA values and converting them to Numeric.
Complete_DataSet = Merged_DataSet %>%
  mutate (
    Bedroom2 = as.numeric(ifelse(is.na(Bedroom2), na.omit(Bedroom2) , Bedroom2)), 
    Bathroom = as.numeric(ifelse(is.na(Bathroom), na.omit(Bathroom) , Bathroom)),
    Car = ifelse(is.na(Car), na.omit(Car) , Car),
    CouncilArea = as.numeric(ifelse(is.na(CouncilArea), na.omit(CouncilArea) , CouncilArea)),
    SellerG = as.numeric(ifelse(is.na(SellerG), na.omit(SellerG) , SellerG)),
    Distance = as.numeric(ifelse(is.na(Distance), na.omit(Distance) , Distance)),
    Landsize = ifelse(Landsize <= 1500, Landsize, ''),
    Landsize = as.numeric(ifelse(is.na(Landsize), na.omit(Landsize) , Landsize)),
    Postcode = as.numeric(as.factor(Merged_DataSet$Postcode)),
    Price = as.numeric(Price)) %>% 
  select(-c(BuildingArea , YearBuilt))

for(col in names(Complete_DataSet)){
  if(sum(is.na(Complete_DataSet[,col]))>0 & !(col %in% c("DataType","Price"))){
    Complete_DataSet[is.na(Complete_DataSet[,col]),col]=mean(Complete_DataSet[Complete_DataSet$DataType=='TRAIN',col],na.rm=T)
  }
}


## Creating Dummy Variables
Complete_DataSet = CreateDummies(Complete_DataSet, 'Rooms' , 100)
Complete_DataSet = CreateDummies(Complete_DataSet, 'Type' , 1000)
Complete_DataSet = CreateDummies(Complete_DataSet, 'Method' , 100)
Complete_DataSet = CreateDummies(Complete_DataSet, 'Bedroom2' , 50)
Complete_DataSet = CreateDummies(Complete_DataSet, 'Bathroom' , 50)
Complete_DataSet = CreateDummies(Complete_DataSet, 'Car' , 30)
Complete_DataSet = CreateDummies(Complete_DataSet, 'Suburb' , 20)
Complete_DataSet = CreateDummies(Complete_DataSet, 'Postcode' , 50)

str(Complete_DataSet)
## Checking the proposition of NA in Datasets
myLnd = sort(prop.table(table(Complete_DataSet$Landsize)))
View(myLnd)
mySub = sort(prop.table(table(Complete_DataSet$Suburb)))
View(mySub)

View(Complete_DataSet) 


## Seperating the Data
Final_train=Complete_DataSet %>% filter(DataType=='TRAIN') %>% select(-DataType)
Final_test=Complete_DataSet %>% filter(DataType=='TEST') %>% select(-DataType,-Price)

# mean(Final_train$Landsize)
# myLand = sort(prop.table(table(Final_train$Landsize)))
# View(mySub)
# 
# boxplot(Final_train$Landsize)
# boxplot(Final_train$Distance)
# 
# Final_train$Landsize123 = Final_train$Landsize
# 
# summary(Final_train$Landsize)
# 
# benchMark = 617 + 1.5*IQR(Final_train$Landsize)
# Final_train$Landsize123[Final_train$Landsize123 > benchMark]


## Splitting Final_train to train1 and test2
# set.seed(123)
# TT_split = sample.split(Final_train$Price , SplitRatio = 0.75)
# train1 = subset(Final_train,TT_split==TRUE)
# test2  = subset(Final_train,TT_split==FALSE)
## Building a Lazy model
#for_vif=lm(Price~.-Rooms_3-Bedroom2_2-Car_1-Bathroom_2-SellerG-Suburb_CarltonNorth-Suburb_Viewbank,data=train1)
#Final_Fit=lm(Price~.-Rooms_3-Bedroom2_2-Car_1-Bathroom_2-SellerG-Suburb_CarltonNorth-Suburb_Viewbank-Suburb_Heidelberg-Suburb_Windsor-Suburb_Rosanna-Suburb_AirportWest-Suburb_Niddrie-Suburb_Strathmore-Suburb_Ivanhoe-Suburb_KeilorEast-Suburb_Prahran-Suburb_Sunshine-Suburb_MooneePonds-Suburb_PortMelbourne-Suburb_Hawthorn-Suburb_Glenroy-Suburb_Essendon-Suburb_SouthYarra-Suburb_StKilda-Method_SP-Method_PI-Car_0-Suburb_Gowanbrae-Suburb_Parkville-Suburb_Yallambie-Suburb_Albion-Suburb_Aberfeldie-Suburb_Alphington,data=Final_train)
# summary(for_vif)

Final_Fit2=lm(Price~.-Bathroom_1,Final_train)
sort(vif(Final_Fit2),decreasing = T)[1:5]
vif(Final_Fit2)
summary(Final_Fit2)
step(Final_Fit2)

test.predictions=predict(Final_Fit,newdata=Final_test)
View(Final_test)

write.csv(test.predictions,'SaiKarthik_Nagadevara_P1_part2.csv',row.names = F)
