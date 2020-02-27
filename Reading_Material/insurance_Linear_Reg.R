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


#Read data
insurance = read.csv("/home/CREDITVIDYA/10016/Desktop/edvancer/Linear Regression/insurance.csv",stringsAsFactors = F)
# 
View(insurance)
glimpse(insurance)

#Convert Sex and Smoker into numeric variables
insurance = insurance %>% 
  mutate(sex = as.numeric(sex=="female")) %>%
  mutate(smoker = as.numeric(smoker=="yes"))

glimpse(insurance)

#Convert region into dummy variables
table(insurance$region)

insurance = CreateDummies(insurance,"region",0)
glimpse(insurance)

#Split Data into Train and Test
set.seed(2)
s=sample(1:nrow(insurance),0.7*nrow(ld_train))
insurance_train1=insurance[s,]
insurance_train2=insurance[-s,]

#Fit the model
fit = lm(charges~.,insurance_train1)
summary(fit)

vif(fit)

#Identify the best fit model based on AIC
fit = step(fit)
summary(fit)

#RMSE
insurance_train2$predicted_charges = predict(fit,insurance_train2)
insurance_train2$error = insurance_train2$charges-insurance_train2$predicted_charges
sqrt((sum(insurance_train2$error)**2)/nrow(insurance_train2))
mean(insurance_train2$charges)
mean(insurance_train2$predicted_charges)