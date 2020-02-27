library(dplyr)
library(ggplot2)
library(caTools)
library(car)


insurance = data.frame(read.csv("D:\\PersonalFiles\\Data\\insurance.csv") , stringsAsFactors = F)

insurance_df = insurance %>% 
                mutate(smoker=as.numeric(smoker=='yes'),
                      sex=as.numeric(sex=='male'),
                      southW=as.numeric(region=='southwest'),
                      southE=as.numeric(region=='southeast'),
                      northW=as.numeric(region=='northwest'),
                      northE=as.numeric(region=='northeast')
                      ) %>% 
                select(-c(region,northW))


set.seed(123)
TT_split = sample.split(insurance_df , SplitRatio = 0.75)
train1 = subset(insurance_df,TT_split==TRUE)
test2  = subset(insurance_df,TT_split==FALSE)

fit = lm(charges~. , train1 )
vif(fit)

## step() to get the best AIC
fit = step(fit)



## Always remove one of the dummy variables, example region has been devided into 4, while making it to the
## example: c = 1 - (A + B)
## Hypothesis for Linear Regression
## if P-Value is greater than 5
##sufficient evidence in support of null hypothysis and we fail to reject it.
## Which means X1 dosent impact Y

## Outlier Treatment
## if calculated Coks distance is more than 1 for any observation then it is a Outlier.\

