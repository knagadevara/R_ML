## Seperating the Data
set.seed(123)
Final_train1=Complete_DataSet %>% filter(DataType=='TRAIN') %>% select(-DataType)
Final_test1=Complete_DataSet %>% filter(DataType=='TEST') %>% select(-DataType,-store)

table(is.na(Final_train1))
View(Final_train1)

# prec <- precision()
# prec
# rec <- recall()
# rec
# 
# f1_stat <- 2 * ((prec * rec) / (prec + rec))
# f1
# 
# install.packages("ROCR")
# library(ROCR)
# ROCR_pred = prediction(val.score , Final_train1$store)
# ROCR_pref = performance(ROCR_pred , 'tpr' , 'fpr')
# plot(ROCR_pref, colorize = TRUE, text.adj = c(-0.2, 1.7))
# perf1 = performance(ROCR_pred, "sens", "spec")
# plot(perf1)


# plot(ROCR_pref, colorize = TRUE)
# plot(ROCR_pref, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
# plot(ROCR_pref, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),main = "ROC CURVE")
# abline(a=0, b=1)
# 
# auc_score21 <- performance(ROCR_pred, measure = "auc")
# auc_score21 <- auc_score21@y.values[[1]]
# auc_score21
# auc_score21 <- round(auc_score21, 4)
# legend (.5,.4,auc_score21, title = "AUC", cex =1)

#write.csv(TestScore_results,'SaiKarthik_Nagadevara_P2_part2.csv',row.names = F ,col.names = 'store')


RM_VIF1=lm(store~. ,Final_train1)
sort(vif(RM_VIF1),decreasing = T)[1:3]
summary(RM_VIF1)
#step(RM_VIF1)
install.packages('e1071')
library(e1071)

# res <- train(lr_scaled[,1:4], lr_scaled$Class, method='glm',metric = 'Accuracy', trControl= trainControl(method='boot', savePredictions = TRUE))
model_glm <- train(store~. , data=Final_train1, method='glm')
val.score = predict(model_glm,newdata = Final_train1 , type = 'prob')

model_glm

model_glm2 = train(store ~., data=Final_train1, method='rpart')#, metric = 'Accuracy', trControl= trainControl(method='boot', savePredictions = TRUE))
model_glm2

fit_log = glm(store~.,data = Final_train1,family = 'binomial')
summary(fit_log)
val.score = predict(model_glm2,newdata = Final_train1 , type = 'response')
auc_score=auc(roc(Final_train1$store , val.score))
auc_score
fitted.results12 <- ifelse(val.score > 0.409,1,0)
table_mat = table(Final_train1$store , fitted.results12)
table_mat_abs = table(Final_train1$store , val.score12 > 0.415)

#summary(fit_log)

val.scorefin = predict(glm_final,newdata = Final_train1 , type = 'response')
auc_scorefin=auc(roc(Final_train1$store , val.scorefin))
auc_scorefin
fitted.results <- ifelse(val.scorefin > 0.41,1,0)
table_mat = table(Final_train1$store , fitted.results)
table_mat_abs = table(Final_train1$store , val.scorefin > 0.415)
accuracy_Test = sum(diag(table_mat_abs)) / sum(table_mat_abs)
accuracy_Test
TestScore = predict(fit_log,newdata = Final_test1 , type = 'response')
TestScore_results <- ifelse(TestScore > 0.41,1,0)



table(is.na(TestScore_results))
