# Analysis : Churn Prediction - Logistic regression
# -------------------
library(data.table)
library(lubridate)
library(DMwR)
library(caret)
library(e1071)

setwd("C:\\Users\\User\\Desktop\\PF\\churn")
member = read.csv("Member Dataset.csv")
member = as.data.table(member)
member[is.na(member)] = 0

#creating age groups
member[,Age_treated := ifelse(Age <= 0 | Age > 100 , mean(Age), Age)]


#Adding type of customer
member[,Cust_type := ifelse(ClosedDate == "", "Active", "Churn")]

#Duration
member[,loyalty_months := ifelse(Cust_type== "Active",
                                 interval(mdy(EarliestMemAcctDate), mdy("10-01-2018")) %/% months(1),
                                 interval(mdy(EarliestMemAcctDate), mdy(ClosedDate)) %/% months(1))]

model_var = member[,-c("Member_ID","Age","EarliestMemAcctDate","ClosedDate","July_Bal","Aug_Bal",
                      "Sept_Bal","ZipCode_Validated")]
#write.csv(model_var,"model_var2.csv", row.names = F)

model_var[, Cust_type:= as.factor(Cust_type)]
Ovr_model_var = SMOTE(Cust_type~., model_var, perc.over = 200, k = 5, learner = NULL)

churn_pred = glm(Cust_type~.,family = binomial(link = 'logit'), Ovr_model_var)


# train control - 4 fold cross validation repeated 4 times
for_cv = trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 4,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

# logistic regression model for fitting on the over sampled data
logreg <- train(Cust_type ~., Ovr_model_var,
                method = "glm",
                family = "binomial",
                trControl = for_cv,
                metric = "ROC")

# Predicting on the original data
mod_pred = predict(logreg,model_var[,-c("Cust_type")],type = "raw")

member[,churnProb := mod_pred]
confusionMatrix(member$Cust_type, member$churnProb)
