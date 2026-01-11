install.packages("randomForest")
library(caret)       
library(randomForest)
library(pROC)

names(data_v2) <- gsub("[^A-Za-z0-9_]", "_", names(data_v2))
names(data_v2) <- gsub("_+", "_", names(data_v2))
names(data_v2) <- gsub("^_|_$", "", names(data_v2))
names(data_v2)

data_v2$Marital_status = factor(data_v2$Marital_status, levels=c("1","2","3","4","5","6"))
levels(data_v2$Marital_status)
data_v2$Course=factor(data_v2$Course, levels=c("33","171","8014","9003","9070","9085","9119","9130","9147","9238","9254","9500","9556","9670","9773","9853","9991"))
levels(data_v2$Course)
data_v2$Daytime_evening_attendance=factor(data_v2$Daytime_evening_attendance, levels=c("1","0"))
levels(data_v2$Daytime_evening_attendance)
data_v2$Nacionality=factor(data_v2$Nacionality, levels=c("1","2","6","11","13","14","17","21","22","24","25","26","32","41","62","100","101","103","105","108","109"))
levels(data_v2$Nacionality)
data_v2$Mother_s_occupation=factor(data_v2$Mother_s_occupation, levels=c("1","2","3","4","5","6","9","10","11","12","14","18","19","26","27","29","30","34","35","36","37","38","39","40","41","42","43","44"))
levels(data_v2$Mother_s_occupation)
data_v2$Father_s_occupation=factor(data_v2$Father_s_occupation, levels=c("1","2","3","4","5","6","9","10","11","12","13","14","18","19","22","25","26","27","29","30","31","33","34","35","36","37","38","39","40","41","42","43","44"))
levels(data_v2$Father_s_occupation)
data_v2$Displaced=factor(data_v2$Displaced, levels=c("0","1"))
levels(data_v2$Displaced)
data_v2$Educational_special_needs=factor(data_v2$Educational_special_needs, levels=c("0","1"))
levels(data_v2$Educational_special_needs)
data_v2$Gender=factor(data_v2$Gender, levels=c("0","1"))
levels(data_v2$Gender)
data_v2$Scholarship_holder=factor(data_v2$Scholarship_holder, levels=c("0","1"))
levels(data_v2$Scholarship_holder)
data_v2$International=factor(data_v2$International, levels=c("0","1"))
levels(data_v2$International)
data_v2$Target=factor(data_v2$Target, levels=c("Dropout","Enrolled","Graduate"))
levels(data_v2$Target)
data_v2$dropout_bin <- ifelse(data_v2$Target == "Dropout", 1, 0)
table(data_v2$Target, data_v2$dropout_bin)

set.seed(123)

split <- sample(c(TRUE, FALSE),
                nrow(data_v2),
                replace = TRUE,
                prob = c(0.8, 0.2))

train <- data_v2[split, ]
test  <- data_v2[!split, ]

train$dropout_bin <- as.factor(train$dropout_bin)
test$dropout_bin  <- as.factor(test$dropout_bin)

train[] <- lapply(train, function(x) if (is.character(x)) as.factor(x) else x)
test[]  <- lapply(test,  function(x) if (is.character(x)) as.factor(x) else x)

for (v in names(train)) {
  if (is.factor(train[[v]])) {
    test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))
  }
}

train <- train[complete.cases(train), ]
test  <- test[complete.cases(test), ]

rf <- randomForest(
  dropout_bin ~ Marital_status +
    Daytime_evening_attendance +
    Previous_qualification_grade +
    Mother_s_occupation +
    Admission_grade +
    Educational_special_needs +
    Scholarship_holder +
    International +
    Inflation_rate +
    Course +
    Previous_qualification +
    Nacionality +
    Father_s_occupation +
    Displaced +
    Gender +
    Age_at_enrollment +
    Unemployment_rate +
    GDP,
  data = train,
  ntree = 500,
  mtry = 4,
  importance = TRUE
)
rf

plot (rf)

pred_rf <- predict(rf, test, type = "response")
pred_rf

prob_rf <- predict(rf, test, type = "prob")
prob_rf

cm <- confusionMatrix(pred_rf, test$dropout_bin, positive = "1")
cm 

cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Pos Pred Value"]

precision <- cm$byClass["Pos Pred Value"]
recall    <- cm$byClass["Sensitivity"]

F1 <- 2 * (precision * recall) / (precision + recall)
F1

prob_rf_vec <- prob_rf[, "1"]
roc_rf <- roc(response = test[["dropout_bin"]],
              predictor = prob_rf_vec,
              levels = rev(levels(test[["dropout_bin"]])))
auc(roc_rf)
plot(roc_rf)

imp <- importance(rf)
imp_gini <- imp[, "MeanDecreaseGini"]
imp_gini_sorted <- sort(imp_gini, decreasing = TRUE)
head(imp_gini_sorted, 5)

