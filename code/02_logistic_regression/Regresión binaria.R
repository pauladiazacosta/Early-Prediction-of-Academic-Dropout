install.packages("openxlsx")
install.packages("pscl")
install.packages("caret")
install.packages("car")
library(caret)
library(pscl)
library(openxlsx)
library(car)
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

names(data_v2)

set.seed(1)

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

model <- glm(
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
  family = "binomial",
  data = train
)

options(scipen=999)

summary(model)

coef_table <- summary(model)$coefficients
results <- as.data.frame(coef_table)
results$OR <- exp(results$Estimate)

CI <- exp(confint(model))
results$CI_low  <- NA
results$CI_high <- NA

common <- intersect(rownames(results), rownames(CI))
results[common, "CI_low"]  <- CI[common, 1]
results[common, "CI_high"] <- CI[common, 2]

results$Variable <- rownames(results)
results <- results[, c("Variable",
                       "Estimate",
                       "Std. Error",
                       "z value",
                       "Pr(>|z|)",
                       "OR",
                       "CI_low",
                       "CI_high")]
write.xlsx(results,
           file = "logistic_regression_results.xlsx",
           rowNames = FALSE)

probabilities <- predict(model,newdata = test,type = "response")
probabilities
tabla_probabilidades <- data.frame(
  alumno = rownames(test),
  prob_dropout = probabilities
)
View(tabla_probabilidades)
write.xlsx(tabla_probabilidades,
           file = "probabilidades_dropout_test.xlsx",
           rowNames = FALSE)

roc_rg <- roc(response = test[["dropout_bin"]],
              predictor = probabilities,
              levels = rev(levels(test[["dropout_bin"]])))
auc(roc_rg)
plot(roc_rg)

predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
predicted_classes <- factor(predicted_classes, levels = c(0, 1))
test$dropout_bin  <- factor(test$dropout_bin, levels = c(0, 1))

cm <- confusionMatrix(predicted_classes, test$dropout_bin, positive = "1")
cm 

cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Pos Pred Value"]

precision <- cm$byClass["Pos Pred Value"]
recall    <- cm$byClass["Sensitivity"]

F1 <- 2 * (precision * recall) / (precision + recall)
F1

