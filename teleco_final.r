library(ggplot2)
library(dplyr)
library(scales)
library(ggpubr)
library(ggcorrplot)
library(leaps)
library(car)
library(pROC)
library(caret)
library(performanceEstimation)

telecom <- read.csv("/Users/yurijeong/Desktop/Telco-Customer-Churn.csv")

# summary of telecom dataset
summary(telecom)

# load first five rows from telecom data
head(telecom)

# check NA value
colSums(is.na(telecom))

# delete NA values
telecom <- na.omit(telecom)

# check NA value
sum(is.na(telecom))

# delete customerID column since it's not needed
telecom <- subset(telecom, select = -customerID)
colnames(telecom)

# check datatype
str(telecom)

# convert all character datatype to factor
telecom <- as.data.frame(unclass(telecom), stringsAsFactors = TRUE)
str(telecom)

# correlation graph
model.matrix(~0+., data = telecom) %>%
  cor(use = "pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, lab = TRUE, lab_size = 3) 

# split data into training and test set
set.seed(123)
trainIndex <- sample(x = nrow(telecom), size = nrow(telecom) * 0.7)
train <- telecom[trainIndex,]
test <- telecom[-trainIndex,]

balanced.data.train <- smote(Churn ~ ., data = train, perc.over = 1, perc.under = 2)
table(balanced.data.train$Churn)

balanced.data.test <- smote(Churn ~ ., data = test, perc.over = 1, perc.under = 2)
table(balanced.data.test$Churn)

# logistic regression model with all columns
originalLogistic <- glm(Churn ~ ., family = "binomial", data = balanced.data.train)
summary(originalLogistic)

# stepwise selection method
stepwise <- step(glm(Churn ~., family = "binomial", data = balanced.data.train), direction = "both")
summary(stepwise)

# Model 1 (Stepwise selection, AIC 4982)
model1 <- glm(Churn ~ Dependents + tenure + MultipleLines + InternetService + 
                OnlineSecurity + OnlineBackup + TechSupport + StreamingTV + 
                Contract + PaperlessBilling + PaymentMethod + TotalCharges, family = "binomial", data = balanced.data.train)

model1

#Graph combining (for ROC graphs later on)
par(mfrow=c(1,2))

# Plot and interpret the ROC curve
probabilities.test <- predict(model1, newdata=balanced.data.test, type="response")
roc1 <- roc(balanced.data.test$Churn, probabilities.test)
plot(roc1, col="red", ylab="Sensitivity - TP rate", xlab="Specificity - FP rate", main = "ROC Curve Model 1")

# Calculate and interpret the AUC
auc <- auc(roc1)
auc

# Model 2 (Variables that were significant in original Logistics, AIC 4201)
model2 <- glm(Churn ~ Dependents + tenure + MultipleLines + InternetService + OnlineSecurity + TechSupport + StreamingTV +
                Contract + PaperlessBilling + PaymentMethod + TotalCharges, family = "binomial", data = balanced.data.train)

model2

# Plot and interpret the ROC curve
probabilities.test <- predict(model2, newdata=balanced.data.test, type="response")
roc2 <- roc(balanced.data.test$Churn, probabilities.test)
plot(roc2, col="blue", ylab="Sensitivity - TP rate", xlab="Specificity - FP rate", main = "ROC Curve Model 2")

# Calculate and interpret the AUC
auc <- auc(roc2)
auc


# Model 1 confusion matrix on the test dataset

m1.probabilities.test <- predict(model1, newdata=balanced.data.test, type="response")
m1.predicted.classes.min <- as.factor(ifelse(m1.probabilities.test >=0.5, "Yes","No"))

confusionMatrix(m1.predicted.classes.min, balanced.data.test$Churn, positive = 'Yes')

# Model 2 confusion matrix on the test dataset

m2.probabilities.test <- predict(model2, newdata=balanced.data.test, type="response")
m2.predicted.classes.min <- as.factor(ifelse(m2.probabilities.test >=0.5, "Yes","No"))

confusionMatrix(m2.predicted.classes.min, balanced.data.test$Churn, positive = 'Yes')







