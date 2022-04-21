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

# count churn
table(telecom$Churn)

# create a graph - churn count
churn_count <- telecom %>% count(Churn)

ggplot(churn_count) +
  aes(x = Churn, y = n, fill = Churn) +
  geom_bar(stat = 'identity', width = 0.5, show.legend = FALSE) +
  labs(title = 'Telecom churn count', y = 'Count') +
  scale_y_continuous(labels = scales::comma) +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5)) +
  scale_fill_manual(values = c("No" = "turquoise4", "Yes" = "gold2"))

# create a graph - churn percent
churn_count$percent <- churn_count$n/length(telecom$Churn)
churn_count$percent <- label_percent()(churn_count$percent)

ggplot(churn_count) +
  aes(x = "", y = percent, fill = Churn) +
  geom_bar(stat = "identity" , width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = percent, label = percent), color = "white", size = 6, position = position_stack(vjust = 0.5)) +
  labs(title = "Telecom churn percentage") +
  scale_fill_manual(values = c("No" = "pink1", "Yes" = "slateblue"))

# total charges of contract 
ggplot(telecom) +
  aes(x = Contract, y = TotalCharges, fill = Churn) +
  geom_boxplot() +
  labs(title = 'Total Charges of Contract') +
  scale_y_continuous(labels = comma) +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5)) +
  scale_fill_manual(values = c("No" = "peachpuff4", "Yes" = "moccasin"))


# create a function to make multiple graphs
churn_bar <- function(dataframe, category_name, category, f) {
  ggplot(dataframe) +
    aes(x = category, fill = f) +
    geom_bar(position = "stack", width = 0.5) +
    geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
    scale_y_continuous(labels = comma) +
    theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5)) +
    labs(x = category_name) +
    guides(fill = guide_legend(title = "Churn"))
}

# create graphs for gender, partner, dependents, phoneservice
gender <- churn_bar(telecom, "Gender", telecom$gender, telecom$Churn)
partner <- churn_bar(telecom, "Partner", telecom$Partner, telecom$Churn)
Dependents <- churn_bar(telecom, "Dependents", telecom$Dependents, telecom$Churn)
PhoneService <- churn_bar(telecom, "PhoneService", telecom$PhoneService, telecom$Churn)

# arrange four graphs together
ggarrange(gender, partner, Dependents, PhoneService)


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







