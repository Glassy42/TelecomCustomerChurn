library(ggplot2)
library(dplyr)
library(scales)
library(ggpubr)

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

# convert all character datatype to factors 
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
