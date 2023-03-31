rm(list = ls())
cat('\014')

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
# install.packages('visreg')
library(visreg)
# install.packages("yardstick")
library(yardstick)
library(adabag)
library(rpart)


# PART A
accident_data <- read.csv("Kag_Airbag.csv")
accident_data <- na.omit(accident_data)

sum(is.na(accident_data))
accident_data$safety_measure <- 0
accident_data$dead_f <- factor(accident_data$dead,
                               levels=c("dead","alive"),
                               labels=c(1,0))
accident_data$safety_measure[accident_data$airbag == "none" & 
                          accident_data$seatbelt == "none"]<-"none"
accident_data$safety_measure[accident_data$airbag == "airbag" & 
                          accident_data$seatbelt == "none"]<-"AB"
accident_data$safety_measure[accident_data$airbag == "none" & 
                          accident_data$seatbelt == "belted"] <-"SB"
accident_data$safety_measure[accident_data$airbag == "airbag" & 
                          accident_data$seatbelt == "belted"] <-"ABSB"
highSpeed_accidents <- accident_data[accident_data$dvcat=="55+",]


# Plotting Safety System VS Severity of Injury
counts<-table(highSpeed_accidents$injSeverity, highSpeed_accidents$safety_measure)
counts
safetyGraph <- barplot(counts,main = "Safety System VS Severity of Injury",
                       xlab ="Safety system",ylab = "Frequency",
                       col = c("#733080","#F4BAC8","#A40607","#7288B9", 
                               "#F0C595","#000000"),
                       legend=c("None", "Possible Injury", "No Incapacity",
                                "Incapacity","Killed","Unkown",
                                "Prior Death"),beside = TRUE,
                       args.legend = list(x="topleft"))


# Plotting Safety System VS Survival
death_alive_count <-table(highSpeed_accidents$dead,highSpeed_accidents$safety_measure)
death_alive_count
deathGraph <- barplot(death_alive_count,main = "Safety System VS Survival",
                      xlab ="Safety system",ylab = "Survival",
                      col = c("#808080","#FFFFFF"),
                      legend=rownames(death_alive_count),beside = TRUE, args.legend = list(x="topleft"))

# Logistic Regression Model for Safety_Measures Vs Survival Rate

set.seed(42)

train_index = createDataPartition(y = highSpeed_accidents$dead_f, p = 0.7, list = FALSE)
train_data = highSpeed_accidents[train_index, ]
test_data = highSpeed_accidents[-train_index, ]


#train_data <- train_data %>%
 # mutate(dead = relevel(factor(dead),
  #                      ref = "alive"))

logit_model_safety_measures <- glm(dead_f ~ safety_measure, family = "binomial", data = train_data)
prediction <- predict(logit_model_safety_measures, train_data, type="response")
prediction

logitPredictClass <- ifelse(prediction > 0.5, 1, 0); logitPredictClass
sum(is.na(logitPredictClass))

#accuracy of the train data
confusionMatrix(as.factor(logitPredictClass), as.factor(train_data$dead_f))


logit_model_safety_measures
summary(logit_model_safety_measures)
str(logit_model_safety_measures)

#acuracy of the test data
logit_model_safety_measures_t <- glm(dead_f ~ safety_measure, family = "binomial", data = test_data)
prediction_t <- predict(logit_model_safety_measures_t, test_data, type="response")
prediction

logitPredictClass_t <- ifelse(prediction > 0.5, 1, 0); logitPredictClass_t
confusionMatrix(as.factor(logitPredictClass_t), as.factor(test_data$dead_f))
#Creates vectors having data points
train_data$dead <- as.factor(train_data$dead)
test_data$dead <-  as.factor(test_data$dead)


#Creating confusion matrix
example <- confusionMatrix(data=train_data$dead, reference = test_data$dead)

#Display results 
example
#---------------------------------------------------------------------
# Visualisation on how safety_measures affect survival rate
visreg(logit_model_safety_measures, "safety_measure", scale = "response")

# Linear Regression Model for Safety_Measures Vs Injury Rate
injury_severity<- lm(injSeverity ~ safety_measure, data = train_data)

# Visualisation on how safety_measures affects injury rate
visreg(injury_severity, "safety_measure", scale = "response")

# need to find accuracy

#######################################################################################

# Part B

accidents_safety_measures <- accident_data[accident_data$safety_measure == 'ABSB',]

set.seed(1)
train_data = accidents_safety_measures
train_data <- train_data %>%
  mutate(dead = relevel(factor(dead),
                        ref = "alive"))

# Logistic Regression Model 
logit_model_speeds<- glm(dead_f ~ dvcat,family = "binomial", data = train_data)

# Visualisation on how speeds affects survival rate even with both safety measures
visreg(logit_model_speeds, "dvcat", scale = "response")

# find accuracy
prediction_B <- predict(logit_model_speeds,train_data, type="response")
prediction_B

logitPredictClass_B <- ifelse(prediction_B > 0.5, 1, 0); logitPredictClass_B
sum(is.na(logitPredictClass_B))
confusionMatrix(as.factor(logitPredictClass_B), as.factor(train_data$dvcat))

prediction_B <- predict(logit_model_speeds,train_data, type="response")
prediction_B

#Accuracy for the test data
prediction_B_t <- predict(logit_model_speeds,test_data, type="response")
prediction_B_t

logitPredictClass_B_t <- ifelse(prediction_B_t > 0.5, 1, 0); logitPredictClass_B_t
sum(is.na(logitPredictClass_B_t))
confusionMatrix(as.factor(logitPredictClass_B_t), as.factor(test_data$dvcat))
