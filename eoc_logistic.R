library

eoc_data <- read.csv("eoc-dl.csv", header = TRUE, sep = ",")

dim(eoc_data)
summary(eoc_data)

str(eoc_data)

eoc_data$La <- as.factor(eoc_data$La)
eoc_data$Ra <- as.factor(eoc_data$Ra)
eoc_data$Ll <- as.factor(eoc_data$Ll)
eoc_data$Rl <- as.factor(eoc_data$Rl)

table(eoc_data$AfterInvestigationTrueAFP)

input_yes <- eoc_data[which(eoc_data$AfterInvestigationTrueAFP == "Yes"),]
input_no <- eoc_data[which(eoc_data$AfterInvestigationTrueAFP == "No"),]

input_yes_training <- sample(1:nrow(input_yes), 0.7*nrow(input_yes))
input_no_training <- sample(1:nrow(input_no), 0.7*nrow(input_no))

training_yes <- input_yes[input_yes_training, ]
training_no <- input_no[-input_no_training, ]
eoc_training <- rbind(training_yes, training_no)


test_yes <- input_yes[-input_yes_training, ]
test_no <- input_no[-input_no_training, ]

eoc_test <- rbind(test_yes, test_no)

logitMod_eoc <- glm(AfterInvestigationTrueAFP ~ ., data = eoc_training, family = binomial(link = "logit"))
predicted <- predict(logitMod_eoc, eoc_test, type = "response")

sum











dt = sort(sample(nrow(eoc_data), nrow(eoc_data)*.7))
eoc_train <- eoc_data[dt,]
eoc_test <- eoc_data[-dt,]

eoc_model <- glm(AfterInvestigationTrueAFP ~ ., family = "binomial", data = eoc_train)
summary(eoc_model)


predict_AFP <- predict(eoc_model, eoc_test, type = "response")

predict_AFP

pred_class_i <- ifelse(predict_AFP > 0.5, 1, 0)

pred_class <- factor(pred_class_i, levels = c(0,1))

pred_act <- eoc_test$AfterInvestigationTrueAFP
str(pred_class)
str(pred_act)

mean(pred_class == as.factor(pred_act))

accuracy <- table(predict_AFP, eoc_test[,"AfterInvestigationTrueAFP"])
sum(diag(accuracy))/sum(accuracy)
