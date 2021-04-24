library(class)
library(caret)

# Mengimport dataset glass
data <- read.csv("E:/utssisdas.csv")
str(data)
data$Kelas <-as.factor(data$Kelas)

# Split data ND dari data frame
data_test <-data[is.na(data$Kelas), ]
data_clean <-data[!is.na(data$Kelas), ]

trainIndex <- createDataPartition(data_clean$Kelas, p = 0.8)[[1]]
datatraining <- data_clean[trainIndex, ]
datavalidation <- data_clean[-trainIndex, ]

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed = 4546
modelLvq <- train(Kelas ~ ., data = datatraining, method = "lvq",preProcess =c("scale", "center"), trControl = control)
importance <- varImp(modelLvq, scale = FALSE)
print(plot(importance, top = 20))

modelLvq

predLvq <- predict(modelLvq, newdata = datavalidation)
confusionMatrix(datavalidation$Kelas, predLvq)

outOfSampleAccuracy <- sum(predLvq == datavalidation$Kelas)/length(predLvq)
outOfSampleError <- (1 - outOfSampleAccuracy) * 100
print(outOfSampleError)

predLvqtest <- predict(modelLvq, newdata = data_test)
print(predLvqtest)

# Menyimpan hasil prediksi
write.csv(datatraining, "datatraining.csv")
write.csv(datavalidation, "datavalidation.csv")
write.csv(data_test, "datatest.csv")
write.csv(predLvqtest, "hasil.csv")