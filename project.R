
library(adabag)
library(rpart)
library(caret)
library(ROCR)
y = read.csv('C:/Users/bssma/Downloads/E0 (1).csv')
#dropping unwanted rows 
 y = y[,7:23,drop= FALSE]
 y = y[,-4,drop= FALSE]
 y = y[,-4,drop= FALSE]
 y
 #converting response variable into 2 class categorical
levels(y$FTR) <- c(levels(y$FTR), "L")
y$FTR[y$FTR == 'A'] <- 'L'

levels(y$FTR) <- c(levels(y$FTR), "L")
y$FTR[y$FTR == 'D'] <- 'L'

#checking if the data is over sampled or undersampled

 table(y$FTR)
#since both the class records are almost equal  avoiding the step of upsampling or down sampling 
# Change Y values to 1's and 0's
 y$FTR <- ifelse(y$FTR == "H", 1, 0)
 y$FTR <- factor(y$FTR, levels = c(0, 1))
 
 

 # Prep Training and Test data.
 
 set.seed(100)
 trainDataIndex <- createDataPartition(y$FTR, p=0.6, list = F)  # % training data
 trainData <- y[trainDataIndex, ]
 testData <- y[-trainDataIndex, ]
 #applying logistic regression model
logitmod <- glm(FTR ~ HTHG + HTAG + HS + AS + HST + AST + HF + AF + HC + AC + HY + AY + HR + AR , family = "binomial", data=trainData)
 summary(logitmod)
pred <- predict(logitmod, newdata = testData, type = "response")
pred= prediction(pred, testData$FTR)
#ROC curve
 roc = performance(pred,"tpr","fpr")
 plot(roc)
 abline(a=0,b=1)

 # Recode factors
 y_pred_num <- ifelse(pred > 0.5, 1, 0)
 y_pred <- factor(y_pred_num, levels=c(0, 1))
 y_act <- testData$FTR
 # Accuracy
 confusionMatrix(y_pred,y_act)
 
 
 ## random forest
rf <- randomForest(as.factor(FTR) ~ ., data = trainData, ntree = 300, mtry = 4, nodesize = 5, importance = TRUE)
## variable importance plot
 varImpPlot(rf, type = 1)
## confusion matrix
rf.pred <- predict(rf, testData)
 confusionMatrix(rf.pred, testData$FTR)
 
 #Neural Network
 library(neuralnet)
 nn <- neuralnet(FTR ~ HTHG + HTAG + HS + AS + HST + AST + HF + AF + HC + AC + HY + AY + HR + AR, data=trainData, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
 nn$result.matrix
 plot(nn)
 #Test the resulting output
 
 nn.results <- compute(nn, testData)
 results <- data.frame(actual = testData$FTR, prediction = nn.results$net.result)
 results
 y_pred_num <- ifelse(results$prediction.2 > 0.5, 1, 0)
 y_pred_num <- factor(y_pred_num, levels=c(0, 1))
 confusionMatrix(y_pred_num,y_act_num)
 

 

