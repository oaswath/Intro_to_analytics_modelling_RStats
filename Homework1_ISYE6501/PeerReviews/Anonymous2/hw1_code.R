# Import credit card data and headers
cc_data <- read.table("C:\\Users\\chada\\Desktop\\Georgia Tech\\IS\\Homework1_ISYE6501\\data 2.2\\credit_card_data-headers.txt", header=TRUE)
print(cc_data)

# Import libraries
install.packages("kernlab")
library(kernlab)

# call KSVM.
# C-svc is support vector classifier 
# Vanilladot is a simple linear kernel.
# C or controls the penalty parameter. Higher C means the model will be more generalized
model <- ksvm(as.matrix(cc_data[,1:10]),as.factor(cc_data[,11]),type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)

# calculate a1…am (the coefficients for each factor)
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
# calculate a0 (intercept term)
a0 <- model@b
a0
# see what the model predicts
pred <- predict(model,cc_data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == cc_data[,11]) / nrow(cc_data)
# create a confusion matrix. Rows are predictions, Columns are actual values.
confusion <- table(cc_data[,11], pred)
confusion
# convert confusion matrix into percent
confusion_pct <- confusion / nrow(cc_data)
confusion_pct

# C = .001
model1 <- ksvm(as.matrix(cc_data[,1:10]),as.factor(cc_data[,11]),type="C-svc",kernel="vanilladot",C=.001,scaled=TRUE)
pred <- predict(model1,cc_data[,1:10])
sum(pred == cc_data[,11]) / nrow(cc_data)

# C = .01
model2 <- ksvm(as.matrix(cc_data[,1:10]),as.factor(cc_data[,11]),type="C-svc",kernel="vanilladot",C=.01,scaled=TRUE)
pred <- predict(model2,cc_data[,1:10])
sum(pred == cc_data[,11]) / nrow(cc_data)

# C = 10
model3 <- ksvm(as.matrix(cc_data[,1:10]),as.factor(cc_data[,11]),type="C-svc",kernel="vanilladot",C=10,scaled=TRUE)
pred <- predict(model3,cc_data[,1:10])
sum(pred == cc_data[,11]) / nrow(cc_data)

# C = 500
model4 <- ksvm(as.matrix(cc_data[,1:10]),as.factor(cc_data[,11]),type="C-svc",kernel="vanilladot",C=500,scaled=TRUE)
pred <- predict(model4,cc_data[,1:10])
sum(pred == cc_data[,11]) / nrow(cc_data)

# C = 2000
model5 <- ksvm(as.matrix(cc_data[,1:10]),as.factor(cc_data[,11]),type="C-svc",kernel="vanilladot",C=2000,scaled=TRUE)
pred <- predict(model5,cc_data[,1:10])
sum(pred == cc_data[,11]) / nrow(cc_data)

# C = 5000
model5 <- ksvm(as.matrix(cc_data[,1:10]),as.factor(cc_data[,11]),type="C-svc",kernel="vanilladot",C=5000,scaled=TRUE)
pred <- predict(model5,cc_data[,1:10])
sum(pred == cc_data[,11]) / nrow(cc_data)

# Try nonlinear predictor using polydot kernel
model_poly <- ksvm(as.matrix(cc_data[,1:10]),as.factor(cc_data[,11]),type="C-svc",kernel="polydot",kpar=list(degree=3), C=100,scaled=TRUE)

# calculate a1…am (the coefficients for each factor)
a <- colSums(model_poly@xmatrix[[1]] * model@coef[[1]])
a
# calculate a0 (intercept term)
a0 <- model_poly@b
a0
# see what the model predicts
pred <- predict(model_poly,cc_data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == cc_data[,11]) / nrow(cc_data)

# create a confusion matrix. Rows are predictions, Columns are actual values.
confusion <- table(cc_data[,11], pred)
confusion
# convert confusion matrix into percent
confusion_pct <- confusion / nrow(cc_data)
confusion_pct

install.packages("kknn")
library(kknn)

#training set leaves everything out but ith data point. Test set only includes ith data point
# I had perplexity generate the below code to help me with the leave one out logic
for(i in 1:nrow(cc_data)) {
kknn_model <- kknn(R1 ~ ., train = cc_data[-i,], test = cc_data[i,], k = 11, scale=TRUE)
predictions[i] <- fitted(kknn_model)
}

#get accuracy 
predictions <- ((predictions > .5)*1)
sum(predictions == cc_data[,11]) / nrow(cc_data)

# create a confusion matrix. Rows are predictions, Columns are actual values.
confusion <- table(cc_data[,11], predictions)
confusion
# convert confusion matrix into percent
confusion_pct <- confusion / nrow(cc_data)
confusion_pct

