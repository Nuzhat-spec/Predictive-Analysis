#load library
set.seed(500)
library(lattice)
library(ggplot2)
library(caret)
library(car)

#load IRIS data
data("iris")
head(iris)
str(iris)
summary(iris)
View(iris)

# Split the data
data <- createDataPartition(iris$Species, p=0.80, list=FALSE)
#data

# Create training data
train_data <- iris[data,]
str(train_data)
summary(train_data)
# Create test data
test_data <- iris[-data,]
str(test_data)

# Summarize the class distribution
percentage <- prop.table(table(train_data$Species))*100
cbind(Freq = table(train_data$Species), Percentage = percentage)

#split dataset into 10 parts, train in 9 and test on 1 and repeat for all combinations of train-test splits.
control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'

#Build model
# Linear Discriminant Analysis (LDA)
library(MASS)
library(rpart)
library(kernlab)
library(ranger)
library(e1071)

lda_model <- train(Species~Sepal.Width, data=train_data, method='lda', 
                   trControl=control, metric=metric)
lda_model 

# Classification and Regression Trees (CART) [Decision Tree]
decision_tree <- train(Species~ Sepal.Width, data=train_data, method='rpart', 
                       trControl=control, metric=metric)
decision_tree

# k-Nearest Neighbors (KNN)

knn_model <- train(Species~Sepal.Width, data=train_data, method='knn', 
                   trControl=control, metric=metric)
knn_model
# Support Vector Machines (SVM) with a radial kernel
svm_model <- train(Species~Sepal.Width, data=train_data, method='svmRadial', 
                   trControl=control, metric=metric)
svm_model

#Random Forest 
random_forest <- train(Species~Sepal.Width, data=train_data, method='ranger', 
                       trControl=control, metric=metric)
random_forest 

# Compare the results of these algorithms
model_compare <- resamples(list(lda=lda_model, cart=decision_tree, knn=knn_model, svm=svm_model, rf=random_forest))

# Table Comparison
summary(model_compare)
# Create a dot plot

#dotplot(model_compare$Accuracy, model_compare$models,
#           main = "Model Performance on Iris Dataset",
#           xlab = "Accuracy",
#           ylab = "Model",
#           pch = 19,
#           col = "blue",
#            cex = 2)
dotplot(model_compare)

#Predict on test data
prediction <- predict(lda_model, test_data)
confusionMatrix(prediction, test_data$Species)

# Calculate the correlation matrix
correlation_matrix = vif(lda_model)
