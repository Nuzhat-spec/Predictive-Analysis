#libraries
set.seed(500)
library(ggplot2)
library(naniar)
library(nnet)
library(car)

#load IRIS data
data("iris")
head(iris)
str(iris)
summary(iris)
View(iris)

#split into train and test data
# Split the data
data <- createDataPartition(iris$Species, p=0.80, list=FALSE)
#data

# Create training data
train <- iris[data,]
str(train)
summary(train)
# Create test data
test <- iris[-data,]
str(test)

# Summarize the class distribution
percentage <- prop.table(table(train_data$Species))*100
cbind(Freq = table(train_data$Species), Percentage = percentage)

#train data
#train_data_1 = iris[1:40,]; train_data_2 = iris[51:90,]; train_data_3 = iris[101:140,]
#train = rbind(train_data_1, train_data_2, train_data_3)
#train
#test data
#test_data_1 = iris[41:50,]; test_data_2 = iris[91:100,]; test_data_3 = iris[141:150,]
#test = rbind(test_data_1, test_data_2, test_data_3)
#test

#Multinomial logistic regression model using train dataset
#Baseline
train$species_setosa = relevel(train$Species, ref = "setosa")
train$species_setosa

#model fit
model_setosa <- multinom(species_setosa ~ Sepal.Length + Sepal.Width + 
                           Petal.Length + Petal.Width , data = train)
summary(model_setosa)

#Baseline
train$species_virginica = relevel(train$Species, ref = "virginica")
train$species_virginica
#model fit
model_virginica <- multinom(species_virginica ~ Sepal.Length + Sepal.Width + 
                           Petal.Length + Petal.Width , data = train)
summary(model_virginica)

#Baseline
train$species_versicolor = relevel(train$Species, ref = "versicolor")
train$species_versicolor
#model fit
model_versicolor <- multinom(species_versicolor ~ Sepal.Length + Sepal.Width + 
                              Petal.Length + Petal.Width , data = train)
summary(model_versicolor)

#Wald test or Z test for calculating p-value
z_test_setosa = summary(model_setosa)$coefficients/summary(model_setosa)$standard.errors
z_test_setosa
#calculating p-value
p_setosa = (1 - pnorm(abs(z_test_setosa), 0, 1)) * 2
p_setosa

#Wald test or Z test for calculating p-value
z_test_virginica = summary(model_virginica)$coefficients/summary(model_virginica)$standard.errors
z_test_virginica
#calculating p-value
p_virginica = (1 - pnorm(abs(z_test_virginica), 0, 1)) * 2
p_virginica

#Wald test or Z test for calculating p-value
z_test_versicolor = summary(model_versicolor)$coefficients/summary(model_versicolor)$standard.errors
z_test_versicolor
#calculating p-value
p_versicolor = (1 - pnorm(abs(z_test_versicolor), 0, 1)) * 2
p_versicolor

## Test the accuracy of model using the test set
library(data.table)
# generate predictions of probabilities for each species using the model
class(test$Species)
probability = predict(model_virginica, newdata =test, type='probs')
probability = data.table(probability)
# generate species data of each observation based on the model 
obs_based_on_model = rep(1,30)                
obs_based_on_model[which(probability$versicolor == apply(probability, 1, max))]=2              
obs_based_on_model[which(probability$virginica == apply(probability, 1, max))]=3 
# generate species data based on true observation
true_obs = c(rep(1,10), rep(2,10), rep(3,10))
# compute the accuracy ratio 
accuracy = sum(obs_based_on_model==true_obs )/30
accuracy
vif(model_virginica)

## Test the accuracy of model using the test set
library(data.table)
# generate predictions of probabilities for each species using the model
class(test$Species)
probability = predict(model_setosa, newdata =test, type='probs')
probability = data.table(probability)
# generate species data of each observation based on the model 
obs_based_on_model = rep(1,30)                
obs_based_on_model[which(probability$versicolor == apply(probability, 1, max))]=2              
obs_based_on_model[which(probability$virginica == apply(probability, 1, max))]=3 
# generate species data based on true observation
true_obs = c(rep(1,10), rep(2,10), rep(3,10))
# compute the accuracy ratio 
accuracy = sum(obs_based_on_model==true_obs )/30
accuracy
vif(model_virginica)

