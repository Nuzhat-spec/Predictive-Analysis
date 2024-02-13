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

#Multinomial logistic regression model using train dataset
#Baseline
train$species_setosa = relevel(train$Species, ref = "setosa")
#train$species_setosa

#model fit
model_setosa <- multinom(species_setosa ~ Sepal.Width  , data = train)
summary(model_setosa)
#Wald test or Z test for calculating p-value
z_test_setosa = summary(model_setosa)$coefficients/summary(model_setosa)$standard.errors
z_test_setosa
#calculating p-value
p_setosa = (1 - pnorm(abs(z_test_setosa), 0, 1)) * 2
p_setosa

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

