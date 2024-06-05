### Load packages and dataset ###
library(dslabs)
library(tidyverse)
library(tidyr)
library(caret)
library(polycor)
data(brca)
outcomes = brca$y
predictors = data.frame(brca$x)
brca = predictors %>%
  mutate(outcome = outcomes)
rm(predictors)
nrow(brca)
ncol(brca)

### Quick summary ###

brca_summary = data.frame(Type = c('Malignant','Benign'), 
           Tally = c(sum(outcomes == 'M'),sum(outcomes == 'B')),
           Proportion = c(round(mean(outcomes == 'M'),3), 
                          round(mean(outcomes == 'B'),3)))

### Exploring correlation coefficients of variables with outcome ###

brca_var_cols = 1:30
cal_cor = function(brca_col){
  cor_test = hetcor(pull(brca[brca_col]), outcomes)
  cor_test$correlations[2]
}
cors = sapply(brca_var_cols, cal_cor)
rm(brca_var_cols)

# Top 10 correlation coefficients
top_10_coefs = data.frame(Variables = colnames(brca[,-31]),
           Correlation_coefficients = round(cors, 4)) %>%
  filter(!is.na(Correlation_coefficients)) %>% 
  arrange(desc(Correlation_coefficients)) %>%
  top_n(10, Correlation_coefficients) 

### Basic plots ###

area_worst_outcome = ggplot(data = brca, 
                            mapping = aes(x=outcome, y=area_worst, 
                                          colour = outcome)) + 
                    geom_boxplot(outlier.colour = 'black',outlier.alpha = 0.5) 

perimeter_mean_outcome = ggplot(data = brca, 
                                mapping = aes(x=outcome, y=perimeter_mean, 
                                              colour = outcome)) +
                    geom_boxplot(outlier.colour = 'black',outlier.alpha = 0.5) 

radius_mean_outcome = ggplot(data = brca, 
                             mapping = aes(x=outcome, y=radius_mean, 
                                           colour = outcome)) +
                    geom_boxplot(outlier.colour = 'black',outlier.alpha = 0.5)

### Exploring correlation coefficients between variables ###

cal_all_cor = function(brca_col){
  cal_curr_cor = function(all_cols){
    cor_value = cor(pull(brca[all_cols]), pull(brca[brca_col]))
    cor_value
  }
  curr_cors = sapply(brca_var_cols, cal_curr_cor)
  curr_cors
}
vars_cors_vector = sapply(brca_var_cols, cal_all_cor)
cor_coef_table = data.frame(vars_cors_vector, row.names = colnames(brca[,-31]))
colnames(cor_coef_table) = colnames(brca[,-31])
view(cor_coef_table)

### Developing prediction and learning models ###

# Predicting perimeter_mean from radius_mean (r = 0.99786)
lm_mean_accuracy = mean(replicate(5,{
  test_index = createDataPartition(brca$perimeter_mean, times=1, p=0.2, list=F)
  test_set = brca[test_index,]
  train_set = brca[-test_index,]
  
  lm_fit = train(perimeter_mean ~ radius_mean, data=train_set, method='lm')
  lm_predictions = predict(lm_fit, test_set)
  lm_rmse = sqrt(sum((lm_predictions - test_set$perimeter_mean)^2)
                 /length(lm_predictions))
  lm_rmse
}))
lm_mean_accuracy/mean(test_set$perimeter_mean)

# Predicting concavity_mean from compactness_mean (r = 0.88312)
lm_mean_accuracy = mean(replicate(5, {
  test_index = createDataPartition(brca$concavity_mean, times=1, p=0.2, list=F)
  test_set = brca[test_index,]
  train_set = brca[-test_index,]
  
  lm_fit = train(concavity_mean ~ compactness_mean, data=train_set, method='lm')
  lm_predictions = predict(lm_fit, test_set)
  lm_rmse = sqrt(sum((lm_predictions - test_set$concavity_mean)^2)
                 /length(lm_predictions))
  lm_rmse
}))
lm_mean_accuracy/mean(test_set$concavity_mean)

# Predicting outcome from various variables 

# kNN method
test_index = createDataPartition(brca$outcome, times=1, p=0.2, list=F)
test_set = brca[test_index,]
train_set = brca[-test_index,]

knn_fit = train(outcome ~ ., data = train_set, method='knn', 
                tuneGrid = data.frame(k=seq(10,25)))
knn_predictions = predict(knn_fit, test_set)
confusionMatrix(knn_predictions,test_set$outcome)

# Decision tree method
test_index = createDataPartition(brca$outcome, times=1, p=0.2, list=F)
test_set = brca[test_index,]
train_set = brca[-test_index,]

rpart_fit = train(outcome ~ ., data=train_set, method='rpart', 
                  tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 50)))
rpart_predictions = predict(rpart_fit, test_set)
confusionMatrix(rpart_predictions,test_set$outcome)
# Visualise decision tree
plot(rpart_fit$finalModel, margin=0.1)
text(rpart_fit$finalModel, cex=0.75)

# Random forest method
test_index = createDataPartition(brca$outcome, times=1, p=0.2, list=F)
test_set = brca[test_index,]
train_set = brca[-test_index,]
rf_fit = train(outcome ~ ., data=train_set, method='rf',
               tuneGrid=data.frame(mtry=c(1,5,10)),
               trControl=trainControl(method='cv',number=5))
rf_predictions = predict(rf_fit, test_set)
confusionMatrix(rf_predictions,test_set$outcome)
plot(rf_fit$finalModel)