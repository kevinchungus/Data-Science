##### PRACTICE ON CARBON FOOTPRINT DATASET #####
### Retrieved from https://www.kaggle.com/datasets/dumanmesut/individual-carbon-footprint-calculation/data?select=Carbon+Emission.csv

# Importing packages
library(tidyverse)
library(tidymodels)
library(stringr)
library(Metrics)
library(ggplot2)
library(corrplot)
library(ggthemes)
library(caret)

# Read dataset into dataframe 
carbon_emission = read_csv('Carbon Emission.csv')

# Preview dataset
head(carbon_emission)

# Quick summary of data
summary(carbon_emission)

##### DATA CLEANING AND WRANGLING #####

# Changing column names for easier reference
newnames = c('body_type','sex','diet','shower','heat','transport',
             'veh_fuel','social','groc_bill','flight','veh_dist','waste_size',
             'waste_count','tv_pc','clothes','internet','energy_eff',
             'recycling','cooking','emission')
colnames(carbon_emission) = newnames

# Checking for NA values in each column
for (col_num in 1:length(newnames)){
  curr_col = carbon_emission[,col_num]
  num_na = sum(is.na(curr_col))
  if (num_na > 0){
    print(paste(c(newnames[col_num],num_na), collapse=','))
  }
}

# Replace NA values in vehicle fuel type column to 'NIL'
carbon_emission$veh_fuel = replace_na(carbon_emission$veh_fuel,'nil')
# Confirm no NA values
sum(is.na(carbon_emission))

# All categorical variables are of character type
carbon_emission %>% 
  summarize_all(class) %>%
  gather(variable, class)

# Checking unique values of categorical variables
# Recycling and cooking have too many unique values for factor conversion
for (col_num in 1:20){
  curr_col = carbon_emission[[col_num]]
  if (is.character(curr_col)){
    print(paste(c('Unique values of ',newnames[col_num],' : ',
                  paste(unique(curr_col), collapse=',')),
                collapse=''))
  }
}

# Converting the rest of categorical variables into factors
col_indexes = c(1,2,3,4,5,6,7,8,10,12,17)
for (col_num in col_indexes){
  carbon_emission[[col_num]] = as.factor(carbon_emission[[col_num]])
}
# Check data types
carbon_emission %>% 
       summarize_all(class) %>%
       gather(variable, class)

# Manual one hot encoding for recycling column
carbon_emission = carbon_emission %>%
  mutate(paper = as.numeric(grepl('Paper',recycling))) %>%
  mutate(plastic = as.numeric(grepl('Plastic',recycling))) %>%
  mutate(glass = as.numeric(grepl('Glass',recycling))) %>%
  mutate(metal = as.numeric(grepl('Metal',recycling))) 
carbon_emission = carbon_emission[,-18]

# Manual one hot encoding for cooking column
carbon_emission = carbon_emission %>%
  mutate(stove = as.numeric(grepl('Stove',cooking))) %>%
  mutate(oven = as.numeric(grepl('Oven',cooking))) %>%
  mutate(microwave = as.numeric(grepl('Microwave',cooking))) %>%
  mutate(grill = as.numeric(grepl('Grill',cooking))) %>%
  mutate(airfryer = as.numeric(grepl('Airfryer',cooking))) 
carbon_emission = carbon_emission[,-18]
carbon_emission = carbon_emission %>% relocate(emission, .after=last_col())

# Check data types
carbon_emission %>% 
       summarize_all(class) %>%
       gather(variable, class)

# Remove temporary variables
rm(col_indexes,col_num,curr_col,newnames,num_na)

# Relocate categorical columns together
carbon_emission = carbon_emission %>%
  relocate(energy_eff, .after = 2) %>%
  relocate(waste_size, .after = 2) %>%
  relocate(flight, .after = 2)

#####  DATA EXPLORATION #####

# Counts of categories for different categorical variables
carbon_emission %>% 
  group_by(body_type) %>%
  summarise(count = n())

carbon_emission %>% 
  group_by(transport) %>%
  summarise(count = n())

carbon_emission %>%
  group_by(veh_fuel) %>%
  summarise(count = n())

# Summary of numerical variables for different groups
carbon_emission %>% 
  group_by(body_type) %>%
  summarise(avg_emissions = mean(emission))
cat('Overall mean emissions is', mean(carbon_emission$emission),'.\n')

carbon_emission %>%
  group_by(diet) %>%
  summarise(avg_emissions = mean(emission))
cat('Overall mean emissions is', mean(carbon_emission$emission),'.\n')

carbon_emission %>%
  group_by(sex) %>%
  summarise(avg_emissions = mean(emission))
cat('Overall mean emissions is', mean(carbon_emission$emission),'.\n')

# Correlations between the numerical variables
corrplot(cor(carbon_emission[,12:27]), method='color', addCoef.col = 'black', 
         number.cex = 0.5, 
         col= colorRampPalette(c("#FFFFFF", "#FDDBC7", "#F4A582", "#D6604D", 
                                 "#B2182B", "#67001F"))(12), 
         tl.col='black', tl.srt=45, order='hclust', type='upper', 
         title = 'Correlation coefficient matrix for variables',
         mar=c(0,0,1,0))

# Visualising relationships between variables
ggplot(carbon_emission, 
       aes(x=reorder(veh_fuel,emission),y=emission,fill=veh_fuel)) +
  geom_boxplot() + 
  ggtitle('Boxplots of carbon emissions based on vehicle fuel type') +
  xlab('Vehicle fuel type') +
  ylab('Carbon emissions (kg)') +
  labs(fill = '') +
  theme_clean()

ggplot(carbon_emission, 
       aes(x=reorder(transport,emission),y=emission,fill=transport)) +
  geom_boxplot() + 
  ggtitle('Boxplots of carbon emissions based on mode of transport') + 
  xlab('Mode of transport') + 
  ylab('Carbon emissions (kg)') + 
  labs(fill = '') +
  theme_clean()

ggplot(carbon_emission,
       aes(x=reorder(transport,emission),y=emission,
           fill=reorder(body_type,emission))) +
  geom_boxplot() +
  ggtitle('Boxplots of carbon emissions based on mode of transport and body type') +
  xlab('Mode of transport') +
  ylab('Carbon emissions (kg)') +
  labs(fill = 'Body type') + 
  theme_clean()

ggplot(carbon_emission,
       aes(x=emission,fill=flight)) +
  geom_density() + 
  theme_clean() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) +
  ggtitle('Density plots of carbon emissions based on flight frequency') +
  xlab('Carbon emissions (kg)') +
  ylab('Density') +
  xlim(c(0,5000)) +
  labs(fill = '') +
  facet_wrap(~factor(flight,
                     levels=c('never','rarely','frequently','very frequently')),
             ncol=1) 

ggplot(carbon_emission,
       aes(x=veh_dist,y=emission)) +
  theme_clean() +
  theme(legend.position = 'none') +
  geom_point(aes(colour='lightpink'),alpha=0.3) +
  geom_smooth(colour='darkblue') +
  ggtitle('Scatter plot showing carbon emissions against vehicle monthly distance') +
  xlab('Vehicle distance per month (km)') +
  ylab('Carbon emissions (kg)')

##### MODEL DEVELOPMENT AND EVALUATION WITH TIDYMODELS #####

# Converting all variables into numerical for easier model development

# One hot encoding for body type
carbon_emission = carbon_emission %>%
  mutate(overweight = as.numeric(body_type == 'overweight')) %>%
  mutate(obese = as.numeric(body_type == 'obese')) %>%
  mutate(underweight = as.numeric(body_type == 'underweight')) %>%
  mutate(normal = as.numeric(body_type == 'normal')) 
carbon_emission = carbon_emission[,-1]

# One hot encoding for sex
carbon_emission = carbon_emission %>%
  mutate(male = as.numeric(sex == 'male')) %>%
  mutate(female = as.numeric(sex == 'female'))
carbon_emission = carbon_emission[,-1]

# One hot encoding for flight
carbon_emission = carbon_emission %>%
  mutate(flight_frequent = as.numeric(flight == 'frequently')) %>%
  mutate(flight_rare = as.numeric(flight == 'rarely')) %>%
  mutate(flight_never = as.numeric(flight == 'never')) %>%
  mutate(flight_very_frequent = as.numeric(flight == 'very frequently')) 
carbon_emission = carbon_emission[,-1]

# One hot encoding for waste size
carbon_emission = carbon_emission %>%
  mutate(waste_large = as.numeric(waste_size == 'large')) %>%
  mutate(waste_extra_large = as.numeric(waste_size == 'extra large')) %>%
  mutate(waste_small = as.numeric(waste_size == 'small')) %>%
  mutate(waste_medium = as.numeric(waste_size == 'medium')) 
carbon_emission = carbon_emission[,-1]

# One hot encoding for energy efficiency
carbon_emission = carbon_emission %>%
  mutate(effi_no = as.numeric(energy_eff == 'No')) %>%
  mutate(effi_some = as.numeric(energy_eff == 'Sometimes')) %>%
  mutate(effi_yes = as.numeric(energy_eff == 'Yes')) 
carbon_emission = carbon_emission[,-1]

# One hot encoding for diet
carbon_emission = carbon_emission %>%
  mutate(omnivore = as.numeric(diet == 'omnivore')) %>%
  mutate(pescatarian = as.numeric(diet == 'pescatarian')) %>%
  mutate(vegan = as.numeric(diet == 'vegan')) %>%
  mutate(vegetarian = as.numeric(diet == 'vegetarian')) 
carbon_emission = carbon_emission[,-1]

# One hot encoding for shower
carbon_emission = carbon_emission %>%
  mutate(shower_once = as.numeric(shower == 'daily')) %>%
  mutate(shower_twice = as.numeric(shower == 'twice a day')) %>%
  mutate(shower_less = as.numeric(shower == 'less frequently')) %>%
  mutate(shower_more = as.numeric(shower == 'more frequently')) 
carbon_emission = carbon_emission[,-1]

# One hot encoding for heat
carbon_emission = carbon_emission %>%
  mutate(coal = as.numeric(heat == 'coal')) %>%
  mutate(electricity = as.numeric(heat == 'electricity')) %>%
  mutate(gas = as.numeric(heat == 'natural gas')) %>%
  mutate(wood = as.numeric(heat == 'wood')) 
carbon_emission = carbon_emission[,-1]

# One hot encoding for transport
carbon_emission = carbon_emission %>%
  mutate(public = as.numeric(transport == 'public')) %>%
  mutate(private = as.numeric(transport == 'private')) %>%
  mutate(walk_bike = as.numeric(transport == 'walk/bicycle')) 
carbon_emission = carbon_emission[,-1]

# One hot encoding for veh fuel
carbon_emission = carbon_emission %>%
  mutate(fuel_nil = as.numeric(veh_fuel == 'nil')) %>%
  mutate(fuel_petrol = as.numeric(veh_fuel == 'petrol')) %>%
  mutate(fuel_diesel = as.numeric(veh_fuel == 'diesel')) %>%
  mutate(fuel_hybrid = as.numeric(veh_fuel == 'hybrid')) %>%
  mutate(fuel_lpg = as.numeric(veh_fuel == 'lpg')) %>%
  mutate(fuel_electric = as.numeric(veh_fuel == 'electric')) 
carbon_emission = carbon_emission[,-1]

# One hot encoding for social
carbon_emission = carbon_emission %>%
  mutate(social_never = as.numeric(social == 'never')) %>%
  mutate(social_some = as.numeric(social == 'sometimes')) %>%
  mutate(social_often = as.numeric(social == 'often')) 
carbon_emission = carbon_emission[,-1]

# Relocate emission to last column
carbon_emission = carbon_emission %>%
  relocate(emission, .after = last_col())

# Standardising remaining numerical columns

# Standardising for grocery bill
carbon_emission$groc_bill = 
  (carbon_emission$groc_bill - min(carbon_emission$groc_bill)) / 
  (max(carbon_emission$groc_bill) - min(carbon_emission$groc_bill))

# Standardising for vehicle distance
carbon_emission$veh_dist = 
  (carbon_emission$veh_dist - min(carbon_emission$veh_dist)) / 
  (max(carbon_emission$veh_dist) - min(carbon_emission$veh_dist))

# Standardising for waste count
carbon_emission$waste_count = 
  (carbon_emission$waste_count - min(carbon_emission$waste_count)) / 
  (max(carbon_emission$waste_count) - min(carbon_emission$waste_count))

# Standardising for tv pc
carbon_emission$tv_pc = 
  (carbon_emission$tv_pc - min(carbon_emission$tv_pc)) / 
  (max(carbon_emission$tv_pc) - min(carbon_emission$tv_pc))

# Standardising for clothes
carbon_emission$clothes = 
  (carbon_emission$clothes - min(carbon_emission$clothes)) / 
  (max(carbon_emission$clothes) - min(carbon_emission$clothes))

# Standardising for internet
carbon_emission$internet = 
  (carbon_emission$internet - min(carbon_emission$internet)) / 
  (max(carbon_emission$internet) - min(carbon_emission$internet))

# Top 10 correlation coefficients with emission ranked 
as.data.frame(cor(carbon_emission)[57,]) %>%
  mutate(Corr_coef = cor(carbon_emission)[57, ]) %>%
  select(Corr_coef) %>%
  arrange(desc(abs(Corr_coef))) %>%
  head(.,10)

# Splitting dataset into training and testing set
carbon_split = initial_split(carbon_emission, prop = 0.8)
carbon_train = training(carbon_split)
carbon_test = testing(carbon_split)

# Multiple linear regression model
lm_spec = linear_reg(mode = 'regression', engine = 'lm')
lm_model = lm_spec %>% 
  fit(emission ~ .,
      data = carbon_train)
summary(lm_model$fit)

# Testing the model with testing set
lm_test = lm_model %>%
  predict(new_data = carbon_test) %>%
  mutate(truth = carbon_test$emission)
head(lm_test)

# RMSE and R squared values
lm_rmse = rmse(actual=lm_test$truth,predicted=lm_test$.pred)
lm_rsq = rsq(lm_test, truth = truth, estimate = .pred)
data.frame(Metric = c('RMSE','R-squared'), Linear_model = c(lm_rmse,lm_rsq[[3]]))

# Polynomial mix multiple linear regression model
mix_model = lm_spec %>%
  fit(emission ~ poly(veh_dist,3) + ., data=carbon_train)
summary(mix_model$fit)

# Testing polynomial model
mix_test = mix_model %>%
  predict(new_data = carbon_test) %>%
  mutate(truth = carbon_test$emission)
head(mix_test)

# RMSE and R squared values
mix_rmse = rmse(actual=mix_test$truth, predicted=mix_test$.pred)
mix_rsq = rsq(mix_test, truth=truth, estimate=.pred)
data.frame(Metric = c('RMSE','R-squared'), Mix_model=c(mix_rmse,mix_rsq[[3]]))

# Interaction mix polynomial regression model
inter_model = lm_spec %>%
  fit(emission ~ poly(veh_dist,3) + veh_dist*private + veh_dist*fuel_nil +
        private*fuel_nil + veh_dist*flight_very_frequent +., 
      data=carbon_train)
summary(inter_model$fit)

# Testing interaction model
inter_test = inter_model %>%
  predict(new_data = carbon_test) %>%
  mutate(truth=carbon_test$emission)
head(inter_test)

# RMSE and R squared values
inter_rmse = rmse(actual=inter_test$truth, predicted=inter_test$.pred)
inter_rsq = rsq(inter_test, truth=truth, estimate=.pred)
data.frame(Metric=c('RMSE','R-squared'), 
           Interaction_model=c(inter_rmse,inter_rsq[[3]]))

# Ridge and lasso regression models
carbon_recipe <-
  recipe(emission ~ ., data = carbon_train)
ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>%
  set_engine("glmnet")
ridge_wf <- workflow() %>%
  add_recipe(carbon_recipe)
ridge_model <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = carbon_train)
ridge_model %>%
  pull_workflow_fit() %>%
  tidy()

ridge_test= ridge_model %>%
  predict(new_data = carbon_test) %>%
  mutate(truth = carbon_test$emission)
head(ridge_test)

ridge_rmse = rmse(actual=ridge_test$truth, predicted=ridge_test$.pred)
ridge_rsq = rsq(ridge_test, truth = truth, estimate = .pred)
data.frame(Metric = c('RMSE','R-squared'), 
           Ridge_model=c(ridge_rmse,ridge_rsq[[3]]))

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")
lasso_wf <- workflow() %>%
  add_recipe(carbon_recipe)
lasso_model <- lasso_wf %>%
  add_model(lasso_spec) %>%
  fit(data = carbon_train)
lasso_model %>%
  pull_workflow_fit() %>%
  tidy()

lasso_test= lasso_model %>%
  predict(new_data = carbon_test) %>%
  mutate(truth = carbon_test$emission)
head(lasso_test)

lasso_rmse = rmse(actual=lasso_test$truth, predicted=lasso_test$.pred)
lasso_rsq = rsq(lasso_test, truth = truth, estimate = .pred)
data.frame(Metric = c('RMSE','R-squared'), 
           Lasso_model=c(lasso_rmse,lasso_rsq[[3]]))

# Comparing results of each model
data.frame(Model=c('Linear','Polynomial','Interaction','Ridge','Lasso'),
           RMSE=c(lm_rmse,mix_rmse,inter_rmse,ridge_rmse,lasso_rmse),
           R_squared=c(lm_rsq[[3]],mix_rsq[[3]],inter_rsq[[3]],
                         ridge_rsq[[3]],lasso_rsq[[3]])) %>%
  arrange(RMSE)

##### TESTING OUT CLASSIFICATION MODELS WITH CARET #####

# Convert emission into categorical factors
classify_emission = function(x){
  emissions_quantiles = quantile(carbon_emission$emission, 
                                 probs=c(0.2,0.4,0.6,0.8))
  if (x < emissions_quantiles[[1]]){
    return('Very low')
  } else if (x < emissions_quantiles[[2]]){
    return('Low')
  } else if (x < emissions_quantiles[[3]]){
    return('Average')
  } else if (x < emissions_quantiles[[4]]){
    return('High')
  } else {
    return('Very high')
  }
}

converted_emissions = sapply(carbon_emission$emission, classify_emission)
carbon_emission$emission = as.factor(converted_emissions)
carbon_emission$emission = factor(carbon_emission$emission, 
                                  levels = c('Very low','Low','Average',
                                             'High','Very high'))
rm(converted_emissions)

# Splitting dataset into training and testing set
test_index = createDataPartition(carbon_emission$emission, 
                                 times=1, p=0.2, list=F)
carbon_test = carbon_emission[test_index,]
carbon_train = carbon_emission[-test_index,]

# K-nearest neighbours classification model
knn_model = train(emission ~ ., data = carbon_train, method='knn', 
                tuneGrid = data.frame(k=50))
knn_predictions = predict(knn_model, carbon_test)
M = confusionMatrix(knn_predictions,carbon_test$emission)
knn_accuracy = round(M$overall[[1]],4)

# Confusion matrix for kNN
ggplot(as.data.frame(M$table), aes(x=Prediction,y=Reference,fill=Freq)) + 
  geom_tile() + 
  geom_text(aes(label=Freq)) +
  xlab('Model predictions') + 
  ylab('Actual values') +
  ggtitle('Confusion matrix for kNN model',
          subtitle = paste(c('Model accuracy: ',knn_accuracy),collapse='')) +
  scale_fill_gradient(low='lightpink',high='red') +
  theme_minimal() +
  theme(legend.position = 'none', axis.ticks=element_blank(),
        axis.line = element_blank()) 

# Decision tree classification method
tree_model = train(emission ~ ., data=carbon_train, method='rpart', 
                  tuneGrid = data.frame(cp = 0.005))
tree_predictions = predict(tree_model, carbon_test)
M = confusionMatrix(tree_predictions,carbon_test$emission)
tree_accuracy = round(M$overall[[1]],4)

# Confusion matrix for decision tree
ggplot(as.data.frame(M$table), aes(x=Prediction,y=Reference,fill=Freq)) + 
  geom_tile() + 
  geom_text(aes(label=Freq)) +
  xlab('Model predictions') + 
  ylab('Actual values') +
  ggtitle('Confusion matrix for decision tree model',
          subtitle = paste(c('Model accuracy: ',tree_accuracy),collapse='')) +
  scale_fill_gradient(low='lightpink',high='red') +
  theme_minimal() +
  theme(legend.position = 'none', axis.ticks=element_blank(),
        axis.line = element_blank()) 

# Visualise decision tree model
plot(tree_model$finalModel, margin=0)
text(tree_model$finalModel, cex=0.6)

# Random forest method
rf_model = train(emission ~ ., data=carbon_train, method='rf',
               tuneGrid=data.frame(mtry=15),
               trControl=trainControl(method='cv',number=5))
rf_predictions = predict(rf_model, carbon_test)
M = confusionMatrix(rf_predictions,carbon_test$emission)
rf_accuracy = round(M$overall[[1]],4)

# Confusion matrix for random forest
ggplot(as.data.frame(M$table), aes(x=Prediction,y=Reference,fill=Freq)) + 
  geom_tile() + 
  geom_text(aes(label=Freq)) +
  xlab('Model predictions') + 
  ylab('Actual values') +
  ggtitle('Confusion matrix for random forest model',
          subtitle = paste(c('Model accuracy: ',rf_accuracy),collapse='')) +
  scale_fill_gradient(low='lightpink',high='red') +
  theme_minimal() +
  theme(legend.position = 'none', axis.ticks=element_blank(),
        axis.line = element_blank()) 