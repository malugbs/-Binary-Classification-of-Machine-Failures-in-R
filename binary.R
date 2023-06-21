#data 
library(stargazer)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(GGally)

##organizing data
library(caret) 
library(stargazer)  
library(tidyverse) 
library(caTools) 

##modeling
library(performance)
library(rpart)
library(rpart.plot)
library(rattle) 

train <- read.csv("C:/Users/guima/Documents/R/Binary Classification of Machine Failures/train.csv")
test <- read.csv("C:/Users/guima/Documents/R/Binary Classification of Machine Failures/test.csv")

train$Type <- factor(train$Type, levels = c("H", "L", "M"))

summary(train)

#overall stats

#most machines are part of the L type
train %>%
  group_by(Type) %>%
  summarise(count = n()) %>%
  mutate(frequency = count*100/sum(count)) %>%
  ggplot(aes(fill=Type, y=frequency, x=Type)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_light() +
  labs(title="(%) observations by type",
       fill="",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="none")

## What makes a machine fail? - Machine type seemly does  not play an important factor. Between the categorical variables, HDF, PWF and OSF seem to be key factors.
  
train %>%
  mutate(Fail = ifelse(Machine.failure == 1, "Failed", "Still Working")) %>%
  group_by(Type, Fail) %>%
  summarise(Observations = n()) %>%
  mutate(`Frequency (%)` = Observations*100/sum(Observations))

train %>%
  mutate(Fail = ifelse(Machine.failure == 1, "Failed", "Still Working")) %>%
  group_by(Type, Fail) %>%
  summarise(Observations = n()) %>%
  mutate(`Frequency (%)` = Observations*100/sum(Observations)) %>%
  ggplot(aes(fill=Fail, y=`Frequency (%)`, x=Type)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_light() +
  labs(title="(%) of fails type",
       fill="",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="top")
    
#Rotational Speed, Torque and Tool Wear also look important.
view (train %>%
  mutate(Fail = ifelse(Machine.failure == 1, "Failed", "Still Working")) %>%
  group_by(Fail) %>%
  summarise(`Average Air Temperature` = mean(Air.temperature..K.),
            `Average Process Temperature` = mean(Process.temperature..K.),
            `Average Rotational Speed` = mean(Rotational.speed..rpm.),
            `Average Torque` = mean(Torque..Nm.),
            `Average Tool Wear` = mean(Tool.wear..min.)))



#Visually
train %>%
  mutate(Fail = ifelse(Machine.failure == 1, "Failed", "Still Working")) %>%
  ggplot(aes(fill=Type, y=Rotational.speed..rpm., x=Type)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  theme_light() +
  facet_wrap(~Fail) +
  labs(title="Median Rotational by type",
       fill="",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="none")

train %>%
  mutate(Fail = ifelse(Machine.failure == 1, "Failed", "Still Working")) %>%
  ggplot(aes(fill=Type, y=Torque..Nm., x=Type)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  theme_light() +
  facet_wrap(~Fail) +
  labs(title="Median Torque by type",
       fill="",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="none")

train %>%
  mutate(Fail = ifelse(Machine.failure == 1, "Failed", "Still Working")) %>%
  ggplot(aes(fill=Type, y=Tool.wear..min., x=Type)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  theme_light() +
  facet_wrap(~Fail) +
  labs(title="Median Tool Wear by type",
       fill="",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="none")


#correlograma
train %>%
  select(!id) %>%
  select(!Product.ID) %>%
  ggcorr(method = c("everything", "pearson"), low = "#4DAF4A" , mid = "#377EB8" , high = "#E41A1C", 
         label = TRUE, label_size = 4, label_color = "white",size =4, layout.exp =1) +
  labs(title="Basic Correlogram",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position="bottom")

#first, set seed and train our model
df <- train %>%
  select(!id) %>%
  select(!Product.ID)

set.seed(237)

train_index = createDataPartition(y = df$Machine.failure,  # y = our dependent variable.
                                  p = .7,  # Specifies split into 70% & 30%.
                                  list = FALSE,  # Sets results to matrix form. 
                                  times = 1)  # Sets number of partitions to create to 1. 


#Now we can split our data into train and test data using the randomly sampled train_index that we just formed.

train_data = df[train_index,]  # Use train_index of iris data to create train_data.
test_data = df[-train_index,]  # Use whatever that is not in train_index to create test_data.

#glm modelo
model_log = glm(Machine.failure~.,
            data = train_data,
            family = "binomial", maxit = 100)

res <- predict(model_log, test_data, type = 'response')
res <- predict(model_log, train_data, type = 'response')


#acurácia
confmatrix <- table(Actual_Value = train_data$Machine.failure, predicted_value = res > 0.5)
confmatrix

(confmatrix[[1,1]] + confmatrix[[2,2]])*100 / sum(confmatrix)

## to predict using logistic regression model, probablilities obtained
pred_log <- predict(model_log, test, type="response")

## Look at probability output
solution_log = data.frame(id = test$id, `Machine failure` = ifelse(pred_log > 0.5, 1, 0))

colnames(solution_log)[2]  <- "Machine failure"

head(solution_log, 5)
view(solution_log)

#regression trees and model trees
##training a model on the data
rt_model <- rpart(Machine.failure~., data = train_data)

rt_predict <- predict(rt_model, test_data)

summary(rt_model)

##Visualizing decision trees
rpart.plot(rt_model, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

##evaluating model performance
summary(rt_predict)

summary(test_data$Machine.failure)

cor(rt_predict, test_data$Machine.failure)

###Measuring performance with mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

MAE(rt_predict, test_data$Machine.failure)

##predicting 
pred_rt <- predict(rt_model, test)

solution_rt = data.frame(id = test$id, `Machine failure` = round(pred_rt,0))

head(solution_rt, n=5)

view(solution_rt)

#decision tree
train_data <- train_data %>%
  mutate(Failed = ifelse(Machine.failure == 1, "Failed", "Still Working")) %>%
  select(!Machine.failure)

test_data <- test_data %>%
  mutate(Failed = ifelse(Machine.failure == 1, "Failed", "Still Working")) %>%
  select(!Machine.failure)

fitControl = trainControl(method = "cv", number = 10, savePredictions = TRUE)

dt_model = train(Failed ~ ., # Set Y variable followed by '~'. The period indicates to include all variables for prediction. 
                  data = train_data, # Data
                  method = 'rpart', # Specify SVM model
                  trControl = fitControl) # Use cross validation

confusionMatrix(dt_model)

### Create object of importance of our variables 

dt_importance <- varImp(dt_model)

#### Create plot of importance of variables

ggplot(data = dt_importance, mapping = aes(x = dt_importance[,1])) + # Data & mapping
  geom_boxplot() + # Create box plot
  theme_light() +
  labs(title="Variable importance: Decision tree model",
       fill="",
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Now lets plot the decision tree using fancyRpartPlot() from the RATTLE package. This will give us clear insight into how the model makes its predictions.

fancyRpartPlot(dt_model$finalModel, sub = '')

###PREDICTION: Decision tree model - Use the created dt_model to run a prediction on the test data.

prediction_dt = predict(dt_model, test_data)

####Check the proportion of the predictions which were accurate.

table(prediction_dt, test_data$Failed) %>% # Create prediction table. 
  prop.table() %>% # Convert table values into proportions instead of counts. 
  round(2) # Round numbers to 2 significant values. 

##predicting 
pred_dt <- predict(dt_model, test)

solution_dt = data.frame(id = test$id, Failed = pred_dt) 

solution_dt <- solution_dt %>%
  mutate(Machine.Failure = ifelse(Failed == "Failed", 1,0))

head(solution_dt, n=5)

view(solution_dt)
