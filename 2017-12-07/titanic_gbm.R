library(caret)
library(readr)
library(dplyr)
library(ggplot2)

#get the data
train_data <- read_csv('~/Documents/CodeClub/2017-12-07/train.csv')
test_data <- read_csv('~/Documents/CodeClub/2017-12-07/test.csv')

# some basic description plots
table(train_data$Survived) # how many survived
table(train_data$Sex) # how many males/femalse

summary(train_data$Age)  # what about age - a continuous variable
plot(hist(train_data$Age))

ggplot(train_data, aes(x=Age)) + geom_density()
ggplot(train_data, aes(x=Age)) + geom_histogram()

ggplot(train_data, aes(x=Age, colour=as.factor(Survived))) + geom_density() 
ggplot(train_data, aes(x=Age, fill=as.factor(Survived))) + 
  geom_histogram() + facet_wrap(~as.factor(Survived), ncol=1)

ggplot(train_data, aes(x=Age, colour=as.factor(Survived))) + geom_density() 

ggplot(train_data, aes(x=as.factor(Sex), fill=as.factor(Survived))) + 
  geom_histogram(stat='count') +
  facet_wrap(~as.factor(Survived), ncol=1)

ggplot(train_data, aes(x=Age, y=Fare)) + geom_point()
ggplot(train_data, aes(x=as.factor(Pclass), y=Fare)) + geom_boxplot()
ggplot(train_data, aes(x=as.factor(Survived), y=Fare)) + geom_boxplot()


simple_model_data <- train_data %>%
  dplyr::transmute(Survived=Survived, 
                   Pclass=as.factor(Pclass), 
                   Age, 
                   Sex=as.factor(Sex))

simple_model <- lm(as.logical(Survived) ~ Pclass + Age + Sex, data=simple_model_data)
summary(simple_model)

simple_glm_model <- glm(as.logical(Survived) ~ Pclass + Age + Sex, 
                        family=binomial(link='logit'),
                        data=simple_model_data)
summary(simple_glm_model)

##########do some modelling

#set up the x-validation scheme we want to use
## 5-fold CV repeated 10 times
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 10)

#set up a control grid of hyperparameters
tg <- tidyr::crossing(n.trees = c(100,200,50), 
                      n.minobsinnode=10,
                      interaction.depth=c(1,2,3), shrinkage=c(0.1,0.2,0.3)) %>% as.data.frame()

#do the fit using gradient boosting
set.seed(825)
glmFit1 <- train(Survived ~ ., data = simple_glm_model, 
                 method = "glm", 
                 trControl = fitControl,
                # tuneGrid = tg,
                na.action=na.omit,
                 verbose = FALSE)

#look at the train object - collection of models
gbmFit1

#look at how we did
caret::plot.train(gbmFit1, plotType = 'level')
caret::plot.train(gbmFit1, plotType = 'scatter')

#do a prediction
#process the test data
test_input <- dplyr::transmute(test_data,  PassengerId, Pclass, Sex, Fare) 

#do the prediction
test_output <- data_frame(PassengerId=test_input$PassengerId, Survived=
                          predict(gbmFit1, newdata=as.data.frame(test_input), na.action=NULL))
