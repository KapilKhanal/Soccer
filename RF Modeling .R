# -----------------------------------------------------------------------------

# Load libraries/data/etc.

# -----------------------------------------------------------------------------

library(randomForest)

library(tidyverse)

train <- read.csv("https://raw.githubusercontent.com/KapilKhanal/Soccer/master/train3-1.csv")

test <- read.csv("https://raw.githubusercontent.com/KapilKhanal/Soccer/master/test3-1.csv")


## split response and predictors.


#TRAIN

train.resp <- train$ExpectedGoal

train.pred <- train %>% select(-ExpectedGoal)


#TEST
test.resp <- test$ExpectedGoal

test.pred <- test %>% select(-ExpectedGoal)




View(train.resp)

# -----------------------------------------------------------------------------

# Data preparation

# -----------------------------------------------------------------------------


#Remove NAs from response 

train.resp <- ifelse(is.na(train.resp), 0 , train.resp)

test.resp <- ifelse(is.na(test.resp), 0 , test.resp)


#use select variables:

train.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,

                        pageRank_End, centrality_Origin, centrality_End, length, close_to_shot)) -> train.pred


test.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,

                       pageRank_End, centrality_Origin, centrality_End, length, close_to_shot)) -> test.pred




View(head(test.pred))

# -----------------------------------------------------------------------------

# Perform random forest

# -----------------------------------------------------------------------------

#MODEL 1 - all 10 variables
rf.model <- randomForest(train.resp~., data = train.pred, importance = TRUE)
rf.model$mse

rf.pred <- predict(rf.model, newdata = test.pred)

mse <- mean((test.resp-rf.pred)^2)
#____________________________________________________ 
#Remove centrality
train.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,
                        
                        pageRank_End, length, close_to_shot)) -> train.pred


test.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,
                       
                       pageRank_End, length, close_to_shot)) -> test.pred


##MODEL 2 - No centrality. 
rf.model2 <- randomForest(train.resp~., data = train.pred, importance = TRUE)

rf.pred <- predict(rf.model2, newdata = test.pred)

mse2 <- mean((test.resp-rf.pred)^2)
mse2



#Remove negatives_____________________________________________________________________
train_2 <- train
test_2 <- test

train_2$ExpectedGoal = ifelse(train_2$ExpectedGoal < 0, 0, train_2$ExpectedGoal)

test_2$ExpectedGoal = ifelse(test_2$ExpectedGoal < 0, 0, test_2$ExpectedGoal)

#_______________________________________________________

#TRAIN
train_2.resp <- train_2$ExpectedGoal

train_2.pred <- train_2 %>% select(-ExpectedGoal)


#TEST
test_2.resp <- test_2$ExpectedGoal

test_2.pred <- test_2 %>% select(-ExpectedGoal)

View(head(train_2, 40))


#Remove NAs from response 

train_2.resp <- ifelse(is.na(train_2.resp), 0 , train_2.resp)

test_2.resp <- ifelse(is.na(test_2.resp), 0 , test_2.resp)


which(test_2.resp <0)

#Without centrality 
train_2.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,
                        
                        pageRank_End, length, close_to_shot)) -> train_2.pred


test_2.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,
                       
                       pageRank_End, length, close_to_shot)) -> test_2.pred

## Model 3 - NO NEGATIVES, ALL EXCEPT CENTRALITY ******

rf.model3 <- randomForest(train_2.resp~., data = train_2.pred, importance = TRUE)
rf.pred <- predict(rf.model3, newdata = test_2.pred)
mse3 <- mean((test_2.resp-rf.pred)^2)


#Disclude non pageRank vars 
train_2.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,
                          
                          pageRank_End)) -> train_2.pred


test_2.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,
                         
                         pageRank_End)) -> test_2.pred


## Model 4 - location and PageRank
rf.model4 <- randomForest(train_2.resp~., data = train_2.pred, importance = TRUE)
rf.pred <- predict(rf.model4, newdata = test_2.pred)
mse4 <- mean((test_2.resp-rf.pred)^2)

mse4

# -----------------------------------------------------------------------------
# Post-model work 
# -----------------------------------------------------------------------------

#Variable importance
rf.model3$importance
varImpPlot(rf.model3) #close to shot the best

#residuals vs fitted plot
res.df <- data.frame(Actual = train_2.resp, Predicted = rf.model3$predicted)

ggplot(res.df, aes(x = Predicted, y = Actual)) + geom_point() + labs(title = "Actual vs Predicted xG",
                                                                     subtitle = "Random Forest")

#do not have to run model3 again
save(train_2, test_2, rf.model3, file = "RandForest.RData")















