# -----------------------------------------------------------------------------
# Load libraries/data/etc.
# -----------------------------------------------------------------------------

library(randomForest)
library(tidyverse)

train <- read.csv("https://raw.githubusercontent.com/KapilKhanal/Soccer/master/train2.csv")
test <- read.csv("https://raw.githubusercontent.com/KapilKhanal/Soccer/master/test2.csv")

## split response and predictors.

#TRAIN
train.resp <- train$shot.statsbomb_xg
train.pred <- train %>% select(-shot.statsbomb_xg)

#TEST
test.pred <- test %>% select(-shot.statsbomb_xg)
test.resp <- test$shot.statsbomb_xg

View(train.resp)



# -----------------------------------------------------------------------------
# Perform random forest
# -----------------------------------------------------------------------------

#Remove NAs from response 
train.resp <- ifelse(is.na(train.resp), 0 , train.resp)
test.resp <- ifelse(is.na(test.resp), 0 , test.resp)

#use only those variables we have used for GAM, Logistic Regression:
train.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,
                        pageRank_End, centrality_Origin, centrality_End, length)) -> train.pred

test.pred %>% select(c(x.location, y.location, end.x.location,end.y.location, pageRank_Origin,
                       pageRank_End, centrality_Origin, centrality_End, length)) -> test.pred

View(head(test.pred))


rf.model <- randomForest(train.resp~., data = train.pred, importance = TRUE)






