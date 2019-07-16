
# -----------------------------------------------------------------------------
# Load libraries/data/etc.
# -----------------------------------------------------------------------------
library(tidyverse)
library(mgcv)

df <- read_csv("soccer.csv") 
train <- read_csv("train.csv")
test <- read_csv("test.csv")

# -----------------------------------------------------------------------------
# Train and test models
# -----------------------------------------------------------------------------
gam.mod.1 <- gam(shot.statsbomb_xg ~ s(x.location, y.location), 
                 data = train, method = "REML")
gam.mod.2 <- gam(shot.statsbomb_xg ~ s(x.location, y.location) +
                     s(end.x.location, end.y.location), 
                 data = train, method = "REML")

mse.1 <- mean((predict.gam(gam.mod.1, test) - test$shot.statsbomb_xg)^2)
mse.2 <- mean((predict.gam(gam.mod.2, test) - test$shot.statsbomb_xg)^2)

c(mse.1, mse.2)
