#Model 1. Generalized Additive Model

# -----------------------------------------------------------------------------
# Load libraries/data/etc.
# -----------------------------------------------------------------------------

library(mgcv)
library(tidyverse)

soccer <- read.csv("soccer.csv")

#Remove Free Kicks, Corners, Goal Kicks, etc.
soccer %>% filter(type.name == "Pass") %>% filter(is.na(pass.type.name)) -> passes



# --------------------------------------------------------------------------------------------------
# Run generalized additive model,check diagnostics, and append fitted values to original data frame.
# --------------------------------------------------------------------------------------------------

model1 <- gam(shot.statsbomb_xg ~  s(x.location,y.location), data = passes)

#'heat' map of expected goals at end of chain by location
plot(model1, scheme = 2)

#Residual Plots and other diagnostics
gam.check(model1)

#append fitted values to pass data frame
passes$fittedxG <- model1$fitted.values

# -----------------------------------------------------------------------------------------
# Function to calculate adjusted Expected Goal (Expected Goal at end of Pass Chain Per Pass)
# -----------------------------------------------------------------------------------------


sum_fitted <- function(df) {
  sumValues <- sum(df$fittedxG)
  df$fittedXG <- sumValues
  df$fittedXG
}

passes %>% group_by(match, possession, shot.statsbomb_xg) %>% nest() %>% mutate(summ = map(data, sum_fitted)) %>% unnest() -> passes
View(passes)
passes %>% mutate(adjusted = (fittedxG/summ)*shot.statsbomb_xg) -> passes.adj
View(passes)

# -----------------------------------------------------------------------------------------
# Adding an effect per pass column 
# -----------------------------------------------------------------------------------------


#create lag column
passes.adj$lag <- lag(passes.adj$adjusted, n = 1)

#then make sure rows where adjusted = 0 have 0 retroactively
passes.adj$lag <- ifelse(passes.adj$adjusted == 0 , 0, passes.adj$lag)

#difference the two columns
passes.adj$Inc.Per.Pass <- passes.adj$adjusted - passes.adj$lag

#if lag is zero, increase per pass zero
passes.adj$Inc.Per.Pass <- ifelse(passes.adj$lag == 0 , 0, passes.adj$Inc.Per.Pass)

#remove lag and make increase per pass NA if zero.
within(passes.adj, rm(lag)) -> passes.adj
passes.adj$Inc.Per.Pass <- ifelse(passes.adj$Inc.Per.Pass == 0 , NA, passes.adj$Inc.Per.Pass)


# -----------------------------------------------------------------------------------------
# Creating Average Increase Per Pass from Position to Position
# -----------------------------------------------------------------------------------------


###CREATING AVERAGE INC.PER.PASS FROM POSITION TO POSITION
#create a to position variable
passes.adj$to.position.name <- lead(passes.adj$position.name, n = 1)


passes.adj %>% filter(Inc.Per.Pass != 0) %>% group_by(position.name, to.position.name) -> Positions_Inc


# -----------------------------------------------------------------------------------------
# Validating for more than one choice of k
# -----------------------------------------------------------------------------------------



