#EDA - Bigrams

# ------------------------------------------------------------------------------------------
# Finding most common bigrams (sequences of two positions passing and receiving the ball)
# ------------------------------------------------------------------------------------------






# ------------------------------------------------------------------------------------------
# Finding average xG of most common Bigrams (irrelevant of direction between players).
# ------------------------------------------------------------------------------------------

xG.df <- data.frame(Position = soccer$position.name, Lagged = soccer$positionLead, xG = soccer$shot.statsbomb_xg)
View(xG.df)

xG.df %>% mutate(group = case_when(
  
  (Position == "Right Wing" & Lagged == "Center Forward") | (Position == "Center Forward" & Lagged == "Right Wing")  ~ "RW_CF",
  (Position == "Center Forward" & Lagged == "Left Wing") | (Position == "Left Wing" & Lagged == "Center Forward")  ~ "CF_LW",
  (Position == "Center Attacking Midfield" & Lagged == "Left Wing") | (Position == "Left Wing" & Lagged == "Center Attacking Midfield")  ~ "CAM_LW"
  
)) -> xG.df 

View(xG.df)

xG.df %>% group_by(group) %>% summarise(mean = mean(xG)) %>% arrange(desc(mean)) %>% filter(!is.na(group)) 


# ------------------------------------------------------------------------------------------
# Plot of most common Bigrams.
# ------------------------------------------------------------------------------------------

