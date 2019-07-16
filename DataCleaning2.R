# Data Management 

#Begins with WomenSoccer df. 

# -----------------------------------------------------------------------------
# Remove .name columns that have a corresponding .id column. 
# -----------------------------------------------------------------------------

WomenSoccer.2 <- within(WomenSoccer, rm('player.id','position.id','pass.recipient.id','pass.height.id','pass.body_part.id','pass.type.id','pass.outcome.id','ball_receipt.outcome.id','dribble.outcome.id','shot.body_part.id',
                           'shot.type.id','shot.outcome.id','goalkeeper.technique.id','goalkeeper.position.id',
                           'goalkeeper.body_part.id','goalkeeper.outcome.id','goalkeeper.type.id',
                           'interception.outcome.id','foul_committed.card.id','foul_committed.type.id', 
                           'duel.type.id','duel.outcome.id','50_50.outcome.id','substitution.outcome.id',
                           'substitution.replacement.id','bad_behaviour.card.id'))


# -----------------------------------------------------------------------------
# Separating by just passes and shots.
# -----------------------------------------------------------------------------

passes_and_shots<-WomenSoccer.2 %>% filter(type.name == "Pass" | type.name == "Shot")

# ------------------------------------------------------------------------------------------
# Create a column that denotes the order of a pass/shot (within a possession), counting down.
# ------------------------------------------------------------------------------------------

passes_and_shots %>% group_by(possession,match) %>% mutate(length = rev(seq(1:n()))) -> passes_and_shots


# ------------------------------------------------------------------------------------------
# Convert NA values in the pass.cross and pass.switch columns to FALSE.
# ------------------------------------------------------------------------------------------

passes_and_shots$pass.cross[which(is.na(passes_and_shots$pass.cross))] <- F
passes_and_shots$pass.switch[which(is.na(passes_and_shots$pass.switch))] <- F


# --------------------------------------------------------------------------------------------------------------------------------
# Assign '0' for shots.statbomb_xg to passes in a chain with no shot taken. Otherwise, assign the value of the shot in the chain.
# --------------------------------------------------------------------------------------------------------------------------------

temp <- head(passes_and_shots, 100)

f <- function(df) {
  current.match <- df$match[nrow(df)]
  current.pos <- df$possession[nrow(df)]
  current.fill <- ifelse(is.na(df$shot.statsbomb_xg[nrow(df)]), 0, df$shot.statsbomb_xg[nrow(df)])
  
  for (ii in nrow(df):1) {
    if (!is.na(df$shot.statsbomb_xg[ii]) && abs(df$shot.statsbomb_xg[ii] - current.fill) > 0.0001) {
      current.fill <- df$shot.statsbomb_xg[ii]
    }
    if (df$match[ii] != current.match || df$possession[ii] != current.pos) {
      current.match <- df$match[ii]
      current.pos <- df$possession[ii]
      current.fill <- ifelse(is.na(df$shot.statsbomb_xg[ii]), 0, df$shot.statsbomb_xg[ii])
    }
    df$shot.statsbomb_xg[ii] <- current.fill
  }
  return(df)
}
#test on first 100 rows of data.

temp <- f(passes_and_shots)
df.adjusted <- temp
View(temp)
View(head(passes_and_shots, 100))

#run for entire dataset. 
df.adjusted <- f(passes_and_shots)

#name to a new file named soccer. 
soccer <- df.adjusted %>% select(match, possession, type.name, possession_team.name, pass.angle, pass.length, pass.switch,
                            pass.cross, duration, shot.statsbomb_xg,minute,second,position.name,pass.height.name,
                            pass.body_part.name,pass.type.name,length,location,pass.end_location,shot.end_location)

# --------------------------------------------------------------------------------------------------------------------------------
# Create separate x and y location columns for both passes and shots. 
# --------------------------------------------------------------------------------------------------------------------------------


soccer$x.location = 1:nrow(soccer)
soccer$y.location = 1:nrow(soccer)

for (i in 1:nrow(soccer)) {
  soccer$x.location[i] = soccer$location[[i]][1]
  soccer$y.location[i] = soccer$location[[i]][2]
}


View(head(data))

# --------------------------------------------------------------------------------------------------------------------------------
# Create combined end x location, end y location columns for both passes and shots. 
# --------------------------------------------------------------------------------------------------------------------------------


true_false<- unlist(map(.x = soccer$pass.end_location,.f = is.null))
true_false

soccer$end.location <- ifelse(true_false, soccer$shot.end_location, soccer$pass.end_location)

View(head(passes_and_shots, n = 40L))

soccer$end.x.location <- 1:nrow(soccer)
soccer$end.y.location <- 1:nrow(soccer)

for (i in 1:nrow(soccer)) {
  soccer$end.x.location[i] = soccer$end.location[[i]][1]
  soccer$end.y.location[i] = soccer$end.location[[i]][2]
}



# --------------------------------------------------------------------------------------------------------------------------------
# Give negative expected Goals for passes that lead to change in possession (in opponents' end.)
# --------------------------------------------------------------------------------------------------------------------------------

(View(head(soccer)))


# --------------------------------------------------------------------------------------------------------------------------------
# Write to Final File.  
# --------------------------------------------------------------------------------------------------------------------------------


write.csv(soccer %>% select(-c(location,pass.end_location,shot.end_location,end.location)), file = "soccer.csv")











