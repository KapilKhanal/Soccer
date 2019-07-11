library(tidyverse)
library(rjson)
library(data.table)
library(magrittr)

# data_path <- 'open-data-master/data/events'
data_path <- './data/events/'

files <- dir(path = data_path,pattern = "*.json")

data <- tibble(match = files) %>% # create a data frame
  # holding the file names
  mutate(data = map(files, function(x) jsonlite::fromJSON(txt = file.path(data_path, x) , flatten = TRUE) ) 
         # a new data column
)

data<-data %>% unnest()

View(head(EventDF))

EventDF <- within(data, rm('player.id','position.id','pass.recipient.id','pass.height.id','pass.body_part.id',
                           'pass.type.id','pass.outcome.id',
                           'ball_receipt.outcome.id','dribble.outcome.id','shot.body_part.id',
                            'shot.type.id','shot.outcome.id','goalkeeper.technique.id','goalkeeper.position.id',
                            'goalkeeper.body_part.id','goalkeeper.outcome.id','goalkeeper.type.id',
                            'interception.outcome.id','foul_committed.card.id','foul_committed.type.id', 
                            'duel.type.id','duel.outcome.id','50_50.outcome.id','substitution.outcome.id',
                            'substitution.replacement.id','bad_behaviour.card.id'))

passes_and_shots<-EventDF %>% filter(type.name == "Pass" | type.name == "Shot") %>% 
    select(match, possession, type.name, possession_team.name, location, pass.angle, pass.length, duration, 
           shot.statsbomb_xg, minute, second, position.name, pass.height.name, pass.body_part.name, 
           pass.switch, pass.cross, pass.type.name)

passes_and_shots$pass.switch[which(is.na(passes_and_shots$pass.switch))] <- FALSE
passes_and_shots$pass.cross[which(is.na(passes_and_shots$pass.cross))] <- FALSE

passesShots<-passes_and_shots %>% group_by(possession,match) %>% mutate(length = rev(seq(1:n())))

View(head(passesShots, 40))

temp <- head(passesShots, 100)

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
temp <- f(passesShots)
df.adjusted <- temp
View(temp)
View(head(passesShots, 100))

View(df)
View(df.adjusted)

View(tail(df.cleaned))

for (jj in 1:nrow(df.cleaned)) {
    df.cleaned$x.location[jj] <- df.cleaned$location[[jj]][1]
    df.cleaned$y.location[jj] <- df.cleaned$location[[jj]][2]
}

old.df <- read.csv("cleaned_data.csv")
df.adjusted <- data.frame(df.adjusted, x.location = old.df$x.location, y.location=old.df$y.location)

write.csv(df.adjusted %>% select(-location), file = "cleaned_data.csv")




