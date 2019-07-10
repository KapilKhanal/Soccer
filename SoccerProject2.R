library(tidyverse)
library(rjson)
library(data.table)
library(magrittr)


data_path <- 'open-data-master/data/events'

files <- dir(path = data_path,pattern = "*.json")

data <- tibble(match = files) %>% # create a data frame
  # holding the file names
  mutate(data = map(files,function(x) jsonlite::fromJSON(txt = file.path(data_path, x) , flatten = TRUE) ) # a new data column
)

data<-data %>% unnest()

View(head(data))

EventDF <- within(data, rm('player.id','position.id','pass.recipient.id','pass.height.id','pass.body_part.id','pass.type.id','pass.outcome.id','ball_receipt.outcome.id','dribble.outcome.id','shot.body_part.id',
                                'shot.type.id','shot.outcome.id','goalkeeper.technique.id','goalkeeper.position.id',
                                'goalkeeper.body_part.id','goalkeeper.outcome.id','goalkeeper.type.id',
                                'interception.outcome.id','foul_committed.card.id','foul_committed.type.id', 
                                'duel.type.id','duel.outcome.id','50_50.outcome.id','substitution.outcome.id',
                                'substitution.replacement.id','bad_behaviour.card.id'))

View(head(EventDF))


passes_and_shots<-EventDF %>% filter(type.name == "Pass" | type.name == "Shot") %>% select(match,possession,type.name,possession_team.name, location, pass.end_location, duration,shot.statsbomb_xg, minute, second)

passesShots<-passes_and_shots %>% group_by(possession,match) %>% mutate(length = rev(seq(1:n())))

View(head(passesShots))


