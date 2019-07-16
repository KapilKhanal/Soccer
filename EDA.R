#Exploratory Data Analysis

# -----------------------------------------------------------------------------
# Load libraries/data/etc.
# -----------------------------------------------------------------------------
install.packages('ggsoccer')
install.packages('gganimate')
install.packages('gifski')
install.packages('png')
library(tidyverse)
library(ggsoccer)
library(gganimate)
library(gifski)
library(png)
# -----------------------------------------------------------------------------
# I. Shot map throughout one match. 
# -----------------------------------------------------------------------------

#subset to first match
first.match <- soccer %>% filter(match =="22921.json") %>% filter(shot.statsbomb_xg != 0) -> first.match

#scale coordinates
first.match %>% mutate(x.location = x.location*1.2, y.location = y.location*.8, end.x.location = end.x.location*1.2,
                       end.y.location = end.y.location*.8)

#creating index for the possessions in this first match. 
first.match$index <- 1:nrow(first.match)

#ggplot object
first <- ggplot(first.match, aes(x.location, y.location, color = factor(possession))) + annotate_pitch(dimensions = pitch_statsbomb) + geom_point(shape = 21, size =3, stroke = 1) + 
  geom_segment(aes(x = x.location, y = y.location,xend =  end.x.location, yend = end.y.location), arrow = arrow(length = unit(0.25, "cm"), type = "closed")) + facet_wrap(~possession_team.name)

#animate shots
first2 <- first + transition_time(possession) + labs(title = "EventID: {frame_time}") +
  shadow_wake(wake_length = 0.01, alpha = TRUE)

animate(first2, nframes = nrow(first.match), duration = 20, fps = 1, end_pause = 5, rewind = FALSE)


# -----------------------------------------------------------------------------
# II. Positions most recurring in sequences leading to shot (Bar Chart)
# -----------------------------------------------------------------------------


first.match %>% group_by(position.name, possession_team.name) %>% summarise(count = log(n())) -> pos.df
pos.df
ggplot(pos.df, aes(x = position.name, y= count, col = factor(possession_team.name), fill = factor(possession_team.name))) +  geom_col() + coord_flip() + facet_wrap(pos.df$possession_team.name) + theme_bw() 


# -----------------------------------------------------------------------------
# III. 'Involved Positions' map - (Unigrams).  
# -----------------------------------------------------------------------------

#concerned primarily with one team's match, those sequences that led to a shot
first.match %>% filter(., possession_team.name == "France Women's") %>%  filter(shot.statsbomb_xg != 0) -> France 


France %>%  mutate(x.location = x.location*1.2, y.location = y.location*.8, end.x.location = end.x.location*1.2,
                    end.y.location = end.y.location*.8)

France %>% group_by(position.name) %>% summarise(involved = n()/nrow(Chelsea)) -> Chelsea.I
France %>% group_by(position.name) %>% summarise(avgx = mean(x.location), avgy = mean(y.location)) -> Chelsea.L
France.4 <- data.frame(Position = Chelsea.I$position.name, X = Chelsea.L$avgx, Y = Chelsea.L$avgy, involved = Chelsea.I$involved)

second <- ggplot(France.4, aes(x= X, y = Y, color = Position)) + annotate_pitch(dimensions = pitch_statsbomb) + geom_point(aes(size = involved)) + theme_bw()



                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 arrow = arrow(length = unit(0.25, "cm"), 
                                                                                                                                                                                                               