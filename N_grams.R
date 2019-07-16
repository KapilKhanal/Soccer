
#Code for the Bigram
#DATAFRAMES NEED TO BE CAHNGEED 
data<- read.csv("file")

Chelsea<-data %>% filter(possession_team.name=="Chelsea FCW") 
Chelsea$position.name<-Chelsea$position.name %>% str_replace_all(" ","")
stringWeNeed<-str_flatten(Chelsea$position.name,collapse = " ")


#Making a corpus

text_df <- tibble(text = stringWeNeed)

#Which length of possesion is most common



getCommon<-function(v){
  uniq<-unique(v)
  uniq[which.max(tabulate(match(v,uniq)))]
}

mostCommon<-firstMatch %>% group_by(possession) %>% summarise(count = n())


#Since this first match has single touches more than double sequences that led to shots we will for now look for sequences of two 

seq = 2
n_grams<-text_df %>% unnest_tokens(n_gram, text, token = "ngrams", n = seq) %>% count(n_gram, sort = TRUE)




firstMatch %>% group_by(., possession) %>% filter(., position.name == "Right Wing" | position.name == "Center Forward") -> RW_CF
#make a data frame with position.name, lag, and shot.statsbomb_xg
#EXAMPLE for BRAIN STORMING 
ex.df <- data.frame(Position = c("CF", "LW", "CF", "RW", "CF", "RW", "LW"), Lagged = c("LW", "CF", "RW", "CF", "RW", "LW", NA),
                    xG = c(.02,.01,.01,.03,.04,.01,.02))


ex.df %>% group_by(Position, Lagged) %>% summarise(mean = mean(xG))

ex.df %>% mutate(group = case_when(
  
  (Position == "CF" & Lagged == "LW") | (Position == "LW" & Lagged == "CF")  ~ 1,
  (Position == "CF" & Lagged == "RW") | (Position == "RW" & Lagged == "CF")  ~ 2,
  (Position == "RW" & Lagged == "LW") | (Position == "LW" & Lagged == "RW")  ~ 3,
  
)) -> ex.df 

ex.df %>% group_by(group) %>% summarise(mean = mean(xG))


##imitate code for real data 
#make column

firstMatch<-firstMatch %>% mutate(positionLead = lead(position.name))

xG.df <- data.frame(Position = firstMatch$position.name, Lead = firstMatch$positionLead, xG = firstMatch$shot.statsbomb_xg)
View(xG.df)

xG.df %>% mutate(group = case_when(
  
  (Position == "Right Wing" & Lead == "Center Forward") | (Position == "Center Forward" & Lead == "Right Wing")  ~ "RW_CF",
  (Position == "Center Forward" & Lead == "Left Wing") | (Position == "Left Wing" & Lead == "Center Forward")  ~ "CF_LW",
  (Position == "Center Attacking Midfield" & Lead == "Left Wing") | (Position == "Left Wing" & Lead == "Center Attacking Midfield")  ~ "CAM_LW"
  
)) -> xG.df 

View(xG.df)

xG_df<-xG.df %>% group_by(group) %>% summarise(mean = mean(xG)) %>% arrange(desc(mean)) %>% filter(!is.na(group)) 

View(xG_df)



#concerned primarily with chelseas one match
rm(chelsea)
first.match %>% filter(., possession_team.name == "Chelsea FCW") %>%  filter(shot.statsbomb_xg != 0) -> Chelsea 

#or all data on them
soccer %>% filter(., possession_team.name == "Chelsea FCW") %>%  filter(shot.statsbomb_xg != 0) -> Chelsea #or all data on 

Chelsea %>%  mutate(x.location = x.location*1.2, y.location = y.location*.8, end.x.location = end.x.location*1.2,
                    end.y.location = end.y.location*.8)

Chelsea %>% group_by(position.name) %>% summarise(involved = n()/nrow(Chelsea)) -> Chelsea.I
Chelsea %>% group_by(position.name) %>% summarise(avgx = mean(x.location), avgy = mean(y.location)) -> Chelsea.L
Chelsea.4 <- data.frame(Position = Chelsea.I$position.name, X = Chelsea.L$avgx, Y = Chelsea.L$avgy, involved = Chelsea.I$involved)

second <- ggplot(Chelsea.4, aes(x= X, y = Y, color = Position)) + annotate_pitch(dimensions = pitch_statsbomb) + geom_point(aes(size = involved)) + theme_bw()

