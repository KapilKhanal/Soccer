
#Code for the Bigram
#DATAFRAMES NEED TO BE CAHNGEED 

library(tidyverse)
df <- read.csv("/Users/qe5106hi/OneDrive - MNSCU/CMSACamp-master/open-data-master/SoccerCMSA_CMU/Scripts/ModelingCode/soccer.txt") 
df<-womenSoccer

firstMatch<-df %>% filter(match == "19714.json") %>% group_by(possession,possession_team.name) %>% nest()

#Make a function that takes the df for each possesion for each match and gives out a df with n-gram and count and we combine all these into one dataframe
library(tidytext)

getNgrams<- function(df){
  pats <- c(" |-")
  col_text<-df$player.name %>% str_replace_all(pats,"_")
  #Making a corpus
  text_df <- tibble(text = col_text)
  n_grams<-text_df %>% unnest_tokens(n_gram , text, token = "ngrams", n = 2) %>% count(n_gram, sort = TRUE)
  
  return (n_grams)
  
}

chelsea<-firstMatch %>% mutate(ngram_counts = map(data,getNgrams)) %>% 
                  select(-data) %>% 
                            filter(possession_team.name == "Chelsea FCW") %>% unnest() %>% 
                                  group_by(n_gram) %>% summarise(counts = n()) %>% filter(!is.na(n_gram)) %>%arrange(desc(counts))

View(chelsea)


View(text_df)

#Which length of possesion is most common
#MAKING A GRAPH
library(igraph)

    

n_gram_graph <- chelsea %>% filter(counts>2) %>% 
    graph_from_data_frame()



library(ggraph)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(n_gram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name)) +
  theme_void()


pass_plot <- chelsea %>%
  head(10) %>%
  ggplot() + 
  geom_col(aes(y = counts , x = reorder(n_gram,counts)),
           fill = "orange") +
  coord_flip() + 
  theme_linedraw() + 
  xlab(label = "Bigrams") + 
  ggtitle("Pass Bigrams")

#Trying to plot the network of the passes
#Instead of the counts we could have the average expected goal of all the passes between those players
#that will help us with seeing which players are playing the important passes
chelsea<-chelsea %>% separate(n_gram, c("source", "destination"),sep = " ")

#Unique Nodes
sources <- chelsea %>%
  distinct(source) %>%
  rename(label = source)

destinations <- chelsea %>%
  distinct(destination) %>%
  rename(label = destination)


nodes <- full_join(sources, destinations, by = "label")

nodes <- nodes %>% rowid_to_column("id")
nodes

#Edge list 

per_route <- chelsea %>%  filter(counts>2) %>% rename(weights = counts)

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- edges %>% select(from, to, weights)
edges

library(network)

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

#base R 
plot(routes_network)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)





#Degreee Centrality 
centr_degree(routes_igraph, mode = c("all", "out", "in", "total"),
             loops = TRUE, normalized = TRUE)
#Maybe this degree centrality should be done for each passes of the possession network 
#network can interfere with th igraph
detach(package:network)

library(igraph)
library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

routes_tidy<-routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weights))
########
# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(routes_tidy)
class(clp)

# Community detection returns an object of class "communities" 
# which igraph knows how to plot: 
edge.start <- ends(routes_tidy, es=E(routes_tidy), names=F)[,1]
edge.col <- V(routes_tidy)$color[edge.start]

plot(clp, routes_tidy, edge.curved=.1,edge.arrow.size=0,vertex.size = degree(routes_tidy), layout =layout.fruchterman.reingold(routes_tidy) )

# We can also plot the communities without relying on their built-in plot:
V(routes_tidy)$community <- clp$membership
colrs <- adjustcolor( c("red", "purple", "gold", "yellowgreen"), alpha=.6)
plot(routes_tidy, vertex.color=colrs[V(routes_tidy)$community], vertex.size = 10,edge.size = 2,arrow.size = 2)




#########
ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weights), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Counts of passes") +
  theme_graph()

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weights), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

#Interactive D3
library(visNetwork)
library(networkD3)
visNetwork(nodes, edges)
edges <- mutate(edges, width = weights/2 + 1)
visNetwork(nodes, edges) %>% visPhysics(stabilization = TRUE) %>%
  visIgraphLayout(layout = "layout_in_circle") %>% 
  visEdges(arrows = "middle")


nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weights", 
             opacity = 1, fontSize = 16, zoom = TRUE)



getCommon<-function(v){
  uniq<-unique(v)
  uniq[which.max(tabulate(match(v,uniq)))]
}

mostCommon<-firstMatch %>% group_by(possession) %>% summarise(count = n())


#Since this first match has single touches more than double sequences that led to shots we will for now look for sequences of two 


View(n_grams)



df %>% group_by(., possession) %>% filter(., position.name == "Right Wing" | position.name == "Center Forward") -> RW_CF
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

df<-df %>% mutate(positionLead = lead(position.name))

xG.df <- data.frame(Position = df$position.name, Lead = df$positionLead, xG = df$shot.statsbomb_xg)
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


#or all data on them
df %>% filter(., possession_team.name == "Chelsea FCW") %>%  filter(shot.statsbomb_xg != 0) -> Chelsea #or all data on 

Chelsea<-Chelsea %>%  mutate(x.location = x.location*1.2, y.location = y.location*.8, end.x.location = end.x.location*1.2,
                    end.y.location = end.y.location*.8)

Chelsea %>% group_by(position.name) %>% summarise(involved = n()/nrow(Chelsea)) -> Chelsea.I
Chelsea %>% group_by(position.name) %>% summarise(avgx = mean(x.location), avgy = mean(y.location)) -> Chelsea.L
Chelsea.4 <- data.frame(Position = Chelsea.I$position.name, X = Chelsea.L$avgx, Y = Chelsea.L$avgy, involved = Chelsea.I$involved)

second <- ggplot(Chelsea.4, aes(x= X, y = Y, color = Position)) + annotate_pitch(dimensions = pitch_statsbomb) + geom_point(aes(size = )) + theme_bw()

