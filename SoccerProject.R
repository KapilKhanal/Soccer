library(tidyverse)
library(rjson)
library(data.table)
library(magrittr)


data_path <- 'open-data-master/data/events'

files <- dir(path = data_path,pattern = "*.json")

allEvents_df<- files %>% map(function(x) jsonlite::fromJSON(txt = file.path(data_path, x) , flatten = TRUE))



data <- tibble(match = files) %>% # create a data frame
  # holding the file names
  mutate(data = map(files,function(x) jsonlite::fromJSON(txt = file.path(data_path, x) , flatten = TRUE) ) # a new data column
)

data<-data %>% unnest()
length(allEvents_df)

(allEvents_df)
View(head(data))

EventDF<- allEvents_df  %>% bind_rows()

View(EventDF)