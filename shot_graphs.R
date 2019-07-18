
# -----------------------------------------------------------------------------
# Load libraries/data/etc.
# -----------------------------------------------------------------------------
library(tidyverse)
library(ggsoccer)

df <- read_csv("soccer.csv") 
train <- read_csv("train.csv")
test <- read_csv("test.csv")

load("EventDF.Rdata")
View(firstMatch %>% filter(possession == 15))

View(df %>% select(match, possession, x.location, y.location, 
                        end.x.location, end.y.location, shot.statsbomb_xg)
        %>% slice(1:3, 17:19))


# -----------------------------------------------------------------------------
# Where are passes/shots being taken?
# -----------------------------------------------------------------------------

set.seed(99036)
df %>% 
    slice(sample(1:nrow(.), 3000)) %>%
    ggplot(aes(x = x.location, y = y.location)) +
        annotate_pitch(dimensions = pitch_statsbomb,
                       fill = "chartreuse4",
                       colour = "white") +
        theme_pitch() +
        scale_color_manual(values=c("orchid1", "yellow")) +
        geom_point(aes(color = type.name), alpha = 0.8) +
        guides(color=guide_legend(title="Event Type"))
        

# Have pretty good data on passes from all over the field.
#   - Obviously not much data right in fromt of the goal since shots are
#     typically taken from there.
#   - Also not a lot in the far corners away from the goal
#
#

df %>%
    dplyr::filter(match == "19714.json" & possession == 15 &
                      type.name == "Pass") %>%
    slice(1:5) %>%
    ggplot() +
        annotate_pitch(dimensions = pitch_statsbomb,
                       fill = "chartreuse4",
                       colour = "white") +
        theme_pitch() +
    geom_segment(aes(x = x.location, y = y.location, 
                     xend = end.x.location, yend = end.y.location),
                 arrow = arrow(length = unit(0.25, "cm"),
                               type = "closed"),
                 color = "yellow") +
    geom_label(aes(x=x.location, y=y.location,  label = X - 60)) +
    geom_segment(x = 110, y = 49, xend = 119, yend = 42,
                 arrow = arrow(length = unit(0.25, "cm"),
                               type = "closed"),
                 color = "yellow") +
    geom_label(x = 110, y = 49, label = "S") +
    geom_label(x = 88, y = 5, label = "Expected Goals from Shot: 0.0444027")

          