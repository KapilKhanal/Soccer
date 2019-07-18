
# -----------------------------------------------------------------------------
# Load libraries/data/etc.
# -----------------------------------------------------------------------------
library(tidyverse)

df<- soccer
df <- first.match

# x.location ranges from 1 to 121 (120 long)
# y.location ranges from 1 to 81 (80 long)
X_FIELD_LENGTH <- 120
Y_FIELD_LENGTH <- 80

# Almost no data at x = 121 and y = 81, these are a pain for round numbers...
# so we just make them 120 and 80 respectively.
df$x.location[which(df$x.location >= 121)] <- 120
df$y.location[which(df$y.location >= 81)] <- 80
df$end.x.location[which(df$end.x.location >= 121)] <- 120
df$end.y.location[which(df$end.y.location >=81)] <- 80



# -----------------------------------------------------------------------------
# Main functions
# -----------------------------------------------------------------------------
get.grid.number <- function(grid_width, grid_height, x.pos, y.pos) {
  stopifnot(1 <= x.pos && x.pos <= 120)
  stopifnot(1 <= y.pos && y.pos <= 80)
  
  # Number grid as: 01 02 03 04 05 06 
  #                 07 08 09 10 11 12 etc.
  cells.in.row <- X_FIELD_LENGTH / grid_width
  y.row <- (y.pos - 1) %/% grid_height
  x.row <- (x.pos - 1) %/% grid_width
  return((cells.in.row * y.row) + x.row + 1)
}

get.pass.matrix <- function(data, grid_width, grid_height) {
  cells.in.row <- X_FIELD_LENGTH / grid_width
  cells.in.col <- Y_FIELD_LENGTH / grid_height
  
  mat <- matrix(rep(NA, (cells.in.col * cells.in.row)^2), 
                ncol = cells.in.col * cells.in.row)
  counts <- matrix(rep(0, (cells.in.col * cells.in.row)^2), 
                   ncol = cells.in.col * cells.in.row)
  
  # Fill based on average of passes from cell i to cell j
  # Start location is the rows of the matrix, end location is the columns
  for (i in 1:nrow(data)) {
    cell.start <- get.grid.number(grid_width, grid_height,
                                  df$x.location[i], df$y.location[i])
    cell.end  <- get.grid.number(grid_width, grid_height,
                                 df$end.x.location[i], 
                                 df$end.y.location[i])
    if (is.na(mat[cell.start, cell.end])) {
      mat[cell.start, cell.end] <- df$shot.statsbomb_xg[i]
      counts[cell.start, cell.end] <- 1
    } else {
      n <- counts[cell.start, cell.end]
      xg <- df$shot.statsbomb_xg[i]
      
      # Average of a stream is (current * n + new) / (n + 1)
      mat[cell.start, cell.end] <- 
        (mat[cell.start, cell.end] * n + xg) / (n + 1)
      counts[cell.start, cell.end] <- counts[cell.start, cell.end] + 1
    }
  }
  
  # Fill NA's with 0's once computation is done
  for (r in 1:nrow(mat)) {
    for (c in 1:ncol(mat)) {
      if (is.na(mat[r, c])) { mat[r, c] <- 0 }
    }
  }
  
  return(mat)
}

# Example usage:
# mat <- get.pass.matrix(df %>% slice(1:10000), 40, 40)

testMat <- get.pass.matrix(soccer, 40, 40)

testMat

# -----------------------------------------------------------------------------
# Split data
# -----------------------------------------------------------------------------
train_perc <- 0.8

set.seed(25644)
values <- sample(1:nrow(df), train_perc * nrow(df), replace = FALSE)

train <- df[values,]
test <- df[-values,]


# -----------------------------------------------------------------------------
# Write results
# -----------------------------------------------------------------------------
write_csv(train, "train.csv")
write_csv(test, "test.csv")

# -----------------------------------------------------------------------------
# Tune and test
# -----------------------------------------------------------------------------

# Split the training data into a true training set and a validation set
set.seed(72214)

train_perc <- 0.8
values <- sample(1:nrow(df), train_perc * nrow(train), replace = FALSE)
true.train <- train[values,]
validation <- train[-values,]

# Function to evaluate MSE of a trained matrix
get.mat.mse <- function(grid_height, grid_width, train.data, test.data)  {
  mat <- get.pass.matrix(train.data, grid_height, grid_width)
  temp <- test.data %>% 
    mutate(start.cell = get.grid.number(grid_height, grid_width,
                                        x.location, y.location),
           end.cell = get.grid.number(grid_height, grid_width, 
                                      end.x.location, end.y.location))
  temp$pred <- NA
  for (i in 1:nrow(temp)) {
    temp$pred[i] <- mat[temp$start.cell[i], temp$end.cell[i]]
  }
  
  return(mean((temp$pred - temp$shot.statsbomb_xg)^2))
}


#Added the function to get the "From" and "To" for each pass...determined by the validation in the box model
getFromTo<-function(grid_height,grid_width,data){
  data_with_box<-data%>%mutate(from = get.grid.number(grid_height,grid_width,data$x.location,data$y.location), 
                               to = get.grid.number(grid_height,grid_width,data$end.x.location,data$end.y.location)
  )
  return (data_with_box)
}

# Tune grid dimensions
x.sizes <- c(8, 10, 12, 15, 20, 24, 30, 40)
y.sizes <- c(8, 10, 16, 20, 40)

mse.summary <- data.frame(x.size = NA, y.size = NA, mse = NA)
for (x.size in x.sizes) {
  for (y.size in y.sizes) {
    mse <- get.mat.mse(x.size, y.size, true.train, validation)
    # Uncomment for verbose output
    # cat("Grid size: ", x.size, "x", y.size, "\n",
    #     "MSE: ", mse, "\n\n")
    mse.summary <- rbind(mse.summary, c(x.size, y.size, mse))
  }
}

mse.summary <- mse.summary %>% slice(-1)

View(mse.summary %>% arrange(desc(mse)))

# -----------------------------------------------------------------------------
# Results
# -----------------------------------------------------------------------------
best <- mse.summary[which.min(mse.summary$mse),]
View(best)

mat.final <- get.pass.matrix(train, best$x.size, best$y.size)
mat.final <- get.pass.matrix(train, 15, 16)
mat.final
get.mat.mse(best$x.size, best$y.size, train, test)

# I get a final output of 0.002916491 on a 15 x 16 grid

typeof(mat.final[1,])
# -----------------------------------------------------------------------------
# Plotting expected goals from a specific box to all other boxes.  
# -----------------------------------------------------------------------------

#needed libraries
library(tidyverse)
library(ggsoccer)



#Dave X's map2color function
#https://stackoverflow.com/questions/15006211/how-do-i-generate-a-mapping-from-numbers-to-colors-in-r
map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

  
 #use of colorRampPalette to create a function to map colors between chosen two colors.
  colfunc <- colorRampPalette(c("beige", "chartreuse4"))
  
  #determining box colors, one for each element in vector 
  box.colors <- map2color(mat.final[20,], colfunc(length(mat.final[20,])))
  
  #plot (for 15*16 grid), change dimensions accordingly in annotate 
  
  library(ggsoccer)
  ggplot(df, aes(x = x.location, y = y.location), col = box1.vector)+ annotate_pitch(dimensions = pitch_statsbomb,
                                                                                     fill = "chartreuse4",
                                                                                     colour = "grey") +
    
    labs(title = "Avg Expected Goal of Passes from 'box' 20 to rest of boxes", subtitle = "15x16 Grid", x = "Width",
         y = "Length") + 
    annotate("rect", xmin = seq(0, X_FIELD_LENGTH-15, by = 15), xmax =seq(15,X_FIELD_LENGTH,15),
             ymin = rep(0, times = 8), ymax = rep(16, times = 8), fill = box.colors[1:8]) +
    annotate("rect", xmin = seq(0, X_FIELD_LENGTH-15, by = 15), xmax =seq(15,X_FIELD_LENGTH,15),
             ymin = rep(16, times = 8), ymax = rep(32, times = 8), fill = box.colors[9:16]) +
    annotate("rect", xmin = seq(0, X_FIELD_LENGTH-15, by = 15), xmax =seq(15,X_FIELD_LENGTH,15),
             ymin = rep(32, times = 8), ymax = rep(48, times = 8), fill = box.colors[17:24]) +
    annotate("rect", xmin = seq(0, X_FIELD_LENGTH-15, by = 15), xmax =seq(15,X_FIELD_LENGTH,15),
             ymin = rep(48, times = 8), ymax = rep(64, times = 8),fill = box.colors[25:32]) +
    annotate("rect", xmin = seq(0, X_FIELD_LENGTH-15, by = 15), xmax =seq(15,X_FIELD_LENGTH,15),
             ymin = rep(64, times = 8), ymax = rep(80, times = 8), fill = box.colors[33:40]) +
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(8, times = 8), label = 1:8, col = "black", size = 5) +
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(24, times = 8), label = 9:16, col = "black", size = 5) +
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(40, times = 8), label = 17:24, col = "black", size = 5) +
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(56, times = 8), label = 25:32, col = "black", size = 5) +
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(72, times = 8), label = 33:40, col = "black", size = 5) +
    geom_hline(yintercept = seq(0, Y_FIELD_LENGTH, by = 16), col = "white") + 
    geom_vline(xintercept = seq(0, X_FIELD_LENGTH, by = 15), col = "white")+
    annotate("rect", xmin = 45, xmax= 60, ymin = 32, ymax = 48, color = "blue", alpha = .0001) +
    theme_bw()
  

#dividing the grid (not filled by expected goals)  
  ggplot(df, aes(x = x.location, y = y.location), col = box1.vector) + annotate_pitch(dimensions = pitch_statsbomb,
                                                                                      fill = "chartreuse4",
                                                                                      colour = "grey") +
    geom_hline(yintercept = seq(0, Y_FIELD_LENGTH, by = 16), col = "white") + 
    geom_vline(xintercept = seq(0, X_FIELD_LENGTH, by = 15), col = "white")+
    labs(title = "Creating the Grid" , subtitle = "15x16", xl = "Width",
         y = "Length") + 
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(8, times = 8), label = 1:8, col = "yellow", size = 5) +
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(24, times = 8), label = 9:16, col = "yellow", size = 5) +
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(40, times = 8), label = 17:24, col = "yellow", size = 5) +
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(56, times = 8), label = 25:32, col = "yellow", size = 5) +
    annotate("text", x = seq(7.5, 112.5, by = 15), y = rep(72, times = 8), label = 33:40, col = "yellow", size = 5) +
    theme_bw()

