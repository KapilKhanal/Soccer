
# -----------------------------------------------------------------------------
# Load libraries/data/etc.
# -----------------------------------------------------------------------------
library(tidyverse)

df <- read_csv("soccer.csv") 
train <- read_csv("train.csv")
test <- read_csv("test.csv")

# x.location ranges from 1 to 121 (120 long)
# y.location ranges from 1 to 81 (80 long)
X_FIELD_LENGTH <- 120
Y_FIELD_LENGTH <- 80

# Almost no data at x = 121 and y = 81, these are a pain for round numbers...
# so we just make them 120 and 80 respectively.
df$x.location[which(df$x.location == 121)] <- 120
df$y.location[which(df$y.location == 81)] <- 80
df$end.x.location[which(df$end.x.location == 121)] <- 120
df$end.y.location[which(df$end.y.location == 81)] <- 80

train$x.location[which(train$x.location == 121)] <- 120
train$y.location[which(train$y.location == 81)] <- 80
train$end.x.location[which(train$end.x.location == 121)] <- 120
train$end.y.location[which(train$end.y.location == 81)] <- 80

test$x.location[which(test$x.location == 121)] <- 120
test$y.location[which(test$y.location == 81)] <- 80
test$end.x.location[which(test$end.x.location == 121)] <- 120
test$end.y.location[which(test$end.y.location == 81)] <- 80


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
                                      data$x.location[i], data$y.location[i])
        cell.end  <- get.grid.number(grid_width, grid_height,
                                     data$end.x.location[i], 
                                     data$end.y.location[i])
        if (is.na(mat[cell.start, cell.end])) {
            mat[cell.start, cell.end] <- data$shot.statsbomb_xg[i]
            counts[cell.start, cell.end] <- 1
        } else {
            n <- counts[cell.start, cell.end]
            xg <- data$shot.statsbomb_xg[i]
            
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

# -----------------------------------------------------------------------------
# Tune and test
# -----------------------------------------------------------------------------

# Split the training data into a true training set and a validation set
set.seed(72214)

train_perc <- 0.8
values <- sample(1:nrow(train), train_perc * nrow(train), replace = FALSE)
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
get.mat.mse(best$x.size, best$y.size, train, test)

# I get a final output of 0.002916491 on a 15 x 16 grid
