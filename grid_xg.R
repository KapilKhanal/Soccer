
# -----------------------------------------------------------------------------
# Load libraries/data/etc.
# -----------------------------------------------------------------------------
library(tidyverse)

df <- read.csv("soccer.csv") 

# x.location ranges from 1 to 121 (120 long)
# y.location ranges from 1 to 81 (80 long)
X_FIELD_LENGTH <- 120
Y_FIELD_LENGTH <- 80

# Almost no data at x = 121 and y = 81, these are a pain for round numbers...
# so we just make them 120 and 80 respectively.
df$x.location[which(df$x.location == 121)] <- 120
df$x.location[which(df$y.location == 81)] <- 80
df$x.location[which(df$end.x.location == 81)] <- 120
df$x.location[which(df$end.y.location == 81)] <- 80


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

# -----------------------------------------------------------------------------
# Results
# -----------------------------------------------------------------------------