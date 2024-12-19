# load libraries
library(tidyverse)
library(BoardGames)

# load data
input <- readLines("input/day_04.txt")

#:--------------------------------------------------------
# Part 1

input_data.frame <- do.call(rbind.data.frame, lapply(input, function(x) {str_split(x, "", simplify = TRUE)}))

# function for finding XMAS
find_xmas <- function(data) {
  
  # running count
  output_number = 0
  
  # horizontal
  for (i in 1:nrow(data)) {
    # left to right
    output_number = output_number + str_count(paste(data[i,], collapse = ""), "XMAS")
    # right to left
    output_number = output_number + str_count(paste(rev(data[i,]), collapse = ""), "XMAS")
  }
  
  # vertical
  for (i in 1:ncol(data)) {
    # left to right
    output_number = output_number + str_count(paste(data[,i], collapse = ""), "XMAS")
    # right to left
    output_number = output_number + str_count(paste(rev(data[,i]), collapse = ""), "XMAS")
  }
  
  # diagonal top left to bottom right (right)
  test <- data
  test[test == "X"] = 1; test[test == "M"] = 2; test[test == "A"] = 3; test[test == "S"] = 4
  
  interim_data <- do.call(rbind.data.frame, get_diags(as.matrix(test), direction = "right")) %>% unname()
  for (i in 1:nrow(interim_data)) {
    if (i < ncol(interim_data)) {interim_data[i, (i+1):ncol(interim_data)] = NA}
    if (i > ncol(interim_data)) {interim_data[i, (ncol(interim_data) + 1 - (i-ncol(interim_data))):ncol(interim_data) ] = NA}
  }
  
  for (i in 1:nrow(interim_data)) {output_number = output_number + str_count(paste(interim_data[i,!is.na(interim_data[i,])], collapse = ""), "1234")}
  for (i in 1:nrow(interim_data)) {output_number = output_number + str_count(paste(rev(interim_data[i,!is.na(interim_data[i,])]), collapse = ""), "1234")}
  
  # diagonal top right to bottom left (left)
  test <- data
  test[test == "X"] = 1; test[test == "M"] = 2; test[test == "A"] = 3; test[test == "S"] = 4
  
  interim_data <- do.call(rbind.data.frame, get_diags(as.matrix(test), direction = "left")) %>% unname()
  for (i in 1:nrow(interim_data)) {
    if (i < ncol(interim_data)) {interim_data[i, (i+1):ncol(interim_data)] = NA}
    if (i > ncol(interim_data)) {interim_data[i, (ncol(interim_data) + 1 - (i-ncol(interim_data))):ncol(interim_data) ] = NA}
  }
  
  for (i in 1:nrow(interim_data)) {output_number = output_number + str_count(paste(interim_data[i,!is.na(interim_data[i,])], collapse = ""), "1234")}
  for (i in 1:nrow(interim_data)) {output_number = output_number + str_count(paste(rev(interim_data[i,!is.na(interim_data[i,])]), collapse = ""), "1234")}
  
  # output
  return(output_number)
  
}

# answer
print(find_xmas(input_data.frame))


#:--------------------------------------------------------
# Part 2

input_data.frame <- do.call(rbind.data.frame, lapply(input, function(x) {str_split(x, "", simplify = TRUE)}))

# function for finding MAS in diagonals
find_xmas_diag <- function(data) {
  
  # running count
  output_number = 0
  
  # diagonal top left to bottom right (right)
  test <- data
  test[test == "M"] = 2; test[test == "A"] = 3; test[test == "S"] = 4
  
  interim_data <- do.call(rbind.data.frame, get_diags(as.matrix(test), direction = "right")) %>% unname()
  for (i in 1:nrow(interim_data)) {
    if (i < ncol(interim_data)) {interim_data[i, (i+1):ncol(interim_data)] = NA}
    if (i > ncol(interim_data)) {interim_data[i, (ncol(interim_data) + 1 - (i-ncol(interim_data))):ncol(interim_data) ] = NA}
  }
  
  for (i in 1:nrow(interim_data)) {output_number = output_number + str_count(paste(interim_data[i,!is.na(interim_data[i,])], collapse = ""), "234")}
  for (i in 1:nrow(interim_data)) {output_number = output_number + str_count(paste(rev(interim_data[i,!is.na(interim_data[i,])]), collapse = ""), "234")}
  
  # diagonal top right to bottom left (left)
  test <- data
  test[test == "M"] = 2; test[test == "A"] = 3; test[test == "S"] = 4
  
  interim_data <- do.call(rbind.data.frame, get_diags(as.matrix(test), direction = "left")) %>% unname()
  for (i in 1:nrow(interim_data)) {
    if (i < ncol(interim_data)) {interim_data[i, (i+1):ncol(interim_data)] = NA}
    if (i > ncol(interim_data)) {interim_data[i, (ncol(interim_data) + 1 - (i-ncol(interim_data))):ncol(interim_data) ] = NA}
  }
  
  for (i in 1:nrow(interim_data)) {output_number = output_number + str_count(paste(interim_data[i,!is.na(interim_data[i,])], collapse = ""), "234")}
  for (i in 1:nrow(interim_data)) {output_number = output_number + str_count(paste(rev(interim_data[i,!is.na(interim_data[i,])]), collapse = ""), "234")}
  
  # output
  if (output_number == 2) {
    return(1)
  } else {
    return(0)
  }
  
}

# object needed
output_number_mas = 0

# iterate by each column
for (i in 1:(ncol(input_data.frame) - 2)) {
  
  for (j in 1:(nrow(input_data.frame) - 2)) {
    
    # find a x-mas
    output_number_mas = output_number_mas + find_xmas_diag(input_data.frame[(0:2)+i,(0:2)+j])
    
  }
  
}

# answer
print(output_number_mas)












