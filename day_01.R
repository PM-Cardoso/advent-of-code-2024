# load libraries
library(readr)
library(tidyverse)

# load data
input <- read_delim(
  "input/day_01.txt", 
  delim = "   ", 
  col_names = FALSE,
  show_col_types = FALSE
) %>%
  as.data.frame()

#:--------------------------------------------------------
# Part 1

# answer
sum(abs(sort(input[,1]) - sort(input[,2])))


#:--------------------------------------------------------
# Part 2

# objects needed
item_list <- unique(input[,1])
times_appearing <- NULL

# calculate the number of times each item appears
for (i in item_list) {
  times_appearing <- c(times_appearing, length(which(input[,2] == i)))
}

# calculate the value for each item
item_value <- item_list * times_appearing

# calculate similarity score
similarity_score = 0
for (i in 1:nrow(input)) {
  
  # add to similarity value
  similarity_score = similarity_score + item_value[which(input[i,1] == item_list)]
  
}

# answer
print(similarity_score)


