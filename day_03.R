# load libraries
library(tidyverse)

# load data
input <- readLines("input/day_03.txt") %>%
  paste(collapse = "")

#:--------------------------------------------------------
# Part 1

# function for calculating multiplication
multi_function <- function(data) {
  
  current_entries <- as.numeric(str_extract_all(data, "\\d+", simplify = TRUE))
  
  if ((current_entries[1] > 0 & current_entries[1] < 1000) & (current_entries[2] > 0 & current_entries[2] < 1000)) {
    
    return(current_entries[1] * current_entries[2])
    
  } else {
    
    return(0)
    
  }
  
}

# find all strings with the corresponding vector
entries <- str_extract_all(input, "mul\\(\\d+,\\d+\\)", simplify = TRUE)

# value 
multiplication_value = 0
for (i in 1:length(entries)) {
  
  # add value
  multiplication_value = multiplication_value + multi_function(entries[i])
  
}

# answer
multiplication_value


#:--------------------------------------------------------
# Part 2

# find all strings with the corresponding vector
entries_long <- str_extract_all(input, "mul\\(\\d+,\\d+\\)|do\\(\\)|don\\'t\\(\\)", simplify = TRUE)

# value
multiplication_value = 0
current_function <- "do()"
for (i in 1:length(entries_long)) {
  
  if (entries_long[i] == "do()") {
    current_function <- "do()"
  } else if (entries_long[i] == "don't()") {
    current_function <- "don't()"
  } 
  
  if (current_function == "do()" & entries_long[i] != "do()") {
    
    # add value
    multiplication_value = multiplication_value + multi_function(entries_long[i])
    
  }
  
}

# answer
multiplication_value




