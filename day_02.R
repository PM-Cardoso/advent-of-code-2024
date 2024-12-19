# load libraries
library(readr)
library(tidyverse)

# load data
input <- read_delim(
  "input/day_02.txt", 
  delim = " ", 
  col_names = FALSE,
  show_col_types = FALSE
) %>%
  as.data.frame()

#:--------------------------------------------------------
# Part 1

# vector of Safe / Not Safe
safety_vector <- NULL

# iterate per row
for (i in 1:nrow(input)) {
  
  # vector of entries
  entries <- input[i, which(!is.na(input[i,]))]
  
  # number of comparisons
  n_comparisons <- length(entries) - 1
  
  # differences between entries
  vector_differences <- diff(unlist(entries))
  
  # if statement: ( all are negative OR all are positive ) AND all are 1 - 3
  if ((length(which(vector_differences <= 0)) == n_comparisons | length(which(vector_differences >= 0)) == n_comparisons) &
      ( length(which(abs(vector_differences) >= 1)) == n_comparisons & length(which(abs(vector_differences) <= 3)) == n_comparisons )) {
    
    # Safety statement
    safety_vector <- c(safety_vector, "Safe")
    
  } else {safety_vector <- c(safety_vector, "Not Safe")}
  
}

# answer
length(which(safety_vector == "Safe"))


#:--------------------------------------------------------
# Part 2

# vector of Safe / Not Safe
safety_vector <- NULL

# iterate per row
for (i in 1:nrow(input)) {
  
  # vector of entries
  entries <- input[i, which(!is.na(input[i,]))]
  
  # number of comparisons
  n_comparisons <- length(entries) - 1
  
  # differences between entries
  vector_differences <- diff(unlist(entries))
  
  # if statement: ( all are negative OR all are positive ) AND all are 1 - 3
  if ((length(which(vector_differences <= 0)) == n_comparisons | length(which(vector_differences >= 0)) == n_comparisons) &
      ( length(which(abs(vector_differences) >= 1)) == n_comparisons & length(which(abs(vector_differences) <= 3)) == n_comparisons )) {
    
    # Safety statement
    safety_vector <- c(safety_vector, "Safe")
    
  } else {
    
    # iterate through, testing the removal of one entry to make it safe
    safety_vector_potential <- NULL
    
    for (j in 1:length(entries)) {
      
      entries_new <- entries[-j]
      
      # number of comparisons
      n_comparisons <- length(entries_new) - 1
      
      # differences between entries
      vector_differences <- diff(unlist(entries_new))
      
      # if statement: ( all are negative OR all are positive ) AND all are 1 - 3
      if ((length(which(vector_differences <= 0)) == n_comparisons | length(which(vector_differences >= 0)) == n_comparisons) &
          ( length(which(abs(vector_differences) >= 1)) == n_comparisons & length(which(abs(vector_differences) <= 3)) == n_comparisons )) {
        
        # Safety statement
        safety_vector_potential <- c(safety_vector_potential, "Safe") 
        
      } else {
        
        safety_vector_potential <- c(safety_vector_potential, "Not Safe")
        
      }
      
    }
    
    if (length(which(safety_vector_potential == "Safe")) > 0) {
      
      safety_vector <- c(safety_vector, "Safe")
      
    } else {
      
      safety_vector <- c(safety_vector, "Not Safe")
      
    }
    
  }
  
}

# answer 
length(which(safety_vector == "Safe"))







