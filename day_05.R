# load libraries
library(tidyverse)

# load data
input_rules <- readLines("input/day_05.01.txt") %>%
  str_split_fixed("\\|", 2)

input_print <- readLines("input/day_05.02.txt")

#:--------------------------------------------------------
# Part 1

# running total
number_answer = 0

# iterate through each string
for (i in input_print) {
  
  # numbers under consideration
  string_numbers = str_split(i, ",", simplify = TRUE)
  
  # number of rule fails
  rule_fails = 0
  
  # string under examination
  for (j in 1:length(string_numbers)) {
    
    if (j != length(string_numbers)) {
      
      # check rules for behind
      if (sum(string_numbers[(j+1):length(string_numbers)] %in% input_rules[which(input_rules[,2] == string_numbers[j]), 1]) > 0) {
        rule_fails = rule_fails + 1
      }
      
    }
    
    if (j != 1) {
      
      # check rules for forward
      if (sum(string_numbers[1:(j-1)] %in% input_rules[which(input_rules[,1] == string_numbers[j]), 2]) > 0) {
        rule_fails = rule_fails + 1
      }
      
    }
    
  }
  
  if (rule_fails == 0) {
    
    number_answer = number_answer + as.numeric(string_numbers[ceiling(length(string_numbers)/2)])
    
  }
  
}

# answer
print(number_answer)


#:--------------------------------------------------------
# Part 2

# running total
number_answer = 0

# iterate through each string
for (i in input_print) {
  
  # numbers under consideration
  string_numbers = str_split(i, ",", simplify = TRUE)
  
  # number of rule fails
  rule_fails = 0
  rules_failed <- NULL
  # string under examination
  for (j in 1:length(string_numbers)) {
    
    if (j != length(string_numbers)) {
      
      # check rules for behind
      if (sum(string_numbers[(j+1):length(string_numbers)] %in% input_rules[which(input_rules[,2] == string_numbers[j]), 1]) > 0) {
        rule_fails = rule_fails + 1
      }
      
    }
    
    if (j != 1) {
      
      # check rules for forward
      if (sum(string_numbers[1:(j-1)] %in% input_rules[which(input_rules[,1] == string_numbers[j]), 2]) > 0) {
        rule_fails = rule_fails + 1
      }
      
    }
    
  }
  
  if (rule_fails > 0) {
    
    stop()
    
    
    
    
    number_answer = number_answer + as.numeric(string_numbers[ceiling(length(string_numbers)/2)])
    
  }
  
}

# answer
print(number_answer)




