library(tidyverse)
library(readr)

yn <- function(x) {
  ifelse(x == "y", 1, 0)
}

get_essays <- function() {
  essays <- read_csv("../data/essays.csv", 
                     locale = locale(),
                     col_names = c("author", "text", "yne", "ynn", "yna", "ync", "yno"),
                     skip = 1)
  
  
  # create binary variables in nice ocean order
  essays$bin_o = yn(essays$yno)
  essays$bin_c = yn(essays$ync)
  essays$bin_e = yn(essays$yne)
  essays$bin_a = yn(essays$yna)
  essays$bin_n = yn(essays$ynn)
  
  # drop old columns, add id and sort table
  essays <- essays %>% 
    select(-c(yno, ync, yne, yna, ynn)) %>%
    mutate(id = row_number()) %>% 
    relocate(id, .before=author)
  
  return (essays)
}


