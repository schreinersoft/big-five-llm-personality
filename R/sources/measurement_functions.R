library(tidyverse)
library(readr)

source("sources/transformation_functions.R")

consolidate_data <- function(name, sep = ",") {
  file_name <- paste("../data/corpus_", name, ".csv", sep = "")
  data_raw <- read.csv(file_name, sep = sep)

  analyzation <- read.csv("../data/corpus_measurement.csv", sep = ",") %>%
    aggregate_model_by_hash()

  data <- left_join(data_raw, analyzation, by = c("hash" = "hash")) %>%
    select(where(~ all(!is.na(.))))

  return(data)
}
