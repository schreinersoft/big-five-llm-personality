library(tidyverse)
library(corrr)

root_folder <- "C:/temp"

source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/measurement_functions.R")
source("sources/output_folders_measurement.R")
source("sources/difference_functions.R")
source("sources/combined_names_EN.R")

gen_years <- function(from, to, total) {
  years <- seq(from, to)
  repeats <- total %/% length(years) # Integer division
  result <- rep(years, each = repeats)
}

corpus_name <- "nonsens"
years_noise <- c(gen_years(20, 50, 250), 50, 50)
data_noise_raw <- consolidate_data(corpus_name)
data_noise <- data_noise_raw %>%
  select(author_name, o_llm, c_llm, e_llm, a_llm, n_llm) %>%
  mutate(author_age = years_noise)

corpus_name <- "benjamin"
data_benjamin_raw <- consolidate_data(corpus_name)
data_benjamin <- data_benjamin_raw %>%
  select(author_name, author_age, o_llm, c_llm, e_llm, a_llm, n_llm)

data_all <- rbind(data_noise, data_benjamin)

############## Unterschiede zwischen den Daten
results <- data.frame()

# grafischer Vergleich
violins <- create_factor_violins(data_all, "author_name")
violins
# ggsave(paste(graphics_output_folder, "/comparison_violins_factors_noise_benjamin.png", sep=""),
#       plot = violins, dpi=300, width = 8, height = 6)

# H-Test between Auth
o_diff <- create_stats_h_test(data_all, "o_llm", "author_name", "O U-Test")
c_diff <- create_stats_h_test(data_all, "c_llm", "author_name", "C U-Test")
e_diff <- create_stats_h_test(data_all, "e_llm", "author_name", "E U-Test")
a_diff <- create_stats_h_test(data_all, "a_llm", "author_name", "A U-Test")
n_diff <- create_stats_h_test(data_all, "n_llm", "author_name", "N U-Test")
results <- rbind(o_diff, c_diff, e_diff, a_diff, n_diff) %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ as.character(.)))
ft <- results %>%
  flextable() %>%
  theme_alafoli() %>%
  autofit()
ft
# save_as_docx(ft, path=paste(tables_output_folder, "/measure_h_test_noise_benjamin.docx",sep=""))
# write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_h_test_noise_benjamin.xlsx",sep=""))
