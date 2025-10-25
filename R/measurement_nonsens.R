library(tidyverse)

root_folder <- "C:/temp"


source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/measurement_functions.R")
source("sources/output_folders.R")

corpus_name <- "nonsens"

data <- consolidate_data(corpus_name)


# DESCRIPTIVES
desc <- data %>%
  psych::describe() %>%
  as.data.frame() %>%
  round(2)
ft <- desc %>%
  flextable()
ft
# save_as_docx(ft, path = paste(tables_output_folder, "/desc_", corpus_name, ".docx", sep=""))
mean(desc$se)

corr_stats <- data %>%
  select(all_of(factors)) %>%
  corr.test()

round(corr_stats$r, 2)
round(corr_stats$p, 3)

stats <- data %>%
  group_by(author_age) %>%
  group_modify(~ describe(select(.x, ends_with("_llm"))) %>%
    rownames_to_column("variable")) %>%
  arrange(variable, author_age) %>%
  ungroup()
ft <- stats %>%
  as.data.frame() %>%
  flextable()
ft
# save_as_docx(ft, path = paste(tables_output_folder, "/desc_age_", corpus_name, ".docx", sep=""))
# write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/desc_age_", corpus_name, ".xlsx",sep=""))
