library(tidyverse)
library(corrr)
library(writexl)

root_folder <- "C:/temp"

source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/measurement_functions.R")
source("sources/output_folders.R")
source("sources/difference_functions.R")
source("sources/combined_names_EN_DE.R")

corpus_name <- "orwell"
data_orwell_raw <- consolidate_data(corpus_name)
data_orwell <- data_orwell_raw %>%
  # filter(text_raw_numtokens >= 150) %>%
  select(
    author_name, author_age, receiver_sex,
    o_llm, c_llm, e_llm, a_llm, n_llm
  )

data_all <- data_orwell

############## Unterschiede zwischen den Autoren
results <- data.frame()

# grafischer Vergleich
violins <- create_factor_violins(data_all, "receiver_sex")
violins
# ggsave(paste(graphics_output_folder, "/comparison_violins_orwell_receivers.png", sep=""),
#       plot = violins, dpi=300, width = 8, height = 6)

data_all %>%
  group_by(receiver_sex) %>%
  summarize(n = n())

data_all <- data_all %>% mutate(receiver_sex = as.factor(receiver_sex))


data_all %>%
  group_by(receiver_sex) %>%
  summarise(
    M_O = mean(o_llm),
    M_C = mean(c_llm),
    M_E = mean(e_llm),
    M_A = mean(a_llm),
    M_N = mean(n_llm),
    SD_O = sd(o_llm),
    SD_C = sd(c_llm),
    SD_E = sd(e_llm),
    SD_A = sd(a_llm),
    SD_N = sd(n_llm),
  )


# H-Tests
o_diff <- create_stats_h_test(data_all, "o_llm", "receiver_sex", "O U-Test")
c_diff <- create_stats_h_test(data_all, "c_llm", "receiver_sex", "C U-Test")
e_diff <- create_stats_h_test(data_all, "e_llm", "receiver_sex", "E U-Test")
a_diff <- create_stats_h_test(data_all, "a_llm", "receiver_sex", "A U-Test")
n_diff <- create_stats_h_test(data_all, "n_llm", "receiver_sex", "N U-Test")
results <- rbind(o_diff, c_diff, e_diff, a_diff, n_diff) %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ as.character(.)))
ft <- results %>%
  flextable()
ft
# save_as_docx(ft, path=paste(tables_output_folder, "/measure_h_test_orwell_receivers.docx",sep=""))
# write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_h_test_orwell_receivers.xlsx",sep=""))
