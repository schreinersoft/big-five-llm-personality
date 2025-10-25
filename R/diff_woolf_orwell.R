library(tidyverse)
library(corrr)

root_folder <- "C:/temp"


source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/measurement_functions.R")
source("sources/output_folders.R")
source("sources/difference_functions.R")
source("sources/combined_names_EN_DE.R")

corpus_name <- "woolf"
data_woolf_raw <- consolidate_data(corpus_name)
data_woolf <- data_woolf_raw %>%
  filter(text_raw_numtokens >= 150) %>%
  select(
    author_name, author_age, author_sex, year, month, text_type,
    o_llm, c_llm, e_llm, a_llm, n_llm
  )

corpus_name <- "orwell"
data_orwell_raw <- consolidate_data(corpus_name)
data_orwell <- data_orwell_raw %>%
  filter(text_raw_numtokens >= 150) %>%
  select(
    author_name, author_age, author_sex, year, month, text_type,
    o_llm, c_llm, e_llm, a_llm, n_llm
  )

data_all <- rbind(data_woolf, data_orwell)
rm(data_woolf_raw, data_orwell_raw, data_woolf, data_orwell)

############## Unterschiede zwischen den Autoren
results <- data.frame()

# grafischer Vergleich
data_all_sorted <- data_all %>%
  mutate(author_name = factor(author_name, levels = c("Virginia Woolf", "George Orwell")))

all_factors <- data_all_sorted %>%
  select(
    starts_with("o_"),
    starts_with("c_"),
    starts_with("e_"),
    starts_with("a_"),
    starts_with("n_")
  ) %>%
  names()

violins <- data_all_sorted %>%
  select(author_name, all_of(c(all_factors))) %>%
  pivot_longer(
    cols = all_of(all_factors),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = factor(variable, levels = all_factors)) %>%
  ggplot(aes(x = .data[["author_name"]], y = value, fill = variable)) +
  ylim(1, 9) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~variable, scales = "fixed", labeller = labeller(variable = variable_names)) +
  labs(
    title = "",
    x = "",
    y = "Skalenwert"
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 11)
  )
violins

# ggsave(paste(graphics_output_folder, "/comparison_violins_factors.jpg", sep=""),
#       plot = violins, dpi=300, width = 8, height = 5)



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
  flextable()
ft
# save_as_docx(ft, path=paste(tables_output_folder, "/measure_h_test_woolf_orwell.docx",sep=""))
# write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_h_test_woolf_orwell.xlsx",sep=""))


results <- data_all %>%
  select(author_name, ends_with("llm")) %>%
  group_by(author_name) %>%
  group_modify(~ psych::describe(.x) %>%
    as.data.frame() %>%
    rownames_to_column("variable")) %>%
  ungroup() %>%
  select(-vars)


ft <- results %>%
  flextable() %>%
  theme_alafoli() %>%
  autofit()
ft
# save_as_docx(ft, path=paste(tables_output_folder, "/descriptives_woolf_orwell.docx",sep=""))
# write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/measure_u_test_woolf_orwell.xlsx",sep=""))

results %>%
  filter(author_name == "Virginia Woolf") %>%
  summary()

results %>%
  filter(author_name != "Virginia Woolf") %>%
  summary()
