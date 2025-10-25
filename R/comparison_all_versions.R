library(car)
library(flextable)
library(effectsize)
library(tidyverse)
library(writexl)

root_folder <- "C:/temp"
source("sources/output_folders.R")

source("sources/get_essays.R")
source("sources/transformation_functions.R")
source("sources/combined_names_EN_DE.R")

essays <- get_essays() %>%
  select(-text, -author, -all_of(ends_with("binary")))

version_list <- c(
  "liwc",
  "v10", "v11", "v12",
  "v20", "v21", "v22",
  "v30",
  "v41",
  "v50"
)
factor_names <- c("O", "C", "E", "A", "N")

# read all data of all versions
versions <- list()
for (version in version_list) {
  filepath <- paste("../data/development_aggregated_", version, ".csv", sep = "")
  version_data <- read.csv2(filepath, sep = ",") %>%
    mutate(across(everything(), ~ as.numeric(.)))

  versions[[version]] <- left_join(essays, version_data, by = c("id" = "essay_id")) %>%
    rename(essay_id = id) %>%
    drop_na()
}

# extract data function
extract_stats_kruskal <- function(object) {
  h_result <- object$kw

  return(list(
    p = h_result$p.value,
    chi2 = h_result$statistic,
    eta2 = object$eta2$rank_eta_squared
  ))
}

# calculate H-Test results
kruskal_wallis_results <- list()
for (version in version_list)
{
  kruskal_wallis_results[[version]] <- list()

  all_factors <- versions[[version]] %>%
    select(
      starts_with("o_"),
      starts_with("c_"),
      starts_with("e_"),
      starts_with("a_"),
      starts_with("n_")
    ) %>%
    names()

  outcomes <- c("bin_o", "bin_c", "bin_e", "bin_a", "bin_n")
  names(outcomes) <- c("O", "C", "E", "A", "N")
  data <- versions[[version]]

  for (i in 1:5) {
    formula <- reformulate(outcomes[i], all_factors[i])
    kruskal_wallis_results[[version]][[factor_names[i]]] <- list(
      kw = kruskal.test(formula, data = data),
      eta2 = rank_eta_squared(formula, data = data),
      data = data
    )
  }
}


# Consolidate measurements
kruskal_wallis_results_df <- data.frame()
score_results <- list()
for (version in version_list) {
  row_data <- c(Modell = version)

  scores <- create_scores_frame(versions[[version]])
  score_results[[version]] <- mean(scores$SCORE)

  for (factor in factor_names) {
    stats <- extract_stats_kruskal(kruskal_wallis_results[[version]][[factor]])

    normrow <- paste("S", factor, sep = "")

    row_data <- c(
      row_data,
      format_psych(stats$chi2),
      format_p_psych(stats$p),
      format_psych(stats$eta2),
      format_psych(mean(scores[[normrow]]))
    )
  }
  kruskal_wallis_results_df <- rbind(kruskal_wallis_results_df, row_data, stringsAsFactors = FALSE)
}
kruskal_wallis_results_df$SCORE <- unlist(map(score_results, ~ sprintf("%.1f", .x)))

# Create results table
colnames(kruskal_wallis_results_df) <- c("Modell", paste0(rep(factor_names, each = 4), "_", rep(c("χ²", "p", "η²", "SC"), 5)), "SCORE")
ft <- flextable(kruskal_wallis_results_df) %>%
  add_header_row(values = c("Modell", rep(c("χ²", "p", "η²", "SC"), 5), "SCORE"), colwidths = rep(1, 22)) %>%
  add_header_row(values = c("", factor_names, "Ø"), colwidths = c(1, rep(4, 5), 1), 1) %>%
  theme_box() %>%
  width(j = 1, width = 1.2) %>%
  width(j = 2:22, width = 0.6) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  fontsize(size = 9, part = "all") %>%
  fontsize(j = 1, size = 10, part = "body") %>%
  bold(part = "header") %>%
  # Hintergrundfarben für bessere Lesbarkeit
  bg(i = 1, part = "header", bg = "#4472C4") %>%
  bg(i = 2, part = "header", bg = "#8DB4E2") %>%
  color(part = "header", color = "white")
print(ft)

# save_as_docx(ft, path=paste(tables_output_folder, "/h-tests.docx",sep=""))
# write_xlsx(as.data.frame(ft$body$dataset), path=paste(tables_output_folder, "/h-tests.xlsx",sep=""))
