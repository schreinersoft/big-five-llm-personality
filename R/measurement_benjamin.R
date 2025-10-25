library(tidyverse)
library(corrr)
library(writexl)

root_folder <- "C:/temp"

source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/measurement_functions.R")
source("sources/output_folders_measurement.R")
source("sources/factor-names-EN.R")


corpus_name <- "benjamin"
data <- consolidate_data(corpus_name)


factors <- data %>%
  select(ends_with("llm")) %>%
  names()

# DESRIPTIVES
desc <- data %>%
  select(all_of(factors)) %>%
  describe() %>%
  as.data.frame() %>%
  round(3)
ft <- desc %>%
  flextable()
ft
# save_as_docx(ft, path = paste(tables_output_folder, "/desc_", corpus_name, ".docx", sep=""))

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


# correlations
data %>%
  select(all_of(factors)) %>%
  cor() %>%
  round(2) %>%
  as.data.frame() %>%
  flextable()

corr_stats <- data %>%
  select(all_of(factors)) %>%
  corr.test()

round(corr_stats$r, 2)
round(corr_stats$p, 3)


# Lebensereignisse definieren
ereignisse <- data.frame(
  author_age =
    c(
      25,
      28,
      34,
      38,
      41,
      48
    ),
  ereignis =
    c(
      "Heirat & Kind",
      "Tod des Vaters",
      "Reise nach Moskau",
      "Scheidung",
      "Emigration nach Paris",
      "Flucht aus Paris"
    )
)

### trendlines jährlich

trendlines <- stats %>%
  mutate(
    variable = factor(variable,
      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
    )
  ) %>%
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.0, linejoin = "round") +
  geom_point(size = 1.2) +
  facet_wrap(~variable, scales = "fixed", labeller = labeller(variable = factor_names)) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "solid", alpha = 0.2, se = TRUE, size = 0.6, fill = "darkgrey") +
  scale_y_continuous(limits = c(3.4, 8), breaks = 1:9) +
  labs(
    title = "",
    x = "Alter",
    y = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")
trendlines
# ggsave(paste(graphics_output_folder,"/lines_with_trend_", corpus_name, ".jpg", sep = ""),
#       plot = trendlines, dpi = 600, width = 6, height = 4)


# alle zusammen
trendlines_flat <- stats %>%
  mutate(
    variable = factor(variable,
      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
    )
  ) %>%
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin = "round") +
  geom_point(size = 1.2) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dashed", alpha = 0.2, se = TRUE, size = 0.2, fill = "darkgrey") +
  geom_vline(
    data = ereignisse,
    aes(xintercept = author_age),
    color = "red",
    linetype = "dashed",
    alpha = 0.7
  ) +
  scale_y_continuous(limits = c(3.4, 8.5), breaks = 1:9) +
  scale_color_discrete(name = "", labels = variable_names) +
  geom_text(
    data = ereignisse,
    aes(x = author_age, y = 8.5, label = ereignis),
    angle = 0, vjust = -0.3, hjust = 1,
    size = 3, color = "red",
    inherit.aes = FALSE
  ) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "M"
  ) +
  theme_minimal()
trendlines_flat
# ggsave(paste(graphics_output_folder,"/lines_flat_", corpus_name, ".jpg", sep = ""),
#       plot = trendlines_flat, dpi = 600, width = 6, height = 4)



# alle zusammen
trendlines_flat_trendonly <- stats %>%
  mutate(
    variable = factor(variable,
      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
    )
  ) %>%
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  # geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  # geom_line(size = 1.0, linejoin="round") +
  # geom_point(size=1.2) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dashed", alpha = 0.3, se = TRUE, size = 1) +
  geom_vline(
    data = ereignisse,
    aes(xintercept = author_age),
    color = "red",
    linetype = "dashed",
    alpha = 0.7
  ) +
  scale_y_continuous(limits = c(3.4, 8.5), breaks = 1:9) +
  scale_color_discrete(name = "", labels = variable_names) +
  geom_text(
    data = ereignisse,
    aes(x = author_age, y = 8.5, label = ereignis),
    angle = 0, vjust = -0.3, hjust = 1,
    size = 3, color = "red",
    inherit.aes = FALSE
  ) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "M"
  ) +
  theme_minimal()
trendlines_flat_trendonly
# ggsave(paste(graphics_output_folder,"/lines_flat_trendonly", corpus_name, ".jpg", sep = ""),
#       plot = trendlines_flat_trendonly, dpi = 600, width = 6, height = 4)
