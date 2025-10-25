library(tidyverse)
library(corrr)
library(psych)
library(writexl)
library(scales)

root_folder <- "C:/temp"


source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/measurement_functions.R")
source("sources/output_folders.R")
source("sources/combined_names_EN_DE.R")

corpus_name <- "orwell"
data <- consolidate_data(corpus_name)

birth_year <- 1903
birth_month <- 6

# correct life year (was not calculated correctly)
data <- data %>%
  mutate(author_age = round((((year * 12) + month - ((birth_year * 12) + birth_month + 1)) / 12), 0))


factors <- data %>%
  select(ends_with("llm")) %>%
  names()


# also possible: group_by(year, month) %>%
# DESCRIPTIVES
desc <- data %>%
  select(all_of(factors)) %>%
  describe() %>%
  as.data.frame() %>%
  round(3)
ft <- desc %>%
  flextable()
ft
# save_as_docx(ft, path = paste(tables_output_folder, "/desc_", corpus_name, ".docx", sep=""))
mean(desc$se)

stats <- data %>%
  group_by(author_age) %>%
  group_modify(~ psych::describe(select(.x, ends_with("_llm"))) %>%
    as.data.frame() %>%
    rownames_to_column("variable")) %>%
  arrange(author_age, variable) %>%
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
      30,
      34,
      39,
      42,
      44
    ),
  ereignis =
    c(
      "Erstes Buch",
      "Bürgerkrieg",
      "Kündigung BBC",
      "Tod Ehefrau",
      "Schottland"
    )
)



### trendlines jährlich
min_max <- stats %>%
  filter(author_age > 28)
min_age <- min(min_max$author_age)
max_age <- max(min_max$author_age)

trendlines <- stats %>%
  filter(author_age > 28) %>%
  mutate(
    variable = factor(variable,
      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
    )
  ) %>%
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 1.0, linejoin = "round") +
  geom_point(size = 1.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~variable, scales = "fixed", labeller = labeller(variable = variable_names)) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "solid", alpha = 0.2, se = TRUE, size = 0.6, fill = "darkgrey") +
  scale_y_continuous(limits = c(3.0, 7.8), breaks = 1:9) +
  scale_x_continuous(limits = c(min_age, max_age), breaks = breaks_width(2)) +
  labs(
    title = "",
    x = "Alter",
    y = "M (Skalenwert)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
trendlines
# ggsave(paste(graphics_output_folder,"/lines_with_trend_", corpus_name, ".jpg", sep = ""),
#       plot = trendlines, dpi = 600, width = 7, height = 4)


thinner_trendlines <- stats %>%
  mutate(
    variable = factor(variable,
      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
    )
  ) %>%
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dotted", alpha = 0.4, se = TRUE, size = 0.3, fill = "darkgrey") +
  geom_line(size = 0.3, linejoin = "round") +
  geom_point(size = 0.7) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.3, color = NA) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~variable, scales = "fixed", labeller = labeller(variable = variable_names)) +
  scale_y_continuous(limits = c(3.0, 7.8), breaks = 1:9) +
  scale_x_continuous(limits = c(min_age, max_age), breaks = breaks_width(2)) +
  labs(
    title = "",
    x = "Alter",
    y = "Skalenwert"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 9)
  )
thinner_trendlines
# ggsave(paste(graphics_output_folder,"/thinner_lines_with_trend_", corpus_name, ".jpg", sep = ""),
#       plot = thinner_trendlines, dpi = 600, width = 7, height = 4)


# alle zusammen
trendlines_flat <- stats %>%
  filter(author_age > 28) %>%
  mutate(
    variable = factor(variable,
      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
    )
  ) %>%
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  scale_color_brewer(palette = "Set1", labels = variable_names) +
  scale_fill_brewer(palette = "Set1") +
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
  scale_y_continuous(limits = c(3.0, 7.8), breaks = 1:9) +
  scale_x_continuous(limits = c(min_age, max_age), breaks = breaks_width(2)) +
  geom_text(
    data = ereignisse,
    aes(x = author_age, y = 3, label = ereignis),
    angle = 0, vjust = -0.3, hjust = 1.05,
    size = 3, color = "red",
    inherit.aes = FALSE
  ) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "M (Skalenwert)",
    color = ""
  ) +
  theme_minimal() #+
# theme(legend.position = "none")  # Remove legend since facets show the variables
trendlines_flat
# ggsave(paste(graphics_output_folder,"/lines_flat_", corpus_name, ".jpg", sep = ""),
#       plot = trendlines_flat, dpi = 600, width = 8, height = 4)





# alle zusammen
thinner_trendlines_flat <- stats %>%
  mutate(
    variable = factor(variable,
      levels = c("o_llm", "c_llm", "e_llm", "a_llm", "n_llm")
    )
  ) %>%
  ggplot(aes(x = author_age, y = mean, color = variable, fill = variable)) +
  scale_color_brewer(palette = "Set1", labels = variable_names) +
  scale_fill_brewer(palette = "Set1") +
  # geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line(size = 0.5, linejoin = "round") +
  geom_point(size = 0.8) +
  geom_smooth(aes(group = variable), method = "loess", linetype = "dotted", alpha = 0.4, se = TRUE, size = 0.4) +
  geom_vline(
    data = ereignisse,
    aes(xintercept = author_age),
    color = "red",
    linetype = "dashed",
    alpha = 0.7
  ) +
  scale_y_continuous(limits = c(3.0, 7.8), breaks = 1:9) +
  scale_x_continuous(limits = c(min_age - 0.5, max_age), breaks = breaks_width(2)) +
  geom_text(
    data = ereignisse,
    aes(x = author_age, y = 3, label = ereignis),
    angle = 0, vjust = -0.3, hjust = 1.05,
    size = 3, color = "red",
    inherit.aes = FALSE
  ) +
  guides(fill = "none") +
  labs(
    title = "",
    x = "Alter",
    y = "Skalenwert",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")


thinner_trendlines_flat
# ggsave(paste(graphics_output_folder,"/thinner_lines_flat_", corpus_name, ".jpg", sep = ""),
#       plot = thinner_trendlines_flat, dpi = 300, width = 7, height = 5)
