# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(ggforce)
library(dplyr)
library(ggplot2)
library(patchwork)

Experiment_2_long <- Experiment_3_Data %>%
  pivot_longer(
    cols = c(height, width_1),
    names_to = "Trait",
    values_to = "Value"
  ) %>%
  mutate(
    Trait = recode(Trait,
                   height = "Height",
                   width_1 = "Width")
  )

# Compute stats
stats <- Experiment_2_long %>%
  group_by(Trait) %>%
  summarise(
    test = list(t.test(Value ~ tree_distance)),
    x_pos = 1.5,
    y_max = max(Value, na.rm = TRUE),
    y_range = diff(range(Value, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    p_value = sapply(test, function(x) x$p.value),
    t_value = sapply(test, function(x) x$statistic),
    label_p = paste0("p = ", signif(p_value, 3)),
    label_t = paste0("t = ", round(t_value, 2)),
    y_pos_p = y_max + y_range * 0.10,
    y_pos_t = y_max + y_range * 0.05
  )

# Separate data sets
height_data <- Experiment_2_long %>% filter(Trait == "Height")
width_data  <- Experiment_2_long %>% filter(Trait == "Width")

stats_height <- stats %>% filter(Trait == "Height")
stats_width  <- stats %>% filter(Trait == "Width")

# Plot Height
plot_height <- ggplot(height_data, aes(x = tree_distance, y = Value, fill = tree_distance)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  scale_fill_manual(values = c("1m" = "darkseagreen", "5m" = "maroon")) +
  scale_x_discrete(labels = c("1m" = "1m (24)", "5m" = "5m (24)")) +
  labs(x = NULL, y = "Height (cm)") +
  geom_text(
    data = stats_height,
    aes(x = x_pos, y = y_pos_p, label = label_p),
    inherit.aes = FALSE,
    family = "Times",
    size = 5
  ) +
  geom_text(
    data = stats_height,
    aes(x = x_pos, y = y_pos_t, label = label_t),
    inherit.aes = FALSE,
    family = "Times",
    size = 5
  ) +
  theme_minimal(base_family = "Times", base_size = 13) +
  theme(legend.position = "none")

# Plot Width
plot_width <- ggplot(width_data, aes(x = tree_distance, y = Value, fill = tree_distance)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  scale_fill_manual(values = c("1m" = "darkseagreen", "5m" = "maroon")) +
  scale_x_discrete(labels = c("1m" = "1m (24)", "5m" = "5m (24)")) +
  labs(x = NULL, y = "Width (cm)") +
  geom_text(
    data = stats_width,
    aes(x = x_pos, y = y_pos_p, label = label_p),
    inherit.aes = FALSE,
    family = "Times",
    size = 5
  ) +
  geom_text(
    data = stats_width,
    aes(x = x_pos, y = y_pos_t, label = label_t),
    inherit.aes = FALSE,
    family = "Times",
    size = 5
  ) +
  theme_minimal(base_family = "Times", base_size = 13) +
  theme(legend.position = "none")

combined_plot <- plot_height + plot_width +
  plot_annotation(
    tag_levels = list(c("A)", "B)")),
    caption = "Distance From Tree",
    theme = theme(
      plot.caption = element_text(hjust = 0.5, family = "Times", size = 13)
    )
  )

combined_plot

