# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(patchwork)


Experiment_1_Data$flowering <- factor(
  Experiment_1_Data$flowering,
  levels = c("Y", "N")
)

# Run one-sided t-tests
t_potassium <- t.test(soil_potassium ~ flowering, data = Experiment_1_Data, alternative = "greater")
t_phosphorus <- t.test(soil_phosphorus ~ flowering, data = Experiment_1_Data, alternative = "greater")

t_val_potassium <- round(t_potassium$statistic, 2)
t_val_phosphorus <- round(t_phosphorus$statistic, 2)

# Soil Potassium Plot
plot1 <- (ggplot(Experiment_1_Data, aes(
  x = factor(flowering, levels = c("Y", "N"), labels = c("Flowering (12)", "Non-Flowering (12)")),
  y = soil_potassium,
  fill = flowering
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("Y" = "coral1", "N" = "cyan3")) +
  labs(x = "Plant Type", y = "Soil Potassium (mg/kg)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  # Add one-sided p-value
  stat_compare_means(
    method = "t.test",
    method.args = list(alternative = "greater"),
    label = "p.format",
    label.x = 1.4,
    label.y = max(Experiment_1_Data$soil_potassium) * 1.10,
    family = "Times",
    size = 5
  ) +
  # Add t-value just below p
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_1_Data$soil_potassium) * 1.05,
    label = paste0("t = ", t_val_potassium),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")
)

# Soil Phosphorus Plot
plot2 <- (ggplot(Experiment_1_Data, aes(
  x = factor(flowering, levels = c("Y", "N"), labels = c("Flowering (12)", "Non-Flowering (12)")),
  y = soil_phosphorus,
  fill = flowering
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("Y" = "coral1", "N" = "cyan3")) +
  labs(x = "Plant Type", y = "Soil Phosphorus (mg/kg)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  # Add one-sided p-value
  stat_compare_means(
    method = "t.test",
    method.args = list(alternative = "greater"),
    label = "p.format",
    label.x = 1.35,
    label.y = max(Experiment_1_Data$soil_phosphorus) * 1.10,
    family = "Times",
    size = 5
  ) +
  # Add t-value just below p
  annotate(
    "text",
    x = 1.45,
    y = max(Experiment_1_Data$soil_phosphorus) * 1.05,
    label = paste0("t = ", t_val_phosphorus),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")
)

# Combine plots
combined_plot <- plot2 + plot1 + 
  plot_annotation(
    tag_levels = list(c("A)", "B)")),
    theme = theme(
      plot.tag = element_text(family = "Times", size = 12)
    )
  )

combined_plot