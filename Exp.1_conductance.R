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
t_conductance <- t.test(conductance_average ~ flowering, data = Experiment_1_Data, alternative = "greater")
t_soil <- t.test(soil_moisture ~ flowering, data = Experiment_1_Data, alternative = "greater")

t_val_conductance <- round(t_conductance$statistic, 2)
t_val_soil <- round(t_soil$statistic, 2)

# Creating Stomatal Conductance Plot
plot1 <- (ggplot(Experiment_1_Data, aes(
  x = factor(flowering, levels = c("Y", "N"), labels = c("Flowering (12)", "Non-Flowering (12)")),
  y = conductance_average,
  fill = flowering
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("Y" = "coral1", "N" = "cyan3")) +
  labs(x = "Plant Type", y = "Average Stomatal Conductance (mmol m-² s-²)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  stat_compare_means(
    method = "t.test",
    method.args = list(alternative = "greater"),
    label = "p.format",
    label.x = 1.4,
    label.y = max(Experiment_1_Data$conductance_average) * 1.10,
    family = "Times",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_1_Data$conductance_average) * 1.05,
    label = paste0("t = ", t_val_conductance),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")
)

# Creating Soil Moisture Plot
plot2 <- (ggplot(Experiment_1_Data, aes(
  x = factor(flowering, levels = c("Y", "N"), labels = c("Flowering (12)", "Non-Flowering (12)")),
  y = soil_moisture,
  fill = flowering
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("Y" = "coral1", "N" = "cyan3")) +
  labs(x = "Plant Type", y = "Soil Moisture (%)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  stat_compare_means(
    method = "t.test",
    method.args = list(alternative = "greater"),
    label = "p.format",
    label.x = 1.35,
    label.y = max(Experiment_1_Data$soil_moisture) * 1.10,
    family = "Times",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.441,
    y = max(Experiment_1_Data$soil_moisture) * 1.05,
    label = paste0("t = ", t_val_soil),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")
)

# Combine plots
combined_plot <- plot1 + plot2 + 
  plot_annotation(
    tag_levels = list(c("A)", "B)")),
    theme = theme(
      plot.tag = element_text(family = "Times", size = 12)
    )
  )

combined_plot
