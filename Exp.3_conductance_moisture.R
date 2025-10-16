# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(patchwork)

# Conduct t-test
t_conductance <- t.test(conductance_average ~ tree_distance, data = Experiment_2_Data)$statistic %>% round(2)
t_soil <- t.test(soil_moisture ~ tree_distance, data = Experiment_2_Data)$statistic %>% round(2)

# Creating Stomatal Conductance Plot
plot3 <- ggplot(Experiment_2_Data, aes(
  x = factor(tree_distance, levels = c("1m", "5m"), labels = c("1m (24)", "5m (24)")),
  y = conductance_average,
  fill = tree_distance
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("1m" = "darkseagreen", "5m" = "maroon")) +
  labs(x = "Distance From Tree", y = "Average Stomatal Conductance (mmol m-² s-²)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  stat_compare_means(
    method = "t.test",
    label = "p.format",
    label.x = 1.37,
    label.y = max(Experiment_2_Data$conductance_average) * 0.82,
    family = "Times",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_2_Data$conductance_average) * 0.78,
    label = paste0("t = ", t_conductance),
    family = "Times",
    size = 5
  ) +
  ylim(0, 250) +
  theme(legend.position = "none")

# Creating Soil Moisture Plot
plot4 <- ggplot(Experiment_2_Data, aes(
  x = factor(tree_distance, levels = c("1m", "5m"), labels = c("1m (24)", "5m (24)")),
  y = soil_moisture,
  fill = tree_distance
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("1m" = "darkseagreen", "5m" = "maroon")) +
  labs(x = "Distance From Tree", y = "Soil Moisture (%)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  stat_compare_means(
    method = "t.test",
    label = "p.format",
    label.x = 1.37,
    label.y = max(Experiment_2_Data$soil_moisture) * 0.3,
    family = "Times",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_2_Data$soil_moisture) * 0.285,
    label = paste0("t = ", t_soil),
    family = "Times",
    size = 5
  ) +
  ylim(0, 4) +
  theme(legend.position = "none")

# Combine plots
combined_plot <- plot3 + plot4 + 
  plot_annotation(
    tag_levels = list(c("A)", "B)")),
    theme = theme(
      plot.tag = element_text(family = "Times", size = 12)
    )
  )

# Display
combined_plot
