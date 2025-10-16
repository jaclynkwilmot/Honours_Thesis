# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

Experiment_1_Data$flowering <- factor(
  Experiment_1_Data$flowering,
  levels = c("Y", "N")
)

# Run one-sided t-test 
t_res <- t.test(chlorophyll_average ~ flowering, data = Experiment_1_Data, alternative = "greater")
t_val <- round(t_res$statistic, 2)

# Create boxplot
ggplot(Experiment_1_Data, aes(
  x = factor(flowering, levels = c("Y", "N"),
             labels = c("Flowering (12)", "Non-Flowering (12)")),
  y = chlorophyll_average,
  fill = flowering
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("Y" = "coral1", "N" = "cyan3")) +
  labs(x = "Plant Type", y = "Average Leaf Chlorophyll (µmol/m²)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  stat_compare_means(
    method = "t.test",
    method.args = list(alternative = "greater"),
    label = "p.format",
    label.x = 1.45,
    label.y = max(Experiment_1_Data$chlorophyll_average) * 1.05,
    family = "Times",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_1_Data$chlorophyll_average) * 1.00,
    label = paste0("t = ", t_val),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")
