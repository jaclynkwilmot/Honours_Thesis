# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# Run t-test
t_res <- t.test(plant_nitrate ~ flowering, data = Experiment_1_Data)
t_val <- round(t_res$statistic, 2)

# Create boxplot
ggplot(Experiment_1_Data, aes(
  x = factor(flowering, levels = c("Y", "N"),
             labels = c("Flowering (12)", "Non-Flowering (12)")),
  y = plant_nitrate,
  fill = flowering
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("Y" = "coral1", "N" = "cyan3")) +
  labs(x = "Plant Type", y = "Plant Nitrate Concentration (mg/kg)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  stat_compare_means(
    method = "t.test",
    label = "p.format",
    label.x = 1.439,
    label.y = max(Experiment_1_Data$plant_nitrate) * 1.05,
    family = "Times",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_1_Data$plant_nitrate) * 1.00,
    label = paste0("t = ", t_val),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")
