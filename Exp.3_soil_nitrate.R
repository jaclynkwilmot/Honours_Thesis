# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# Run t-test
t_res <- t.test(soil_nitrate ~ tree_distance, data = Experiment_3_Data)
t_val <- round(t_res$statistic, 2)

# Create boxplot
ggplot(Experiment_3_Data, aes(
  x = factor(tree_distance, levels = c("1m", "5m"), labels = c("1m (24)", "5m (24)")), 
  y = soil_nitrate,
  fill = tree_distance
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("1m" = "darkseagreen", "5m" = "maroon")) +
  labs(x = "Distance From Tree", y = "Soil Nitrate Concentration (mg/kg)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  # Add p-value
  stat_compare_means(
    method = "t.test", 
    label = "p.format",
    label.x = 1.45,
    label.y = max(Experiment_3_Data$soil_nitrate) * 1.05,
    family = "Times", 
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_3_Data$soil_nitrate) * 1.00,
    label = paste0("t = ", t_val),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")

