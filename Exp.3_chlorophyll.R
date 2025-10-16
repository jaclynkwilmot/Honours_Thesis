# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("ggplot2")
library(ggpubr)

# Run t-test
t_res <- t.test(chlorophyll_average ~ tree_distance, data = Experiment_2_Data)
t_val <- round(t_res$statistic, 2)

# Create boxplot for chlorophyll
ggplot(Experiment_2_Data, aes(
  x = factor(tree_distance, levels = c("1m", "5m"), labels = c("1m (24)", "5m (24)")), 
  y = chlorophyll_average,
  fill = tree_distance
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("1m" = "darkseagreen", "5m" = "maroon")) +
  labs(x = "Distance From Tree", y = "Average Leaf Chlorophyll (µmol/m²)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  stat_compare_means(
    method = "t.test", 
    label = "p.format",
    label.x = 1.45,
    label.y = max(Experiment_2_Data$chlorophyll_average) * 1.05,
    family = "Times", 
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_2_Data$chlorophyll_average) * 1.00,
    label = paste0("t = ", t_val),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")

