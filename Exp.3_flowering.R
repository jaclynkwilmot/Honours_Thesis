# Load required libraries
library(readxl)
library(ggplot2)
library(ggpubr)

# Convert flowering to numeric
Experiment_3_Data$flowering_num <- ifelse(Experiment_3_Data$flowering == "Y", 1, 0)

# Run t-test
ttest <- t.test(flowering_num ~ tree_distance, data = Experiment_3_Data)
t_val <- round(ttest$statistic, 3)
p_val <- signif(ttest$p.value, 3)

# Count flowering plants
flowering_counts <- Experiment_3_Data %>%
  filter(flowering == "Y") %>%
  group_by(tree_distance) %>%
  summarise(flowering_count = n())

# Create bar plot
ggplot(flowering_counts, aes(
  x = factor(tree_distance, levels = c("1m", "5m")),
  y = flowering_count,
  fill = tree_distance
)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  scale_fill_manual(values = c("1m" = "darkseagreen", "5m" = "maroon")) +
  labs(
    x = "Distance From Tree",
    y = "Number of Flowering Plants (out of 48)"
  ) +
  theme_minimal(base_family = "Times", base_size = 13) +
  theme(legend.position = "none") +
  expand_limits(y = max(flowering_counts$flowering_count) * 1.3) +
  annotate(
    "text",
    x = 1.5,
    y = max(flowering_counts$flowering_count) * 1.25,
    label = paste0("p = ", p_val),
    family = "Times",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(flowering_counts$flowering_count) * 1.15,
    label = paste0("t = ", t_val),
    family = "Times",
    size = 5
  )
