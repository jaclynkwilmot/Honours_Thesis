# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# Reshape data
Book3_long <- Exp_1_Tissue_Results %>%
  pivot_longer(
    cols = -Flowering,
    names_to = "Nutrient",
    values_to = "Concentration"
  )

Book3_long$Flowering <- ifelse(Book3_long$Flowering == "Y", "Flowering (12)", "Non-Flowering (12)")


# Run t-tests
stats <- Book3_long %>%
  group_by(Nutrient) %>%
  summarise(
    test = list(t.test(Concentration ~ Flowering)),
    .groups = "drop"
  ) %>%
  mutate(
    p_value = sapply(test, function(x) x$p.value),
    t_value = sapply(test, function(x) x$statistic),
    label_p = paste0("p = ", signif(p_value, 3)),
    label_t = paste0("t = ", round(t_value, 2))
  )

# Compute y-positions
y_positions <- Book3_long %>%
  group_by(Nutrient) %>%
  summarise(
    y_max = max(Concentration, na.rm = TRUE),
    y_range = diff(range(Concentration, na.rm = TRUE))
  )

stats <- left_join(stats, y_positions, by = "Nutrient") %>%
  mutate(
    y_pos_p = y_max + y_range * 0.14,   # p-value a bit higher
    y_pos_t = y_max + y_range * 0.02    # t-value just below
  )

# Plot nutrients boxplots
ggplot(Book3_long, aes(x = Flowering, y = Concentration, fill = Flowering)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, color = "black") +
  geom_text(
    data = stats,
    aes(x = 1.5, y = y_pos_p, label = label_p),
    inherit.aes = FALSE,
    family = "Times New Roman",
    size = 3
  ) +
  geom_text(
    data = stats,
    aes(x = 1.5, y = y_pos_t, label = label_t),
    inherit.aes = FALSE,
    family = "Times New Roman",
    size = 3
  ) +
  facet_wrap(~Nutrient, scales = "free_y") +
  labs(x = "Plant Type", y = "Nutrient Concentration", fill = "Flowering") +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    strip.text = element_text(size = 11, family = "Times New Roman"),
    legend.title = element_blank(),
    legend.text = element_blank()
  )
