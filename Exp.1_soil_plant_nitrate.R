# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(patchwork)

# Run t-tests
t_soil <- t.test(soil_nitrate ~ flowering, data = Experiment_1_Data)
t_plant <- t.test(plant_nitrate ~ flowering, data = Experiment_1_Data)

t_val_soil <- round(t_soil$statistic, 2)
t_val_plant <- round(t_plant$statistic, 2)

# Creating Soil Nitrate Plot
plot1 <- (ggplot(Experiment_1_Data, aes(
  x = factor(flowering, levels = c("Y", "N"), labels = c("Flowering (12)", "Non-Flowering (12)")),
  y = soil_nitrate,
  fill = flowering
)) +
  geom_boxplot(outlier.shape = NA, width = 0.4) +
  scale_fill_manual(values = c("Y" = "coral1", "N" = "cyan3")) +
  labs(x = "Plant Type", y = "Soil Nitrate Concentration (mg/kg)") +
  theme_minimal(base_family = "Times", base_size = 13) +
  stat_compare_means(
    method = "t.test",
    label = "p.format",
    label.x = 1.4,
    label.y = max(Experiment_1_Data$soil_nitrate) * 1.10,
    family = "Times",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_1_Data$soil_nitrate) * 1.05,
    label = paste0("t = ", t_val_soil),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")
)

# Creating Plant Nitrate Plot
plot2 <- (ggplot(Experiment_1_Data, aes(
  x = factor(flowering, levels = c("Y", "N"), labels = c("Flowering (12)", "Non-Flowering (12)")),
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
    label.x = 1.35,
    label.y = max(Experiment_1_Data$plant_nitrate) * 1.10,
    family = "Times",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(Experiment_1_Data$plant_nitrate) * 1.05,
    label = paste0("t = ", t_val_plant),
    family = "Times",
    size = 5
  ) +
  theme(legend.position = "none")
)

# Combine plots
combined_plot <- plot1 + plot2 + 
  plot_annotation(tag_levels = list(c("A)", "B)")),  
                  theme = theme(
                    plot.tag = element_text(family = "Times", size = 12)
                  )
  )

combined_plot
