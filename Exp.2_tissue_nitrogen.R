# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(patchwork)

data_long <- Exp_2_plant_data %>%
  pivot_longer(-Plot, names_to = "Treatment", values_to = "Value")

# Clean treatment names
data_long <- data_long %>%
  mutate(Treatment = case_when(
    str_detect(Treatment, "high_flowering") ~ "High fertiliser",
    str_detect(Treatment, "low_flowering") ~ "Low fertiliser",
    str_detect(Treatment, "water_only") ~ "Water only",
    str_detect(Treatment, "control") ~ "Control",
    TRUE ~ Treatment
  ))

nitrate <- data_long %>% filter(Plot == "Nitrate (mg/kg)")
nitrogen <- data_long %>% filter(Plot == "Nitrogen (% w/w)")

# Nitrate Plot
p_nitrate <- ggplot(nitrate, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +  # <- remove legend for this plot
  scale_fill_manual(values = c(
    "Control" = "hotpink2",
    "High fertiliser" = "darkgoldenrod2",
    "Low fertiliser" = "forestgreen",
    "Water only" = "steelblue1"
  )) +
  labs(y = "Nitrate (mg/kg)", x = "Treatment") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(family = "Times New Roman")
  )

# Nitrogen plot
p_nitrogen <- ggplot(nitrogen, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Control" = "hotpink2",
    "High fertiliser" = "darkgoldenrod2",
    "Low fertiliser" = "forestgreen",
    "Water only" = "steelblue1"
  )) +
  labs(y = "Nitrogen (% w/w)", x = "Treatment", fill = "Treatment") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(family = "Times New Roman"),
    legend.position = "right"  # this will become the combined legend
  )

# Combine plots
combined_plot <- p_nitrate + p_nitrogen + 
  plot_annotation(
    tag_levels = list(c("A)", "B)")),
    theme = theme(
      plot.tag = element_text(family = "Times", size = 12)
    )
  ) & theme(legend.position = "none")  # ensures one shared legend on the right

combined_plot

# One-way ANOVA. - NITRATE
anova_nitrate <- aov(Value ~ Treatment, data = nitrate)
summary(anova_nitrate)

# Post hoc Tukey HSD
tukey_nitrate <- TukeyHSD(anova_nitrate)
tukey_nitrate

# One-way ANOVA - NITROGEN
anova_nitrogen <- aov(Value ~ Treatment, data = nitrogen)
summary(anova_nitrogen)

# Post hoc Tukey HSD
tukey_nitrogen <- TukeyHSD(anova_nitrogen)
tukey_nitrogen

