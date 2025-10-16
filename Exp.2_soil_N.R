# Load libraries
library(tidyverse)
library(ez)
library(patchwork)

data1 <- Exp_2_initial_final_data
data1$plant_code <- factor(data1$plant_code)
data1$sample <- factor(data1$sample, levels = c("initial", "final"))
data1$plot <- factor(data1$plot)

# Soil nitrate ANOVA
anova_nitrate <- ezANOVA(
  data = data1,
  dv = soil_nitrate,
  wid = plant_code,
  within = .(sample),
  between = .(plot),
  detailed = TRUE
)
print(anova_nitrate)

# Soil ammonium ANOVA
anova_ammonium <- ezANOVA(
  data = data1,
  dv = soil_ammonium,
  wid = plant_code,
  within = .(sample),
  between = .(plot),
  detailed = TRUE
)
print(anova_ammonium)


# Reshape to long format
data_long <- data1 %>%
  pivot_longer(cols = c(soil_nitrate, soil_ammonium),
               names_to = "nutrient",
               values_to = "concentration")

# Plot ammonium
data_summary <- data1 %>%
  group_by(sample, plot) %>%
  summarise(mean_ammonium = mean(soil_ammonium, na.rm = TRUE),
            sd = sd(soil_ammonium, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            .groups = "drop")

plot_ammonium <- ggplot(data_summary, aes(x = sample, y = mean_ammonium,
                               color = plot, group = plot)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_ammonium - se,
                    ymax = mean_ammonium + se),
                width = 0.0, size = 0.0) +
  labs(x = "Period of Observation",
       y = expression("Soil Ammonium (mg/kg)"),
       color = "Treatment") +
  coord_cartesian(ylim = c(0, 60)) +  # avoids clipping values
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("control" = "hotpink2",
                                "high_fertiliser" = "darkgoldenrod2",
                                "low_fertiliser" = "forestgreen",
                                "water_only" = "steelblue1"),
                     labels = c("Control", 
                                "High Fertiliser", 
                                "Low Fertiliser", 
                                "Water Only")) +
  scale_x_discrete(labels = c("initial" = "Initial (April)",
                              "final" = "Final (September)")) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "right")


# Plot ammonium
data_summary <- data1 %>%
  group_by(sample, plot) %>%
  summarise(mean_nitrate = mean(soil_nitrate, na.rm = TRUE),
            sd = sd(soil_nitrate, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            .groups = "drop")

plot_nitrate <- ggplot(data_summary, aes(x = sample, y = mean_nitrate,
                                          color = plot, group = plot)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_nitrate - se,
                    ymax = mean_nitrate + se),
                width = 0.0, size = 0.0) +
  labs(x = "Period of Observation",
       y = expression("Soil Nitrate (mg/kg)"),
       color = "Treatment") +
  coord_cartesian(ylim = c(0, 40)) +  # avoids clipping values
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("control" = "hotpink2",
                                "high_fertiliser" = "darkgoldenrod2",
                                "low_fertiliser" = "forestgreen",
                                "water_only" = "steelblue1"),
                     labels = c("Control", 
                                "High Fertiliser", 
                                "Low Fertiliser", 
                                "Water Only")) +
  scale_x_discrete(labels = c("initial" = "Initial (April)",
                              "final" = "Final (September)")) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "right")

# Combine plots
combined_plot <- plot_nitrate + plot_ammonium + 
  plot_annotation(
    tag_levels = list(c("A)", "B)")),
    theme = theme(
      plot.tag = element_text(family = "Times", size = 12)
    )
  )

combined_plot


#post hoc analysis
install.packages("emmeans")
library(emmeans)
library(dplyr)
library(ggplot2)
library(lme4)

# Post hoc for ammonium
# Fit linear mixed model
model_volume <- lmer(soil_ammonium ~ sample * plot + (1 | plant_code), data = data1)

#comparing differences between plots
emmeans(model_volume, pairwise ~ plot, adjust = "tukey")

#comparing differences between plots over time
emmeans(model_volume, pairwise ~ plot | sample, adjust = "tukey")

# Post hoc for nitrate
# Fit linear mixed model - nitrate
model_volume <- lmer(soil_nitrate ~ sample * plot + (1 | plant_code), data = data1)

#comparing differences between plots
emmeans(model_volume, pairwise ~ plot, adjust = "tukey")

#comparing differences between plots over time
emmeans(model_volume, pairwise ~ plot | sample, adjust = "tukey")


