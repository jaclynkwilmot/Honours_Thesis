# Load libraries
library(ez)
library(ggplot2)
library(dplyr)

data1 <- Exp_2_initial_final_data
data1$plant_code <- factor(data1$plant_code)
data1$sample <- factor(data1$sample, levels = c("initial", "final"))
data1$plot <- factor(data1$plot)

# Soil moisture ANOVA
anova_soil_moisture <- ezANOVA(
  data = data1,
  dv = soil_moisture,
  wid = plant_code,
  within = .(sample),
  between = .(plot),
  detailed = TRUE
)
print(anova_soil_moisture)

# Reshape to long format
data_long <- data1 %>%
  pivot_longer(cols = c(soil_moisture))

# Plotting soil moisture
soil_moisture_plot <- ggplot(data1, aes(x = sample, y = soil_moisture,
                               color = plot, group = plot)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Period of Observation", 
       y = "Soil Moisture (%)", 
       color = "Treatment") +
  ylim(0, 1) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("control" = "hotpink2",
                                "high_fertiliser" = "darkgoldenrod2",
                                "low_fertiliser" = "forestgreen",
                                "water_only" = "steelblue1"),
                     labels = c("Control", 
                                "High Fertiliser", 
                                "Low Fertiliser", 
                                "Water Only")) +
  scale_x_discrete(labels = c("initial" = "Initial",
                              "final" = "Final")) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "right")   # remove legend here

soil_moisture_plot

# Post hoc analysis
# Fit linear mixed model
model_volume <- lmer(soil_moisture ~ sample * plot + (1 | plant_code), data = data1)

#comparing differences between plots
emmeans(model_volume, pairwise ~ plot, adjust = "tukey")

#comparing differences between plots over time
emmeans(model_volume, pairwise ~ plot | sample, adjust = "tukey")
