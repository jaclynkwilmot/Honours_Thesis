# Load libraries
library(ez)
library(ggplot2)
library(dplyr)
install.packages("lme4")
library(lme4)

data <- Exp_2_chlorophyll_conductance_data

data$plant_code <- factor(data$plant_code)
data$sample <- factor(data$sample, levels = c("initial", "mid", "final"))
data$plot <- factor(data$plot, 
                    levels = c("low_fertiliser", "high_fertiliser", "control", "water_only"))

# Repeated measures ANOVA
anova_results <- ezANOVA(
  data = data,
  dv = chlorophyll_average,
  wid = plant_code,
  within = .(sample),
  between = .(plot),
  detailed = TRUE
)
print(anova_results)

library(ggplot2)
library(dplyr)
library(ez)

# Making colour map
colour_map <- c(
  "control"         = "hotpink2",
  "high_fertiliser" = "darkgoldenrod2",
  "low_fertiliser"  = "forestgreen",
  "water_only"      = "steelblue1"
)

# Plot chlorophyll
p <- ggplot(data, aes(x = sample, y = chlorophyll_average, color = plot, group = plot)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  scale_color_manual(
    values = colour_map,
    breaks = c("control", "high_fertiliser", "low_fertiliser", "water_only"),
    labels = c("Control", "High Fertiliser", "Low Fertiliser", "Water Only")
  ) +
  scale_x_discrete(labels = c("initial" = "Initial (April)",
                              "mid"     = "Middle (July)",
                              "final"   = "Final (September)")) +
  labs(
    x = "Period of Observation",
    y = expression("Average Leaf Chlorophyll ("*mu*"mol/m"^2*")"),
    color = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "right"
  )

# Display plot
p

#post hoc analysis
install.packages("emmeans")
library(emmeans)

# Fit linear mixed model
model_volume <- lmer(chlorophyll_average ~ sample * plot + (1 | plant_code), data = data)

#comparing differences between plots
emmeans(model_volume, pairwise ~ plot, adjust = "tukey")

#comparing differences between plots over time
emmeans(model_volume, pairwise ~ plot | sample, adjust = "tukey")
