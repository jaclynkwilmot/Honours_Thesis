# Load libraries
library(tidyverse)
library(ez)
library(patchwork)

data1 <- Exp_2_chlorophyll_conductance_data

data1$plant_code <- factor(data1$plant_code)
data1$sample <- factor(data1$sample, levels = c("initial", "mid", "final"))
data1$plot <- factor(data1$plot)

# Repeated measures ANOVA
anova_conductance <- ezANOVA(
  data = data1,
  dv = conductance_average,
  wid = plant_code,
  within = .(sample),
  between = .(plot),
  detailed = TRUE
)
print(anova_conductance)

# Summarise data
data_summary <- data1 %>%
  group_by(sample, plot) %>%
  summarise(mean_conductance = mean(conductance_average, na.rm = TRUE),
            sd = sd(conductance_average, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            .groups = "drop")

# Plot conductance
p1 <- ggplot(data_summary, aes(x = sample, y = mean_conductance,
                               color = plot, group = plot)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_conductance - se,
                    ymax = mean_conductance + se),
                width = 0.0, size = 0.0) +
  labs(x = "Period of Observation",
       y = expression("Average Stomatal Conductance (mmol" ~ m^-2 ~ s^-1*")"),
       color = "Treatment") +
  coord_cartesian(ylim = c(40, 100)) +  # avoids clipping values
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
                              "mid" = "Middle (July)",
                              "final" = "Final (September)")) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "right")

p1

#post hoc analysis
install.packages("emmeans")
library(emmeans)

# Fit linear mixed model
model_volume <- lmer(conductance_average ~ sample * plot + (1 | plant_code), data = data1)

#comparing differences between plots
emmeans(model_volume, pairwise ~ plot, adjust = "tukey")

#comparing differences between plots over time
emmeans(model_volume, pairwise ~ plot | sample, adjust = "tukey")


library(dplyr)
library(ggplot2)
library(emmeans)

raw_means <- data1 %>%
  group_by(sample, plot) %>%
  summarise(mean_conductance = mean(conductance_average),
            sd = sd(conductance_average),
            n = n())

emm <- emmeans(model_volume, ~ plot | sample)

ggplot() +
  geom_line(data = raw_means, aes(x = sample, y = mean_conductance, color = plot, group = plot), linetype = "dashed") +
  geom_point(data = raw_means, aes(x = sample, y = mean_conductance, color = plot)) +
  geom_line(data = as.data.frame(emm), aes(x = sample, y = emmean, color = plot, group = plot), size = 1) +
  geom_point(data = as.data.frame(emm), aes(x = sample, y = emmean, color = plot), shape = 16)

