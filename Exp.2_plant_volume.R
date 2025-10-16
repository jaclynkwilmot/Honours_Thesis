# Load libraries
library(ez)
library(ggplot2)
library(dplyr)
library(lme4)

data1 <- Exp_2_initial_final_data

data1$plant_code <- factor(data1$plant_code)
data1$sample <- factor(data1$sample, levels = c("initial", "mid", "final"))
data1$plot <- factor(data1$plot)

# ANOVA
anova_volume <- ezANOVA(
  data = data1,
  dv = plant_volume,
  wid = plant_code,
  within = .(sample),
  between = .(plot),
  detailed = TRUE
)
print(anova_volume)

data_summary <- data1 %>%
  group_by(sample, plot) %>%
  summarise(mean_volume = mean(plant_volume, na.rm = TRUE),
            sd = sd(plant_volume, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            .groups = "drop")

# Plot volume
p1 <- ggplot(data_summary, aes(x = sample, y = mean_volume,
                               color = plot, group = plot)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_volume - se,
                    ymax = mean_volume + se),
                width = 0.0, size = 0.0) +
  labs(x = "Period of Observation",
       y = expression("Plant Volume (m"^3*")"),
       color = "Treatment") +
  coord_cartesian(ylim = c(0, 1)) +  # avoids clipping values
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
model_volume <- lmer(plant_volume ~ sample * plot + (1 | plant_code), data = data1)

#comparing differences between plots
emmeans(model_volume, pairwise ~ plot, adjust = "tukey")

#comparing differences between plots over time
emmeans(model_volume, pairwise ~ plot | sample, adjust = "tukey")

