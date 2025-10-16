# Load libraries
library(tidyverse)
library(ez)
library(ggplot2)
library(readxl)

data <- Exp_2_flowering_data
colnames(data) <- c("PlantCode", "Month", "Plot", "Flowering", "Seeding")
data$PlantCode <- factor(data$PlantCode)
data$Plot <- factor(data$Plot)
data$Month <- factor(data$Month, 
                     levels = c("April","May","June","July","August","September","October","November"),
                     ordered = TRUE)

# Repeated measures ANOVA
anova_results <- ezANOVA(
  data = data,
  dv = .(Seeding),
  wid = .(PlantCode),
  within = .(Month),
  between = .(Plot),
  type = 3,
  detailed = TRUE
)

print(anova_results)

# Summarise data
summary_data <- data %>%
  group_by(Plot, Month) %>%
  summarise(SeedingCount = sum(Seeding, na.rm = TRUE),
            .groups = "drop")

# Plot seeding rates
ggplot(summary_data, aes(x = Month, y = SeedingCount, 
                         group = Plot, color = Plot)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "",
       x = "Month of Observation",
       y = "Number of Seeding Plants (out of 40)",
       color = "Treatment") +  
  scale_color_manual(values = c("control" = "hotpink2",
                                "high_fertiliser" = "darkgoldenrod2",
                                "low_fertiliser" = "forestgreen",
                                "water_only" = "steelblue1"),
                     labels = c("Control", 
                                "High Fertiliser", 
                                "Low Fertiliser", 
                                "Water Only")) +
  expand_limits(y = c(0, 7)) + 
  theme_classic(base_family = "Times New Roman") +
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(),
        panel.grid.minor = element_line()) 

