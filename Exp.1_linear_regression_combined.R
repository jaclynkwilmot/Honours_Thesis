#load librarys
library(ggplot2)
library(patchwork)

# Fit linear regression model
model1 <- lm(average_chlorophyll ~ plant_nitrate, data = linear_regression_data)
model2 <- lm(average_chlorophyll ~ soil_nitrate, data = linear_regression_data)

# Extract R2 and p-values
r2_1 <- summary(model1)$r.squared
pval_1 <- summary(model1)$coefficients[2,4]

r2_2 <- summary(model2)$r.squared
pval_2 <- summary(model2)$coefficients[2,4]

# Plot plant nitrogen and chlorophyll
plot_a <- ggplot(linear_regression_data, aes(x = plant_nitrate, y = average_chlorophyll)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "lightblue") +
  annotate("text", 
           x = max(linear_regression_data$plant_nitrate, na.rm = TRUE), 
           y = max(linear_regression_data$average_chlorophyll, na.rm = TRUE), 
           label = paste0("R² = ", round(r2_1, 3), 
                          ", p = ", signif(pval_1, 3)),
           hjust = 1, vjust = 1, 
           family = "Times New Roman", size = 5) +
  annotate("text", x = -Inf, y = Inf, label = "A)", 
           hjust = -0.3, vjust = 1.5, size = 6, fontface = "bold",
           family = "Times New Roman") +
  labs(
    x = "Plant Nitrate (mg/kg)",
    y = "Average Leaf Chlorophyll (µmol/m²)"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

# Plot soil nitrate and chlorophyll
plot_b <- ggplot(linear_regression_data, aes(x = soil_nitrate, y = average_chlorophyll)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "lightblue") +
  annotate("text", 
           x = max(linear_regression_data$soil_nitrate, na.rm = TRUE), 
           y = max(linear_regression_data$average_chlorophyll, na.rm = TRUE), 
           label = paste0("R² = ", round(r2_2, 3), 
                          ", p = ", signif(pval_2, 3)),
           hjust = 1, vjust = 1, 
           family = "Times New Roman", size = 5) +
  annotate("text", x = -Inf, y = Inf, label = "B)", 
           hjust = -0.3, vjust = 1.5, size = 6, fontface = "bold",
           family = "Times New Roman") +
  labs(
    x = "Soil Nitrate (mg/kg)",
    y = "Average Leaf Chlorophyll (µmol/m²)"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

# combine plots
combined_plot <- plot_a + plot_b + plot_layout(ncol = 2)

combined_plot