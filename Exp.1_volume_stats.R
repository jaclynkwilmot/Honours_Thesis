# Load librarys
library(readxl)

data <- Experiment_1_Data
data$flowering <- as.factor(data$flowering)
levels(data$flowering)

# Run t-test 
t_test_result <- t.test(volume ~ flowering, data = data)

# Print results
t_test_result