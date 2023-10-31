# Install and load the segmented package if not already installed
# install.packages("segmented")
library(segmented)
# Generate example data with a single breakpoint
set.seed(123)
X <- 1:100
Y <- ifelse(X <= 50, 2 * X + 10 + rnorm(100, sd = 5), -2 * X + 200 + rnorm(100, sd = 10))

# Initialize variables to store the best results
best_psi <- NULL
best_R2 <- -Inf

# Define a range of potential psi values to search within
psi_values <- 10:90  # Adjust the range as needed

# Loop through the potential psi values
for (psi in psi_values) {
  # Fit a segmented regression model with the current psi
  seg_model <- segmented(lm(Y ~ X), seg.Z = ~X, psi = psi)
  
  # Calculate R-squared value for the model
  R2 <- summary(seg_model)$r.squared
  
  # Check if this psi value results in a better R-squared value
  if (R2 > best_R2) {
    best_R2 <- R2
    best_psi <- psi
  }
}

# Fit the final segmented regression model with the best psi
final_seg_model <- segmented(lm(Y ~ X), seg.Z = ~X, psi = best_psi)

# Print the results
cat("Best psi value:", best_psi, "\n")
summary(final_seg_model)

# Get the estimated breakpoint
breakpoint <- final_seg_model$psi
cat("Estimated breakpoint:", breakpoint, "\n")
# Plot the data and the segmented regression model
plot(X, Y, pch = 19, col = "blue", xlab = "X", ylab = "Y")
abline(h = 0, col = "gray")  # Add a horizontal line at y = 0
lines(X, final_seg_model$fit, col = "red", lwd = 2)