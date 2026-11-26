# Define a function to compute the normalised RMSD using the absolute median for normalization
calculate_normalised_rmsd_median <- function(model) {
  # Predicted values
  predicted_values <- model$fitted.values
  
  # Observed values
  observed_values <- model$model[[1]]
  
  # Residuals
  residuals <- model$residuals
  
  # RMSD calculation
  rmsd <- sqrt(mean(residuals^2))
  
  # Normalization using the absolute median of the observed values
  observed_median <- abs(median(observed_values))
  
  # Normalised RMSD
  normalised_rmsd <- rmsd / observed_median
  
  # Return the normalised RMSD
  return(normalised_rmsd)
}

# Example usage
# Assuming 'model' is your fitted linear model obtained via lm()
# model <- lm(response ~ predictor, data = df)
# normalised_rmsd_median <- calculate_normalised_rmsd_median(model)
# print(normalised_rmsd_median)