#' Plot a Plate map
#'
#' This function generates a visual plot of a plate layout using ggplot2, where different well types are represented by different colors.
#'
#' @param plate_df A data frame containing details of the plate layout with required columns `Row`, `Column`, and `Well Type`.
#'                 - `Row`: Numeric: The row designation of the plate.
#'                 - `Column`: Numeric: The column designation of the plate.
#'                 - `Well Type`: Character indicating the type of well (e.g., "Compound", "Neutral Control","Inhibitor control" etc.).
#' @return A ggplot object representing the plate map with wells colored according to their type.
#' @details 
#' The function performs the following operations:
#' 1. Converts "Sample" entries in `Well Type` to "Compound".
#' 2. Converts "Stimulator Control" entries in `Well Type` to "Inhibitor Control".
#' 3. Plots the plate layout using ggplot2, where rows are reversed in order for visual consistency.
#' 4. Customizes axes and plot themes for a clear presentation.
#' @examples
#' # Assuming plate_df is a data frame with the necessary columns
#' example_plate <- data.frame(Row = rep(LETTERS[1:8], each = 12), 
#'                             Column = rep(1:12, times = 8),
#'                             `Well Type` = sample(c("Sample", "Control", "Stimulator Control"), 96, replace = TRUE))
#' plot_plate(example_plate)
#' 
#' @export
#' @export
#' 

ctrl_cols <- c("Neutral Control"="#E69F00",
"Inhibitor Control"="#8DA0CB",
"Compound"="#66C2A5")

plot_plate <- function(plate_df) {
  # Transform well types
  plate_df <- mutate(plate_df, `Well Type` = ifelse(`Well Type` == "Sample", "Compound", `Well Type`))
  plate_df <- mutate(plate_df, `Well Type` = ifelse(`Well Type` == "Stimulator Control", "Inhibitor Control", `Well Type`))
  
  print("Plotting plate")

 
  
  # Determine maximum number of columns
  max_cols <- max(plate_df$Column)
  
  # Create the plate plot
  plate_plot <- ggplot(plate_df, aes(x = Column, y = factor(Row, levels = rev(levels(factor(Row)))))) +
    geom_tile(aes(fill = `Well Type`), colour = "grey42") +
    labs(y = "Row", x = "Column") +
    scale_x_continuous(breaks = 1:max_cols, expand = c(0, 0)) +  # Removes space on x axis
    scale_fill_manual(values = ctrl_cols) +
    theme_classic() +
    theme(
      axis.ticks.length = unit(0, "points"),  # Aligns ticks with the plot
      axis.line.y = element_line(size = 0.5, colour = "black"),
      axis.text.y = element_text(hjust = 1),  # Aligns text closer to axis
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Removes plot margin
      panel.spacing = unit(0, "lines"),  # Ensures no extra space
      axis.title.y = element_text(vjust = 0.5)  # Vertically center y-axis title
    )
  
  return(plate_plot)
}
