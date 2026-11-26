

calc_point_line2<-function(p1,p2,px,py){
  p<-c(px,py)
  dist=abs( (p2[1]-p1[1])*(p1[2]-p[2]) - (p1[1]-p[1])*(p2[2]-p1[2]) )/sqrt( (p2[1]-p1[1])^2 + (p2[2]-p1[2])^2)
  return(dist)
}

calc_point_line2_dt <- function(p1, p2, dt, px_col, py_col) {
  # Requires data.table; loaded here so sourcing this file has no side effects
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")
  # Ensure the input is a data.table
  if (!data.table::is.data.table(dt)) {
    dt<-data.table::data.table(dt)
  }
  
  # Check if specified columns exist in the data table
  if (!(px_col %in% names(dt)) || !(py_col %in% names(dt))) {
    stop("Specified columns do not exist in the data.table.")
  }
  
  # Calculate the distance for each point in the data table
  dt[, dist := abs((p2[1] - p1[1]) * (p1[2] - get(py_col)) - (p1[1] - get(px_col)) * (p2[2] - p1[2])) / 
       sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)]
  
  # Return the distances
  return(dt$dist)
}

# # Example usage
# p1 <- c(1, 2)
# p2 <- c(3, 4)
# dt <- data.table(px = c(5, 7), py = c(6, 8))
# 
# distances <- calc_point_line2_dt(p1, p2, dt, "px", "py")
# print(distances)