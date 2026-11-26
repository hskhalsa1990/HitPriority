library(data.table)
library(dplyr)
library(tidyr)
library(readr)

#' Process CSV matrices
#'
#' This function processes a list of CSV file paths, reading each file,
#' splitting the data based on the presence of rows full of NAs, and then
#' transforming the data from wide to long format. Finally, it binds the
#' channels together and pivots the data wider.
#'
#' @param file_paths A character vector containing paths to the CSV files.
#' @return A list of data frames, where each data frame corresponds to a
#'         processed and combined CSV file.
#'
#' @examples
#' file_paths <- c("path/to/file1.csv", "path/to/file2.csv")
#' result <- process_csv_matrices(file_paths)
process_csv_matrices <- function(file_paths) {{
  # Apply the processing function to each file path
  csv_tables <- lapply(file_paths, function(file_path) {{
    # Read the CSV file
    df <- fread(file = file_path, sep = ",", fill = TRUE)
    
    # Identify rows full of NAs
    n <- rowSums(is.na(df)) == ncol(df)
    
    # Generate a cumulative sum of the NA rows to create groups
    cs <- cumsum(n) + 1
    
    # Split the data frame based on the groups
    s <- split(df[!n, ], cs[!n])
    names(s) <- paste0("Ch", names(s))
    
    # Process each chunk of data
    s <- lapply(s, function(x) {{
      # Determine the number of columns in the data frame
      num_columns <- ncol(x)
      
      # Dynamically create the column names
      column_names <- paste0("V", 1:num_columns)
      
      # Transform the data from wide to long format
      x_long <- x %>%
        mutate(Row = row_number()) %>%
        pivot_longer(cols = all_of(column_names), names_to = "Column") %>%
        mutate(Column = parse_number(Column))
      
      return(x_long)
    }})
    
    # Bind the channels together and pivot wider
    dfs <- bind_rows(s, .id = "Channel") %>%
      pivot_wider(names_from = Channel, values_from = value)
    
    return(dfs)
  }})
  
  return(csv_tables)
}}


process_mapped_input <- function(file_paths) {
  # Apply the processing function to each file path
  csv_tables <- lapply(file_paths, function(file_path) {
    # Read the CSV file
    df <- fread(file = file_path, sep = ",", fill = TRUE)
  })
  csv_tables <- rbindlist(csv_tables)
  return(csv_tables)
}

 