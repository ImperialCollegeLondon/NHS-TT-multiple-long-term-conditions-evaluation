#' Save analysis outputs to CSV files
#'
#' @description
#' Saves one or more analysis result objects to CSV files using standardised
#' naming conventions.
#'
#' @param ... One or more analysis output objects.
#'
#' @return
#' Invisibly returns NULL. CSV files are written to disk.
#'
#' @export


get_save_lists_to_csv <- function(..., output_dir = "outputs") {
  # Capture lists passed as arguments
  lists <- list(...)
  list_names <- as.list(substitute(list(...)))[-1L]
  list_names <- sapply(list_names, deparse)
  
  # Create the main output folder if not exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Loop through each list
  for (i in seq_along(lists)) {
    list_obj <- lists[[i]]
    list_name <- list_names[i]
    
    # Create subfolder for this list
    subdir <- file.path(output_dir, list_name)
    if (!dir.exists(subdir)) {
      dir.create(subdir)
    }
    
    # Save each element as CSV
    for (j in seq_along(list_obj)) {
      elem_name <- names(list_obj)[j]
      if (is.null(elem_name) || elem_name == "") {
        elem_name <- paste0("file_", j)
      }
      write.csv(list_obj[[j]], 
                file = file.path(subdir, paste0(elem_name, ".csv")),
                row.names = FALSE)
    }
  }
}
