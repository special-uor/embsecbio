#' Add records from tibble or data frame to database
#' @param conn DB connection object.
#' @param table Table name.
#' @param data Tibble object with the records to be inserted in the database.
#' @param dry_run Boolean flag to return the query without running it.
#' @param ... Optional parameters, including a boolean flag to hide status
#'     messages (\code{quiet}).
#'
#' @keywords internal
add_records <- function(conn, table, data, dry_run = FALSE, ...) {
  query <- paste0("INSERT INTO ", table,
                  "(", paste0(colnames(data), collapse = ", "), ") ",
                  "VALUES ")
  # Find data types for each column
  datatypes <- unlist(lapply(data, class))
  idx <- datatypes == "character" # Extract indices of characters
  for (i in seq_len(nrow(data))) {
    values <- data[i, ]
    # Add quotes to character columns
    values[idx] <- lapply(values[idx], dabr::quote)
    values <- ifelse(is.na(values) | values == "'NA'", "NULL", values)
    query <- paste0(query, "(", paste0(values, collapse = ", "), ")")
    if (i < nrow(data))
      query <- paste0(query, ", ")
  }
  if (dry_run)
    return(query)
  tryCatch(dabr::insert(conn, query, ...),
           error = function(e) {
             message("The following query: \n",
                     query,
                     "\n\nFailed with the error: \n",
                     e)
             NULL
           })
}
