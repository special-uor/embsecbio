#' Match input data types with the expected ones
#'
#' @param x Tibble with the input data.
#' @param tb String with the table name.
#' @param coerce Boolean flag to indicate if columns should be coerced to the
#'     right type.
#' @param default Numeric value to be used when coercing non-numeric values.
#'
#' @return Invisibly return \code{x}
#' @keywords internal
inspect <- function(x, tb = class(x)[1], coerce = FALSE, default = -999999) {
  # Update types of missing data
  x <- purrr::modify(x, replace_missing)

  # Get types of each column
  ctypes <- purrr::map_chr(x, typeof)

  # Obtain metadata
  meta <- rpd()[[tb]]
  if (is.null(meta))
    stop("Invalid table name, ", tb, "!", call. = FALSE)
  # Ignore columns with ID_* prefix
  meta <- meta[!grepl("ID_", meta$names), ]
  # Ignore columns with type == "character"
  meta <- meta[meta$types != "character", ]
  # Filter columns that are in both the input and DB
  meta <- meta[meta$names %in% colnames(x), ]
  # Check the input columns match the expected types
  idx <- ctypes[meta$names] != meta$types

  # Display columns with wrong data types, if any exists
  res <- sum(idx, na.rm = TRUE)
  if (res > 0) {
    if (coerce) {
      suppressWarnings({
        x[, meta$names[idx]] <-
          purrr::map2_df(x[, meta$names[idx]], meta$types[idx], methods::as) %>%
          purrr::map2_df(x[, meta$names[idx]],
                         function(x, y) ifelse(is.na(x) & !is.na(y),
                                               default, x)
          )
      })

      warning("The following column",
              ifelse(res > 1, "s", ""),
              " in the table `",
              tb,
              "`, ",
              ifelse(res > 1, "have", "has"),
              " been coerced to the expected data type: \n",
              paste0("- ", meta$names[idx], ": ", ctypes[meta$names][idx],
                     " ---> ", meta$types[idx], collapse = "\n"),
              "\n",
              call. = FALSE)
    } else {
      stop("The following column",
           ifelse(res > 1, "s", ""),
           " in the table `",
           tb,
           "`, ",
           ifelse(res > 1, "have", "has"),
           " the wrong data type: \n",
           paste0("- ", meta$names[idx], ": ", ctypes[meta$names][idx], " => ",
                  meta$types[idx],
                  collapse = "\n"),
           "\n",
           call. = FALSE)
    }
  }
  invisible(x)
}

#' List workbook sheets
#'
#' @return Vector with sheet names.
#' @keywords internal
list_sheets <- function() {
  c("Site metadata",
    "Entity metadata",
    "Sample metadata",
    "Pollen data",
    "Date info",
    "Age model")
}

#' Map string to reference list
#'
#' @param str String to match.
#' @param ref Vector with reference strings.
#'
#' @return Matching string (if any).
#' @keywords internal
map_string <- function(str, ref = embsecbio()$tables) {
  str <- gsub(" ", "_",
              trimws(gsub("metadata",
                          "",
                          tolower(str))))
  idx <- pmatch(str, ref)
  if (is.na(idx))
    return(str)
  return(ref[idx])
}

#' Update column names
#'
#' Update column names from workbook sheets to match the names in the tables of
#' the database.
#'
#' @param vars Vector of strings with the original names.
#' @param sheet String with workbook sheet name.
#'
#' @return Updated names.
#' @keywords internal
update_names <- function(vars, sheet = NA) {
  purrr::map_chr(vars, function(x) {
    if (x == "basin_size") { # site table
      "ID_BASIN_SIZE"
    } else if (x == "catch_size" |
               x == "catchment_size") { # site table
      "ID_CATCH_SIZE"
    } else if (x == "sample_depth" |
               x == "sample_depth (m)" |
               x == "sample_depth_midpoint (m)") { # sample table
      "avg_depth"
    } else if (x == "sample_thickness (m)" |
               x == "sample thickness" |
               (x == "thickness" & !is.na(sheet) & grepl("sample", sheet))) {
      "sample_thickness"
    } else if (x == "sample_type") {
      "ID_SAMPLE_TYPE"
    } else if (x == "publication(s)") { # entity table
      "citation"
    } else if (x == "entity_type") { # entity table
      "ID_ENTITY_TYPE"
    } else if (x == "material_dated") { # date_info table
      "ID_MAT_DATED"
    } else if (x == "date_type") { # date_info table
      "ID_DATE_TYPE"
    } else if (x == "average_depth") { # date_info table
      "avg_depth"
    } else if (x == "lab_num") { # date_info table
      "lab_number"
    } else {
      x
    }
  })
}
