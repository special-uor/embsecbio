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
