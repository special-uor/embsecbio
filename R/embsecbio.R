#' EMBSeCBIO Database structure
#'
#' @return EMBSeCBIO structure (including tables and attributes).
#' @keywords internal
embsecbio <- function() {
  tables <- c("age_model",
              "basin_size",
              "catch_size",
              "date_info",
              "date_type",
              "entity",
              "entity_pub",
              "entity_type",
              "mat_dated",
              "pollen_data",
              "pub",
              "sample",
              "sample_type",
              "site",
              "site_type")

  age_model <- tibble::tribble(
    ~"names", ~"types",
    "ID_SAMPLE", "integer",
    "est_age_provided", "integer",
    "est_age_original", "integer",
    "est_age_bacon_intcal13", "integer",
    "est_age_bacon_intcal13_max", "integer",
    "est_age_bacon_intcal13_min", "integer"
  )

  basin_size <- tibble::tribble(
    ~"names", ~"types",
    "ID_BASIN_SIZE", "integer",
    "basin_desc", "character"
  )

  catch_size <- tibble::tribble(
    ~"names", ~"types",
    "ID_CATCH_SIZE", "integer",
    "catch_size", "character"
  )

  date_info <- tibble::tribble(
    ~"names", ~"types",
    "ID_DATE_INFO", "integer",
    "ID_ENTITY", "integer",
    "avg_depth", "double",
    "thickness", "double",
    "lab_number", "character",
    "ID_MAT_DATED", "character",
    "dated_age", "double",
    "error_positive", "double",
    "error_negative", "double",
    "ID_DATE_TYPE", "character",
    "comments", "character"
  )

  date_type <- tibble::tribble(
    ~"names", ~"types",
    "ID_DATE_TYPE", "character",
    "date_type", "character"
  )

  entity <- tibble::tribble(
    ~"names", ~"types",
    "ID_SITE", "integer",
    "ID_ENTITY", "integer",
    "entity_name", "character",
    "latitude", "double",
    "longitude", "double",
    "elevation", "double",
    "ID_ENTITY_TYPE", "character",
    "source", "character"
  )

  entity_pub <- tibble::tribble(
    ~"names", ~"types",
    "ID_ENTITY", "integer",
    "ID_PUB", "integer"
  )

  entity_type <- tibble::tribble(
    ~"names", ~"types",
    "ID_ENTITY_TYPE", "character",
    "entity_type", "character",
    "mod_or_0ka_class", "character",
    "comments", "character"
  )

  mat_dated <- tibble::tribble(
    ~"names", ~"types",
    "ID_MAT_DATED", "character",
    "ID_MAT_DATED_HIGH", "integer",
    "mat_dated", "character"
  )

  pollen_data <- tibble::tribble(
    ~"names", ~"types",
    "ID_SAMPLE_TAX", "integer",
    "ID_SAMPLE", "integer",
    "taxon_clean", "character",
    "taxon_count", "integer",
  )

  pub <- tibble::tribble(
    ~"names", ~"types",
    "ID_PUB", "integer",
    "citation", "character"
  )

  sample <- tibble::tribble(
    ~"names", ~"types",
    "ID_ENTITY", "integer",
    "ID_SAMPLE", "integer",
    "sample_name", "character",
    "avg_depth", "double",
    "ID_SAMPLE_TYPE", "character",
    "count_type", "character",
  )

  sample_type <- tibble::tribble(
    ~"names", ~"types",
    "ID_SAMPLE_TYPE", "character",
    "sample_type", "character"
  )

  site <- tibble::tribble(
    ~"names", ~"types",
    "ID_SITE", "integer",
    "site_name", "character",
    "latitude", "double",
    "longitude", "double",
    "elevation", "double",
    "ID_SITE_TYPE", "character",
    "ID_BASIN_SIZE", "character",
    "ID_CATCH_SIZE", "character"
  )

  list(tables = tables,
       age_model = age_model,
       basin_size = basin_size,
       catch_size = catch_size,
       date_info = date_info,
       entity = entity,
       entity_pub = entity_pub,
       mat_dated = mat_dated,
       pollen_data = pollen_data,
       pub = pub,
       sample = sample,
       sample_type = sample_type,
       site = site)
}

#' Read workbook
#'
#' Read workbook. This is expected to have at least three sheets with the
#' following names:
#' \describe{
#'    \item{\code{Site Metadata}}{}
#'    \item{\code{Entity metadata}}{}
#'    \item{\code{SampleCharcoalChronology}}{}
#'    \item{\code{Date Info}}{}
#' }
#'
#' @param workbook Workbook filename.
#' @param wdir Path to the workbook.
#' @param sheets Sheet names, by default it takes the names listed in the
#'     description.
#' @param skip Number of lines to skip.
#' @inheritDotParams readxl::read_excel -col_types -path -sheet -skip
#'
#' @return List with tibbles for each sheet in the workbook.
#' @keywords internal
read_workbook <- function(workbook,
                          wdir = NA,
                          sheets = list_sheets(),
                          skip = 1,
                          ...) {
  # Split wdir and workbook name
  if (is.na(wdir)) {
    wdir <- dirname(workbook)
    workbook <- basename(workbook)
  }

  workbook <- file.path(wdir, workbook)
  if (!file.exists(workbook))
    stop("The specified workbook does not exist: \n",
         workbook)
  out <- vector("list", length(sheets))
  # Extract the names of the sheets in the workbook
  workbook_sheets <- readxl::excel_sheets(workbook)
  # Find matching sheets
  idx <- as.numeric(purrr::map(sheets, agrep, x =  workbook_sheets))
  if (sum(!is.na(idx)) != length(sheets)) {
    stop("The given workbook, `", basename(workbook), "`, it is missing the ",
         "following sheet(s): \n",
         paste0("- ", sheets[is.na(idx)], collapse = "\n"),
         call. = TRUE)
  }
  for (i in seq_along(sheets)) {
    col_names <- colnames(readxl::read_excel(workbook, idx[i], n_max = 0))
    skip_tmp <- skip
    while (skip_tmp >= 0) {
      suppressMessages(
        tmp <- readxl::read_excel(workbook,
                                  sheet = idx[i],
                                  skip = skip_tmp,
                                  col_types = "text",
                                  ...)
      )

      tmp <- tmp[rowSums(is.na(tmp)) != ncol(tmp), ] # Find rows with all NAs
      # Change case of column names and match them the ones in the RPD
      colnames(tmp) <- update_names(tolower(col_names), tolower(sheets[i]))
      tmp <- tmp[, !is.na(colnames(tmp))] # Remove columns without names
      if (nrow(tmp) > 0)
        break
      skip_tmp <- skip_tmp - 1
    }
    if(nrow(tmp) == 0)
      stop("The sheet `", workbook_sheets[idx[i]], "` is empty.", call. = FALSE)
    # Detect column types, based on all rows:
    suppressMessages(
      tmp <- tmp %>%
        readr::type_convert()
    )
    # Update tibble's sub-class
    class(tmp) <- c(map_string(sheets[i]), class(tmp))
    out[[i]] <- tmp
  }
  names(out) <- purrr::map_chr(sheets, map_string)
  return(out)
}
