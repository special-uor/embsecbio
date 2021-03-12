#' Commit contents of workbook
#'
#' Commit contents of workbook to database, \code{conn}.
#'
#' @importFrom magrittr %>%
#' @param quiet Boolean flag to hide status messages.
#' @param allow_duplicates Boolean flag to indicate if duplicate records should
#'     be allowed.
#' @inheritParams read_workbook
#' @inheritParams add_records
#' @inheritParams inspect
#' @inheritDotParams read_workbook
#' @inheritDotParams readxl::read_excel -path -sheet -skip
#' @return Tibble with report of inserted records.
#'
#' @export
commit <- function(conn,
                   workbook,
                   wdir = NA,
                   quiet = TRUE,
                   coerce = FALSE,
                   default = -999999,
                   allow_duplicates = FALSE,
                   ...) {
  if (!dabr::is.connected(conn))
    stop("Invalid DB connection object!")
  # Local bindings
  depth_bottom <- depth_top <- entity_name <- thickness <- NULL
  # Split wdir and workbook name
  if (is.na(wdir)) {
    wdir <- dirname(workbook)
    workbook <- basename(workbook)
  }

  msg("Loading workbook", quiet = quiet)

  # Read workbook
  input <- read_workbook(workbook, wdir)

  # Status flags:
  new_charcoal <- new_chronology <-  new_dates <- new_entities <- 0
  new_model_names <- new_samples <- new_sites <- new_pubs <- new_units <- 0
  total_model_names <- total_pubs <- total_units <- 0

  # Extract tables from workbook
  site_tb <- input$site %>%
    inspect(tb = "site", coerce = coerce, default = default) %>%
    dplyr::select(-dplyr::starts_with("notes")) # Drop the notes column
  entity_tb <- input$entity %>%
    dplyr::select(-dplyr::starts_with("chron_source")) %>% # Ignore old column, `chron_source`
    inspect(tb = "entity", coerce = coerce, default = default) %>%
    dplyr::select(-dplyr::starts_with("notes")) # Drop the notes column
  # Update values in entity_type
  entity_tb$TYPE <- purrr::map_chr(entity_tb$TYPE, entity_type)
  date_info_tb <- input$date_info %>%
    inspect(tb = "date_info", coerce = coerce, default = default) %>%
    dplyr::select(-dplyr::starts_with("notes"))%>%
    dplyr::select(-dplyr::starts_with("comments"))
  # Convert `age_used` to all lower case
  date_info_tb$age_used <- tolower(date_info_tb$age_used)

  ## Split samplecharcoalchronology sheet into three tables
  if(ncol(input$samplecharcoalchronology) == 8) {
    # Workbooks with both depth_bottom and depth_top
    tmp <- split_sheet(input$samplecharcoalchronology,
                       list(2:4, 5:6, 7:8),
                       c("sample", "charcoal", "chronology"))
  } else if(ncol(input$samplecharcoalchronology) == 7) {
    # Workbooks without depth_bottom and depth_top
    tmp <- split_sheet(input$samplecharcoalchronology,
                       list(2:3, 4:5, 6:7),
                       c("sample", "charcoal", "chronology"))

  } else {
    stop("The SampleCharcoalChronology sheet is expected to have 7 or 8 columns ",
         "not ", ncol(input$samplecharcoalchronology), ".", call. = FALSE)
  }

  sample_tb <- tmp$sample %>%
    inspect(tb = "sample", coerce = coerce, default = default)
  # Check if there's a variable called thickness and rename to sample_thickness
  if (("thickness" %in% names(sample_tb))) {
    sample_tb <- sample_tb %>%
      dplyr::mutate(sample_thickness = thickness)
  }
  # Calculate sample_thickness from depth_bottom and depth_top
  if (!("sample_thickness" %in% names(sample_tb))) {
    sample_tb <- sample_tb %>%
      dplyr::mutate(sample_thickness = depth_bottom - depth_top)
  }
  # Drop old columns: depth_top and depth_bottom
  sample_tb <- sample_tb %>%
    dplyr::select(-dplyr::starts_with("depth_"))

  charcoal_tb <- tmp$charcoal %>%
    inspect(tb = "charcoal", coerce = coerce, default = default)
  # Merge sample and charcoal
  sample_tb <- sample_tb %>%
    dplyr::bind_cols(charcoal_tb %>%
                       dplyr::select(-entity_name))
  chronology_tb <- tmp$chronology %>%
    inspect(tb = "chronology", coerce = coerce, default = default) # %>%
  # dplyr::filter(!is_missing(original_age_model),
  #               !is_missing(original_est_age)) # Filter rows with all missing

  # Run checks on enumerate fields
  msg("Checking enumerates", quiet = quiet)
  msg("Site table", quiet = quiet)
  site_names <- cln_str(unique(site_tb$site_name))
  checks <- c(check_enum(site_tb$basin_size_class, # Site table
                         basin_size_class,
                         "basin_size_class",
                         quiet),
              check_enum(site_tb$catch_size_class,
                         catch_size_class,
                         "catch_size_class",
                         quiet),
              check_enum(site_tb$flow_type,
                         flow_type,
                         "flow_type",
                         quiet),
              check_enum(site_tb$site_type,
                         site_type,
                         "site_type",
                         quiet))
  msg("Entity table", quiet = quiet)
  entity_names <- cln_str(unique(entity_tb$entity_name))
  checks <- c(checks,
              check_enum(cln_str(entity_tb$site_name), # Entity table
                         function(x) x %in% site_names,
                         "site_name",
                         quiet),
              check_enum(entity_tb$TYPE,
                         TYPE,
                         "TYPE",
                         quiet),
              check_enum(entity_tb$core_location,
                         core_location,
                         "core_location",
                         quiet),
              check_enum(entity_tb$depositional_context,
                         depositional_context,
                         "depositional_context",
                         quiet),
              check_enum(entity_tb$measurement_method,
                         measurement_method,
                         "measurement_method",
                         quiet),
              check_enum(entity_tb$source,
                         source,
                         "source",
                         quiet))
  msg("Date Info table", quiet = quiet)
  checks <- c(checks,
              check_enum(cln_str(date_info_tb$site_name), # Date Info table
                         function(x) x %in% site_names,
                         "site_name",
                         quiet),
              check_enum(cln_str(date_info_tb$entity_name),
                         function(x) x %in% entity_names,
                         "entity_name",
                         quiet),
              check_enum(date_info_tb$age_used,
                         age_used,
                         "age_used",
                         quiet),
              check_enum(date_info_tb$date_type,
                         date_type,
                         "date_type",
                         quiet),
              check_enum(getElement(date_info_tb, "explanation"),
                         explanation,
                         "explanation",
                         quiet),
              check_enum(date_info_tb$material_dated,
                         material_dated,
                         "material_dated",
                         quiet))
  msg("Chronology table", quiet = quiet)
  checks <- c(checks,
              check_enum(getElement(charcoal_tb, "original_age_model"), # Charcoal table
                         original_age_model,
                         "original_age_model",
                         quiet))
  msg("Sample table", quiet = quiet)
  checks <- c(checks,
              check_enum(cln_str(sample_tb$entity_name), # Sample table
                         function(x) x %in% entity_names,
                         "entity_name",
                         quiet))
  if (!allow_duplicates) {
    msg("Checking for duplicated rows", quiet = quiet)
    # Check for duplicates where unique data is expected
    checks <- c(checks,
                purrr::map_lgl(unique(sample_tb$entity_name),
                               ~check(sample_tb[sample_tb$entity_name == ., ])),
                purrr::map_lgl(unique(sample_tb$entity_name),
                               function(e) {
                                 # Check for duplicated charcoal measurements
                                 data <- sample_tb[sample_tb$entity_name == e, ]
                                 idx <-
                                   duplicated(data$charcoal_measurement,
                                              incomparables = c(NA,
                                                                -999999,
                                                                -777777)) &
                                   duplicated(data$avg_depth)
                                 if (any(idx)) {
                                   warning("There are duplicated charcoal ",
                                           "measurements (quantity). ",
                                           "Entity name: ",
                                           unique(data$entity_name), ".\n",
                                           "This should be fixed before adding ",
                                           "the records to the database. ",
                                           "The duplicated ",
                                           ifelse(sum(idx) != 1,
                                                  "entries are in rows:\n",
                                                  "entry is in row: "),
                                           paste0(which(idx), collapse = ", "),
                                           "\n",
                                           call. = FALSE,
                                           immediate. = !quiet)
                                   return(TRUE)
                                 }
                                 return(FALSE)
                               }))
  } else {
    warning("Not checking for duplicated records!",
            call. = FALSE,
            immediate. = TRUE)
  }

  if (sum(checks, na.rm = TRUE) > 0)
    stop("There are issues with some of the columns, check the ",
         "warning messages, `warnings()`, and try again.", call. = FALSE)


  msg("Inserting data", quiet = quiet)
  # Loop through each entry in the site table
  for (i in seq_len(nrow(site_tb))) {
    msg(site_tb$site_name[i], quiet = quiet)
    # Insert site (if new) and retrieve ID_SITE
    entity_tb$ID_SITE <- rpd::insert(site_tb[i, ], conn, quiet = quiet)
    if (!all(is.null(entity_tb$ID_SITE))) new_sites <- new_sites + 1

    # Subset entities linked to the current site
    entity_idx <- cln_str(entity_tb$site_name) == cln_str(site_tb$site_name[i])
    entity_tmp <- entity_tb[entity_idx, ]

    # Extract unique entity(ies) linked to the current site
    entities <- unique(entity_tmp$entity_name)

    # Loop through each entity for the current site
    for (j in seq_along(entities)) {
      msg(entities[j], quiet = quiet)
      tmp <- entity_tmp[cln_str(entity_tmp$entity_name) == cln_str(entities[j]), ]

      # Extract publications/citations from the entity table
      pub_tb <- get_pub(tmp)
      total_pubs <- total_pubs + nrow(pub_tb)
      tmp$citation <- NULL

      # Extract units from the entity table
      unit_tb <- get_unit(tmp)
      total_units <- total_units + nrow(unit_tb)
      tmp$units <- NULL

      # Insert units (if new) and retrieve ID_UNIT
      tmp$ID_UNIT <- purrr::map_dbl(seq_len(nrow(unit_tb)),
                                    ~rpd::insert(data = unit_tb[.x, ],
                                                 conn = conn,
                                                 quiet = quiet)
      )
      if (!all(is.null(tmp$ID_UNIT))) new_units <- new_units + nrow(unit_tb)
      ID_ENTITY <- purrr::map_dbl(seq_len(nrow(tmp)),
                                  ~rpd::insert(data = tmp[.x, ],
                                               conn = conn,
                                               quiet = quiet)
      )
      # All values in ID_ENTITY should be identical
      if (length(unique(ID_ENTITY)) > 1)
        stop("There are duplicated rows, `entity_name`.",
             call. = FALSE)
      if (!all(is.null(ID_ENTITY))) new_entities <- new_entities + length(ID_ENTITY)
      # Extract unique ID
      ID_ENTITY <- unique(ID_ENTITY)

      # Insert publications
      ID_PUB <- purrr::map(seq_len(nrow(pub_tb)),
                           function(p) {
                             ID_PUB <- pub_tb[p, ] %>%
                               tibble::as_tibble() %>%
                               magrittr::set_class(c("pub", class(.))) %>%
                               rpd::insert(conn = conn, quiet = quiet)
                             rpd::insert(get_entity_link_pub(ID_ENTITY,
                                                             ID_PUB),
                                         conn = conn,
                                         quiet = quiet)
                             ID_PUB
                           }) %>%
        purrr::flatten_dbl()

      if (!all(is.null(ID_PUB))) new_pubs <- new_pubs + length(ID_PUB)

      # Subset date_info records linked to the current site and entity
      date_info_idx <-
        cln_str(date_info_tb$site_name) == cln_str(site_tb$site_name[i]) &
        cln_str(date_info_tb$entity_name) == cln_str(entities[j])
      date_info_tmp <- date_info_tb[date_info_idx, ]
      date_info_tmp$ID_ENTITY <- ID_ENTITY
      ID_DATE_INFO <- rpd::insert(date_info_tmp, conn, quiet = quiet)
      if (!all(is.null(ID_DATE_INFO)))
        new_dates <- new_dates + length(ID_DATE_INFO)

      # Subset sample records linked to the current entity
      sample_idx <- cln_str(sample_tb$entity_name) == cln_str(entities[j])
      sample_tmp <- sample_tb[sample_idx, ]
      sample_tmp$ID_ENTITY <- ID_ENTITY

      # Find rows with `not_recorded` in the avg_depth column
      idx <- sample_tmp$avg_depth %in% c("not recorded", "-999999")
      # Replace entries with `not_recorded` to a sequence of integers
      if (sum(idx, na.rm = TRUE) > 0 && allow_duplicates) {
        sample_tmp$avg_depth[idx] <- seq_len(sum(idx, na.rm = TRUE))
      }
      ID_SAMPLE <- rpd::insert(sample_tmp, conn, quiet = quiet)
      if (!all(is.null(ID_SAMPLE))) {
        new_samples <- new_samples + length(ID_SAMPLE)
        new_charcoal <- new_charcoal + length(ID_SAMPLE)
      }
      if (length(unique(ID_SAMPLE)) != length(ID_SAMPLE)) {
        stop("Duplicated samples found for\n - Site: ",
             site_tb$site_name[i], "\n - Entity: ",
             entities[j], ". \nCheck the input for rounding issues.")
        dup_idx <- which(duplicated(ID_SAMPLE))
        for (k in dup_idx)
          log_warnings(sample_tmp[k, ], "dup_samples.csv")
      }

      # Update records to their original, `not_recorded` value
      if (sum(idx, na.rm = TRUE) > 0 && allow_duplicates) {
        dabr::update(conn,
                     "UPDATE sample SET avg_depth = -999999",
                     "WHERE ID_SAMPLE IN (",
                     paste0(ID_SAMPLE[idx], collapse = ", "),
                     ")",
                     quiet = quiet)
      }
    }
  }

  # Report
  report <- tibble::tibble(table = c(#"charcoal",
    "chronology",
    "date_info",
    "entity",
    "sample-charcoal",
    "site",
    "pub",
    "unit"),
    done = c(#new_charcoal,
      new_chronology,
      new_dates,
      new_entities,
      new_samples,
      new_sites,
      new_pubs,
      new_units),
    expected = c(#nrow(charcoal_tb),
      nrow(chronology_tb),
      nrow(date_info_tb),
      nrow(entity_tb),
      nrow(sample_tb),
      nrow(site_tb),
      total_pubs,
      total_units))
  class(report) <- c("commit", class(report))
  report
}

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
    "ID_BASIN_SIZE", "character",
    "basin_desc", "character"
  )

  catch_size <- tibble::tribble(
    ~"names", ~"types",
    "ID_CATCH_SIZE", "character",
    "catch_size", "character"
  )

  date_info <- tibble::tribble(
    ~"names", ~"types",
    "ID_DATE_INFO", "integer",
    "ID_ENTITY", "integer",
    "avg_depth", "double",
    "thickness", "double",
    "lab_num", "character",
    "ID_MAT_DATED", "character",
    "dated_age", "double",
    "error_positive", "double",
    "error_negative", "double",
    "ID_DATE_TYPE", "character",
    "date_comments", "character"
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
    "ID_SAMPLE_TAXA", "integer",
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
                          skip = 0,
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
  idx <- as.numeric(purrr::map(sheets, agrep, x =  workbook_sheets, max.distance = 1))
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
      # Change case of column names and match them the ones in the EMBSeCBIO
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
