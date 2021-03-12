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

#' Insert records in the DB
#'
#' @importFrom magrittr `%>%`
#' @param data Data to be inserted, must have column names and records across
#' the rows. It can be generated using
#' \code{\link[tibble:tibble]{tibble::tibble}}.
#' @param ... Optional parameters, including connection object (\code{conn})
#' and boolean flag to hide status messages (\code{quiet}).
#'
#' @return ID of the inserted record(s).
#' @rdname insert
#' @export
insert <- function(data, ...) {
  UseMethod("insert", data)
}

#' @inheritParams dabr::select
#' @rdname insert
#' @export
insert.age_model2 <- function(data, conn, ...) {
  quiet <- get_quiet(...)

  if ("site_name" %in% colnames(data))
    data$entity_name <- NULL
  ID_SAMPLE <- c()
  data$sample_size <- with(data,
                           ifelse(is.na(sample_size), "UNKN", sample_size))
  for (i in seq_len(nrow(data))) {
    # This check might be redundant (CHECK)
    if (is.na(data$sample_size[i])) {
      warning("The sample_size for row ", i, " is empty.")
      log_warnings(data[i, ], "charcoal.csv")
      next
    }

    # Get info from EMBSeCBIO
    db <- dabr::select(conn,
                       "SELECT * FROM charcoal WHERE",
                       "ID_SAMPLE = ", data$ID_SAMPLE[i],
                       # "AND quantity", na(data$quantity[i]),
                       # "AND sample_size", na(data$sample_size[i], TRUE),
                       quiet = quiet)
    if (nrow(db) == 1) {
      if (!quiet)
        message(paste0("Existing charcoal: ", i))
    } else if (nrow(db) > 1) {
      stop(paste0("Duplicated charcoal: ", i, "\nIDs: ",
                  paste0(db$ID_SAMPLE, collapse = ", ")))
    } else {
      if (!quiet)
        message(paste0("New charcoal: ", i))
      add_records(conn, "charcoal", data[i, ], ...)
      db <- dabr::select(conn,
                         "SELECT * FROM charcoal WHERE",
                         "ID_SAMPLE = ", data$ID_SAMPLE[i],
                         # "AND quantity", na(data$quantity[i]),
                         # "AND sample_size", na(data$sample_size[i], TRUE),
                         quiet = quiet)
    }
    ID_SAMPLE <- c(ID_SAMPLE, db$ID_SAMPLE)
  }
  ID_SAMPLE
}

#' @inheritParams dabr::select
#' @rdname insert
#' @export
insert.age_model <- function(data, conn, ...) {
  quiet <- get_quiet(...)
  if ("site_name" %in% colnames(data))
    data$site_name <- NULL
  if ("entity_name" %in% colnames(data))
    data$entity_name <- NULL
  ID_SAMPLE <- c()
  for (i in seq_len(nrow(data))) {
    if (is.na(data$est_age_provided[i])) {
      warning("The est_age_provided for row ", i, " is empty.")
      log_warnings(data[i, ], "age_model.csv")
      next
    }

    # Get info from EMBSeCBIO
    db <- dabr::select(conn,
                       "SELECT * FROM age_model WHERE",
                       "ID_SAMPLE = ", data$ID_SAMPLE[i],
                       quiet = quiet)
    if (nrow(db) == 1) {
      if (!quiet)
        message(paste0("Existing age_model: ", i))
    } else if (nrow(db) > 1) {
      stop(paste0("Duplicated age_model: ", i, "\nIDs: ",
                  paste0(db$ID_SAMPLE, collapse = ", ")))
    } else {
      if (!quiet)
        message(paste0("New age_model: ", i))
      add_records(conn, "age_model", data[i, ], ...)
      db <- dabr::select(conn,
                         "SELECT * FROM age_model WHERE",
                         "ID_SAMPLE = ", data$ID_SAMPLE[i],
                         quiet = quiet)
    }
    ID_SAMPLE <- c(ID_SAMPLE, db$ID_SAMPLE)
  }
  ID_SAMPLE
}

#' @inheritParams dabr::select
#' @rdname insert
#' @export
insert.date_info <- function(data, conn, ...) {
  quiet <- get_quiet(...)

  if ("site_name" %in% colnames(data))
    data$site_name <- NULL
  if ("entity_name" %in% colnames(data))
    data$entity_name <- NULL
  ID_DATE_INFO <- c()
  # Remove double white spaces
  data$date_type <- unlist(lapply(data$date_type,
                                  gsub, pattern = "  ", replacement = " "))
  for (i in seq_len(nrow(data))) {
    # Get info from EMBSeCBIO
    db <- dabr::select(conn,
                       "SELECT * FROM date_info WHERE ID_ENTITY = ", data$ID_ENTITY[i],
                       "AND avg_depth", na(data$avg_depth[i]),
                       "AND thickness", na(data$thickness[i]),
                       "AND lab_num", na(data$lab_num[i], TRUE),
                       "AND ID_MAT_DATED", na(data$ID_MAT_DATED[i]),
                       "AND dated_age", na(data$dated_age[i]),
                       "AND error_positive", na(data$error_positive[i]),
                       "AND error_negative", na(data$error_negative[i]),
                       "AND ID_DATE_TYPE", na(data$ID_DATE_TYPE[i]),
                       quiet = quiet)
    if (nrow(db) == 1) {
      if (!quiet)
        message(paste0("Existing date: ", data$lab_number[i]))
    } else if (nrow(db) > 1) {
      stop(paste0("Duplicated date: ", data$lab_number[i], "\nIDs: ",
                  paste0(db$ID_DATE_INFO, collapse = ", ")))
    } else {
      if (!quiet)
        message(paste0("New date: ", data$lab_number[i]))
      add_records(conn, "date_info", data[i, ], ...)
      db <- dabr::select(conn,
                         "SELECT * FROM date_info WHERE ID_ENTITY = ", data$ID_ENTITY[i],
                         "AND avg_depth", na(data$avg_depth[i]),
                         "AND thickness", na(data$thickness[i]),
                         "AND lab_num", na(data$lab_num[i], TRUE),
                         "AND ID_MAT_DATED", na(data$ID_MAT_DATED[i]),
                         "AND dated_age", na(data$dated_age[i]),
                         "AND error_positive", na(data$error_positive[i]),
                         "AND error_negative", na(data$error_negative[i]),
                         "AND ID_DATE_TYPE", na(data$ID_DATE_TYPE[i]),
                         quiet = quiet)
    }
    ID_DATE_INFO <- c(ID_DATE_INFO, db$ID_DATE_INFO)
  }
  ID_DATE_INFO
}

#' @inheritParams dabr::select
#' @rdname insert
#' @export
insert.entity <- function(data, conn, ...) {
  quiet <- get_quiet(...)

  if ("site_name" %in% colnames(data)) {
    db <- dabr::select(conn,
                       "SELECT * FROM site WHERE site_name = ",
                       dabr::quote(data$site_name),
                       quiet = quiet)
    if (nrow(db) == 1) {
      if (!("ID_SITE" %in% colnames(data)))
        data$ID_SITE <- db$ID_SITE
      if (db$ID_SITE != data$ID_SITE)
        stop("There is a mistmatch between ID_SITE and site_name!")
    } else {
      stop("There is a problem with the site_name, double check!")
    }
    data$site_name <- NULL
  }
  # Get info from EMBSeCBIO
  db <- dabr::select(conn,
                     "SELECT * FROM entity WHERE entity_name = ",
                     dabr::quote(data$entity_name), "AND ID_SITE = ",
                     data$ID_SITE,
                     quiet = quiet)
  if (nrow(db) == 1) {
    if (!quiet)
      message(paste0("Existing entity: ", data$entity_name))
  } else if (nrow(db) > 1) {
    stop(paste0("Duplicated entity: ", data$entity_name, "\nIDs: ",
                paste0(db$ID_ENTITY, collapse = ", ")))
  } else {
    if (!quiet)
      message(paste0("New entity: ", data$entity_name))
    add_records(conn, "entity", data, ...)
    db <- dabr::select(conn,
                       "SELECT * FROM entity WHERE entity_name = ",
                       dabr::quote(data$entity_name), "AND ID_SITE = ",
                       data$ID_SITE,
                       quiet = quiet)
  }
  db$ID_ENTITY
}

#' @inheritParams dabr::select
#' @rdname insert
#' @export
insert.entity_pub <- function(data, conn, ...) {
  quiet <- get_quiet(...)

  # Get info from EMBSeCBIO
  db <- dabr::select(conn,
                     "SELECT * FROM entity_pub WHERE ID_ENTITY =",
                     data$ID_ENTITY, "AND ID_PUB =",
                     data$ID_PUB,
                     quiet = quiet)
  if (nrow(db) == 0) {
    add_records(conn, "entity_pub", data, ...)
    db <- dabr::select(conn,
                       "SELECT * FROM entity_pub WHERE ID_ENTITY =",
                       data$ID_ENTITY, "AND ID_PUB =",
                       data$ID_PUB,
                       quiet = quiet)
  }
}
#'
#' #' @inheritParams dabr::select
#' #' @rdname insert
#' #' @export
#' insert.model_name <- function(data, conn, ...) {
#'   quiet <- get_quiet(...)
#'
#'   # Get info from the EMBSeCBIO
#'   db <- dabr::select(conn,
#'                      "SELECT * FROM model_name WHERE model_name = ",
#'                      dabr::quote(data$model_name),
#'                      quiet = quiet)
#'   if (nrow(db) == 0) {
#'     add_records(conn, "model_name", data, ...)
#'     db <- dabr::select(conn,
#'                        "SELECT * FROM model_name WHERE model_name = ",
#'                        dabr::quote(data$model_name),
#'                        quiet = quiet)
#'   }
#'   db$ID_MODEL
#' }

#' @inheritParams dabr::select
#' @rdname insert
#' @export
insert.pollen_data <- function(data, conn, ...) {
  quiet <- get_quiet(...)

  data <- data %>%
    dplyr::transmute(ID_SAMPLE = ID_SAMPLE,
                     taxon_clean = taxon_clean,
                     taxon_count = taxon_count)
  ID_SAMPLE_TAXA <- c()
  for (i in seq_len(nrow(data))) {
    # Get info from EMBSeCBIO
    db <- dabr::select(conn,
                       "SELECT * FROM pollen_data WHERE ID_SAMPLE = ",
                       data$ID_SAMPLE[i],
                       "AND taxon_clean", na(data$taxon_clean[i], TRUE),
                       quiet = quiet)
    if (nrow(db) == 1) {
      if (!quiet)
        message(paste0("Existing pollen sample: ", data$taxon_clean[i]))
    } else if (nrow(db) > 1) {
      stop(paste0("Duplicated pollen sample: ", data$taxon_clean[i], "\nIDs: ",
                  paste0(db$ID_SAMPLE_TAXA, collapse = ", ")))
    } else {
      if (!quiet)
        message(paste0("New pollen sample: ", data$taxon_clean[i]))
      add_records(conn, "pollen_data", data[i, ], ...)
      db <- dabr::select(conn,
                         "SELECT * FROM pollen_data WHERE ID_SAMPLE = ",
                         data$ID_SAMPLE[i],
                         "AND taxon_clean", na(data$taxon_clean[i], TRUE),
                         quiet = quiet)
    }
    ID_SAMPLE_TAXA <- c(ID_SAMPLE_TAXA, db$ID_SAMPLE_TAXA)
  }
  ID_SAMPLE_TAXA
}

#' @inheritParams dabr::select
#' @rdname insert
#' @export
insert.pub <- function(data, conn, ...) {
  quiet <- get_quiet(...)

  # Get info from EMBSeCBIO
  db <- dabr::select(conn,
                     "SELECT * FROM pub WHERE citation = ",
                     dabr::quote(data$citation),
                     quiet = quiet)
  if (nrow(db) == 0) {
    add_records(conn, "pub", data, ...)
    db <- dabr::select(conn,
                       "SELECT * FROM pub WHERE citation = ",
                       dabr::quote(data$citation),
                       quiet = quiet)
  }
  db$ID_PUB
}

#' @inheritParams dabr::select
#' @rdname insert
#' @export
insert.sample <- function(data, conn, ...) {
  quiet <- get_quiet(...)

  if ("site_name" %in% colnames(data))
    data$site_name <- NULL
  if ("entity_name" %in% colnames(data))
    data$entity_name <- NULL
  ID_SAMPLE <- c()

  for (i in seq_len(nrow(data))) {
    # Get info from EMBSeCBIO
    db <- dabr::select(conn,
                       "SELECT * FROM sample WHERE",
                       "ID_ENTITY = ", data$ID_ENTITY[i],
                       "AND sample_name", na(data$sample_name[i], TRUE),
                       "AND avg_depth", na(data$avg_depth[i]),
                       "AND ID_SAMPLE_TYPE", na(data$ID_SAMPLE_TYPE[i], TRUE),
                       "AND count_type", na(data$count_type[i], TRUE),
                       quiet = quiet)
    if (nrow(db) == 1) {
      if (!quiet)
        message(paste0("Existing sample: ", i))
    } else if (nrow(db) > 1) {
      stop(paste0("Duplicated sample: ", i, "\nIDs: ",
                  paste0(db$ID_SAMPLE, collapse = ", ")))
    } else {
      if (!quiet)
        message(paste0("New sample: ", i))
      add_records(conn, "sample", data[i, ], ...)
      db <- dabr::select(conn,
                         "SELECT * FROM sample WHERE",
                         "ID_ENTITY = ", data$ID_ENTITY[i],
                         "AND sample_name", na(data$sample_name[i], TRUE),
                         "AND avg_depth", na(data$avg_depth[i]),
                         "AND ID_SAMPLE_TYPE", na(data$ID_SAMPLE_TYPE[i], TRUE),
                         "AND count_type", na(data$count_type[i], TRUE),
                         quiet = quiet)
    }
    ID_SAMPLE <- c(ID_SAMPLE, db$ID_SAMPLE)
  }
  ID_SAMPLE
}

#' @inheritParams dabr::select
#' @rdname insert
#' @export
insert.site <- function(data, conn, ...) {
  quiet <- get_quiet(...)

  # Get info from EMBSeCBIO
  db <- dabr::select(conn,
                     "SELECT * FROM site WHERE site_name = ",
                     dabr::quote(data$site_name),
                     quiet = quiet)
  if (nrow(db) == 1) {
    if (!quiet)
      message(paste0("Existing site: ", data$site_name))
  } else if (nrow(db) > 1) {
    stop(paste0("Duplicated site: ", data$site_name, "\nIDs: ",
                paste0(db$ID_SITE, collapse = ", ")))
  } else {
    if (!quiet)
      message(paste0("New site: ", data$site_name))
    add_records(conn, "site", data, ...)
    db <- dabr::select(conn,
                       "SELECT * FROM site WHERE site_name = ",
                       dabr::quote(data$site_name),
                       quiet = quiet)
  }
  db$ID_SITE
}
