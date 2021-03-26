EC-S: ageR ouputs - EMBSeCBIO
================

## Open connection to RPD:

``` r
# Local server
conn <- dabr::open_conn_mysql("EMBSeCBIO", password = rstudioapi::askForPassword())
# UoR server
conn <- dabr::open_conn_mysql("EMBSeCBIO", port = 3307, user = "roberto", password = rstudioapi::askForPassword())
db <- purrr::map(embsecbio:::embsecbio()$tables, dabr::select_all, conn = conn)
```

Define a helper function to insert/update age model records:

``` r
add_records <- function(conn, table, data, PK, quiet = TRUE) {
  # Create index to process 100 rows at the time:
  idx <- data.frame(start = seq(1, nrow(data), 100),
                    end = seq(1, nrow(data), 100) + 100)
  idx[nrow(idx), 2] <- nrow(data) + 1
  pb <- progress::progress_bar$new(
      format = "(:current/:total) [:bar] :percent",
      total = nrow(idx), clear = TRUE, width = 80)
  for (i in seq_len(nrow(idx))) {
    rpd:::update_records(conn, 
                         table, 
                         data,
                         PK = PK,
                         quiet = quiet)
    pb$tick()
  }
}
```

### Update the `age_model` table

``` sql
ALTER TABLE EMBSEcBIO.age_model
ADD COLUMN `est_age_bacon_intcal20_mean` int DEFAULT NULL,
ADD COLUMN `est_age_bacon_intcal20_median` int DEFAULT NULL,
ADD COLUMN `est_age_bacon_intcal20_uncert_5` int DEFAULT NULL,
ADD COLUMN `est_age_bacon_intcal20_uncert_95` int DEFAULT NULL,
ADD COLUMN `est_age_bacon_intcal20_uncert_25` int DEFAULT NULL,
ADD COLUMN `est_age_bacon_intcal20_uncert_75` int DEFAULT NULL,
MODIFY `est_age_provided` int DEFAULT NULL,
MODIFY `est_age_original` int DEFAULT NULL,
MODIFY `est_age_bacon_intcal13` int DEFAULT NULL,
MODIFY `est_age_bacon_intcal13_max` int DEFAULT NULL,
MODIFY `est_age_bacon_intcal13_min` int DEFAULT NULL,
MODIFY `est_age_bacon_intcal20_mean` int DEFAULT NULL,
MODIFY comment varchar(1000) DEFAULT NULL AFTER est_age_bacon_intcal20_uncert_75;
```

### Read input files

``` r
ecs_path <- "~/Downloads/Bacon chronologies"
ecs_files <- list.files(ecs_path, "\\.csv", full.names = TRUE)
ecs_entities_mistmatch <- NULL
ecs_entities_units <- NULL
for (e in ecs_files) {
  rpd:::msg(basename(e))
  suppressMessages({
    ecs_chrc <- readr::read_csv(e)
  })
  # Select records from the sample-charcoal table
  sample_tb <- dabr::select(conn,
                            "SELECT * FROM sample",
                            "WHERE ID_SAMPLE IN (",
                            paste0(ecs_chrc$sample_ids, collapse = ", "),
                            ")",
                            quiet = TRUE) %>%
    dplyr::arrange(ID_ENTITY)
  if (nrow(sample_tb) == 0) { # Query records using the entity name
    sample_tb <- dabr::select(conn,
                            "SELECT * FROM sample",
                            "LEFT JOIN entity ON entity.ID_ENTITY = sample.ID_ENTITY",
                            "WHERE entity_name = ",
                            dabr::quote(gsub("_bacon_chronology.csv",
                                             "",
                                             basename(e))),
                            quiet = TRUE) %>%
    dplyr::arrange(ID_ENTITY)
  }
  if (nrow(sample_tb) == 0) { # Query records using the entity name
    rpd:::msg("NO data was found for the entity.")
    ecs_entities_mistmatch <- c(ecs_entities_mistmatch, e)
    next()
  }
  # Verify all the samples exist in the RPD
  if (nrow(sample_tb) == nrow(ecs_chrc)) {
    depth_diff <- abs(ecs_chrc$depths / 100 - sample_tb$avg_depth) > 0.001
    if (sum(depth_diff) == nrow(sample_tb)) { # RPD has the wrong units
      depth_diff <- abs(ecs_chrc$depths - sample_tb$avg_depth) > 0.001
      if (sum(depth_diff) != nrow(sample_tb)) {
        rpd:::msg("The records in the RPD have the wrong units, `cm`.")
        ecs_entities_units <- c(ecs_entities_units, e)
      }
    }
    if (sum(depth_diff) > 0) {
      rpd:::msg("Mistmatch between depths detected.")
      ecs_entities_mistmatch <- c(ecs_entities_mistmatch, e)
      next()
    }
    
    # Prepare raw data for insertion
    ecs_chrc <- ecs_chrc %>%
      dplyr::mutate(ID_SAMPLE = sample_tb$ID_SAMPLE, .before = 1) %>%
      dplyr::select(-c(sample_ids, depths)) %>%
      magrittr::set_names(c("ID_SAMPLE", paste0("est_age_bacon_intcal20_",
                                                colnames(.)[-1]))) %>%
      purrr::map_df(as.integer) # Convert columns to integer
    add_records(conn, "age_model", ecs_chrc, PK = 1, quiet = FALSE)
    
    # Check inserted records
    age_model_tb <-
      dabr::select(conn,
                   "SELECT * FROM age_model WHERE ID_SAMPLE IN (",
                   paste0(ecs_chrc$ID_SAMPLE, collapse = ", "),
                   ")",
                   quiet = TRUE) %>%
      tibble::as_tibble() %>%
      magrittr::set_names(tolower(colnames(.)))
    # Change case of columns
    colnames(ecs_chrc) <- tolower(colnames(ecs_chrc))
    # Change column order
    common_cols <- intersect(colnames(age_model_tb), colnames(ecs_chrc))
    print(waldo::compare(ecs_chrc[, common_cols], 
                         age_model_tb[, common_cols]))#, tolerance = 0.01))
  }
}
```