`%>%` <- magrittr::`%>%`

# Local server
conn <- dabr::open_conn_mysql("EMBSeCBIO", password = rstudioapi::askForPassword())

# UoR server
conn <- dabr::open_conn_mysql("EMBSeCBIO", port = 3307, user = "roberto", password = rstudioapi::askForPassword())

# Generate code to export all the tables
embsecbio:::embsecbio()$tables %>%
  purrr::walk(function(tbl) {
    glue::glue('{tbl} <- conn %>%
  dabr::select_all("{tbl}") %>%
  magrittr::set_class(c("{tbl}", class(.)))
usethis::use_data({tbl}, overwrite = TRUE)\n\n\n') %>%
      cat()
  })

# Generate documentation for all the tables
embsecbio:::embsecbio()$tables %>%
  purrr::walk(function(tbl) {
    db <- conn %>%
      dabr::select_all(tbl, quiet = TRUE)
    columns <- db %>%
      glue::glue_data('#\'   \\item{<colnames(db)>}{PENDING}', sep = "\n", .open = "<", .close = ">") %>%
      stringr::str_c(collapse = "\n")
    glue::glue('#\' \\code{<tbl>}
#\'
#\' A tibble with records for <tbl>.
#\'
#\' @format A data frame with <nrow(db)> rows and <ncol(db)> variables:
#\' \\describe{
<columns>
#\' }
"<tbl>"\n\n\n', .open = "<", .close = ">") %>%
      cat()
  })

age_model <- conn %>%
  dabr::select_all("age_model") %>%
  magrittr::set_class(c("age_model", class(.)))
usethis::use_data(age_model, overwrite = TRUE)

basin_size <- conn %>%
  dabr::select_all("basin_size") %>%
  magrittr::set_class(c("basin_size", class(.)))
usethis::use_data(basin_size, overwrite = TRUE)

catch_size <- conn %>%
  dabr::select_all("catch_size") %>%
  magrittr::set_class(c("catch_size", class(.)))
usethis::use_data(catch_size, overwrite = TRUE)

date_info <- conn %>%
  dabr::select_all("date_info") %>%
  magrittr::set_class(c("date_info", class(.)))
usethis::use_data(date_info, overwrite = TRUE)

date_type <- conn %>%
  dabr::select_all("date_type") %>%
  magrittr::set_class(c("date_type", class(.)))
usethis::use_data(date_type, overwrite = TRUE)

entity <- conn %>%
  dabr::select_all("entity") %>%
  magrittr::set_class(c("entity", class(.)))
usethis::use_data(entity, overwrite = TRUE)

entity_pub <- conn %>%
  dabr::select_all("entity_pub") %>%
  magrittr::set_class(c("entity_pub", class(.)))
usethis::use_data(entity_pub, overwrite = TRUE)

entity_type <- conn %>%
  dabr::select_all("entity_type") %>%
  magrittr::set_class(c("entity_type", class(.)))
usethis::use_data(entity_type, overwrite = TRUE)

mat_dated <- conn %>%
  dabr::select_all("mat_dated") %>%
  magrittr::set_class(c("mat_dated", class(.)))
usethis::use_data(mat_dated, overwrite = TRUE)

pollen_data <- conn %>%
  dabr::select_all("pollen_data") %>%
  magrittr::set_class(c("pollen_data", class(.)))
usethis::use_data(pollen_data, overwrite = TRUE)

pub <- conn %>%
  dabr::select_all("pub") %>%
  magrittr::set_class(c("pub", class(.)))
usethis::use_data(pub, overwrite = TRUE)

sample <- conn %>%
  dabr::select_all("sample") %>%
  magrittr::set_class(c("sample", class(.)))
usethis::use_data(sample, overwrite = TRUE)

sample_type <- conn %>%
  dabr::select_all("sample_type") %>%
  magrittr::set_class(c("sample_type", class(.)))
usethis::use_data(sample_type, overwrite = TRUE)

site <- conn %>%
  dabr::select_all("site") %>%
  magrittr::set_class(c("site", class(.)))
usethis::use_data(site, overwrite = TRUE)

site_type <- conn %>%
  dabr::select_all("site_type") %>%
  magrittr::set_class(c("site_type", class(.)))
usethis::use_data(site_type, overwrite = TRUE)

# Close database connection
dabr::close_conn(conn)
