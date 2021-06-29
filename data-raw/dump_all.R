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


################################################################################
#################### Code to prepare the `age_model` dataset ###################
################################################################################
age_model <- conn %>%
  dabr::select_all("age_model") %>%
  magrittr::set_class(c("age_model", class(.)))
usethis::use_data(age_model, overwrite = TRUE, compress = "xz")

################################################################################
#################### Code to prepare the `date_info` dataset ###################
################################################################################
date_type <- conn %>%
  dabr::select_all("date_type") %>%
  magrittr::set_class(c("date_type", class(.)))

mat_dated <- conn %>%
  dabr::select_all("mat_dated") %>%
  magrittr::set_class(c("mat_dated", class(.)))

date_info <- conn %>%
  dabr::select_all("date_info") %>%
  magrittr::set_class(c("date_info", class(.))) %>%
  dplyr::left_join(date_type, by = "ID_DATE_TYPE") %>%
  dplyr::left_join(mat_dated, by = "ID_MAT_DATED") %>%
  dplyr::select(-ID_DATE_TYPE, -ID_MAT_DATED, -ID_MAT_DATED_HIGH)
usethis::use_data(date_info, overwrite = TRUE, compress = "xz")
# usethis::use_data(date_type, overwrite = TRUE, compress = "xz")
# usethis::use_data(mat_dated, overwrite = TRUE, compress = "xz")

################################################################################
##################### Code to prepare the `entity` dataset #####################
################################################################################
entity_type <- conn %>%
  dabr::select_all("entity_type") %>%
  magrittr::set_class(c("entity_type", class(.)))

entity <- conn %>%
  dabr::select_all("entity") %>%
  magrittr::set_class(c("entity", class(.))) %>%
  dplyr::left_join(entity_type, by = "ID_ENTITY_TYPE") %>%
  dplyr::select(-ID_ENTITY_TYPE)
usethis::use_data(entity, overwrite = TRUE, compress = "xz")
# usethis::use_data(entity_type, overwrite = TRUE, compress = "xz")

entity_pub <- conn %>%
  dabr::select_all("entity_pub") %>%
  magrittr::set_class(c("entity_pub", class(.)))
usethis::use_data(entity_pub, overwrite = TRUE, compress = "xz")

pub <- conn %>%
  dabr::select_all("pub") %>%
  magrittr::set_class(c("pub", class(.)))
usethis::use_data(pub, overwrite = TRUE, compress = "xz")

################################################################################
################### Code to prepare the `pollen_data` dataset ##################
################################################################################
pollen_data <- conn %>%
  dabr::select_all("pollen_data") %>%
  magrittr::set_class(c("pollen_data", class(.)))
usethis::use_data(pollen_data, overwrite = TRUE, compress = "xz")

################################################################################
###################### Code to prepare the `site` dataset ######################
################################################################################
sample_type <- conn %>%
  dabr::select_all("sample_type") %>%
  magrittr::set_class(c("sample_type", class(.)))

sample <- conn %>%
  dabr::select_all("sample") %>%
  magrittr::set_class(c("sample", class(.))) %>%
  dplyr::left_join(sample_type, by = "ID_SAMPLE_TYPE") %>%
  dplyr::select(-ID_SAMPLE_TYPE)
usethis::use_data(sample, overwrite = TRUE, compress = "xz")
# usethis::use_data(sample_type, overwrite = TRUE, compress = "xz")

################################################################################
###################### Code to prepare the `site` dataset ######################
################################################################################
basin_size <- conn %>%
  dabr::select_all("basin_size") %>%
  magrittr::set_class(c("basin_size", class(.)))

catch_size <- conn %>%
  dabr::select_all("catch_size") %>%
  magrittr::set_class(c("catch_size", class(.)))

site_type <- conn %>%
  dabr::select_all("site_type") %>%
  magrittr::set_class(c("site_type", class(.)))

site <- conn %>%
  dabr::select_all("site") %>%
  magrittr::set_class(c("site", class(.))) %>%
  dplyr::left_join(site_type, by = "ID_SITE_TYPE") %>%
  dplyr::left_join(basin_size, by = "ID_BASIN_SIZE") %>%
  dplyr::left_join(catch_size, by = "ID_CATCH_SIZE") %>%
  dplyr::select(-ID_SITE_TYPE, -ID_HIGHER, -ID_BASIN_SIZE, -ID_CATCH_SIZE) %>%
  dplyr::rename(site_type = desc_site_type,
                basin_size = basin_desc)
usethis::use_data(site, overwrite = TRUE, compress = "xz")
# usethis::use_data(basin_size, overwrite = TRUE, compress = "xz")
# usethis::use_data(catch_size, overwrite = TRUE, compress = "xz")
# usethis::use_data(site_type, overwrite = TRUE, compress = "xz")

# Close database connection
dabr::close_conn(conn)
