#' \code{age_model}
#'
#' A tibble with records for age_model.
#'
#' @format A data frame with 10131 rows and 13 variables:
#' \describe{
#'   \item{ID_SAMPLE}{PENDING}
#'   \item{est_age_provided}{PENDING}
#'   \item{est_age_original}{PENDING}
#'   \item{est_age_bacon_intcal13}{PENDING}
#'   \item{est_age_bacon_intcal13_max}{PENDING}
#'   \item{est_age_bacon_intcal13_min}{PENDING}
#'   \item{est_age_bacon_intcal20_mean}{PENDING}
#'   \item{est_age_bacon_intcal20_median}{PENDING}
#'   \item{est_age_bacon_intcal20_uncert_5}{PENDING}
#'   \item{est_age_bacon_intcal20_uncert_95}{PENDING}
#'   \item{est_age_bacon_intcal20_uncert_25}{PENDING}
#'   \item{est_age_bacon_intcal20_uncert_75}{PENDING}
#'   \item{comment}{PENDING}
#' }
"age_model"

#' \code{date_info}
#'
#' A tibble with records for date_info.
#'
#' @format A data frame with 916 rows and 11 variables:
#' \describe{
#'   \item{ID_DATE_INFO}{PENDING}
#'   \item{ID_ENTITY}{PENDING}
#'   \item{avg_depth}{PENDING}
#'   \item{thickness}{PENDING}
#'   \item{lab_num}{PENDING}
#'   \item{mat_dated}{PENDING}
#'   \item{dated_age}{PENDING}
#'   \item{error_positive}{PENDING}
#'   \item{error_negative}{PENDING}
#'   \item{date_type}{PENDING}
#'   \item{date_comments}{PENDING}
#' }
"date_info"

#' \code{entity}
#'
#' A tibble with records for entity.
#'
#' @format A data frame with 1802 rows and 10 variables:
#' \describe{
#'   \item{ID_ENTITY}{PENDING}
#'   \item{ID_SITE}{PENDING}
#'   \item{entity_name}{PENDING}
#'   \item{latitude}{PENDING}
#'   \item{longitude}{PENDING}
#'   \item{elevation}{PENDING}
#'   \item{entity_type}{PENDING}
#'   \item{mod_or_0ka_class}{PENDING}
#'   \item{comments}{PENDING}
#'   \item{source}{PENDING}
#' }
"entity"

#' \code{entity_pub}
#'
#' A tibble with records for entity_pub.
#'
#' @format A data frame with 784 rows and 3 variables:
#' \describe{
#'   \item{ID_ENTITY_PUB}{PENDING}
#'   \item{ID_ENTITY}{PENDING}
#'   \item{ID_PUB}{PENDING}
#' }
"entity_pub"

#' \code{pollen_data}
#'
#' A tibble with records for pollen_data.
#'
#' @format A data frame with 301197 rows and 4 variables:
#' \describe{
#'   \item{ID_SAMPLE_TAXA}{PENDING}
#'   \item{ID_SAMPLE}{PENDING}
#'   \item{taxon_clean}{PENDING}
#'   \item{taxon_count}{PENDING}
#' }
"pollen_data"

#' \code{pub}
#'
#' A tibble with records for pub.
#'
#' @format A data frame with 189 rows and 2 variables:
#' \describe{
#'   \item{ID_PUB}{PENDING}
#'   \item{citation}{PENDING}
#' }
"pub"

#' \code{sample}
#'
#' A tibble with records for sample.
#'
#' @format A data frame with 10339 rows and 6 variables:
#' \describe{
#'   \item{ID_SAMPLE}{PENDING}
#'   \item{ID_ENTITY}{PENDING}
#'   \item{sample_name}{PENDING}
#'   \item{avg_depth}{PENDING}
#'   \item{sample_type}{PENDING}
#'   \item{count_type}{PENDING}
#' }
"sample"

#' \code{site}
#'
#' A tibble with records for site.
#'
#' @format A data frame with 1463 rows and 8 variables:
#' \describe{
#'   \item{ID_SITE}{PENDING}
#'   \item{site_name}{PENDING}
#'   \item{latitude}{PENDING}
#'   \item{longitude}{PENDING}
#'   \item{elevation}{PENDING}
#'   \item{site_type}{PENDING}
#'   \item{basin_size}{PENDING}
#'   \item{catch_size}{PENDING}
#' }
"site"
