#' \code{age_model}
#'
#' A tibble with records for age_model.
#'
#' @format A data frame with `r nrow(embsecbio::age_model)` rows and `r ncol(embsecbio::age_model)` variables:
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
#' @format A data frame with `r nrow(embsecbio::date_info)` rows and `r ncol(embsecbio::date_info)` variables:
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
#' @format A data frame with `r nrow(embsecbio::entity)` rows and `r ncol(embsecbio::entity)` variables:
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
#' @format A data frame with `r nrow(embsecbio::entity_pub)` rows and `r ncol(embsecbio::entity_pub)` variables:
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
#' @format A data frame with `r nrow(embsecbio::pollen_data)` rows and `r ncol(embsecbio::pollen_data)` variables:
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
#' @format A data frame with `r nrow(embsecbio::pub)` rows and `r ncol(embsecbio::pub)` variables:
#' \describe{
#'   \item{ID_PUB}{PENDING}
#'   \item{citation}{PENDING}
#' }
"pub"

#' \code{sample}
#'
#' A tibble with records for sample.
#'
#' @format A data frame with `r nrow(embsecbio::sample)` rows and `r ncol(embsecbio::sample)` variables:
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
#' @format A data frame with `r nrow(embsecbio::site)` rows and `r ncol(embsecbio::site)` variables:
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
