EC-S: Updates 2021-03 - EMBSeCBIO
================

## Open connection to RPD:

``` r
conn <- dabr::open_conn_mysql("EMBSeCBIO", password = rstudioapi::askForPassword())
db <- purrr::map(embsecbio:::embsecbio()$tables, dabr::select_all, conn = conn)
```

``` r
path <- "~/Desktop/iCloud/UoR/2021/Data/EMBSeCBIO"
inspect <- function(...) embsecbio:::inspect(...)
insert <- function(...) embsecbio::insert(...)
```

## Modify EMBSeCBIO

Modify the `entity_pub` table, to use an autonumeric in the column
`ID_ENTITY_PUB`

``` sql
ALTER TABLE entity_pub 
MODIFY COLUMN ID_ENTITY_PUB 
int unsigned NOT NULL AUTO_INCREMENT;
```

### `Adding_Lake_Van_Pickarski et al. 2015`

``` r
workbook <- embsecbio:::read_workbook(file.path(path, "Adding_Lake_Van_Pickarski et al. 2015.xlsx"))
#### Site data
site_tb <- workbook$site %>%
  inspect(coerce = TRUE)
ID_SITE <- site_tb %>%
  insert(conn = conn, quiet = TRUE)

#### Entity data
entity_tb <- workbook$entity %>%
  inspect(coerce = TRUE) %>%
  dplyr::mutate(ID_SITE = ID_SITE)
ID_ENTITY <- entity_tb %>%
  insert(conn = conn, quiet = TRUE)
entity_tb <- entity_tb %>%
  dplyr::mutate(ID_ENTITY = ID_ENTITY, .before = 1)

#### Sample data
sample_tb <- workbook$sample %>%
  inspect(coerce = TRUE) %>%
  dplyr::mutate(ID_ENTITY = ID_ENTITY)
ID_SAMPLE <- sample_tb %>%
  insert(conn = conn, quiet = TRUE)
sample_tb <- sample_tb %>%
  dplyr::mutate(ID_SAMPLE = ID_SAMPLE, .before = 1)

#### Pollen data
pollen_tb <- workbook$pollen_data %>%
  inspect(coerce = TRUE) %>%
  dplyr::inner_join(sample_tb %>%
                      dplyr::select(ID_SAMPLE, sample_name), by = "sample_name")
ID_SAMPLE_TAXA <- pollen_tb %>%
  insert(conn = conn, quiet = TRUE)

#### Date info data
date_info_tb <- workbook$date_info %>%
  inspect(coerce = TRUE) %>%
  dplyr::inner_join(entity_tb %>%
                      dplyr::select(ID_ENTITY, entity_name), by = "entity_name")
ID_DATE_INFO <- date_info_tb %>%
  insert(conn = conn, quiet = TRUE)

#### Age model data
age_model_tb <- workbook$age_model %>%
    inspect(coerce = TRUE)
ID_SAMPLE2 <- age_model_tb %>%
  dplyr::mutate(ID_SAMPLE = ID_SAMPLE, .before = 1) %>%
  insert(conn = conn, quiet = TRUE)
```

### `Dated_ages_for_38_entities_March2021`
