
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EMBSeCBIO <img src="documentation/embsecbio/inst/images/logo.png" alt="logo" align="right" height=200px/>

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-yellow.svg)](https://github.com/special-uor/embsecbio)
[![](https://www.r-pkg.org/badges/version/embsecbio?color=black)](https://cran.r-project.org/package=embsecbio)
[![R build
status](https://github.com/special-uor/embsecbio/workflows/R-CMD-check/badge.svg)](https://github.com/special-uor/embsecbio/actions)
<!-- badges: end -->

The goal of EMBSeCBIO is to provide functions to work with the Eastern
Mediterranean-Black Sea-Caspian-Corridor region and biome
reconstructions database.

## Installation

You can(not) install the released version of EMBSeCBIO from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("embsecbio")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("special-uor/embsecbio")
```

## Example

Obtain the top 10 sites and their corresponding metadata:

``` r
`%>%` <- magrittr::`%>%`
embsecbio::site %>%
  dplyr::slice(1:10) %>%
  knitr::kable()
```

| ID\_SITE | site\_name | latitude | longitude | elevation | site\_type                                    | basin\_size            | catch\_size       |
| -------: | :--------- | -------: | --------: | --------: | :-------------------------------------------- | :--------------------- | :---------------- |
|        2 | Sakhare    |    41.58 |     45.32 |       800 | lacustrine, playa                             | small (0.01-1 km2)     | small (\<10 km2)  |
|        3 | Kumisi     |    41.58 |     44.83 |       469 | lacustrine, natural open-water, tectonic lake | medium (1.1-50 km2)    | small (\<10 km2)  |
|        4 | Tsavkisi   |    41.68 |     44.72 |      1100 | terrestrial, mire, bog                        | small (0.01-1 km2)     | small (\<10 km2)  |
|        5 | Imera      |    41.65 |     44.22 |      1610 | terrestrial, mire, bog                        | small (0.01-1 km2)     | small (\<10 km2)  |
|        6 | Aligol     |    41.63 |     44.02 |      1550 | terrestrial, mire, bog                        | small (0.01-1 km2)     | small (\<10 km2)  |
|        7 | Ispani-II  |    41.87 |     41.80 |         2 | terrestrial, mire, bog, raised bog            | medium (1.1-50 km2)    | small (\<10 km2)  |
|        8 | Lake Urmia |    37.50 |     45.50 |      1297 | lacustrine, natural open-water                | very large (\>500 km2) | large (\>500 km2) |
|        9 | Adange     |    43.31 |     41.33 |      1750 | fluvial                                       | small (0.01-1 km2)     | small (\<10 km2)  |
|       10 | Amtkel     |    43.27 |     41.31 |      1830 | fluvial                                       | small (0.01-1 km2)     | small (\<10 km2)  |
|       11 | Gagra 471  |    43.28 |     40.27 |         0 | fluvial                                       | small (0.01-1 km2)     | small (\<10 km2)  |

Obtain the top 10 entities and their corresponding metadata:

``` r
`%>%` <- magrittr::`%>%`
embsecbio::entity %>%
  dplyr::slice(1:10) %>%
  knitr::kable()
```

| ID\_ENTITY | ID\_SITE | entity\_name     | latitude | longitude | elevation | source    | entity\_type       | mod\_or\_0ka\_class | comments               |
| ---------: | -------: | :--------------- | -------: | --------: | --------: | :-------- | :----------------- | :------------------ | :--------------------- |
|          2 |        2 | Sakhare core 1   |    41.58 |     45.32 |       800 | EMBSECBIO | core               | PCT                 | if sample age = modern |
|          3 |        3 | Kumisi core 1    |    41.58 |     44.83 |       469 | EMBSECBIO | core               | PCT                 | if sample age = modern |
|          4 |        4 | Tsavkisi core 1  |    41.68 |     44.72 |      1100 | EMBSECBIO | core               | PCT                 | if sample age = modern |
|          5 |        5 | Imera core 1     |    41.65 |     44.22 |      1610 | EMBSECBIO | core               | PCT                 | if sample age = modern |
|          6 |        6 | Aligol core 1    |    41.63 |     44.02 |      1550 | NOAA      | core               | PCT                 | if sample age = modern |
|          7 |        7 | Ispani II core 1 |    41.87 |     41.80 |         2 | EMBSECBIO | peat core          | PCT                 | if sample age = modern |
|          8 |        8 | Urmia core 20    |    37.50 |     45.50 |      1297 | NOAA      | core               | PCT                 | if sample age = modern |
|          9 |        9 | Adange core 1    |    43.31 |     41.33 |      1750 | NOAA      | core               | PCT                 | if sample age = modern |
|         10 |       10 | Amtkel core 1    |    43.27 |     41.31 |      1830 | NOAA      | core               | PCT                 | if sample age = modern |
|         11 |       11 | Gagra \#471      |    43.28 |     40.27 |         0 | NOAA      | profile or section | PCT                 | if sample age = modern |

Find entities linked to sites:

``` r
`%>%` <- magrittr::`%>%`
ents <- embsecbio::site %>%
  dplyr::slice(1:4) %>%
  embsecbio::get_entities()

ents %>%
  knitr::kable()
```

| ID\_ENTITY | ID\_SITE | entity\_name    | latitude | longitude | elevation | source    | entity\_type    | mod\_or\_0ka\_class | comments               |
| ---------: | -------: | :-------------- | -------: | --------: | --------: | :-------- | :-------------- | :------------------ | :--------------------- |
|          2 |        2 | Sakhare core 1  |    41.58 |     45.32 |       800 | EMBSECBIO | core            | PCT                 | if sample age = modern |
|          3 |        3 | Kumisi core 1   |    41.58 |     44.83 |       469 | EMBSECBIO | core            | PCT                 | if sample age = modern |
|          4 |        4 | Tsavkisi core 1 |    41.68 |     44.72 |      1100 | EMBSECBIO | core            | PCT                 | if sample age = modern |
|          5 |        5 | Imera core 1    |    41.65 |     44.22 |      1610 | EMBSECBIO | core            | PCT                 | if sample age = modern |
|       1169 |        2 | Sakhare-2       |    41.58 |     45.32 |       800 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1170 |        2 | Sakhare-3       |    41.58 |     45.32 |       800 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1171 |        2 | Sakhare-4       |    41.58 |     45.32 |       800 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1172 |        3 | Kumisi-2        |    41.58 |     44.83 |       469 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1173 |        3 | Kumisi-3        |    41.58 |     44.83 |       469 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1174 |        3 | Kumisi-4        |    41.58 |     44.83 |       469 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1175 |        4 | Tsavkisi-1      |    41.68 |     44.72 |      1100 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1176 |        4 | Tsavkisi-2      |    41.68 |     44.72 |      1100 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1177 |        4 | Tsavkisi-3      |    41.68 |     44.72 |      1100 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1178 |        4 | Tsavkisi-4      |    41.68 |     44.72 |      1100 | EMBSECBIO | surface sample  | SS                  | NA                     |
|       1186 |        5 | Imera-1         |    41.65 |     44.22 |      1610 | EMBSECBIO | sediment sample | SS                  | NA                     |
|       1187 |        5 | Imera-2         |    41.65 |     44.22 |      1610 | EMBSECBIO | sediment sample | SS                  | NA                     |
|       1188 |        5 | Imera-3         |    41.65 |     44.22 |      1610 | EMBSECBIO | sediment sample | SS                  | NA                     |
|       1189 |        5 | Imera-4         |    41.65 |     44.22 |      1610 | EMBSECBIO | sediment sample | SS                  | NA                     |
|       1197 |        2 | Sakhare-1       |    41.58 |     45.32 |       800 | EMBSECBIO | surface sample  | SS                  | NA                     |

<!-- This is a basic example which shows you how to solve a common problem: -->

<!-- ```{r example} -->

<!-- library(embsecbio) -->

<!-- ## basic example code -->

<!-- ``` -->

## Datasets

1.  Harrison, Sandy and Marinova, Elena (2017): EMBSeCBIO modern pollen
    biomisation. University of Reading. Dataset.
    <http://dx.doi.org/10.17864/1947.109>
