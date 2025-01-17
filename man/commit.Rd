% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embsecbio.R
\name{commit}
\alias{commit}
\title{Commit contents of workbook}
\usage{
commit(
  conn,
  workbook,
  wdir = NA,
  quiet = TRUE,
  coerce = FALSE,
  default = -999999,
  allow_duplicates = FALSE,
  ...
)
}
\arguments{
\item{conn}{DB connection object.}

\item{workbook}{Workbook filename.}

\item{wdir}{Path to the workbook.}

\item{quiet}{Boolean flag to hide status messages.}

\item{coerce}{Boolean flag to indicate if columns should be coerced to the
right type.}

\item{default}{Numeric value to be used when coercing non-numeric values.}

\item{allow_duplicates}{Boolean flag to indicate if duplicate records should
be allowed.}

\item{...}{
  Arguments passed on to \code{\link[=read_workbook]{read_workbook}}, \code{\link[readxl:read_excel]{readxl::read_excel}}
  \describe{
    \item{\code{sheets}}{Sheet names, by default it takes the names listed in the
description.}
    \item{\code{skip}}{Number of lines to skip.}
    \item{\code{range}}{A cell range to read from, as described in \link[readxl]{cell-specification}.
Includes typical Excel ranges like "B3:D87", possibly including the sheet
name like "Budget!B2:G14", and more. Interpreted strictly, even if the
range forces the inclusion of leading or trailing empty rows or columns.
Takes precedence over \code{skip}, \code{n_max} and \code{sheet}.}
    \item{\code{col_names}}{\code{TRUE} to use the first row as column names, \code{FALSE} to get
default names, or a character vector giving a name for each column. If user
provides \code{col_types} as a vector, \code{col_names} can have one entry per
column, i.e. have the same length as \code{col_types}, or one entry per
unskipped column.}
    \item{\code{col_types}}{Either \code{NULL} to guess all from the spreadsheet or a
character vector containing one entry per column from these options:
"skip", "guess", "logical", "numeric", "date", "text" or "list". If exactly
one \code{col_type} is specified, it will be recycled. The content of a cell in
a skipped column is never read and that column will not appear in the data
frame output. A list cell loads a column as a list of length 1 vectors,
which are typed using the type guessing logic from \code{col_types = NULL}, but
on a cell-by-cell basis.}
    \item{\code{na}}{Character vector of strings to interpret as missing values. By
default, readxl treats blank cells as missing data.}
    \item{\code{trim_ws}}{Should leading and trailing whitespace be trimmed?}
    \item{\code{n_max}}{Maximum number of data rows to read. Trailing empty rows are
automatically skipped, so this is an upper bound on the number of rows in
the returned tibble. Ignored if \code{range} is given.}
    \item{\code{guess_max}}{Maximum number of data rows to use for guessing column
types.}
    \item{\code{progress}}{Display a progress spinner? By default, the spinner appears
only in an interactive session, outside the context of knitting a document,
and when the call is likely to run for several seconds or more. See
\code{\link[readxl:readxl_progress]{readxl_progress()}} for more details.}
    \item{\code{.name_repair}}{Handling of column names. By default, readxl ensures
column names are not empty and are unique. If the tibble package version is
recent enough, there is full support for \code{.name_repair} as documented in
\code{\link[tibble:tibble]{tibble::tibble()}}. If an older version of tibble is present, readxl falls
back to name repair in the style of tibble v1.4.2.}
  }}
}
\value{
Tibble with report of inserted records.
}
\description{
Commit contents of workbook to database, \code{conn}.
}
