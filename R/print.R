#' @rdname print
#' @export
print.commit <- function(x, ...) {
  if (nrow(x) > 0) {
    cat(rep("_", 44), "\n", sep = "")
    cat("| ",
        stringr::str_pad("Table name", 20, "right"),
        stringr::str_pad("Inserted", 10, "right"),
        stringr::str_pad("Expected", 10, "right"),
        " |\n",
        sep = "")
    cat(rep("_", 44), "\n", sep = "")
    purrr::pwalk(x, ~cat("| ",
                         stringr::str_pad(..1, 20, "right"),
                         stringr::str_pad(..2, 10, "right"),
                         stringr::str_pad(..3, 10, "right"),
                         " |\n",
                         sep = ""))
    cat(rep("_", 44), "\n", sep = "")
    cat("| ",
        stringr::str_pad("", 20, "right"),
        stringr::str_pad(sum(x[, 2], na.rm = TRUE), 10, "right"),
        stringr::str_pad(sum(x[, 3], na.rm = TRUE), 10, "right"),
        " |\n",
        sep = "")
    cat(rep("_", 44), "\n", sep = "")
  }
  invisible(x)
}

#' @rdname print
#' @export
print.site <- function(x, ...) {
  # Local bindidng
  . <- NULL
  x %>%
    magrittr::set_class(class(.)[-1]) %>% # Remove `site` class
    print()
  invisible(x)
}
