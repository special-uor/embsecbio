#' Plot Age-Depth
#'
#' @param entity_name String with the name of the entity.
#' @param date_fill String with Hexadecimal colour for the dates.
#' @param ... Extra parameters (unused).
#'
#' @inheritParams commit
#' @return \code{ggplot2} object.
#' @export
plot_age_depth <- function(conn, entity_name, date_fill = "#F1C40F", quiet = TRUE,  ...) {
  # Load data from the database
  entity_tb <- dabr::select(conn,
                            "SELECT * FROM entity WHERE entity_name = ",
                            dabr::quote(entity_name),
                            quiet = quiet)
  if (nrow(entity_tb) == 0)
    stop("The entity '", entity_name, "', was not found on the database.",
         call. = FALSE)
  sample_tb <- dabr::select(conn,
                            "SELECT * FROM sample WHERE ID_ENTITY = ",
                            entity_tb$ID_ENTITY,
                            quiet = quiet)
  if (nrow(sample_tb) == 0)
    stop("No samples were found, linked to the entity '", entity_name, "'.",
         call. = FALSE)
  date_info_tb <- dabr::select(conn,
                               "SELECT * FROM date_info WHERE ID_ENTITY = ",
                               entity_tb$ID_ENTITY,
                               quiet = quiet)
  age_model_tb <- dabr::select(conn,
                               "SELECT * FROM age_model WHERE ID_SAMPLE IN (",
                               paste0(sample_tb$ID_SAMPLE, collapse = ", "),
                               ")",
                               quiet = quiet)

  # Create plot
  p <- tibble::tibble(depths = sample_tb$avg_depth,
                      Original = age_model_tb$est_age_original,
                      `BACON-IntCal13` = age_model_tb$est_age_bacon_intcal13,
                      `BACON-IntCal20` = age_model_tb$est_age_bacon_intcal20_mean) %>%
    tidyr::pivot_longer(cols = c(Original, `BACON-IntCal13`, `BACON-IntCal20`),
                        names_to = "age_model") %>%
    ggplot2::ggplot(ggplot2::aes(x = depths, y = value)) +
    ggplot2::geom_line(ggplot2::aes(colour = age_model)) + # Add chronology lines
    ggplot2::geom_point(data = date_info_tb,
                        ggplot2::aes(x = avg_depth,
                                     y = dated_age,
                                     fill = ""),
                        shape = 24,
                        size = 3) +
    ggplot2::scale_colour_brewer(name = "Source", palette = "Set1") +
    ggplot2::scale_fill_manual(name = "Dates", values = date_fill) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::labs(x = "Depth [cm]",
                  y = "cal Age [yrs BP]",
                  title = entity_name) +
    ggplot2::theme_bw()
  return(p)
}

.plot_age_depth <- function(df, dates, entity = NULL, hiatuses = NULL, dates_fill = "orange") {
  # Local binding
  x <- y <- q_low <- q_up <- depth <- age <- age_min <- age_max <- NULL
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_line(ggplot2::aes(x, y), col = "red") +
    ggplot2::geom_line(ggplot2::aes(x, q_low), col = "black", lty = 2) +
    ggplot2::geom_line(ggplot2::aes(x, q_up), col = "black", lty = 2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q_low, ymax = q_up),
                         fill = grey(0.5),
                         alpha = 0.35) +
    ggplot2::geom_pointrange(ggplot2::aes(x = depth,
                                          y = age,
                                          ymin = age_min,
                                          ymax = age_max),
                             data = dates,
                             fill = dates_fill,
                             # size = 2,
                             shape = 24) +
    # ggplot2::scale_colour_manual(values = dates$col) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = nrow(dates))) +
    ggplot2::labs(x = "Depth [cm]",
                  y = "cal Age [yrs BP]",
                  title = entity) +
    # ggplot2::coord_cartesian(xlim = c(0, max(depths_eval * 10))) +
    # ggplot2::coord_cartesian(xlim = c(0, max(depths_eval * 10)),
    #                          ylim = c(0, max(df$q_up))) +
    # ggplot2::scale_x_continuous(limits = c(0, max(depths_eval * 10))) +
    # ggplot2::scale_y_continuous(limits = c(0, max(df$q_up))) +
    ggplot2::theme_bw()
  if (!is.null(hiatuses))
    for (i in seq_len(nrow(hiatuses))) {
      p <- p +
        ggplot2::geom_vline(xintercept = hiatuses[i, 2],
                            col = "grey",
                            lty = 2)
    }
  return(p)
}
