
#' Plot trawl survey biomass (design-based and VAST)
#'
#' @param year model year
#' @param folder location to place results - a "figs" folder will be created within
#' @param vast default is null, otherwise a .csv file placed in the user_input folder
#'
#' @return
#' @export plot_tsb
#'
#' @examples
plot_tsb <- function(year, folder, vast = NULL){

  if (!dir.exists(here::here(year, folder, "figs"))) {dir.create(here::here(year, folder, "figs"))}

  # plot survey results ----

  sb <- vroom::vroom(here::here(year, "data", "output",  "goa_ts_biomass_db.csv"))

  sb |>
    dplyr::rename(t = biomass) |>
    dplyr::mutate(Model = "Design-based") -> dat

  if(!is.null(vast)){
  vast <- vroom::vroom(here::here(year, "data", "user_input", vast))

  vast |>
    dplyr::mutate(lci = t - sd * 1.96,
           uci = t + sd * 1.96) |>
    dplyr::select(-sd) |>
    dplyr::mutate(Model = "VAST") |>
    dplyr::bind_rows(dat) -> dat
  }

  dat %>%
    tidyr::drop_na(year) %>%
    dplyr::group_by(Model) %>%
    dplyr::mutate(mean = mean(t)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(year, t, fill = Model, color = Model)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lci, ymax = uci), alpha = 0.2, color = NA) +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(sb, year)$breaks,
                       labels = funcr::tickr(sb, year)$labels) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::ylab("Metric tons\n") +
    ggplot2::xlab("\nYear") +
    ggplot2::expand_limits(y = 0) +
    scico::scale_fill_scico_d(palette = "roma", begin = 0.2) +
    scico::scale_color_scico_d(palette = "roma", begin = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = mean), lty = 3) +
    funcr::theme_report() +
    ggplot2::theme(legend.position = c(0.2, 0.8))

  ggplot2::ggsave(here::here(year, folder, "figs", "ts_biomass.png"), width = 6.5, height = 5.5, units = "in", dpi = 200)
}
