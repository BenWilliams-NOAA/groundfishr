#' longline survey relative population numbers
#'
#' @param year of interest
#' @param area "goa", "bsai"
#' @param file using an alt-ll survey rnp? (file must have format: year, rnp, cv, sd, lci, uci)
#' @param filter_yrs any years that would like removed from survey biomass estimates e.g., 1997 or c(2001, 2003)
#'
#' @return
#' @export lls_rpn
#'
#' @examples
#'
lls_rpn <- function(year, area, file = NULL, filter_yrs = NULL){

  # For GOA only at this time
  if(is.null(file) & is.null(filter_yrs)){

    read.csv(here::here(year, "data", "raw", paste0(area, "_lls_biomass_data.csv"))) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::filter(survey != "Japan",
                    area_id >= 3, # goa
                    year > 1992) %>% # poor data prior to this year?
      dplyr::group_by(year) %>%
      dplyr::summarise(rpn = sum(rpn)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cv = sqrt(var(rpn)) / mean(rpn),
                    sd = cv * rpn,
                    lci = rpn - 1.96 * sd,
                    uci = rpn + 1.96 * sd) %>%
      dplyr::mutate(lci = ifelse(lci < 0, 0, lci)) %>%
      dplyr::mutate_if(is.double, round) %>%
      dplyr::select(-cv) -> sb

  } else if(is.null(file) & !is.null(filter_yrs)){

    read.csv(here::here(year, "data", "raw", paste0(area, "_lls_biomass_data.csv"))) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::filter(!(year %in% filter_yrs),
                    survey != "Japan",
                    area_id >= 3, # goa
                    year > 1992) %>% # poor data prior to this year?
      dplyr::group_by(year) %>%
      dplyr::summarise(rpn = sum(rpn)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cv = sqrt(var(rpn)) / mean(rpn),
                    sd = cv * rpn,
                    lci = rpn - 1.96 * sd,
                    uci = rpn + 1.96 * sd) %>%
      dplyr::mutate(lci = ifelse(lci < 0, 0, lci)) %>%
      dplyr::mutate_if(is.double, round) %>%
      dplyr::select(-cv) -> sb
  } else if(!is.null(file)){
    read.csv(here::here(year, "data", "user_input", file)) -> sb
  }

  write.csv(sb, here::here(year, "data", "output", paste0(area, "_lls_numbers.csv")), row.names = FALSE)

  sb
}
