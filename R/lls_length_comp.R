#' Longline length composition analysis
#'
#' @param year assessment year
#' @param species enter if REBS, otherwise NULL
#' @param lenbins lenbin file if left NULL it looks for (year/data/user_input/len_bin_labels.csv")
#'
#' @return
#' @export lls_length_comp
#'
#' @examples
lls_length_comp <- function(year, species = NULL, region, lenbins = NULL){

  if(is.null(lenbins)){
    stop("Please provide the length bin file that is in the user_input folder e.g.,('lengthbins.csv')")
  } else {
    lenbins = read.csv(here::here(year, "data", "user_input", lenbins))$len_bins
  }


  # filters
  # min area_code, remove these areas, country to ignore, min year
  drop <- list(0, -9, "Japan", 0)
  # max station number, exploitable
  drop2 <- list(Inf, c(0,1))


  if(species == "REBS"){
    # min area_code, remove these areas, country to ignore, min year
    drop <- list(25, c(28, 31, 36, 41, 42, 43, 48), "Japan", 1993)
    # max station number, exploitable
    drop2 <- list(499, 1)
  }



  read.csv(here::here(year, "data", "raw", paste0(region, "_lls_specimen_data.csv"))) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename(area_code = geographic_area_code) %>%
    dplyr::filter(area_code > drop[[1]],
                  !(area_code %in% drop[[2]]),
                  country != drop[[3]],
                  year >= drop[[4]],
                  station_number <= drop2[[1]],
                  exploitable %in% drop2[[2]]) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(n_s = sum(frequency),
                     n_h = length(unique(station_number))) %>%
    dplyr::ungroup() -> dat


  read.csv(here::here(year, "data", "raw", paste0(region, "_lls_length_data.csv"))) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(area_code > drop[[1]],
                  !(area_code %in% drop[[2]]),
                  country != drop[[3]],
                  year >= drop[[4]]) %>%
    dplyr::mutate(length = ifelse(length >= max(lenbins), max(lenbins), length)) %>%
    dplyr::select(year, length, length_frequency) %>%
    dplyr::filter(length %in% lenbins) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(tot = sum(length_frequency )) %>%
    dplyr::group_by(year, length) %>%
    dplyr::summarise(prop = sum(length_frequency ) / mean(tot)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(expand.grid(year = unique(.$year), length = lenbins), .) %>%
    tidyr::replace_na(list(prop = 0)) %>%
    dplyr::left_join(dat) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(SA_Index = 1,
                  n_s = mean(n_s, na.rm = T),
                  n_h = mean(n_h, na.rm = T)) %>%
    tidyr::pivot_wider(names_from = length, values_from = prop) -> size_comp

  write.csv(size_comp, here::here(year, "data", "output", paste0(region, "_lls_length_comp.csv")),
            row.names = FALSE)

  size_comp

}
