
#' Fishery size composition data
#'
#' @param year assessment year
#' @param fishery default is "fsh1"
#' @param lenbins lenbin file if left NULL it looks for here::here(year, "data", "user_input", "len_bin_labels.csv")
#' @param rec_age recruitment age
#'
#' @return
#' @export fishery_length_comp
#'
#' @examples
fishery_length_comp <- function(year, fishery = "fsh1", rec_age, lenbins = NULL){

  if(is.null(lenbins)){
    stop("Please provide the length bin file that is in the user_input folder e.g.,('lengthbins.csv')")
  } else {
    lenbins = read.csv(here::here(year, "data", "user_input", lenbins))$len_bins
  }

  Y = year
  read.csv(here::here(year, "data", "raw", paste0(fishery, "_age_comp_data.csv"))) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(!is.na(age), age>=rec_age, year>1990 & year<Y) %>%
    dplyr::group_by(year) %>%
    dplyr::tally(name = "age") %>%
    dplyr::filter(age >= 50) %>%
    dplyr::ungroup() -> ages

  read.csv(here::here(year, "data", "raw", "fishery_size_comp_freq.csv"),
           colClasses = c(HAUL_JOIN = "character",
                          PORT_JOIN = "character")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(!(year %in% unique(ages$year)), year>1990 & year<year) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(tot = sum(frequency),
                  length = ifelse(length >= max(lenbins), max(lenbins), length),
                  n_h = length(unique(na.omit(haul_join))) + length(unique(na.omit(port_join)))) %>%
    dplyr::group_by(year, length) %>%
    dplyr::summarise(n_s = mean(tot),
                     n_h = mean(n_h),
                     length_tot = sum(frequency)) %>%
    dplyr::mutate(prop = length_tot / n_s) %>%
    dplyr::left_join(expand.grid(year = unique(.$year), length = lenbins), .) %>%
    tidyr::replace_na(list(prop = 0)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(SA_Index = 1,
                  n_s = mean(n_s, na.rm = T),
                  n_h = mean(n_h, na.rm = T)) %>%
    dplyr::select(-length_tot) %>%
    tidyr::pivot_wider(names_from = length, values_from = prop) -> fish_length_comp

  readr::write_csv(fish_size_comp, here::here(year, "data", "output", paste0(fishery, "_length_comp.csv")))

  fish_length_comp

}
