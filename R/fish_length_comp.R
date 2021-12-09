
#' fishery length composition analysis
#'
#' @param year assessment year
#' @param fishery default is "fsh1"
#' @param lenbins lenbin file if left NULL it looks for here::here(year, "data", "user_input", "len_bin_labels.csv")
#' @param rec_age recruitment age
#'
#' @return
#' @export fish_length_comp
#'
#' @examples
fish_length_comp <- function(year, fishery = "fsh", rec_age, lenbins = NULL){

  if(is.null(lenbins)){
    stop("Please provide the length bin file that is in the user_input folder e.g.,('lengthbins.csv')")
  } else {
    lenbins =  vroom::vroom(here::here(year, "data", "user_input", lenbins), delim = ",")$len_bins
  }

  Y = year
  read.csv(here::here(year, "data", "raw", paste0(fishery, "_age_comp_data.csv"))) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(!is.na(age), age>=rec_age, year>1990 & year<Y) %>%
    dplyr::group_by(year) %>%
    dplyr::tally(name = "age") %>%
    dplyr::filter(age >= 50) %>%
    dplyr::ungroup() -> ages

  vroom::vroom(here::here(year, "data", "raw", paste0(fishery,"_length_comp_data.csv")),
               col_types = list(HAUL_JOIN = "c",
                                PORT_JOIN = "c")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(!(year %in% unique(ages$year)), year>1990 & year<Y) %>%
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

  vroom::vroom_write(fish_length_comp, here::here(year, "data", "output", paste0(fishery, "_length_comp.csv")), ",")

  fish_length_comp

}
