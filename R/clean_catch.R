#' clean up catch data
#'
#' @param year  year of assessment
#' @param species species of interest e.g., "SABL", "DUSK"
#' @param fishery identify the fishery default is "fsh1"
#' @param TAC last three TAC in form: c(year-3, year-2, year-1)
#' @param fixed_catch if catch is frozen place the file in user_input folder (format: Year, Catch)
#'
#' @return
#' @export clean_catch
#'
#' @examples
#' \dontrun{
#' clean_catch(year, TAC = c(2874, 2756, 3100), fixed_catch = "goa_rebs_catch_1977_2004")
#' }
#'
clean_catch <- function(year, species, fishery = "fsh1", TAC = c(3333, 2222, 1111), fixed_catch = NULL){

  if(sum(TAC == c(3333, 2222, 1111)) == 3) {
    stop("check your TAC!")
  }

  if(!is.null(fixed_catch)){
    fixed_catch = vroom::vroom(here::here("data", "user_input", "fixed_catch"))
  } else if(is.null(fixed_catch)){
    if(species == "NORK"){
      fixed_catch = groundfishr::goa_nork_catch_1961_1992
    }
    if(species == "SABL"){
      fixed_catch = groundfishr::sabl_fixed_abundance |>
        dplyr::filter(variable == "catch")
    }
    if(species == "REBS"){
      fixed_catch = groundfishr::goa_rebs_catch_1977_2004
    }
    if(species == "DUSK"){
      fixed_catch = groundfishr::goa_dusk_catch_1970_1990
    }
    if(species == "POPA"){
      fixed_catch = groundfishr::goa_pop_catch_1960_1990
    }
  }

  names(fixed_catch) <- c("Year", "Catch")

  # Fishery catch data ----
  read.csv(here::here(year, "data", "raw", paste0(fishery, "_catch_data.csv"))) -> catch_data
  read.csv(here::here(year, "data", "raw",  paste0(fishery, "_obs_data.csv"))) -> obs_data

  # Estimate catch ratio in final year to end of year
  obs_data %>%
    dplyr::filter(YEAR %in% (year-3):(year-1)) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::mutate(tot_catch = sum(EXTRAPOLATED_WEIGHT),
                  test_date = paste0(YEAR, substr(max(as.Date(catch_data$WEEK_END_DATE)),5,10))) %>%
    dplyr::filter(HAUL_DATE <= test_date) %>%
    dplyr::summarise(oct_catch = round(sum(EXTRAPOLATED_WEIGHT)),
                     tot_catch = round(mean(tot_catch))) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(ratio = 1 + (sum(tot_catch) - sum(oct_catch)) / sum(oct_catch)) %>%
    dplyr::pull(ratio) -> ratio

  # Compute catch
  if(nrow(fixed_catch)>=1){
  catch_data %>%
    dplyr::select(Year = YEAR, Catch = WEIGHT_POSTED) %>%
    dplyr::filter(Year > max(fixed_catch$Year)) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(Catch = round(sum(Catch), 4)) %>%
    dplyr::bind_rows(fixed_catch) %>%
    dplyr::arrange(Year) %>%
    dplyr::mutate(Catch = ifelse(Year==year, round(Catch * ratio, 4), Catch)) -> catch
  } else {
    catch_data %>%
      dplyr::select(Year = YEAR, Catch = WEIGHT_POSTED) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(Catch = round(sum(Catch), 4)) %>%
      dplyr::arrange(Year) %>%
      dplyr::mutate(Catch = ifelse(Year==year, round(Catch * ratio, 4), Catch)) -> catch
  }

  write.csv(catch, here::here(year, "data", "output",  paste0(fishery, "_catch.csv")), row.names = FALSE)

  # estimate yield ratio of previous 3 years relative to TAC
  catch %>%
    dplyr::filter(Year %in% (year-3):(year-1)) %>%
    dplyr::bind_cols(tac = TAC) %>%
    dplyr::mutate(yld = Catch / tac) %>%
    dplyr::summarise(yld = mean(yld)) %>%
    dplyr::pull(yld) -> yld

    data.frame(yld = yld, catch_rat = ratio) %>%
      write.csv(here::here(year, "data", "output", "yld_rat.csv"), row.names = FALSE)
}
