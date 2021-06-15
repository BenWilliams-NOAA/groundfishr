#' Sablefish historical catch data
#'
#' A dataset containing both fixed gear and trawl gear catch by year for 1960-1990
#'
#' @format A data frame with 62 observations and 3 variables:
#' \describe{
#' \item{year}{year of catch}
#' \item{catch}{weight of catch in 1,000 t}
#' \item{type}{fishery type is either fixed (includes longline and pot gear) or trawl}
#' }
"sabl_catch_1960_1990"

#' Sablefish historical CPUE
#'
#' A dataset containing both relative population weight (RPW) and relative population numbers (RPN) from the Japanese cooperative longline fishery for 1979-1994
#'
#' @format A data frame with 16 observations and 9 variables:
#' \describe{
#' \item{year}{year of cpue}
#' \item{rpn}{relative population number}
#' \item{rpn_se}{the standard error of rpn}
#' \item{rpw}{relative population weight}
#' \item{rpw_se}{the standard error of rpw}
#' \item{rpn_lci}{lower confidence interval: 2 * se}
#' \item{rpn_uci}{upper confidence interval: 2 * se}
#' \item{rpw_lci}{lower confidence interval: 2 * se}
#' \item{rpw_uci}{upper confidence interval: 2 * se}
#' }
"sabl_japan_coop_lls"

#' Sablefish fixed weights at length
#'
#' A dataset containing length bins and associated male and female weights
#'
#' @format A data frame with 30 observations and 3 variables:
#' \describe{
#' \item{length_bin}{length bin groups}
#' \item{wt_m}{male weight (kg)}
#' \item{wt_f}{female weight (kg)}
#' }
"sabl_old_wts_sex"


#' vessel lengths
#'
#' Vessel lengths for calculating whale depredation rates in the sablefish fishery
#'
#' @format A data frame with 7558 observations and 2 variables:
#' \describe{
#' \item{VESSEL_CODE}{vessel id}
#' \item{LENGTH}{length of the vessel}
#' }
"sabl_vessel_lengths"

#' GOA northern rockfish historical catch data
#'
#' A dataset containing trawl gear catch by year for 1961-1992
#'
#' @format A data frame with 32 observations and 2 variables:
#' \describe{
#' \item{year}{year of catch}
#' \item{catch}{weight of catch in 1,000 t}
#' }
"goa_nork_catch_1961_1992"


#' GOA rougheye/blackspotted rockfish historical catch data
#'
#' A dataset containing trawl gear catch by year for 1977-2004
#'
#' @format A data frame with 28 observations and 2 variables:
#' \describe{
#' \item{year}{year of catch}
#' \item{catch}{weight of catch in 1,000 t}
#' }
"goa_rebs_catch_1977_2004"
