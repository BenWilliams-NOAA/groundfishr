#' Sablefish historical abundance data
#'
#' A dataset containing both fixed gear and trawl gear catch, cpue, rpn & rpw data by year
#'
#' @format A data frame with 121 observations and 5 variables:
#' \describe{
#' \item{year}{year}
#' \item{value}{weight of catch in 1,000 t, or relative abundance}
#' \item{variable}{rpw, rpn, cpue, or catch}
#' \item{fleet}{japan or domestic}
#' \item{gear}{lls = longline survey, llf = longline fishery, tf = trawl fishery}
#' }
"sabl_fixed_abundance"

#' Sablefish age and length comp data
#'
#' A dataset containing both age and length composition data for multiple fisheries and surveys
#'
#' @format A data frame with 2010 observations and 7 variables:
#' \describe{
#' \item{year}{year}
#' \item{age}{if relevant, otherwise NA}
#' \item{comp}{age or length}
#' \item{fleet}{japan or domestic}
#' \item{type}{age or length}
#' \item{gear}{lls = longline survey, tf = trawl fishery, ts = trawl survey}
#' \item{sex}{male, female or NA}
#' }
"sabl_fixed_comps"

#' Sablefish fixed weights at age
#'
#' A dataset containing weights at age for two time blocks, by sex
#'
#' @format A data frame with 120 observations and 3 variables:
#' \describe{
#' \item{waa}{length bin groups}
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
