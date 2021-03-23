#' Sablefish historical catch data for 1960-1990
#'
#' A dataset containing both fixed gear and trawl gear catch by year
#'
#' @format A data frame with 62 observations and 3 variables:
#' \describe{
#' \item{year}{year of catch}
#' \item{catch}{weight of catch in 1,000 t}
#' \item{type}{fishery type is either fixed (includes longline and pot gear) or trawl}
#' }
"sabl_catch_1960_1990"

#' Sablefish historical CPUE relative population weight (RPW) from the Japanese longline fishery for 1964-1981
#'
#' A dataset containing both cpue data by year
#'
#' @format A data frame with 18 observations and 5 variables:
#' \describe{
#' \item{year}{year of cpue}
#' \item{rpw}{relative population weight}
#' \item{se}{the standard error of rpw}
#' \item{lci}{lower confidence interval: 2 * se}
#' \item{uci}{upper confidence interval: 2 * se}
#' }
"sabl_japan_llf_rpw"
