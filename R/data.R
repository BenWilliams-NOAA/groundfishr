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

#' Sablefish historical CPUE relative population weight (RPW) and relative population numbers (RPN) from the Japanese cooperative longline fishery for 1979-1994
#'
#' A dataset containing both cpue data by year
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
