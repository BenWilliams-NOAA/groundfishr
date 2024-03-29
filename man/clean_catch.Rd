% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_catch.R
\name{clean_catch}
\alias{clean_catch}
\title{clean up catch data}
\usage{
clean_catch(
  year,
  species,
  fishery = "fsh",
  TAC = c(3333, 2222, 1111),
  discard = NULL,
  gear = NULL,
  fixed_catch = NULL,
  save = TRUE
)
}
\arguments{
\item{year}{year of assessment}

\item{species}{species of interest e.g., "SABL", "DUSK"}

\item{fishery}{identify the fishery default is "fsh"}

\item{TAC}{last three TAC in form: c(year-3, year-2, year-1)}

\item{discard}{if summarizing catch by discard/retained is desired change to TRUE}

\item{gear}{if summarizing catch by gear type is desired change to TRUE}

\item{fixed_catch}{if early catch is frozen place the file in user_input folder (format: Year, Catch)}

\item{save}{}
}
\value{

}
\description{
clean up catch data
}
\examples{
\dontrun{
clean_catch(year, TAC = c(2874, 2756, 3100))
}

}
