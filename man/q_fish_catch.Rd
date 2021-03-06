% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/q_fish_catch.R
\name{q_fish_catch}
\alias{q_fish_catch}
\title{fishery catch data query}
\usage{
q_fish_catch(year, fishery = "fsh1", species, area, akfin, save = TRUE)
}
\arguments{
\item{year}{assessment year}

\item{fishery}{default is fsh1, change if age comps from multiple fisheries}

\item{species}{group species code}

\item{area}{GOA or BSAI}

\item{akfin}{the database to query}

\item{save}{save the file in designated folder}
}
\value{

}
\description{
fishery catch data query
}
\examples{
\dontrun{q_fish_catch(year, fishery = "fsh1", species, area, akfin, save = TRUE)
}
}
