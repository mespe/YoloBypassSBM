% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/passage_survival.R
\name{passage_survival}
\alias{passage_survival}
\title{Passage survival from Fremont Weir to Chipps Island}
\usage{
passage_survival(abundance, fork_length, flow, route, sim_type)
}
\arguments{
\item{abundance}{Abundance of cohort on day route entered}

\item{fork_length}{Fork length (mm) at Fremont Weir}

\item{flow}{Flow (cfs) at Freeport on day route entered}

\item{route}{Route: Sacramento River (Sac) or Yolo Bypass (Yolo)}

\item{sim_type}{Simulation type: deterministic or stochastic}
}
\description{
Passage survival from Fremont Weir to Chipps Island based on fork length, flow, and route
}
\examples{
passage_survival(1000, 70, 60000, "Sac", "deterministic")
passage_survival(1000, 70, 60000, "Yolo", "deterministic")

}
