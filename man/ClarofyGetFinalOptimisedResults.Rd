% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all-in-one-functions.R
\name{ClarofyGetFinalOptimisedResults}
\alias{ClarofyGetFinalOptimisedResults}
\title{Get the optimised results}
\usage{
ClarofyGetFinalOptimisedResults(url, id)
}
\arguments{
\item{url}{API url}

\item{id}{Session id created by \link{CreateSession}}
}
\description{
Returns the optimised results from the all-in-one optimisation workflow
}
\section{Optimisation Workflow}{
\itemize{
\item Returns the optimised results from the all-in-one optimisation workflow
\item Returns a time series plot of the predictions
}
}

\examples{

\dontrun{

FinalOptimisedPredictions <-
ClarofyGetFinalOptimisedResults(url = url, id = sessionID)

}

}
